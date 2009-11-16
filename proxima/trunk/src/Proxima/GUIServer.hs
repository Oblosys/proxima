{-# LANGUAGE CPP #-} 
module Proxima.GUIServer where

import Common.CommonTypes ( DebugLevel (..), debug, showDebug, showDebug', debugIO, debugLnIO
                          , Settings (..) )
import qualified Common.CommonTypes as CommonTypes

import Common.CommonUtils
import Rendering.RenTypes
import Layout.LayTypes
import System.IO
import Data.IORef
import Proxima.Wrap

import Data.Time.Clock
import Control.Exception
import Data.Char
import System.Directory
{- HAppS -}
import HAppS.Server hiding (unwrap)
import HAppS.Server.SimpleHTTP
import HAppS.State
import System.Environment
import Data.Time
import System.Locale
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Control.Monad.Trans
import Data.List

{- End of HApps imports -}

{- Salvia imports 
import Data.Maybe
import Data.Record.Label
--import Misc.Misc
import Control.Concurrent.STM
import Control.Monad.State
import Network.Protocol.Http hiding (server)
import Network.Protocol.Uri
import Network.Salvia.Httpd
import Network.Salvia.Handlers
 End of Salvia imports -}


import Control.Concurrent
import Data.List
import Evaluation.DocTypes (DocumentLevel, EditDocument'_ (..))
import Arrangement.ArrTypes
import Presentation.PresTypes (UpdateDoc)
import Evaluation.EnrTypes -- why doesn't this work?: (SaveFileEnr)
import System.Environment
import Control.Concurrent
import System.Time
import Data.Typeable hiding (cast)
import Control.Monad.Trans
import Control.Monad hiding (when)
import Control.Monad.Writer hiding (when)
import Data.List

initialize (settings,handler,renderingLvlVar,viewedAreaRef,_) = 
 do { fh <- openFile "queriedMetrics.txt" WriteMode
    ; hPutStr fh ""
    ; hClose fh

    ; fh' <- openFile "metricsQueries.txt" WriteMode
    ; hPutStr fh' ""
    ; hClose fh'
    }              

-- withCatch is identity in GUIServer, it is defined only in the GUIGtk module.
withCatch io = io

startEventLoop params@(settings,h,rv,vr) = withProgName "proxima" $
 do { initR <- newIORef (True)
    ; menuR <- newIORef []
    ; actualViewedAreaRef <- newIORef ((0,0),(0,0)) -- is used when reducing the viewed area, see mkSetViewedAreaHtml
    ; currentSessionsRef <- newIORef []
    ; serverInstanceId <- fmap (formatTime defaultTimeLocale "%s") getCurrentTime
    ; putStrLn $ "Starting Proxima server on port " ++ show (serverPort settings) ++ "."
    ; let startServer = server params initR menuR actualViewedAreaRef serverInstanceId currentSessionsRef

    ; hSetBuffering stdin NoBuffering
    ; stdInAvailable <- do { hReady stdin
                           ; return True
                           } `Control.Exception.catch` \(err :: SomeException) -> return False
    -- this seems to be the only way to determine whether stdin is EOF!
    -- using hIsEOF works if it is EOF, but hangs because of buffering issues when stdin is not EOF
    
    ; if stdInAvailable
      then -- if we have stdin, start server in a thread and wait for return in this one
       do { tId <- forkIO $ startServer
          ; putStrLn "Press <Return> to terminate server."
          ; getLine
          ; killThread tId    
          }
      else -- no stdin, so execute server in main thread. Server stops when process is killed
       do { putStrLn "No stdin: server process will continue indefinitely."
          ; startServer
          }
    }

{-
HAPPS
Server error: Prelude.last: empty list
is the error you get when fileServe cannot find a file

ServerPart is basically a Reader monad for requests

The Ok part of the WebT monad contains a function out that is applied to the responses before
sending them to the client. If the result is of type Response, set/addHeader can be fmapped to
the monad, but it will only do something if the header is not set in the out part of Ok.

Header modifications must therefore be applied to out rather than be fmapped to the monad.
-}

server params@(settings,_,_,_) initR menuR actualViewedAreaRef serverInstanceId currentSessionsRef =
  simpleHTTP (Conf (serverPort settings) Nothing) 
             (sessionHandler params initR menuR actualViewedAreaRef serverInstanceId currentSessionsRef)
{-
handle:
http://<server url>/                    response: <proxima executable dir>/src/proxima/scripts/Editor.xml
http://<server url>/favicon.ico         response: <proxima executable dir>/src/etc/favicon.ico
http://<server url>/img/<filename>      response: <proxima executable dir>/img/<filename>
http://<server url>/handle?commands=<commands separated by ;>                    
                                        response: from handleCommands

TODO: The proxima server requires that the proxima directory is present for favicon and 
      Editor.xml, these files should be part of a binary distribution.
-}

modifyResponseSP :: (Response -> Response) -> ServerPart a -> ServerPart a
modifyResponseSP modResp (ServerPartT f) =
  withRequest $ \rq -> modifyResponseW modResp $ f rq
    
modifyResponseW modResp w =
 do { a <- w
    ; modifyResponse modResp
    ; return a
    }
    
noCache :: Response -> Response  
noCache = addHeader "Expires" "Mon, 28 Jul 2000 11:24:47 GMT"
-- TODO: figure out if noCache is really necessary, both for editor.xml and handle
-- It does not work for IE
 
withAgentIsMIE f = withRequest $ \rq -> 
                     (unServerPartT $ f ("MSIE" `isInfixOf` (show $ getHeader "user-agent" rq))) rq
                     -- not the most elegant method of checking for Internet explorer

                     -- IE does not support SVG and XHTML
                     -- XHTML is not a big problem, but for SVG we need an alternative
                     -- Maybe we also need to switch to POST for IE, since it
                     -- cannot handle large queries with GET


-- todo lookup of viewed area is bad (with head) and what about actualViewedArea?
-- TODO: add thread safety!!!
-- if we put Arrangement layer and maybe level in session (by indexing), incrementality should be back
-- then also rendering level (may be easy) and presentation focus and maybe arrangement focus must be indexed
-- then we have multi editing!
-- in case of multiple sessions, editors should poll every .. seconds

-- bug. declaration form becomes weird after single session timeout
sessionHandler params@(settings,handler,renderingLvlVar, viewedAreaRef) initR menuR actualViewedAreaRef 
               serverInstanceId currentSessionsRef = 
  [ do { removeExpiredSessions currentSessionsRef
       ; (sessionId,viewedArea) <- getCookieSessionId serverInstanceId currentSessionsRef
       ; currentSessions <- liftIO $ readIORef currentSessionsRef
               
       ; let isPrimarySession = case currentSessions of
                                  [] -> False -- should not occur
                                  (i,_,_):_ -> i == sessionId

       ; if isPrimarySession
         then liftIO $ putStrLn "\n\nPrimary editing session"
         else liftIO $ putStrLn "\n\nSecondary editing session"
       ; liftIO $ putStrLn $ "Session "++show sessionId ++", all sessions: "++ show (currentSessions) 
       ; liftIO $ writeIORef viewedAreaRef viewedArea

       ; liftIO $ putStrLn $ "Viewed area for this session: " ++ show viewedArea

       ; response <- multi $ handlers params initR menuR actualViewedAreaRef sessionId isPrimarySession (length currentSessions)
       ; viewedArea' <- liftIO $ readIORef viewedAreaRef
       ; liftIO $ putStrLn $ "And now viewed area is " ++ show viewedArea'
       ; liftIO $ writeIORef currentSessionsRef $ 
                            [ (i, t, if i == sessionId then viewedArea' else v)
                            | (i,t,v) <- currentSessions 
                            ]
       ; return response
       } ]
                     
handlers params@(settings,handler,renderingLvlVar,viewedAreaRef) initR menuR actualViewedAreaRef 
         sessionId isPrimarySession nrOfSessions = 
  debugFilter $
  [ withAgentIsMIE $ \agentIsMIE ->
      (methodSP GET $ do { -- liftIO $ putStrLn $ "############# page request"
                           let setTypeToHTML = if agentIsMIE 
                                               then setHeader "Content-Type" "text/html"
                                               else id
                                           
                         ; let filePath = "src/proxima/scripts/Editor.xml"
                         ; exist <- liftIO $ doesFileExist filePath
                         ; if exist then return () else error $ "File not found: " ++ filePath  
                                    
                         ; modifyResponseSP (noCache. setTypeToHTML) $
                              fileServe [] filePath
                         })
                 

  , dir "img"
        [ fileServe [] "img" ]  
  , dir "favicon.ico"
        [ methodSP GET $ fileServe ["favicon.ico"] "src/proxima/etc"]
  , dir "Document.xml"
        [ methodSP GET $ do { _<- liftIO $ genericHandler settings handler renderingLvlVar viewedAreaRef () $ 
                                     castEnr $ SaveFileEnr "Document.xml" 
                              -- ignore the html rendering of the save command (is empty)
                            ; modifyResponseSP (setHeader "Content-Disposition" "attachment;") $
                                fileServe ["Document.xml"] "."
                            }
        ]
  , dir "upload"
        [ withData $ \(Upl doc) -> 
        [ method POST $
           do { when (doc /= "") $ liftIO $
                 do { fh <- openFile "Document.xml" WriteMode
                    ; hPutStrLn fh doc
                    ; hClose fh
                    ; genericHandler settings handler renderingLvlVar viewedAreaRef () $ 
                        castEnr $ OpenFileEnr "Document.xml" 
                      -- ignore html output, the page will be reloaded after pressing the button
                    ; return ()
                    }
                      -- simply serving the Editor.xml does not work, as the browser will have upload in its menu bar
                      -- (also the page doesn't load correctly)
                      -- Instead, we show a page that immediately goes back to the editor page
              ; let responseHtml =
                      "<html><head><script type='text/javascript'><!--\n" ++
                      "history.go(-1);" ++
                      "\n--></script></head><body></body></html>"
                      -- newlines between <!-- & javascript and javascript and --> are necessary!!
                      --"<html><body>Document has been uploaded.<p><button onclick=\"location.href='/'\">Return to editor</button></html>"
              ; modifyResponseW (setHeader "Content-Type" "text/html") $ -- todo there must be a nicer way to get an html response
                  ok $ toResponse responseHtml
                         
              }
        ]
        ]
  , dir "handle" 
   [ withData (\cmds -> [ methodSP GET $ 
                          do { liftIO $ putStrLn $ "Command received " ++ take 60 (show cmds)

                             ; responseHtml <-
                                 liftIO $ catchExceptions $ handleCommands params initR menuR actualViewedAreaRef
                                                                           sessionId isPrimarySession nrOfSessions
                                                                           cmds
--                             ; liftIO $ putStrLn $ "\n\n\n\ncmds = "++show cmds
--                             ; liftIO $ putStrLn $ "\n\n\nresponse = \n" ++ show responseHTML
                             

                             ; seq (length responseHtml) $ return ()
                             ; liftIO $ putStrLn $ "Sending response sent to client:\n" ++
                                                   take 160 responseHtml ++ "..."
                             --; modifyResponseW noCache $
                             ;  anyRequest $ ok $ toResponse responseHtml 
                             }
                          
                        ])
   ] 
  ]

catchExceptions io =
  io `Control.Exception.catch` \(exc :: SomeException) ->
       do { let exceptionText = 
                  "\n\n\n\n###########################################\n\n\n" ++
                  "Exception: " ++ show exc ++ "\n\n\n" ++
                  "###########################################" 
          
          ; putStrLn exceptionText
          ; let responseHTML = "<div id='updates'><div id='alert' op='alert' text='"++filter (/='\'') exceptionText++"'></div></div>"
                
          ; return responseHTML
          }

type ServerInstanceId = String
type SessionId = Int
type Sessions = [(SessionId, UTCTime, CommonTypes.Rectangle)]

cookieLifeTime = sessionExpirationTime + 60 -- only needs to be as long as sessionExpirationTime

sessionExpirationTime = 30

removeExpiredSessions :: IORef Sessions -> ServerPart ()
removeExpiredSessions currentSessionsRef = liftIO $
 do { time <- getCurrentTime
    ; currentSessions <- readIORef currentSessionsRef
    ; writeIORef currentSessionsRef $
        filter (\(_,lastSessionEventTime,_) -> diffUTCTime time lastSessionEventTime < sessionExpirationTime) currentSessions 
    }

getCookieSessionId :: ServerInstanceId -> IORef Sessions -> ServerPart (SessionId, CommonTypes.Rectangle)
getCookieSessionId serverInstanceId currentSessionsRef = withRequest $ \rq ->
 do { let cookieMap = rqCookies rq
      -- TODO: happs cookie parser sometimes fails on weird cookies.
      --       we should parse the cookie header ourselves here 

    ; let mCookieSessionId = case lookup "proxima" cookieMap of
                      Nothing -> Nothing -- * no webviews cookie on the client
                      Just c  -> case safeRead (cookieValue c) of
                                   Nothing               -> Nothing -- * ill formed cookie on client
                                   Just (cookieServerInstanceId::String,key::Int) -> 
                                     if cookieServerInstanceId /= serverInstanceId
                                     then Nothing  -- * cookie from previous Proxima run
                                     else Just key -- * correct cookie for this run

    ; currentSessions <- liftIO $ readIORef currentSessionsRef

    ; (sessionId, _, viewedArea) <-
        case mCookieSessionId of
          Just cookieSessionId | cookieSessionId `elem` map fst3 currentSessions ->
            do { time <- liftIO $  getCurrentTime
               ; liftIO $ writeIORef currentSessionsRef $ 
                            [ (i, if i == cookieSessionId then time else t,v)
                            | (i,t,v) <- currentSessions 
                            ]
               ; addCookie 60 $ mkCookie "proxima" $ show (serverInstanceId, cookieSessionId)
               ; let viewedArea = thd3 . head $ filter ((==cookieSessionId).fst3) currentSessions 
               ; return (cookieSessionId, time, viewedArea)
               }

          -- no or wrong cookie, or sessionId is not in currentSessions (because it expired)
          _ -> makeNewSessionCookie serverInstanceId currentSessionsRef
    ; liftIO $ putStrLn $ "SessionId:" ++ show sessionId
    ; return (sessionId, viewedArea)
    } 

makeNewSessionCookie serverInstanceId currentSessionsRef =
 do { currentSessions <- liftIO $ readIORef currentSessionsRef
    ; time <- liftIO $  getCurrentTime
    
    ; let newViewedArea = ((0,0),(0,0))
          newSessionId = maximum (0: map fst3 currentSessions) + 1
          newSession = (newSessionId, time, newViewedArea)
    ; addCookie 60 $ mkCookie "proxima" $ show (serverInstanceId, newSessionId)
    -- one minute
    
    ; liftIO $ writeIORef currentSessionsRef $ currentSessions ++ [newSession]
    ; return newSession
    }


whenPrimary isPrimarySession act = if not isPrimarySession then return [] else act
     
 
newtype Upl = Upl String deriving Show

instance FromData Upl where
  fromData = liftM Upl (look "documentFile")


data Commands = Commands Int String deriving (Show, Read)

instance FromData Commands where
  fromData = liftM parseCommands (look "commands")
    where parseCommands str = case safeRead str of
                                Just cmds -> cmds 
                                Nothing  -> error ("Syntax error in commands: "++str) 



splitCommands commandStr =
  case break (==';') commandStr of
    ([],[])             -> []
    (_, [])              -> error $ "Syntax error in commands: " ++ commandStr
    (command, (_:commandStr')) -> command : splitCommands commandStr'
        
-- handle each command in commands and send the updates back
handleCommands (settings,handler,renderingLvlVar,viewedAreaRef) initR menuR actualViewedAreaRef
               sessionId isPrimarySession nrOfSessions (Commands requestId commandStr) =
 do { let commands = splitCommands commandStr
   -- ; putStrLn $ "Received commands:"++ show commands
    
    ; renderingHTMLss <-
        mapM (handleCommandStr (settings,handler,renderingLvlVar,viewedAreaRef) initR menuR actualViewedAreaRef
                               sessionId isPrimarySession nrOfSessions)
             commands
 
    ; let renderingHTML = concat . concat $ renderingHTMLss

    ; focusRenderingHTML <- drawFocusHTML settings renderingLvlVar viewedAreaRef 



    ; pendingQueriesTxt <-  readFile "metricsQueries.txt"
    --; putStrLn $ "About to send following metrics queries to client:\n"++pendingQueriesTxt 
    ; seq (length pendingQueriesTxt) $ return ()
    ; let pendingQueries = map read $ lines pendingQueriesTxt :: [(String, Int, Bool, Bool)]
          queryHTML = concat [ "<div op='metricsQuery' family='"++fam++"' size='"++show sz++"' "++
                               "isBold='"++ show isBold ++"' "++
                               "isItalic='"++ show isItalic ++"' "++
                               "></div>" 
                             | (fam,sz,isBold,isItalic) <- pendingQueries]
                      ++ if null pendingQueries then "" else "<div op='refresh'></div>"

    
    ; fh <- openFile "metricsQueries.txt" WriteMode
    ; hClose fh
    
--                  ; return $ "<div id='updates'>"++testRenderingHTML++"</div>"
--    ; if null pendingQueries then putStrLn "Sending rendering and focus" else return ()
    ; return $ "<div id='updates' sessionId='" ++ show sessionId ++"' "++ 
                                 "responseId='" ++ show requestId ++"' "++ 
                                 "sessionType='" ++ (if isPrimarySession then "primary" else "secondary") ++"' "++
                                 "nrOfSessions='"++show nrOfSessions ++ "'>" ++ 
                                      (if null pendingQueries 
                                       then renderingHTML++focusRenderingHTML
                                       else "") 
                                   ++queryHTML++"</div>"            
    }
        
data Command = Metrics ((String,Int,Bool,Bool),(Int,Int,[Int]))
             | ContextMenuRequest ((Int,Int),(Int,Int))
             | ContextMenuSelect Int  
             | Key (Int,Modifiers)
             | Chr (Int,Modifiers)
             | Mouse MouseCommand (Int,Int, Modifiers)
             | EditStyle StyleEdit 
             | Find String
             | SetViewedArea CommonTypes.Rectangle
             | ClearMetrics 
               deriving (Show, Read)
                        
type Modifiers = (Bool,Bool,Bool)
data MouseCommand = MouseDown | MouseMove | MouseUp | MouseDragStart | MouseDrop
                    deriving (Show, Read)

handleCommandStr (settings,handler,renderingLvlVar,viewedAreaRef) initR menuR actualViewedAreaRef
                 sessionId isPrimarySession nrOfSessions eventStr =
  case safeRead eventStr of
    Just cmd -> handleCommand (settings,handler,renderingLvlVar,viewedAreaRef) initR menuR actualViewedAreaRef
                sessionId isPrimarySession nrOfSessions cmd
    Nothing  -> error ("Syntax error in command: "++eventStr) 


handleCommand :: (Show token, Show node, Show enr, Show doc) => 
               (Settings
               ,((RenderingLevel doc enr node clip token, EditRendering doc enr node clip token) -> IO (RenderingLevel doc enr node clip token, [EditRendering' doc enr node clip token]))
               , IORef (RenderingLevel doc enr node clip token) 
               , IORef CommonTypes.Rectangle 
               ) -> IORef Bool -> IORef [Wrapped doc enr node clip token] -> IORef CommonTypes.Rectangle ->
               SessionId -> Bool -> Int ->
               Command -> IO [String]
handleCommand (settings,handler,renderingLvlVar,viewedAreaRef) initR menuR actualViewedAreaRef
              sessionId isPrimarySession nrOfSessions command =
  case command of
    Metrics receivedMetrics@(font,_) ->
     do { putStrLn $ "Received metrics for: "++show font
        ; fh' <- openFile "queriedMetrics.txt" AppendMode
        ; hPutStrLn fh' $ show receivedMetrics
        ; hClose fh'
        ; return [""]
        }
    
    -- Current structure of handlers causes focus to be repainted after context request
    -- this is not really a problem though
    ContextMenuRequest ((proxX,proxY),(screenX,screenY)) ->
     whenPrimary isPrimarySession $ 
     do { html <- genericHandler settings handler renderingLvlVar viewedAreaRef () $
                    castLay ParseLay
        ; setViewedAreaHtml <- mkSetViewedAreaHtml settings viewedAreaRef actualViewedAreaRef
        
        ; (RenderingLevel _ makePopupMenuHTML _ _ _ _ _ _)  <- readIORef renderingLvlVar
                                                                   
        ; let (itemStrs,upds) = unzip $ makePopupMenuHTML proxX proxY
              itemsHTML = concat 
                            [ "<div class='menuItem' item='"++show i++"'>"++item++"</div>"
                            | (i,item) <- zip [0..] itemStrs
                            ]
          -- for separator lines: "<hr></hr>"
        
        ; writeIORef menuR upds
                                
        ; return $ html ++ [setViewedAreaHtml] ++
                   [ "<div op='contextMenu' screenX='"++show screenX++"' screenY='"++show screenY++"'>" ++
                   itemsHTML ++ "</div>" ]
        }
    
    ContextMenuSelect selectedItemNr ->
     whenPrimary isPrimarySession $
     do { menuItems <- readIORef menuR
        ; let editDoc = index "GUI.handleContextMenuSelect" menuItems selectedItemNr
        ; genericHandler settings handler renderingLvlVar viewedAreaRef () $
            unwrap editDoc
        }

    Key (keyCode,(shiftDown, ctrlDown, altDown)) ->
      let ms = CommonTypes.Modifiers shiftDown ctrlDown altDown
          evt = case keyCode of
                  46 -> KeySpecialRen CommonTypes.DeleteKey ms
                  8  -> KeySpecialRen CommonTypes.BackspaceKey ms
                  37 -> KeySpecialRen CommonTypes.LeftKey  ms
                  39 -> KeySpecialRen CommonTypes.RightKey  ms
                  38 -> KeySpecialRen CommonTypes.UpKey  ms
                  40 -> KeySpecialRen CommonTypes.DownKey  ms
                  13 -> KeySpecialRen CommonTypes.EnterKey  ms
                  112 -> KeySpecialRen CommonTypes.F1Key  ms
                  113 -> KeySpecialRen CommonTypes.F2Key  ms
                  114 -> KeySpecialRen CommonTypes.F3Key  ms
                  115 -> KeySpecialRen CommonTypes.F4Key  ms
                  116 -> KeySpecialRen CommonTypes.F5Key  ms
                  117 -> KeySpecialRen CommonTypes.F6Key  ms
                  118 -> KeySpecialRen CommonTypes.F7Key  ms
                  119 -> KeySpecialRen CommonTypes.F8Key  ms
                  120 -> KeySpecialRen CommonTypes.F9Key  ms
                  121 -> KeySpecialRen CommonTypes.F10Key  ms
                  122 -> KeySpecialRen CommonTypes.F11Key  ms
                  123 -> KeySpecialRen CommonTypes.F12Key  ms
                  _  -> SkipRen 0
      in  do { html <- genericHandler settings handler renderingLvlVar viewedAreaRef () evt
             ; setViewedAreaHtml <- mkSetViewedAreaHtml settings viewedAreaRef actualViewedAreaRef
             ; return $ html ++ [setViewedAreaHtml]
             }
    
    Chr (keyChar,(shiftDown, ctrlDown, altDown)) ->
      whenPrimary isPrimarySession $
      let ms = CommonTypes.Modifiers shiftDown ctrlDown altDown
          evt = if not ctrlDown && not altDown 
                then KeyCharRen (chr keyChar)
                else KeySpecialRen (CommonTypes.CharKey (toLower $ chr keyChar)) ms

      in  do { html <- genericHandler settings handler renderingLvlVar viewedAreaRef () evt
             ; setViewedAreaHtml <- mkSetViewedAreaHtml settings viewedAreaRef actualViewedAreaRef
             ; return $ html ++ [setViewedAreaHtml]
             }

-- TODO: add guaranteeFocusInView edit op at ArrTranslate, so we can take into account the kind of edit operation.
--       Now, it can go wrong if the focus is extended to the left or up.
    -- (note: maybe not, focus is always from to.)    
    
    Mouse mouseCommand (x, y, (shiftDown, ctrlDown, altDown)) ->
      whenPrimary isPrimarySession $
      let ms = CommonTypes.Modifiers shiftDown ctrlDown altDown
          evt = case mouseCommand of
                     MouseDown -> MouseDownRen x y ms 1
                     MouseUp -> MouseUpRen x y ms
                     MouseMove -> MouseDragRen x y ms
                     -- move events are only sent when button is down, to prevent flooding    
                     MouseDragStart -> DragStartRen x y 
                     MouseDrop -> DropRen x y
                     
      in  do { html <- genericHandler settings handler renderingLvlVar viewedAreaRef () evt
             ; setViewedAreaHtml <- mkSetViewedAreaHtml settings viewedAreaRef actualViewedAreaRef
             ; return $ html ++ [setViewedAreaHtml]
             }
    
    EditStyle style ->
      whenPrimary isPrimarySession $
      do { html <- genericHandler settings handler renderingLvlVar viewedAreaRef () $ 
                     castLay $ EditStyleLay style
         ; setViewedAreaHtml <- mkSetViewedAreaHtml settings viewedAreaRef actualViewedAreaRef
         ; return $ html ++ [setViewedAreaHtml]
         }

    Find str ->
      whenPrimary isPrimarySession $
      do { html <- genericHandler settings handler renderingLvlVar viewedAreaRef () $ 
                     castLay $ FindLay (Just str)
         ; setViewedAreaHtml <- mkSetViewedAreaHtml settings viewedAreaRef actualViewedAreaRef
         ; return $ html ++ [setViewedAreaHtml]
         }

    SetViewedArea newViewedArea ->
     do { writeIORef viewedAreaRef newViewedArea
        ; writeIORef actualViewedAreaRef newViewedArea
        ; reduceViewedArea settings viewedAreaRef
        ; genericHandler settings handler renderingLvlVar viewedAreaRef () $
            SkipRen (-2)
        }

    ClearMetrics ->
     whenPrimary isPrimarySession $
     do { putStrLn "\n\n\n\n\n\n\nClear\n\n\n\n\n\n\n"
        ; fh <- openFile "queriedMetrics.txt" WriteMode -- TODO: clearing this file should be done after Metrics are read in FontLib.hs
        ; hClose fh
        ; genericHandler settings handler renderingLvlVar viewedAreaRef () $ 
            castArr $ ClearMetricsArr
        }
     
genericHandler :: (Show token, Show node, Show enr, Show doc) => Settings ->
               ((RenderingLevel doc enr node clip token, EditRendering doc enr node clip token) -> IO (RenderingLevel doc enr node clip token, [EditRendering' doc enr node clip token])) ->
               IORef (RenderingLevel doc enr node clip token) -> IORef CommonTypes.Rectangle -> 
               () -> -- is here so the type is compatible with genericHandler from GUIGtk
               EditRendering doc enr node clip token -> IO [String]
genericHandler settings handler renderingLvlVar viewedAreaRef () evt =   
 do { renderingLvl <- readIORef renderingLvlVar
    ; putStrLn $ "Generic handler server started for edit op: " ++ show evt
    ; viewedArea <- readIORef viewedAreaRef
    ; putStrLn $ "Viewed area that is about to be rendered: " ++ show viewedArea
          
    ; (renderingLvl', editsRendering) <- handler (renderingLvl,evt)
    ; htmlRenderings <- mapM process editsRendering
    ; return $ concat htmlRenderings
    }
 where process (SkipRen' _) = return [""]
       process (WrapRen' w) = do { htmlRenderings <- genericHandler settings handler renderingLvlVar viewedAreaRef () $ Proxima.Wrap.unwrap w
                                 ; return $ htmlRenderings
                                 }
       process (AlertRen' str) = return [ "<div id='alert' op='alert' text='"++
                                          filter (/='\'') str ++"'></div>" 
                                        ]
       -- TODO: clear any following updates to client? The alert will prevent them from being processed for a while.
       process (SetRen' renderingLvl''@(RenderingLevel scale _ renderingHTML _ (newW,newH) _ updRegions _)) =
         do { writeIORef renderingLvlVar renderingLvl''
            ; viewedArea <- readIORef viewedAreaRef
            ; let htmlRendering = execWriter $ renderingHTML viewedArea
            ; return [htmlRendering]
            }
    

drawFocusHTML settings renderingLvlVar viewedAreaRef = 
 do { RenderingLevel scale _ _ focusRenderingHTML (w,h) debug updRegions _ <- readIORef renderingLvlVar
    ; viewedArea <- readIORef viewedAreaRef
    ; let htmlFocusRendering = execWriter $ focusRenderingHTML viewedArea
    ; return htmlFocusRendering
    }
 
reduceViewedArea :: Settings -> IORef CommonTypes.Rectangle -> IO ()
reduceViewedArea settings viewedAreaRef =
 do { ((x,y),(w,h)) <- readIORef viewedAreaRef
    ; if reducedViewedArea settings -- return a smaller viewed area, for testing incrementality algorithms.
      then writeIORef viewedAreaRef $ 
             ((x+ (w `div` 4),y + (h `div` 4)),(w `div` 2,h `div` 2))
      else return ()
    }                     

-- the actualViewedArea contains the viewedArea from the client. We need it because undoing the reduction is not possible
-- with only the viewedArea, as the div on the width and height is not invertible for uneven numbers.
mkSetViewedAreaHtml settings viewedAreaRef actualViewedAreaRef =
 do { ((_,_),(w,h)) <- readIORef actualViewedAreaRef
    ; ((x,y),(_,_)) <- readIORef viewedAreaRef
    ; let (x',y') = if reducedViewedArea settings
                    then (x- w `div` 4,y - h `div` 4)
                    else (x,y)
    --; putStr $ "set client viewed area: " ++ show (x,y)
    ; return $ "<div op='setViewedArea' x='"++show x'++"' y='"++show y'++"' w='"++show w++"' h='"++show h++"'></div>"
    }

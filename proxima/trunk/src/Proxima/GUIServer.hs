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
import qualified Data.ByteString.Char8 as BS
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
import Control.Concurrent.MVar
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
 do { mutex <- newMVar ()
    ; menuR <- newIORef []
    ; actualViewedAreaRef <- newIORef ((0,0),(0,0)) -- is used when reducing the viewed area, see mkSetViewedAreaHtml
    ; currentSessionsRef <- newIORef ([],0) -- list of active session and session id counter
                                           
    ; serverInstanceId <- mkServerInstanceId settings
    ; putStrLn $ "Starting Proxima server on port " ++ show (serverPort settings) ++ "."
    ; let startServer = server params mutex menuR actualViewedAreaRef serverInstanceId currentSessionsRef

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

-- Return string with epoch seconds, current picoseconds, and name of editor. Unique in most cases.
-- (unless the same editor is started twice within one picosecond)
mkServerInstanceId settings =
 do { currentTime <- getCurrentTime
    
    ; return $ formatTime defaultTimeLocale "%s" currentTime ++ "_" ++
               formatTime defaultTimeLocale "%q" currentTime ++ "_" ++
               safeStr (applicationName settings)
    }
 where safeStr [] = []
       safeStr (c:cs) | isAlpha c = c : safeStr cs
                      | otherwise = '_' : safeStr cs

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

server params@(settings,_,_,_) mutex menuR actualViewedAreaRef serverInstanceId currentSessionsRef =
  simpleHTTP (Conf (serverPort settings) Nothing) 
             (sessionHandler params mutex menuR actualViewedAreaRef serverInstanceId currentSessionsRef)
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

-- currently, having secondary edit sessions destroys incrementality
-- if we put Arrangement layer and maybe level in session (by indexing), incrementality should be back
-- then also rendering level (may be easy) and presentation focus and maybe arrangement focus must be indexed
-- then we have multi editing!

sessionHandler params@(settings,handler,renderingLvlVar, viewedAreaRef) mutex menuR actualViewedAreaRef 
               serverInstanceId currentSessionsRef = 
  [ do { liftIO $ putStrLn "Trying to obtain mutex"
       ; liftIO $ takeMVar mutex -- obtain mutex
       -- Proxima is not thread safe yet, so only one thread at a time is allowed to execute.
       ; liftIO $ putStrLn "Obtained mutex"
        
       ; removeExpiredSessions currentSessionsRef
       ; sessionId <- getCookieSessionId serverInstanceId currentSessionsRef
       ; (currentSessions, idCounter) <- liftIO $ readIORef currentSessionsRef
               
       ; let isPrimarySession = case currentSessions of
                                  [] -> False -- should not occur
                                  (i,_):_ -> i == sessionId

       ; if isPrimarySession
         then liftIO $ putStrLn "\n\nPrimary editing session"
         else liftIO $ putStrLn "\n\nSecondary editing session"
       ; liftIO $ putStrLn $ "Session "++show sessionId ++", all sessions: "++ show (currentSessions) 

       ; response <- multi $ handlers params menuR actualViewedAreaRef sessionId isPrimarySession (length currentSessions)

       ; liftIO $ writeIORef currentSessionsRef $ 
                          ( currentSessions 
                          , idCounter
                          )

       ; liftIO $ putStrLn "About to release mutex"
       ; liftIO $ putMVar mutex () -- release the mutex
       ; liftIO $ putStrLn "Mutex released"
       ; return response
       } ]
                     
handlers params@(settings,handler,renderingLvlVar,viewedAreaRef) menuR actualViewedAreaRef 
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
  , dir "etc"
        [ fileServe [] "src/proxima/etc" ]  
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
                             --; liftIO $ putStrLn "Pausing.."
                             --; liftIO $ threadDelay 1000000
                             ; liftIO $ putStrLn "Done"
                             ; (responseHtml,responseLength) <-
                                 liftIO $ catchExceptions $
                                   do { html <- handleCommands params menuR actualViewedAreaRef
                                                               sessionId isPrimarySession nrOfSessions
                                                               cmds
                                      ; let responseLength = length html 
                                      ; seq responseLength $ return ()
                                      ; return (html, responseLength)
                                      } -- kind of tricky, we need to make sure that the html string is evaluated here, so
                                        -- any possible exceptions are thrown and caught. If not, HApps silently sends the
                                        -- exception text as the html response :-(

--                             ; liftIO $ putStrLn $ "\n\n\n\ncmds = "++show cmds
--                             ; liftIO $ putStrLn $ "\n\n\nresponse = \n" ++ show responseHTML
                             ; liftIO $ putStrLn $ "Sending response sent to client (" -- ++show responseLength++ ")" 
                                                   ++ {-take 160-} show responseLength
                             --; modifyResponseW noCache $
                             ;  anyRequest $ ok $ toResponse responseHtml 
                             }
                          
                        ])
   ] 
  ]

-- NOTE: this does not catch syntax errors in the fromData on Commands, as these are handled before we get in the IO monad.
-- This only occurs when there is a problem with string quotes in the command string. If parsing fails, we get a happs server error:... 
-- The separate commands on the other hand are parsed safely.
catchExceptions io =
  io `Control.Exception.catch` \(exc :: SomeException) ->
       do { let exceptionText = 
                  "\n\n\n\n###########################################\n\n\n" ++
                  "Exception: " ++ show exc ++ "\n\n\n" ++
                  "###########################################" 
          
          ; putStrLn exceptionText
          ; let responseHTML = mkAlertResponseHTML exceptionText
                
          ; return (responseHTML, length responseHTML)
          }

mkAlertResponseHTML alertMsg =  "<div id='updates'><div id='alert' op='alert' text='"++filter (/='\'') alertMsg ++"'></div></div>"

type ServerInstanceId = String
type SessionId = Int
type Sessions = [(SessionId, UTCTime)]
type CurrentSessionsRef = IORef (Sessions, SessionId)


removeExpiredSessions :: CurrentSessionsRef -> ServerPart ()
removeExpiredSessions currentSessionsRef = liftIO $
 do { time <- getCurrentTime
    ; (currentSessions, idCounter) <- readIORef currentSessionsRef
    ; writeIORef currentSessionsRef $
        ( filter (\(_,lastSessionEventTime) -> diffUTCTime time lastSessionEventTime < sessionExpirationTime) currentSessions 
        , idCounter
        )
    }

getCookieSessionId :: ServerInstanceId -> CurrentSessionsRef -> ServerPart SessionId
getCookieSessionId serverInstanceId currentSessionsRef = withRequest $ \rq ->
 do { let mCookieSessionId = parseCookie serverInstanceId rq
    ; (currentSessions,idCounter) <- liftIO $ readIORef currentSessionsRef
--    ; liftIO $ putStrLn $ "parsed cookie id is " ++ show mCookieSessionId
    ; sessionId <-
        case mCookieSessionId of
          Just cookieSessionId | cookieSessionId `elem` map fst currentSessions ->
              -- if there is a cookie for this server instance and it's session id is in the current sessions
              -- then update it in the current sessions
            do { time <- liftIO $  getCurrentTime
               ; liftIO $ writeIORef currentSessionsRef $ 
                   ( [ (i, if i == cookieSessionId then time else t)
                     | (i,t) <- currentSessions 
                     ]
                   , idCounter
                   )
                 
               -- renew the cookie
               ; addCookie cookieLifeTime $ mkCookie (mkCookieName serverInstanceId) $ show cookieSessionId
               ; return cookieSessionId
               }

          -- no or wrong cookie, or sessionId is not in currentSessions (because it expired)
          _ -> makeNewSessionCookie serverInstanceId currentSessionsRef
    ; liftIO $ putStrLn $ "SessionId:" ++ show sessionId
    ; return sessionId
    } 



mkCookieName (serverInstanceId::ServerInstanceId) = "Proxima_"++serverInstanceId
-- The serverInstanceId (server start-up time in epoch seconds) is added to distinguish between
-- several editors being used in a single browser.

cookieLifeTime = sessionExpirationTime + 60 -- only needs to be as long as sessionExpirationTime

sessionExpirationTime = 60 -- make sure this value is equal to sessionExpirationTime in Editor.xml

-- Added a (primitive) cookie parser because Happs cookie parser is buggy when other cookies exist with _ in the cookie name 
parseCookie serverInstanceId rq = 
 do { case fmap (safeRead . show) $ getHeader "cookie" rq of
        Nothing -> Nothing  -- TODO: weird: this is not a ByteString but a Lazy.Internal.ByteString
                            -- Therefore we must do this stupid show safeRead thing instead of unpack
                            -- Even weirder is that it does work in GHC when importing ByteString.Char8, but not in scion
        Just Nothing -> Nothing -- should not occur, since show safeRead should always yield a string
        Just (Just (cookieHeader :: String)) -> getCookieFromHeader cookieHeader
    }
 where getCookieFromHeader [] = Nothing
       getCookieFromHeader xs@(_:xs') = 
         if not $ cookiePrefix `isPrefixOf` xs 
         then getCookieFromHeader xs'
         else case safeRead $ takeWhile (/= ';') $ drop (length cookiePrefix) xs of
                Nothing -> -- the pathological case that 'Proxima_SERVERID=' appears in the value of another cookie
                           getCookieFromHeader $ drop (length cookiePrefix) xs
                Just (valueStr :: String) ->
                  case safeRead valueStr of
                    Just (key::Int) -> return key
                    Nothing -> -- cannot occur, since 'Proxima_SERVERID=' followed by '".."' cannot appear in the value of another
                               -- cookie (the double quotes would have been escaped)
                               -- hence, when we have 'Proxima_SERVERID=".."' the .. will be a valid session key
                               getCookieFromHeader $ drop (length cookiePrefix) xs  
       cookiePrefix = (mkCookieName serverInstanceId)++"="

makeNewSessionCookie serverInstanceId currentSessionsRef =
 -- create a new session, put it in the current sessions, and send a cookie to the client
 do { (currentSessions,idCounter) <- liftIO $ readIORef currentSessionsRef
    ; time <- liftIO $  getCurrentTime
    
    ; let newSessionId = idCounter -- need a fresh unique id, otherwise an old cookie may contain the same id
          newSession = (newSessionId, time)
    ; addCookie cookieLifeTime $ mkCookie (mkCookieName serverInstanceId) $ show newSessionId
    
    ; liftIO $ writeIORef currentSessionsRef $ (currentSessions ++ [newSession], idCounter + 1)
    ; return newSessionId
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
handleCommands (settings,handler,renderingLvlVar,viewedAreaRef) menuR actualViewedAreaRef
               sessionId isPrimarySession nrOfSessions (Commands requestId commandStr) =
 do { let commands = splitCommands commandStr
    --; putStrLn $ "Received commands:"++ show commands
    
    ; renderingHTMLss <-
        mapM (handleCommandStr (settings,handler,renderingLvlVar,viewedAreaRef) menuR actualViewedAreaRef
                               sessionId isPrimarySession nrOfSessions)
             commands
 
    ; let renderingHTML = concat . concat $ renderingHTMLss

    ; focusRenderingHTML <- case map safeRead commands of
                              [Just (SetViewedArea _)] -> return ""
                              _                        -> drawFocusHTML settings renderingLvlVar viewedAreaRef 
    -- focus only needs to be rendered if there is a possibility that it changed, but in case of SetViewedArea, it
    -- even creates a problem if the focus is rendered, because on init, this command is given, but there is no rendering yet.
    -- Therefore, we don't render the focus if the command is a single SetViewedArea


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
             | Redraw
             | Rearrange
             | ClearMetrics 
               deriving (Show, Read)
                        
type Modifiers = (Bool,Bool,Bool)
data MouseCommand = MouseDown | MouseMove | MouseUp | MouseDragStart | MouseDrop
                    deriving (Show, Read)

handleCommandStr (settings,handler,renderingLvlVar,viewedAreaRef) menuR actualViewedAreaRef
                 sessionId isPrimarySession nrOfSessions eventStr =
  case safeRead eventStr of
    Just cmd -> handleCommand (settings,handler,renderingLvlVar,viewedAreaRef) menuR actualViewedAreaRef
                sessionId isPrimarySession nrOfSessions cmd
    Nothing  -> error ("Syntax error in command: "++eventStr) 


handleCommand :: (Show token, Show node, Show enr, Show doc) => 
               (Settings
               ,((RenderingLevel doc enr node clip token, EditRendering doc enr node clip token) -> IO (RenderingLevel doc enr node clip token, [EditRendering' doc enr node clip token]))
               , IORef (RenderingLevel doc enr node clip token) 
               , IORef CommonTypes.Rectangle 
               ) -> IORef [Wrapped doc enr node clip token] -> IORef CommonTypes.Rectangle ->
               SessionId -> Bool -> Int ->
               Command -> IO [String]
handleCommand (settings,handler,renderingLvlVar,viewedAreaRef) menuR actualViewedAreaRef
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

    -- Allow this F5 key also for secondary sessions to refresh (and initialize) the screen.
    -- This is a hack. TODO: make a special command for this.
    Key (116,(False, False, False)) ->
      let evt = KeySpecialRen CommonTypes.F5Key  (CommonTypes.Modifiers False False False)
      in  do { html <- genericHandler settings handler renderingLvlVar viewedAreaRef () evt
             ; setViewedAreaHtml <- mkSetViewedAreaHtml settings viewedAreaRef actualViewedAreaRef
             ; return $ html ++ [setViewedAreaHtml]
             }

    Key (keyCode,(shiftDown, ctrlDown, altDown)) ->
      whenPrimary isPrimarySession $
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
         ; seq (read "x" :: Int) $ return ()
         ; return $ html ++ [setViewedAreaHtml]
         }

    SetViewedArea newViewedArea ->
     do { writeIORef viewedAreaRef newViewedArea
        ; writeIORef actualViewedAreaRef newViewedArea
        ; reduceViewedArea settings viewedAreaRef
        ; return []
        }

    Redraw ->
     do { genericHandler settings handler renderingLvlVar viewedAreaRef () $
            SkipRen (-2)
        }

    Rearrange ->
     do { genericHandler settings handler renderingLvlVar viewedAreaRef () $
            SkipRen (-1)
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
       process (SetRen' renderingLvl''@(RenderingLevel scale _ renderingHTML _ (newW,newH) _ _ _)) =
         do { writeIORef renderingLvlVar renderingLvl''
            ; viewedArea <- readIORef viewedAreaRef
            ; let htmlRendering = execWriter $ renderingHTML viewedArea
            ; return [htmlRendering]
            }
    

drawFocusHTML settings renderingLvlVar viewedAreaRef = 
 do { RenderingLevel scale _ _ focusRenderingHTML (w,h) debug _ _ <- readIORef renderingLvlVar
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

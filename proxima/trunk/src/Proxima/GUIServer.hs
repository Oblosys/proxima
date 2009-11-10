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
import System.Time
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

initialize (settings,handler,renderingLvlVar,viewedAreaRef,initialWindowSize) = 
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
    ; actualViewedAreaRef <- newIORef ((0,0),(0,0))

    ; putStrLn $ "Starting Proxima server on port " ++ show (serverPort settings) ++ "."
    ; let startServer = server params initR menuR actualViewedAreaRef

    ; b <- hIsEOF stdin
    ; if b 
      then -- no stdin, so execute server in main thread. Server stops when process is killed
       do { putStrLn "No stdin"
          ; startServer
          }
      else -- if we have stdin, start server in a thread and wait for return in this one
       do { tId <- forkIO $ startServer
          ; putStrLn "Press <Return> to terminate server"
          ; getLine
          ; killThread tId    
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

server params@(settings,_,_,_) initR menuR actualViewedAreaRef =
  
  simpleHTTP (Conf (serverPort settings) Nothing) (handlers params initR menuR actualViewedAreaRef)
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
{-
overrideHeaders :: [(String,String)] -> ServerPart a -> ServerPart a
overrideHeaders headers s =
 do { response <- s
    ; modifyResponse (setHeader "Content-Type" "text/xml")
    ; return s
    } 
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
                     
-- handlers :: [ServerPartT IO Response]
handlers params@(settings,handler,renderingLvlVar,viewedAreaRef) initR menuR actualViewedAreaRef = 
  -- debugFilter $
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
   [ withData (\cmds -> [ method GET $ 
                          do { liftIO $ putStrLn $ "Command received " ++ take 60 (show cmds)
                      
                             ; responseHtml <- 
                                 liftIO $ catchExceptions $ handleCommands params initR menuR actualViewedAreaRef
                                                            cmds
--                             ; liftIO $ putStrLn $ "\n\n\n\ncmds = "++show cmds
--                             ; liftIO $ putStrLn $ "\n\n\nresponse = \n" ++ show responseHTML
                             
                             ; seq (length responseHtml) $ return ()
                             ; liftIO $ putStrLn $ "Sending response sent to client:\n" ++
                                                   take 160 responseHtml ++ "..."
                             ; modifyResponseW noCache $
                                ok $ toResponse responseHtml
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


newtype Upl = Upl String deriving Show

instance FromData Upl where
  fromData = liftM Upl (look "documentFile")


newtype Commands = Commands String deriving Show

instance FromData Commands where
  fromData = liftM Commands (look "commands")


{- Salvia -} {-
server params@(settings,handler,renderingLvlVar,viewedAreaRef) initR menuR actualViewedAreaRef =
 do { let handler =
            hPathRouter
             [ ("/",            hFileResource "src/proxima/scripts/Editor.xml"
               )
             
             , ("/favicon.ico", hFileResource "src/proxima/etc/favicon.ico")
             ]
             $ hFakeDir "/img"    (hFileSystem "img")
             $ -- hFakeDir "/handle"  -- does not work since command is not "handle/?"
                (do { liftIO $ putStrLn "handle"
                    ; parameters <- hParameters
                    -- ; liftIO $ putStrLn $ show parameters
                    ; let commandsStr = 
                            case lookup "commands" parameters of
                              Just (Just commandsStr) -> commandsStr
                              _                       -> ""
                    
                    ; responseHTML <- 
                        liftIO $ handleCommands params initR menuR actualViewedAreaRef
                                   (Commands commandsStr)
--                    ; liftIO $ putStrLn $ "response" ++ responseHTML    
                    ; enterM response $ do
                        setM contentLength (Just $ fromIntegral $ length responseHTML )
                        setM contentType ("text/plain", Nothing)
--                        setM (comp safeRead (show) (header "Content-Length"))
--                             (Just 29)
                    ; sendStr $ responseHTML
                    })
          {-   $ do { badRequest <- getM (path % uri % request)
                  ; liftIO $ putStrLn $ show badRequest
                  ; hCustomError BadRequest $ "Unhandled request" ++ show badRequest
                  }
         -}
    ; defaultC <- defaultConfig  
    ; putStrLn $ "Starting Proxima server on port " ++ show (serverPort settings) ++ "."    
    ; start (defaultC {listenPort = fromIntegral $ serverPort settings}) $ -- hDefaultEnv handler
                   hKeepAlive $
                   hParser (1000 * 15)
            (wrapper Nothing . parseError)
            (wrapper Nothing $ hHead handler) 

    }
before :: Handler ()
before = hBanner "salvia-httpd"

after :: Maybe (TVar Int) -> Handler ()
after mc = 
  do hPrinter
     maybe
       (hLog stdout)
       (\c -> hCounter c >> hLogWithCounter c stdout)
       mc

wrapper :: Maybe (TVar Int) -> Handler a -> Handler ()
wrapper c h = before >> h >> after c

parseError :: String -> Handler ()
parseError err = 
  do hError BadRequest
     sendStrLn []
     sendStrLn err


{-

Connection	Keep-Alive
Content-Length	686
Content-Type	text/plain
Date	Thu, 05 Feb 2009 19:13:29 GMT
Expires	Mon, 28 Jul 2000 11:24:47 GMT
Server	HAppS/0.9.2

GET /handle?commands=Key(116,(False,False,False)); HTTP/1.2
GET /handle?commands=Metrics((%22Times%20New%20Roman%22,12),(19,15,[40,50,50,80,80,130,120,30,50,50,80,90,40,60,40,40,80,80,80,80,80,80,80,80,80,80,30,40,120,90,90,70,150,110,100,110,110,90,90,110,110,50,60,120,90,140,120,120,90,120,100,90,90,110,110,150,110,110,90,50,40,50,80,80,50,70,80,70,80,70,40,70,70,30,40,80,30,110,70,80,80,80,50,60,40,70,70,110,70,70,60,70,30,80,90,130,130,130,130,130,130,0,130,130,130,130,130,130,130,130,130,130,130,130,130,130,130,130,130,130,130,130,130,130,130,130,130,130,40,50,80,80,80,80,30,80,50,120,50,70,90,0,120,80,60,90,50,50,50,70,70,40,40,50,50,70,120,120,120,70,110,110,110,110,110,110,130,110,90,90,90,90,50,50,50,50,110,120,120,120,120,120,120,90,120,110,110,110,110,110,100,80,70,70,70,70,70,70,110,70,70,70,70,70,30,30,30,30,80,70,80,80,80,80,80,90,80,70,70,70,70,70,80,70]));

-}
hFakeDir :: FilePath -> Handler () -> Handler () -> Handler ()
hFakeDir dir handler def = 
    hPath   dir (hRedirect $ fromJust $ parseURI (dir ++ "/"))
  $ hPrefix dir handler
  $ def


-- cannot import Misc.Misc??? (because salvia 0.0.5 is supposedly hidden)
safeRead :: Read a => String -> Maybe a
safeRead s = case reads s of
  [(x, "")] -> Just x
  _         -> Nothing

-}



splitCommands commandStr =
  case break (==';') commandStr of
    ([],[])             -> []
    (_, [])              -> error $ "Syntax error in commands: " ++ commandStr
    (command, (_:commandStr')) -> command : splitCommands commandStr'
        
-- handle each command in commands and send the updates back
handleCommands (settings,handler,renderingLvlVar,viewedAreaRef) initR menuR actualViewedAreaRef (Commands commandStr) =
 do { let commands = splitCommands commandStr
   -- ; putStrLn $ "Received commands:"++ show commands
    
    ; renderingHTMLss <-
        mapM (handleCommandStr (settings,handler,renderingLvlVar,viewedAreaRef) initR menuR actualViewedAreaRef)
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
    ; return $ "<div id='updates'>"++ (if null pendingQueries 
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

handleCommandStr (settings,handler,renderingLvlVar,viewedAreaRef) initR menuR actualViewedAreaRef eventStr =
  case safeRead eventStr of
    Just cmd -> handleCommand (settings,handler,renderingLvlVar,viewedAreaRef) initR menuR actualViewedAreaRef cmd
    Nothing  -> error ("Syntax error in command: "++eventStr) 


handleCommand :: (Show token, Show node, Show enr, Show doc) => 
               (Settings
               ,((RenderingLevel doc enr node clip token, EditRendering doc enr node clip token) -> IO (RenderingLevel doc enr node clip token, [EditRendering' doc enr node clip token]))
               , IORef (RenderingLevel doc enr node clip token) 
               , IORef CommonTypes.Rectangle 
               ) -> IORef Bool -> IORef [Wrapped doc enr node clip token] -> IORef CommonTypes.Rectangle -> Command -> IO [String]
handleCommand (settings,handler,renderingLvlVar,viewedAreaRef) initR menuR actualViewedAreaRef command =
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
      do { html <- genericHandler settings handler renderingLvlVar viewedAreaRef () $ 
                     castLay $ EditStyleLay style
         ; setViewedAreaHtml <- mkSetViewedAreaHtml settings viewedAreaRef actualViewedAreaRef
         ; return $ html ++ [setViewedAreaHtml]
         }

    Find str ->
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
 do { renderingLvl@(RenderingLevel _ _ _ _ (w,h) _ _ _) <- readIORef renderingLvlVar
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
         do { (RenderingLevel _ _ _ _ (w,h) _ _ _) <- readIORef renderingLvlVar
            ; writeIORef renderingLvlVar renderingLvl''
  --          ; putStrLn $ "Drawing " ++ show (w,h) ++ show (newW,newH)
            
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

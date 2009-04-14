{-# LANGUAGE CPP #-} 
module Proxima.GUIServer where

import Common.CommonTypes ( DebugLevel (..), debug, showDebug, showDebug', debugIO, debugLnIO
                          , Settings (..) )
import qualified Common.CommonTypes as CommonTypes
import Common.CommonUtils
import Rendering.RenTypes
import System.IO
import Data.IORef
import Proxima.Wrap

import Data.Time.Clock
import Control.Exception
import Data.Char

{- HAppS -}
import HAppS.Server
import HAppS.Server.SimpleHTTP
import HAppS.State
import System.Environment
import System.Time
import Control.Monad.Trans
import Control.Monad
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
import System.Environment
import Control.Concurrent
import System.Time
import Data.Typeable hiding (cast)
import Control.Monad.Trans
import Control.Monad hiding (when)
import Control.Monad.Writer
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

startEventLoop params@(settings,_,_,_) = withProgName "proxima" $
 do { initR <- newIORef (True)
    ; menuR <- newIORef []

    ; tId <- forkIO $ server params initR menuR
    ; putStrLn $ "Starting Proxima server on port " ++ show (serverPort settings) ++ "."
    ; putStrLn "Press <Enter> to terminate server"
    ; getLine `Control.Exception.catch` exceptionHandler
    ; killThread tId    
    }
 where exceptionHandler :: Exception-> IO String
       exceptionHandler err =
        do { -- if getLine fails, we assume to have stdin from /dev/null, so 
             -- we just wait until the process is killed externally
           ; putStrLn "No stdin, waiting for explicit termination"
           ; threadDelay 31536000000000000 -- wait a thousand years
           ; return "" -- not reached
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

server params@(settings,_,_,_) initR menuR =
  simpleHTTP (Conf (serverPort settings) Nothing) (handlers params initR menuR)
{-
handle:
http://<server url>/                    response: <proxima executable dir>/../proxima/scripts/Editor.xml
http://<server url>/favicon.ico         response: <proxima executable dir>/img/favicon.ico
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
handlers params@(settings,handler,renderingLvlVar,viewedAreaRef) initR menuR = 
  -- debugFilter $
  [ withAgentIsMIE $ \agentIsMIE ->
      (methodSP GET $ do { -- liftIO $ putStrLn $ "############# page request"
                           liftIO $ writeIORef viewedAreaRef ((0,0),(1000,800)) 
                           -- todo: take this from an init event
                         ; let setTypeToHTML = if agentIsMIE 
                                               then setHeader "Content-Type" "text/html"
                                               else id
                                           
                         ; modifyResponseSP (noCache. setTypeToHTML) $
                              fileServe [] "../proxima/scripts/Editor.xml" 
                         })
                 

  , dir "img"
        [ fileServe [] "img" ]  
  , dir "favicon.ico"
        [ methodSP GET $ fileServe ["favicon.ico"] "../proxima/etc"]

  , dir "handle" 
   [ withData (\cmds -> [ method GET $ 
                          do { liftIO $ putStrLn $ "Command received " ++ take 10 (show cmds)
                      
                             ; responseHTML <- 
                                 liftIO $ handleCommands params initR menuR
                                                         cmds
--                             ; liftIO $ putStrLn $ "\n\n\n\ncmds = "++show cmds
--                             ; liftIO $ putStrLn $ "\n\n\nresponse = \n" ++ show responseHTML
                             
                             ; seq (length responseHTML) $ return ()
                             ; liftIO $ putStrLn $ "Sending response sent to client: " ++
                                                   take 10 responseHTML ++ "..."
                             ; modifyResponseW noCache $
                                ok $ toResponse responseHTML
                             }
                        ])
   ]
  ]



instance FromData Commands where
  fromData = liftM Commands (look "commands")



{- Salvia -} {-
server params@(settings,handler,renderingLvlVar,viewedAreaRef) initR menuR =
 do { let handler =
            hPathRouter
             [ ("/",            do { liftIO $ writeIORef viewedAreaRef ((0,0),(1000,800)) 
                                   ; hFileResource "../proxima/scripts/Editor.xml"
                                   }
               )
             
             , ("/favicon.ico", hFileResource "../proxima/etc/favicon.ico")
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
                        liftIO $ handleCommands params initR menuR 
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


data Commands = Commands String deriving Show

splitCommands commandStr =
  case break (==';') commandStr of
    ([],[])             -> []
    (_, [])              -> error "Syntax error in commands"
    (command, (_:commandStr')) -> command : splitCommands commandStr'
        
-- handle each command in commands and send the updates back
handleCommands (settings,handler,renderingLvlVar,viewedAreaRef) initR menuR (Commands commandStr) =
 do { let commands = splitCommands commandStr
   -- ; putStrLn $ "Received commands:"++ show commands
    
    ; renderingHTMLss <-
        mapM (handleCommand (settings,handler,renderingLvlVar,viewedAreaRef) initR menuR)
             commands
 
    ; let renderingHTML = concat . concat $ renderingHTMLss

    ; focusRenderingHTML <- drawFocusHTML settings renderingLvlVar viewedAreaRef 



    ; pendingQueriesTxt <-  readFile "metricsQueries.txt"
    ; seq (length pendingQueriesTxt) $ return ()
    ; let pendingQueries = map read $ lines pendingQueriesTxt :: [(String, Int)]
          queryHTML = concat [ "<div id='metricsQuery' op='metricsQuery' family='"++fam++"' size='"++show sz++"'></div>" 
                             | (fam,sz) <- pendingQueries]
                      ++ if null pendingQueries then "" else "<div id='refresh' op='refresh'></div>"

    
    ; fh <- openFile "metricsQueries.txt" WriteMode
    ; hClose fh
    
--                  ; return $ "<div id='updates'>"++testRenderingHTML++"</div>"
    ; if null pendingQueries then putStrLn "Sending rendering and focus" else return ()
    ; return $ "<div id='updates'>"++ (if null pendingQueries 
                                       then renderingHTML++focusRenderingHTML
                                       else "")
                                   ++queryHTML++"</div>"            
    }
    
    
handleCommand (settings,handler,renderingLvlVar,viewedAreaRef) initR menuR event =
 do { -- putStrLn $ "Handling: " ++ take 70 event
     if "Metrics" `isPrefixOf` event  -- is not handled by genericHandler
      then handleMetrics event
      else if "ContextRequest" `isPrefixOf` event  -- is not handled by genericHandler
      then handleContextMenuRequest renderingLvlVar menuR event
      else do { (event) <-
                  if "Key" `isPrefixOf` event || "Chr" `isPrefixOf` event 
                    then handleKey event "" 0
                    else if "Mouse" `isPrefixOf` event
                    then handleMouse event "" 0
                    else if "ContextSelect" `isPrefixOf` event
                    then handleContextMenuSelect menuR event
                    else if "Special" `isPrefixOf` event
                    then handleSpecial viewedAreaRef event "" 0
                    else do { putStrLn $ "Event not recognized: "++event
                            ; return $ SkipRen 0
                            }
              --; print event               
              ; genericHandler settings handler renderingLvlVar viewedAreaRef () event
              }
    }

handleMetrics ('M':'e':'t':'r':'i':'c':'s':event) =
 do { let receivedMetrics@(font,_) :: ((String,Int),(Int,Int,[Int])) = read $ event
    ; putStrLn $ "Received metrics for: "++show font
    ; fh' <- openFile "queriedMetrics.txt" AppendMode
    ; hPutStrLn fh' $ show receivedMetrics
    ; hClose fh'
    ; return [""]
    }
    
-- Current structure of handlers causes focus to be repainted after context request
-- this is not really a problem though
handleContextMenuRequest renderingLvlVar menuR ('C':'o':'n':'t':'e':'x':'t':'R':'e':'q':'u':'e':'s':'t':event) =
 do { let ((proxX,proxY),(screenX,screenY)) :: ((Int,Int),(Int,Int)) = read event

    ; (RenderingLevel _ makePopupMenuHTML _ _ _ _ _ _)  <- readIORef renderingLvlVar
    ; let (itemStrs,upds) = unzip $ makePopupMenuHTML proxX proxY
          itemsHTML = concat 
                        [ "<div class='menuItem' item='"++show i++"'>"++item++"</div>"
                        | (i,item) <- zip [0..] itemStrs
                        ]
      -- for separator lines: "<hr></hr>"
    
    ; writeIORef menuR $ upds
                            
    ; return [ "<div op='contextMenu' screenX='"++show screenX++"' screenY='"++show screenY++"'>" ++
               itemsHTML ++
               "</div>"
             ]
    }
 
handleContextMenuSelect :: forall doc enr clip node token .
                           IORef [UpdateDoc doc clip] -> String -> IO (EditRendering doc enr node clip token)
handleContextMenuSelect menuR ('C':'o':'n':'t':'e':'x':'t':'S':'e':'l':'e':'c':'t':event) =
 do { menuItems <- readIORef menuR
    ; let selectedItemNr :: Int = read event
          editDoc = index "GUI.handleContextMenuSelect" menuItems selectedItemNr
    ; return $ cast (UpdateDoc' editDoc :: EditDocument' doc enr node clip token)
    }
    
handleKey ('K':'e':'y':event) editStr focus = return $
 let (keyCode,(shiftDown :: Bool, ctrlDown :: Bool, altDown :: Bool)) = read $ takeWhile (/='?') event
     key = 
       case keyCode of
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
     ms = CommonTypes.Modifiers shiftDown ctrlDown altDown
  in key 
handleKey ('C':'h':'r':event) editStr focus = return $
 let (keyChar,(shiftDown :: Bool, ctrlDown :: Bool, altDown :: Bool)) = read $ takeWhile (/='?') event
     ms = CommonTypes.Modifiers shiftDown ctrlDown altDown
  in if not ctrlDown && not altDown 
     then KeyCharRen (chr keyChar)
     else KeySpecialRen (CommonTypes.CharKey (chr keyChar)) ms
handleKey malEvent editStr focus =
 do { putStrLn $ "Internal error: malformed key event: " ++ malEvent
    ; return $ SkipRen 0
    }
    
insertChar c editStr focus = (take focus editStr ++ [c] ++ drop focus editStr, focus +1)

handleMouse ('M':'o':'u':'s':'e':event) editStr focus = 
 do { putStrLn $ "Mouse event: " ++ event
    ; let action:args = event
    ; let (x:: Int, y :: Int,(shiftDown :: Bool, ctrlDown :: Bool, altDown :: Bool)) = read args
          
    ; return $ case action of
                     'D' -> MouseDownRen x y (CommonTypes.Modifiers shiftDown ctrlDown altDown) 1
                     'U' -> MouseUpRen x y (CommonTypes.Modifiers shiftDown ctrlDown altDown)
                     'C' -> SkipRen 0
                     'M' ->  MouseDragRen x y  (CommonTypes.Modifiers shiftDown ctrlDown altDown)
                     _   -> SkipRen 0
    -- move events are only sent when button is down, to prevent flooding    
    }
handleMouse malEvent editStr focus =
 do { putStrLn $ "Internal error: malformed mouse event: " ++ malEvent
    ; return $ SkipRen 0
    }

-- sig is necessary to scope type vars in cast
handleSpecial ::forall a doc enr node clip token .
                Read a => IORef a -> String -> String -> Int -> IO (EditRendering doc enr node clip token)
handleSpecial viewedAreaRef ('S':'p':'e':'c':'i':'a':'l':event) editStr focus = 
 do { putStrLn $ "Special event: " ++ event
    ; if "Scroll" `isPrefixOf` event
      then do { writeIORef viewedAreaRef $ read $ drop 6 event
              
              ; return $ SkipRen (-2)
              }
      else if "ClearMetrics" `isPrefixOf` event
      then do { fh <- openFile "queriedMetrics.txt" WriteMode -- TODO: clearing this file should be done after Metrics are read in FontLib.hs
              ; hClose fh
              ; return $ cast (ClearMetricsArr :: EditArrangement doc enr node clip token)
              }
      else do { putStrLn $ "Unrecognized special event: "++event
              ; return $ SkipRen 0
              } 
    }            
handleSpecial viewedAreaRef malEvent editStr focus =
 do { putStrLn $ "Internal error: malformed special event: " ++ malEvent
    ; return $ SkipRen 0
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
--    ; putStrLn $ "Viewed area that is about to be rendered: " ++ show viewedArea
          
    ; (renderingLvl', editsRendering) <- handler (renderingLvl,evt)
    ; htmlRenderings <- mapM process editsRendering
    ; return $ htmlRenderings
    }
 where process (SkipRen' _) = return "" -- set the renderingLvlVar ??
       process (SetRen' renderingLvl''@(RenderingLevel scale _ renderingHTML _ (newW,newH) _ updRegions _)) =
         do { (RenderingLevel _ _ _ _ (w,h) _ _ _) <- readIORef renderingLvlVar
            ; writeIORef renderingLvlVar renderingLvl''
  --          ; putStrLn $ "Drawing " ++ show (w,h) ++ show (newW,newH)
            
            ; viewedArea <- readIORef viewedAreaRef
            ; let htmlRendering = execWriter $ renderingHTML viewedArea
            ; return htmlRendering
            }
    

drawFocusHTML settings renderingLvlVar viewedAreaRef = 
 do { RenderingLevel scale _ _ focusRenderingHTML (w,h) debug updRegions _ <- readIORef renderingLvlVar
    ; viewedArea <- readIORef viewedAreaRef
    ; let htmlFocusRendering = execWriter $ focusRenderingHTML viewedArea
    ; return htmlFocusRendering
    }


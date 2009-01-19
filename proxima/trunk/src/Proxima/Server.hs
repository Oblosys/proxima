{-# OPTIONS -fglasgow-exts #-}

{-# LANGUAGE TemplateHaskell #-}
module Server where


import HAppS.Server
import HAppS.Server.SimpleHTTP
import HAppS.State
import System.Environment
import Control.Concurrent
import System.Time
import Data.Typeable
import Control.Monad.Trans
import Control.Monad
import Data.List
import HAppS.Data.Xml.HaXml

import Text.XHtml.Strict hiding (method)



-----------------------
{-
GET http://localhost:8080/handle?commands=Key(116,(False,False,False));
GET /handle?commands=Key(116,(False,False,False));
-}



serverHAppS params = withProgName "proxima" $
 do { initR <- newIORef (True)
    ; menuR <- newIORef []

    ; tid <- forkIO $ simpleHTTP (Conf 8080 Nothing) (handlers params initR menuR)
    ; putStrLn . ( ( "Proxima 2.0 server started on port 8080\n" ++
                 "shut down with ctrl-c" ) ++) =<< time


    ; waitForTermination
    ; killThread tid
      
      
    ; putStrLn .  ( "exiting: " ++ ) =<< time
    }
 where time = return . ("\ntime: " ++ ) . show  =<< getClockTime

{-
handle:
http://<server url>/                    response: <proxima executable dir>/../proxima/scripts/Editor.xml
http://<server url>/favicon.ico         response: <proxima executable dir>/img/favicon.ico
http://<server url>/img/<filename>      response: <proxima executable dir>/img/<filename>
http://<server url>/handle?commands=<commands separated by ;>                    
                                        response: from handleCommands
-}
-- handlers :: [ServerPartT IO Response]
handlers params@(settings,handler,renderingLvlVar,viewedAreaRef) initR menuR = 
  -- debugFilter $
  [ 
    methodSP GET $ do { -- liftIO $ putStrLn $ "############# page request"
                        liftIO $ writeIORef viewedAreaRef ((0,0),(1000,800)) 
                        -- todo: take this from an init event
                      ; fmap noCache $
                          fileServe [] "../proxima/scripts/Editor.xml" 
                      }

  , dir "img"
        [ fileServe [] "img" ]  
  , dir "favicon.ico"
        [ methodSP GET $ fileServe ["favicon.ico"] "img"]

  , dir "handle" 
   [ withData (\cmds -> [ method GET $ 
                          do { -- liftIO $ putStrLn $ "############# commands request"
                      
                               responseHTML <- 
                                 liftIO $ handleCommands params initR menuR
                                                         cmds
--                             ; liftIO $ putStrLn $ "\n\n\n\ncmds = "++show cmds
--                             ; liftIO $ putStrLn $ "\n\n\nresponse = \n" ++ show responseHTML
                             ; ok $ addHeader "Content-Type:" "text/xml" $
                                    noCache $
                                    toResponse $ responseHTML
                             }
                        ])
   ]
  ]

noCache :: Response -> Response  
noCache = addHeader "Expires:" "Mon, 28 Jul 2000 11:24:47 GMT"

data Commands = Commands String deriving Show

instance FromData Commands where
  fromData = liftM Commands (look "commands")

serverLoop params initR menuR serverSocket = loop $
 do { connection <- accept serverSocket
    ; putStrLn $ "\nNew connection" ++ show connection
    ; serve params initR menuR connection 
    } `Control.Exception.catch` \err -> 
 do { putStrLn $ "Socket handler terminated:\n"++ show err
    }
           
serve params initR menuR (handle, remoteHostName, portNumber) =
 do {
    ; hSetBuffering handle LineBuffering
    ; putStrLn $ "Connected to "++show remoteHostName ++ " on port " ++ show portNumber
    
    ; loop (handleRequest params initR menuR handle)
    }

handleRequest (settings,handler,renderingLvlVar,viewedAreaRef) initR menuR handle =
 do { commandLines <- hGetLinez handle
    ; fh <- openFile "rendering.html" WriteMode
    ; hClose fh
    
    ; putStrLn $ "Handling on socket: " ++ show handle
--    ; mapM putStrLn commandLines
    
    ; let arg = (takeWhile (/=' ') (drop 5 (head commandLines)))
    ; putStrLn $ "arg (len="++show (length arg) ++"):\n" ++ take 70 arg
    ; if arg == ""  
      then
       do { writeIORef viewedAreaRef ((0,0),(1000,800)) -- todo: take this from an init event
          ; page <- readFile "../proxima/scripts/Editor.html" -- in Proxima tree, changes location when proxima is not in subdir
          ; seq (length page) $ return ()
          -- ; print page
          ; hPutStr handle $ toHTTP page
          ; hFlush handle
          }
      else if arg == "favicon.ico" then
       do { hPutStr handle $ httpNotFound
          ; hFlush handle
          }
      else if "img/" `isPrefixOf` arg then handleImage handle arg
      else if "handle?commands="  `isPrefixOf` arg
           then do { result <- handleCommands (settings,handler,renderingLvlVar,viewedAreaRef) initR menuR
                                              (Commands $ drop 16 arg) -- drop the "handle?command="
                   ; hPutStr handle $ toHTTP $ result
                   ; hFlush handle
                   ; putStrLn $ "\n\n\n\ncmds = "++show (drop 16 arg)
                   ; putStrLn $ "\n\n\nresponse = \n" ++ show result
                             
                   }
      else  error "Unhandled request"
    }


splitCommands commandStr =
  case break (==';') commandStr of
    ([],[])             -> []
    (_, [])              -> error "Syntax error in commands"
    (command, (_:commandStr')) -> command : splitCommands commandStr'
        
-- handle each command in commands and send the updates back
handleCommands (settings,handler,renderingLvlVar,viewedAreaRef) initR menuR (Commands commandStr) =
 do { let commands = splitCommands commandStr
   -- ; putStrLn $ "Received commands:"++ show commands
    
    ; mapM (handleCommand (settings,handler,renderingLvlVar,viewedAreaRef) initR menuR handle)
           commands
    
    ; drawFocusHTML settings renderingLvlVar viewedAreaRef
    -- render the focus, so focusRendering.html is updated
    
--          ; testRenderingHTML <- readFile "testRendering.html"
--          ; seq (length testRenderingHTML) $ return ()
    ; renderingHTML <- readFile "rendering.html" -- todo rename rendering to updates
    ; seq (length renderingHTML) $ return ()
    ; focusRenderingHTML <- readFile "focusRendering.html"
    ; seq (length focusRenderingHTML) $ return ()
    
    ; fh <- openFile "rendering.html" WriteMode
    ; hClose fh

    
    ; pendingQueriesTxt <-  readFile "metricsQueries.txt"
    ; seq (length pendingQueriesTxt) $ return ()
    ; let pendingQueries = map read $ lines pendingQueriesTxt :: [(String, Int)]
          queryHTML = concat [ "<div id='metricsQuery' op='metricsQuery' family='"++fam++"' size='"++show sz++"'></div>" 
                             | (fam,sz) <- pendingQueries]
                      ++ if null pendingQueries then "" else "<div id='refresh' op='refresh'></div>"

    
    ; fh <- openFile "metricsQueries.txt" WriteMode
    ; hClose fh
    
    ; isInitialRequest <- readIORef initR
    ; treeUpdates <-
        if isInitialRequest 
        then
        do { putStrLn "Initial request"
            ; writeIORef initR False
--                  ; return $ "<div id='updates'>"++testRenderingHTML++"</div>"
            ; return $ "<div id='updates'>"++(if null pendingQueries 
                                             then renderingHTML++focusRenderingHTML
                                             else "")
                                           ++queryHTML++"</div>"
            }
        else 
        do { putStrLn "Later request"
--                  ; return $ "<div id='updates'>"++testRenderingHTML++"</div>"
            ; return $ "<div id='updates'>"++ (if null pendingQueries 
                                             then renderingHTML++focusRenderingHTML
                                             else "")
                                           ++queryHTML++"</div>"
            }
    ; return treeUpdates
    }
    
    
handleCommand (settings,handler,renderingLvlVar,viewedAreaRef) initR menuR handle event =
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
              ; genericHandlerServer settings handler renderingLvlVar viewedAreaRef event
              }
    }


handleMetrics ('M':'e':'t':'r':'i':'c':'s':event) =
 do { let receivedMetrics :: ((String,Int),(Int,Int,[Int])) = read $ fromHTML event
--    ; putStrLn $ "Received metrics:"++show receivedMetrics
    ; fh' <- openFile "queriedMetrics.txt" AppendMode
    ; hPutStrLn fh' $ show receivedMetrics
    ; hClose fh'
    }
    
fromHTML [] = []
fromHTML ('%':'2':'0':cs) = ' ': fromHTML cs
fromHTML ('%':'2':'2':cs) = '"': fromHTML cs -- "
fromHTML ('%':'5':'B':cs) = '[': fromHTML cs
fromHTML ('%':'5':'D':cs) = ']': fromHTML cs
fromHTML (c:cs) = c:fromHTML cs
-- Current structure of handlers causes focus to be repainted after context request
-- this is not really a problem though
handleContextMenuRequest renderingLvlVar menuR ('C':'o':'n':'t':'e':'x':'t':'R':'e':'q':'u':'e':'s':'t':event) =
 do { let ((proxX,proxY),(screenX,screenY)) :: ((Int,Int),(Int,Int)) = read event

    ; (RenderingLevel _ _ makePopupMenuHTML _ _ _ _ _ _ _ _)  <- readIORef renderingLvlVar
    ; let (itemStrs,upds) = unzip $ makePopupMenuHTML proxX proxY
          itemsHTML = concat 
                        [ "<div class='menuItem' item='"++show i++"'>"++item++"</div>"
                        | (i,item) <- zip [0..] itemStrs
                        ]
      -- for separator lines: "<hr></hr>"
    
    ; writeIORef menuR $ upds
                            
    ; fh <- openFile "rendering.html" WriteMode
    ; hPutStr fh $ "<div op='contextMenu' screenX='"++show screenX++"' screenY='"++show screenY++"'>"++
                   itemsHTML++"</div>"
    ; hClose fh
    }
 
{-
 
                           
-}
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

handleImage handle ('i':'m':'g':'/':unsafeImageFilename) = 
 do { let imageFilename = [c | c <- unsafeImageFilename, c /= '/' && c /='\\' ]
          extension = reverse . takeWhile (/='.') . reverse $ imageFilename
    ; putStrLn $ "Image download: " ++ imageFilename
    ; h <- openBinaryFile ("img/"++imageFilename) ReadMode 
    ; bytes <- hGetContents h -- httpImage computes length, so whole file is read
    ; hPutStr handle $ httpImage extension bytes
    ; hFlush handle
    ; hClose h
    } `Control.Exception.catch`
   \err -> 
    do { putStrLn $ "Problem while uploading "++unsafeImageFilename++":\n"
       ; hPutStr handle $ httpNotFound
       ; hFlush handle
       }         
handleImage handle malEvent =
 do { putStrLn $ "Internal error: malformed image event: " ++ malEvent
    }
    
-- todo: figure out whether caching is sufficient
httpImage extension bytes = httpImageHeader extension (length bytes) ++ bytes
httpImageHeader extension len = 
  unlines 
    [ "HTTP/1.1 200 OK"
    , "Date: Mon, 28 Jul 2008 11:24:47 GMT"
    , "Server: Proxima"
    , "Last-Modified: Tue, 23 Jan 2007 16:10:01 GMT"
    --ETag: "179010-2d22-427b76900c840\""
    , "Accept-Ranges: bytes"
    , "Content-Length: "++show len
    , "Keep-Alive: timeout=1000, max=100"
    , "Connection: Keep-Alive"
    , "Content-Type: image/"++extension
    , ""
    ]
-- todo: somehow this does not produce a 404 page on firefox.    
httpNotFound = unlines $ header ++ page
 where header =
         [ "HTTP/1.1 404 Not Found"
         , "Date: Sat, 09 Aug 2008 15:41:47 GMT"
         , "Server: Proxima"
         , "Last-Modified: Fri, 11 Nov 2005 13:52:33 GMT"
         , "ETag: \"288266-2d8-4054a6ee40a40\""
         , "Accept-Ranges: bytes"
         , "Content-Length: "++show (length page)
         , "Keep-Alive: timeout=1000, max=100"
         , "Connection: Keep-Alive"
         , "Content-Type: text/html"
         , ""
         ]
       page =
         [ "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">"
         , "<HTML>"
         , "<HEAD>"
         , "<TITLE>404 Not Found</TITLE>"
         , "<META HTTP-EQUIV=\"content-type\" CONTENT=\"text/html; charset=ISO-8859-1\">"
         , "</HEAD>"
         , "<BODY>"
         , "<H3>404 Not Found</H3>"
         , "The document requested was not found on this server."
         , "</BODY>"
         , "</HTML>"
         ]
{-
http request created by firefox for url: http://localhost/img/squiggly.png

--
GET /img/squiggly.png HTTP/1.1
Host: localhost
User-Agent: Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US; rv:1.9.0.1) Gecko/2008070208 Firefox/3.0.1
Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8
Accept-Language: en-us,en;q=0.5
Accept-Encoding: gzip,deflate
Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7
Keep-Alive: 300
Connection: keep-alive
If-Modified-Since: Tue, 23 Jan 2007 16:10:01 GMT
Cache-Control: max-age=0

--
If-Modified-Since: is only added when image was requested before:) 
-}

toHTTP string = unlines (header (length string)) ++ string
header len =  
  [ "HTTP/1.1 200 OK"
  , "Date: Mon, 28 Jul 2008 11:24:47 GMT"
  , "Server: Proxima"
  , "Last-Modified: Wed, 16 Jul 2008 15:56:44 GMT"
  , "Expires: Mon, 28 Jul 2000 11:24:47 GMT"
  , "ETag: \"3a387-627-452120dff27aa\""
  , "Accept-Ranges: bytes"
  , "Content-Length: " ++ show len
  , "Keep-Alive: timeout=1000, max=100"
  , "Connection: Keep-Alive"
  , "Content-Type: text/xml"
  , ""
  ]
  

html (focusX,focusY) status text =  
  "<div id=\"info\" focusX=\""++show focusX ++"\" focusY=\""++show focusY ++
                  "\" status=\""++status ++ "\"></div>" ++
  concatMap htmlChar text ++
  ""
 where htmlChar '\n' = "<br/>"
       --htmlChar ' '  = "&#8194;"
       htmlChar ' '  = "&nbsp;"
       htmlChar '<'  = "&lt;"
       htmlChar '>'  = "&gt;"
       htmlChar c    = [c]


genericHandlerServer :: Settings ->
               ((RenderingLevel doc enr node clip token, EditRendering doc enr node clip token) -> IO (RenderingLevel doc enr node clip token, [EditRendering' doc enr node clip token])) ->
               IORef (RenderingLevel doc enr node clip token) -> IORef CommonTypes.Rectangle -> 
               EditRendering doc enr node clip token -> IO ()
genericHandlerServer settings handler renderingLvlVar viewedAreaRef evt =   
 do { renderingLvl@(RenderingLevel _ _ _ _ _ _ _ (w,h) _ _ _) <- readIORef renderingLvlVar
    
    ; viewedArea <- readIORef viewedAreaRef
    ; putStrLn $ "Viewed area that is about to be rendered: " ++ show viewedArea
          
    ; (renderingLvl', editsRendering) <- handler (renderingLvl,evt)
    ; mapM_ process editsRendering
    }
 where process (SkipRen' _) = return () -- set the renderingLvlVar ??
       process (SetRen' renderingLvl''@(RenderingLevel scale _ _ _ _ _ _ (newW,newH) _ updRegions _)) =
         do { (RenderingLevel _ _ _ _ _ _ _ (w,h) _ _ _) <- readIORef renderingLvlVar
            ; writeIORef renderingLvlVar renderingLvl''
  --          ; putStrLn $ "Drawing " ++ show (w,h) ++ show (newW,newH)
            
            ; drawRenderingHTML settings renderingLvlVar viewedAreaRef
    
            }
    
drawRenderingHTML settings renderingLvlVar viewedAreaRef = 
 do { RenderingLevel scale mkPopupMenu _ _ _ renderingHTML _ (w,h) debug updRegions _ <- readIORef renderingLvlVar
--    ; putStrLn "Drawing rendering"
    ; viewedArea <- readIORef viewedAreaRef
    -- ; putStrLn $ "The viewed area is" ++ show viewedArea
    ; renderingHTML viewedArea -- rendering only viewedArea is not extremely useful,
                                        -- since arranger already only arranges elements in view
                                        -- currently, it only prevents rendering edges out of view
    }

drawFocusHTML settings renderingLvlVar viewedAreaRef = 
 do { RenderingLevel scale _ _ _ _ renderingHTML focusRenderingHTML (w,h) debug updRegions _ <- readIORef renderingLvlVar
    ; viewedArea <- readIORef viewedAreaRef
    ; focusRenderingHTML viewedArea
    }

-- Utility functions

    
hGetLinez :: Handle -> IO [String]
hGetLinez handle = 
 do { str <- hGetLine handle
    -- ; putStrLn $ "Socket "++show handle ++": "++str
    ; strs <- if str /= "\r"
              then hGetLinez handle
              else return []
    ; return $ str:strs
    }



loop io = loop'
 where loop' = 
        do { io
           ; loop'
           }
           

getHostIP :: IO String
getHostIP = 
 do { hostName <- getHostName
    ; hostEntry <- getHostByName hostName
    ; let addr = hostAddress hostEntry
          (b1,b2,b3,b4) = ( addr `div` 16777216 `mod` 256, addr `div` 65536 `mod` 256
                          , addr `div` 256 `mod` 256, addr `mod` 256)
    ; return $ show b4 ++ "." ++ show b3 ++ "." ++ show b2 ++ "." ++ show b1
    }
           

module Main where

import Data.Maybe
import Data.Record.Label
import Control.Concurrent.STM
import Control.Monad.State
import Network.Protocol.Http
import Network.Protocol.Uri
import Network.Salvia.Httpd
import Network.Salvia.Handlers.Default
import Network.Salvia.Handlers.Printer
import Network.Salvia.Handlers.Error
import Network.Salvia.Handlers.FileSystem
import Network.Salvia.Handlers.Redirect
import Network.Salvia.Advanced.ExtendedFileSystem
import Network.Salvia.Handlers.Login (readUserDatabase, UserPayload)
import Network.Salvia.Handlers.Session (mkSessions, Sessions)
import Network.Salvia.Handlers.PathRouter
import Network.Salvia.Handlers.File
{-
turn of logging?
[127.0.0.1:4958] 0      GET     /zoe -> ?[32m200 OK?[0;39;49m

error "AAAAp" in Parser.hs


-}

main =
 do { let handler =
            hPathRouter
             [ ("/",            hFileResource "editor.xml")
             , ("/favicon.ico", hFileResource "favicon.ico")
             ]
             $ hFakeDir "/img"    (hFileSystem "img")
             $ hFakeDir "/handle" 
                (do { liftIO $ putStrLn "handle"
                    ; params <- hParameters
                    ; liftIO $ putStrLn $ show params
                    ; let commandsStr = 
                            case lookup "commands" params of
                              Just (Just commandsStr) -> commandsStr
                              _                       -> ""
                        
                    ; sendStr $ "<b>CommandsString is </b>" ++ commandsStr
                    })
             $ do { badRequest <- getM (path % uri % request)
                  ; hCustomError BadRequest $ "Unhandled request" ++ show badRequest
                  }
         
    ; defaultC <- defaultConfig  
    ; start (defaultC {listenPort = 8080}) $ hSimple handler
    }
    
hFakeDir :: FilePath -> Handler () -> Handler () -> Handler ()
hFakeDir dir handler def = 
    hPath   dir (hRedirect $ fromJust $ parseURI (dir ++ "/"))
  $ hPrefix dir handler
  $ def

{-
main =
 do { defaultC <- defaultConfig
    ; count    <- atomically $ newTVar 0
    ; let myHandler = const $ -- hExtendedFileSystem "."
                              do { -- req <- gets request
                                 ; --if (null $ path $ uri req) || (last $ path $ uri req) /= '/'
                                   --then hRedirect (modPath (++"/") $ uri req)
                                   --else dirHandler dirName
                                 ; sendStrLn $ "foekoe" --  ++ show (queryParams (uri req))
                                 }
                                 
    ; sessions <- mkSessions :: IO (Sessions (UserPayload ()))
  
    ; start (defaultC {listenPort = 8080}) $ hDefault count sessions myHandler
--    ; start (defaultC {listenPort = 8080}) $ hExtendedFileSystem "."
    }
-}
{- does not work:
    ; start (defaultC {listenPort = 8080}) $
       do { hPrinter
          ; hError BadRequest
          ; sendStrLn []
          ; sendStrLn "bla"
          ; error "handler"
          }
-}
{-        do { s <- get
           ; liftIO $ putStrLn $ show $ request s
           ; liftIO $ putStrLn $ show $ response s
           ; liftIO $ putStrLn $ show $ sock s
           ; liftIO $ putStrLn $ show $ address s
           ; sendHeaders
           ; return ()
           }
-}
    
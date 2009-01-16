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

{-
no cookie fixing
-}



main = withProgName "Proxima Server" $
 do   tid <- forkIO $ simpleHTTP (Conf 8080 Nothing) handlers
      putStrLn . ( ( "simpleHttp started on port 8080 " ++ "\n" ++
                 "shut down with ctrl-c" ) ++) =<< time

      -- waitForTermination
      putStrLn "getting char"
      c <- getChar 
      putStrLn "killing thread"
      killThread tid
      
      
      --createCheckpoint control
      -- shutdownSystem control 
      putStrLn .  ( "exiting: " ++ ) =<< time
         where time = return . ("\ntime: " ++ ) . show  =<< getClockTime

handlers :: [ServerPartT IO Response]
handlers = -- debugFilter $
  [ method GET $ okHtml ("edi<t>or")

  , dir "x" [ method GET $ ok $ addHeader "Content-Type:" "text/xml" $
                                toResponse ("edit<o>r") ]
  , dir "img"
        [ fileServe [] "img" ]  
  , dir "favicon.ico"
        [ methodSP GET $ fileServe ["favicon.ico"] "."]

  , dir "handle" 
   [ withData (\d -> [ method GET $ 
                       do { responseHTML <- liftIO $ handleCommands d
                          ; ok $ toResponse $ primHtml $ responseHTML
                          }
                    ])
   ]
{-
  , methodSP GET $ fileServe [] "editor.xml" -- serverurl/  returns editor.html
                                              -- serverurl/something fails because of methodSP get
    -- does not work as planned, this one must be last
-}
  ]


data Commands = Commands String deriving Show

instance FromData Commands where
  fromData = liftM Commands (look "commands")

handleCommands :: Commands -> IO String
handleCommands (Commands cmds) = return ("data<div></div>"++show cmds)
{-
browsedir' :: (ToMessage a, ToMessage b) => (String -> [FilePath] -> a)
              -> (String -> String -> b)
              -> FilePath
              -> FilePath
              -> ServerPartT IO Response
browsedir' paintdir paintfile diralias syspath = multi [
  ServerPartT $ \rq -> do
    let aliaspath = ( mypathstring . rqPaths $ rq )
    if (not $ isPrefixOf (addTrailingPathSeparator diralias) $ (addTrailingPathSeparator aliaspath) )
       then noHandle
       else do
         -- to do: s/rqp/realpath/
         let realpath = mypathstring $ syspath : (tail  $ rqPaths rq)
         isDir <- liftIO $ doesDirectoryExist realpath
         if isDir
           then do
             fs <- liftIO $ getDirectoryContents realpath             
             return . toResponse  $ paintdir aliaspath fs              
           else do
             isfile <- liftIO $ doesFileExist realpath     
             f <- liftIO $ readFile realpath
             return . toResponse $ paintfile realpath f 
  ]            
  where 
    -- Windows/Unix/Mac compatible 
    mypathstring pathparts =
      let sep :: String
          sep = [pathSeparator] 
      in intercalate sep pathparts
-}
simpleHandlers :: [ServerPartT IO Response]
simpleHandlers = [

  ServerPartT $ \rq -> do
    if rqURL rq == "/helloworld"
       then do { liftIO $ putStrLn "okidoki"
               ; return $ toResponse  "hello world, this is HAppzzz" 
               }
       else noHandle
    ]



htmlPage :: (HTML a) => a -> Html
htmlPage content =
  (header (thetitle $ toHtml "bla") +++
    (body (toHtml content)))

okHtml :: (HTML a) => a -> Web Response
okHtml content = do { liftIO (putStrLn "responding") 
                    ; ok $ addHeader "Expires" "Mon, 28 Jul 2000 11:24:47 GMT" $ toResponse $ htmlPage content
                    }

{-
stateProxy :: Proxy State
stateProxy = Proxy


run = do
    putStrLn $ "happs tutorial running in ghci. \n" ++
             "exit :q ghci completely and reenter ghci, before restarting."
--    tDirGroups <- getTemplateGroups
    smartserver (Conf 5001 Nothing) "happs-tutorial" 
                ([] :: [ServerPartT IO Response])
                 {- (controller tDirGroups True True) -}
                stateProxy





-- run the happs server on some port
-- include cookie fix, various other enhancements that make things simpler
smartserver :: (Methods st, Component st, ToMessage a) =>
               Conf -> String -> [ServerPartT IO a] -> Proxy st -> IO ()
smartserver conf progName c stateProxy = withProgName progName $ do
      putStrLn . ( "starting happs server" ++ ) =<< time
      control <- startSystemState stateProxy -- start the HAppS state system

      putStrLn . ( "happs state started" ++ ) =<< time

      tid <- forkIO $ simpleHTTP conf c
      putStrLn . ( ( "simpleHttp started on port " ++ (show . port $ conf) ++ "\n" ++
                 "shut down with ctrl-c" ) ++) =<< time

      waitForTermination
      killThread tid
      putStrLn . ( "creating checkpoint: " ++ ) =<< time
      createCheckpoint control

      putStrLn .  ( "shutting down system: " ++ ) =<< time
      shutdownSystem control 
      putStrLn .  ( "exiting: " ++ ) =<< time
         where time = return . ("\ntime: " ++ ) . show  =<< getClockTime


{-



-- main controller
controller :: STDirGroups String -> Bool -> Bool -> [ServerPartT IO Response]
controller tDirGroups dynamicTemplateReload allowStressTests =  
    -- staticfiles handler *has* to go first, or some content (eg images) will fail to load nondeterministically,
    -- eg http://localhost:5001/static/Html2/index.html (this loads ok when staticfiles handler goes first,
    -- but has the problem when staticfiles handler goes after tutorial handler)
    -- Also interesting: the order doesn't matter when dynamicTemplateReload is false
    -- This still feels to me like a bug: it was quite a headache to diagnose, and why should
    -- the order of the static content handler matter anyway?
    -- At the very least, fileServer should have a highly visible comment warning about this problem.
   []
   {-
   staticfiles      
   ++ ( tutorial tDirGroups dynamicTemplateReload allowStressTests )  
    ++ simpleHandlers 
    ++ [ myFavoriteAnimal ]         
      ++ [ msgToSp "Quoth this server... 404." ]
   -}
   
   -}
   
    
    
    

    
    
data State = State deriving (Show, Typeable)
    

instance Version State
$(deriveSerialize ''State)
    

instance Component State where
   type Dependencies State = End
   initialValue = State
    
    


instance Methods State





-}
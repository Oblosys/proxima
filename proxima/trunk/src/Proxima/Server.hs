{-# OPTIONS -fglasgow-exts #-}

{-# LANGUAGE TemplateHaskell #-}
module Server where


import HAppS.Server
import HAppS.State
import System.Environment
import Control.Concurrent
import System.Time
import Data.Typeable

import Text.XHtml.Strict hiding (method)

{-
no cookie fixing
-}




run = withProgName "Proxima Server" $
 do   tid <- forkIO $ simpleHTTP (Conf 8080 Nothing) handlers
      --putStrLn . ( ( "simpleHttp started on port " ++ (show . port $ conf) ++ "\n" ++
      --           "shut down with ctrl-c" ) ++) =<< time

      waitForTermination
      killThread tid
      putStrLn . ( "creating checkpoint: " ++ ) =<< time
      --createCheckpoint control

      putStrLn .  ( "shutting down system: " ++ ) =<< time
      -- shutdownSystem control 
      putStrLn .  ( "exiting: " ++ ) =<< time
         where time = return . ("\ntime: " ++ ) . show  =<< getClockTime


pageTitle = "Proxima 2.8"

handlers :: [ServerPartT IO Response]
handlers =
  [dir "create"
    [ dir "bloe"
        [anyRequest $ okHtml "testing bloe"]
    ]
  ]
htmlPage :: (HTML a) => a -> Html
htmlPage content =
  (header (thetitle $ toHtml pageTitle) +++
    (body (toHtml content)))

okHtml :: (HTML a) => a -> Web Response
okHtml content = ok  $ toResponse $ htmlPage content


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
   
simpleHandlers :: [ServerPartT IO Response]
simpleHandlers = [

  ServerPartT $ \rq -> do
    ru  <- (return . rqURL) rq
    if ru == "/helloworld"
       then ( return . toResponse ) "hello world, this is HAppS" 
       else noHandle
      :: WebT IO Response
    ]
    
    
    

    
    
data State = State deriving (Show, Typeable)
    

instance Version State
$(deriveSerialize ''State)
    

instance Component State where
   type Dependencies State = End
   initialValue = State
    
    


instance Methods State





-}
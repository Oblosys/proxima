module DebugLevels ( DebugLevel (..)
                   , debug
                   , showDebug
                   , showDebug'
                   , debugIO
                   , debugLnIO
                   ) where

import IOExts

-- The debugLevels value is not exported, so a change on it does not lead to recompilation of all modules
-- It doesn't work perfectly, GHC compiled modules are still recompiled when levels have changed.

-- Putting the levels in a shared IORef might be an alternative, but requires unsafePerformIO

data DebugLevel = Err | Main | GUI | Ren | Arr | Lay | Prs | Eva | Rdcr | Par | UnA | GI | Cmn deriving (Show, Eq)

-- Err is for showing internal inconsistencies. It is not advisable to turn this off


debugLevels :: [DebugLevel]
debugLevels = [Err]                     -- for no Debugging
--debugLevels = levels                 -- for debugging on levels

levels =      [ Err 
              , Main
              , GUI
--              , Ren
              , Arr
              , Lay
              , Prs
--              , Eva
--              , Rdcr
--              , Par
--              , UnA
--              , GI
--             , Cmn
              ]

-- maybe use a global IORef for levels, so change of level does not lead to recompilation
-- This will probably affect performance, but when performance is required, all debugs can be removed anyway.
-- However, it might be a bit tricky in GHCi: when the levels value is updated it might hold on to the old one
{-
debugLevels :: [DebugLevel]
debugLevels = [ Err 
              , Main
              , GUI
              , Ren
              , Arr
--              , Prs
              , Eva
              , Rdcr
              , Par
              , UnA
              , GI
              , Cmn
              ]
-}
debugLevel :: DebugLevel -> a -> a -> a
debugLevel level val debugVal = if level `elem` debugLevels then debugVal else val

debug :: DebugLevel -> String -> a -> a
debug      level str x = debugLevel level x $ trace str           x

showDebug :: Show a => DebugLevel -> a -> a
showDebug  level     x = debugLevel level x $ trace (show x)      x

showDebug' :: Show a => DebugLevel -> String -> a -> a
showDebug' level str x = debugLevel level x $ trace (str++show x) x

debugIO :: DebugLevel -> String -> IO ()
debugIO    level str   = debugLevel level (return ()) $ putStr   str

debugLnIO :: DebugLevel -> String -> IO ()
debugLnIO  level str   = debugLevel level (return ()) $ putStrLn str


{- could use these for Draw and GUI
debugIOm :: IOMonad m => DebugLevel -> String -> m ()
debugIOm  level str   = debugLevel level (return ()) $ liftIO $ putStr   str

debugLnIOm :: IOMonad m => DebugLevel -> String -> m ()
debugLnIOm level str   = debugLevel level (return ()) $ liftIO $ putStrLn str
-}
-- if debug calls are to verbose, specialized defs can be given here and imported in each module under the
-- general name
-- eg. CommonTypes: debugRen = debug Ren            Renderer: import debugRen as debug

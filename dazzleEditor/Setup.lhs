#! /usr/bin/runghc

> import Distribution.Simple
> import Distribution.PackageDescription
> import System.Process hiding (runCommand)
> import System.IO
>
> main = defaultMainWithHooks (defaultUserHooks { preBuild = insertCurrentVersion } )

> insertCurrentVersion args buildflags = 
>   do putStrLn "Proxima pre-build hook: executing 'make generate presenter'"
>      output <- runCommand "make" ["generator"]
>      putStrLn output
>      output <- runCommand "make" ["generate"]
>      putStrLn output
>      output <- runCommand "make" ["presenter"]
>      putStrLn output
>      putStrLn "end of pre-build hook"
>      return emptyHookedBuildInfo

> runCommand command args = 
>   do (inh,outh,errh,proch) <- runInteractiveProcess command args Nothing Nothing
>      hClose inh
>      hClose errh
>      output <- hGetContents outh
>      seq output $ waitForProcess proch
>      return output

#! /usr/bin/runghc

> import Distribution.Simple
> import Distribution.PackageDescription
> import System.Cmd
> import System.Exit

> main = defaultMainWithHooks (defaultUserHooks { preBuild = runMake } )

> runMake args buildflags = 
>   do putStrLn "Proxima pre-build hook: executing 'make generate presenter'"
>      putStrLn "> make proxima"
>      errorOnFailure $ system "make proxima"
>      putStrLn "> make generate"
>      errorOnFailure $ system "make generate"
>      putStrLn "> make presenter"
>      errorOnFailure $ system "make presenter"
>      putStrLn "end of pre-build hook"
>      return emptyHookedBuildInfo


-- throw an error, so the build process does not continue

> errorOnFailure :: IO ExitCode -> IO ()
> errorOnFailure cmd =
>  do { exitCode <- cmd
>     ; case exitCode of 
>         ExitSuccess   -> return ()
>         ExitFailure _ -> error $ "Pre-build hook failed."
>     }
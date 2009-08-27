#! /usr/bin/runghc

> import Distribution.Simple
> import Distribution.PackageDescription
> import System.Cmd
> import System.Exit
> 
> main = defaultMainWithHooks (simpleUserHooks { preBuild = runMake } )

> runMake args buildflags = 
>   do putStrLn "Proxima pre-build hook: executing 'make generate presenter'"
>      putStrLn "> make proxima"
>      errorOnFailure $ system "make proxima"
>      putStrLn "> make generate"
>      errorOnFailure $ system "make generate"
>      putStrLn "> make presenter"
>      errorOnFailure $ system "make presenter"
>      putStrLn "> make lexer"
>      errorOnFailure $ system "make lexer"
>      putStrLn "end of pre-build hook"
>      return emptyHookedBuildInfo

The make lexer part builds the Alex lexer. Cabal can do this automatically, but does not allow
the specification of options for Alex.

Because make updates ScannerSheet.hs, Cabal does not preprocess ScannerSheet.x at all.


-- throw an error, so the build process does not continue

> errorOnFailure :: IO ExitCode -> IO ()
> errorOnFailure cmd =
>  do { exitCode <- cmd
>     ; case exitCode of 
>         ExitSuccess   -> return ()
>         ExitFailure _ -> error $ "Pre-build hook failed."
>     }
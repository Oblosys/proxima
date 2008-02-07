#! /usr/bin/runghc

> import Distribution.Simple
> import Distribution.PackageDescription
> import System.Cmd
> 
> main = defaultMainWithHooks (defaultUserHooks { preBuild = runMake } )

> runMake args buildflags = 
>   do putStrLn "Proxima pre-build hook: executing 'make generate presenter'"
>      putStrLn "> make proxima"
>      system "make proxima"
>      putStrLn "> make generate"
>      system "make generate"
>      putStrLn "> make presenter"
>      system "make presenter"
>      putStrLn "> make lexer"
>      system "make lexer"
>      putStrLn "end of pre-build hook"
>      return emptyHookedBuildInfo

The make lexer part builds the Alex lexer. Cabal can do this automatically, but does not allow
the specification of options for Alex.

Because make updates ScannerSheet.hs, Cabal does not preprocess ScannerSheet.x at all.
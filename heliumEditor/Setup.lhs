#! /usr/bin/runghc

> import Distribution.Simple
> import Distribution.PackageDescription
> import System.Cmd
> 
> main = defaultMainWithHooks (defaultUserHooks { preBuild = runMake } )

> runMake args buildflags = 
>   do putStrLn "Proxima pre-build hook: executing 'make generate presenter'"
>      putStrLn "> make generator"
>      system "make generator"
>      putStrLn "> make generate"
>      system "make generate"
>      putStrLn "> make presenter"
>      system "make presenter"
>      putStrLn "end of pre-build hook"
>      return emptyHookedBuildInfo
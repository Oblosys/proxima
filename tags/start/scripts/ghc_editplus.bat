@echo off
rem to be called from editplus or ultraEdit

ghc  ..\Gui\main.hs --make -o Proxima.exe -package objectio -fglasgow-exts -fallow-undecidable-instances -package lang -i../Evaluation -i../Presentation -i../Layout -i../Arrangement -i../Rendering -i../Main -i../Common                                           -fallow-overlapping-instances -package util -package net -i../Helium/helium:../Helium/lvm/src/lib/common:../Helium/lvm/src/lib/common/ghc:../Helium/lvm/src/lib/lvm:../Helium/lvm/src/lib/asm:../Helium/lvm/src/lib/core:../Helium/helium/utils:../Helium/helium/parsec:../Helium/helium/parser:../Helium/helium/staticanalysis:../Helium/helium/uha:../Helium/helium/uhaToCore

IF %ERRORLEVEL%==0 del ..\Main\Main.o
rem IF %ERRORLEVEL%==0 proxima

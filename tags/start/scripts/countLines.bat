@echo off
rem collect sources so lines can be counted. Directories are specified so Helium is not counted
rem don't want to recurse below one level, but this seems hard. Now old stuff and test are included
echo Count is about 700 lines too high

c:\apps\cygwin\bin\grep -R --exclude=*_OIO* --include=*.hs --include=*.ag --exclude=Arrangement/ArrangerAG.hs --exclude=Presentation/PresentationAG.hs '' Rendering Arrangement Layout  Presentation Evaluation Main Common | c:\apps\cygwin\bin\grep -c ''
pause

rem add these to count Helium  helium root dir is not counted because of recursive dir behaviour of grep
rem so about 400 lines too few are counted

rem Helium/lvm/src/lib/common Helium/lvm/src/lib/common/ghc Helium/lvm/src/lib/lvm Helium/lvm/src/lib/asm Helium/lvm/src/lib/core Helium/helium/utils Helium/helium/parsec Helium/helium/parser Helium/helium/staticanalysis Helium/helium/uha Helium/helium/uhaToCore

rem use -c in first grep (and disable second grep) to see which files are counted
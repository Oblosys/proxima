rem   Cannot find the file specified is normal behavior
@echo off
rem rather crappy, but pkzip offers very little control over paths in the zip, so need to cd

rd /s /q c:\windows\temp\proxima
mkdir c:\windows\temp\proxima
c:
cd c:\windows\temp\proxima

copy d:\data\src\hugs\proxima\Test\Proxima.exe 
copy c:\apps\ghc\bin\wxc.dll 
copy c:\apps\ghc\bin\wxcd.dll 
copy d:\data\src\hugs\proxima\Test\*.lvm
copy d:\data\src\hugs\proxima\Test\*.hs
xcopy d:\data\src\hugs\proxima\Test\Docs            Docs\
xcopy d:\data\src\hugs\proxima\Test\Fonts           Fonts\
xcopy d:\data\src\hugs\proxima\Test\Img             Img\
cd ..

del "c:\Documents and Settings\Martijn\Desktop\ProximaBin.zip"
cd  \windows\temp
pkzip25 "c:\Documents and Settings\Martijn\Desktop\ProximaBin.zip" -add  -recurse -path=specify Proxima\*.* 

rd /s /q c:\windows\temp\proxima
d:
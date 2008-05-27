rem   Cannot find the file specified is normal behavior
@echo off
rem rather crappy, but pkzip offers very little control over paths in the zip, so need to cd

rd /s /q c:\windows\temp\proxima
mkdir c:\windows\temp\proxima
c:
cd c:\windows\temp\proxima

copy D:\Data\Src\Haskell\ProximaCVS\proxima\test\Proxima.exe 
copy c:\apps\ghc\bin\wxc.dll 
copy c:\apps\ghc\bin\wxcd.dll 
xcopy D:\Data\Src\Haskell\ProximaCVS\proxima\test\lvm        lvm\
xcopy D:\Data\Src\Haskell\ProximaCVS\proxima\test\img        img\
copy D:\Data\Src\Haskell\ProximaCVS\proxima\test\*.hs
xcopy D:\Data\Src\Haskell\ProximaCVS\proxima\docs            docs\
xcopy D:\Data\Src\Haskell\ProximaCVS\proxima\etc\fonts       fonts\
cd ..

del "c:\Documents and Settings\Martijn\Desktop\ProximaBin.zip"
cd  \windows\temp
pkzip25 "c:\Documents and Settings\Martijn\Desktop\ProximaBin.zip" -add  -recurse -path=specify Proxima\*.* 

rd /s /q c:\windows\temp\proxima
d:
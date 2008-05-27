rem does not work for separate helium, lvm, parsec yet

del "c:\Documents and Settings\Martijn\Desktop\ProximaSrc.zip"
pkzip25 "c:\Documents and Settings\Martijn\Desktop\ProximaSrc.zip" -add  -recurse -path=specify -exclude=*.o -exclude=*.exe -exclude=*.hi -exclude=*.c -exclude=*.core -exclude=*.out ..\proxima\*.* 

rem cd TexSources\Pics
rem for  %%f  in  (*.*)   do d:\bin\imagemagick\convert.exe   %%f eps\%%f.eps 
rem cd ..\..

cd TexSources\Pics\Screenshots
for  %%f  in  (*.png)   do d:\bin\imagemagick\convert.exe %%f %%f.eps 
cd ..\..\..

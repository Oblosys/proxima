cd TexSources\Pics
for  %%f  in  (*.png)   do d:\bin\imagemagick\convert.exe   %%f eps\%%f.eps 
cd ..\..

cd TexSources\Pics\Screenshots
for  %%f  in  (*.png)   do d:\bin\imagemagick\convert.exe %%f %%f.eps 
cd ..\..\..

@echo off

rem bibtex has no output dir option!
cd TexOutput
bibtex -include-directory ..\TexSources %1
cd ..

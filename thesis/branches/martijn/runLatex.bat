@echo off
rem %1.tex, otherwise if %1 contains a .tex extension, latex will run, but copy will fail on name.tex.dvi
latex %1.tex --src-specials=cr -quiet -halt-on-error -include-directory TexSources -include-directory Styles -include-directory TexOutput -output-directory TexOutput
IF %ERRORLEVEL%==0 set texerr=0
IF %ERRORLEVEL%==1 set texerr=1
rem store errorlevel so we can use it after copy. In case of file not found, latex happily returns level 0  :-(
copy TexOutput\%1.dvi texSources\latest.dvi
IF %texerr%==0 start yap texSources\latest.dvi --single-instance
rem IF %texerr%==1 


rem dvi is copied to source directory, so yap can find eps pictures and tex sources
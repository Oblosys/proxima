@echo off
rem for some reason, 'runLatex EntireThesis' does not return, so we call latex explicitly here

rem cannot call bibAll, because bibTex stops batch file on warnings
rem bibAll



latex EntireThesis.tex --src-specials=cr  -include-directory TexSources -include-directory Styles -include-directory TexOutput -output-directory TexOutput | grep -E "defined|rerun"

rem show undefined and multiply defined labels + rerun warnings

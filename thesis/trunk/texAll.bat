@echo off
rem for some reason, 'runLatex EntireThesis' does not return, so we call latex explicitly here

latex EntireThesis.tex --src-specials=cr  -include-directory TexSources -include-directory Styles -include-directory TexOutput -output-directory TexOutput

copy TexOutput\EntireThesis.aux TexOutput\Thesis.aux
rem .aux is copied, so Thesis can be TeXed with the right chapter references.
rem .toc is not copied, so Thesis.tex must have been TeXed before running texAll, to get the toc right

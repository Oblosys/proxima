@echo off
echo #
echo # Creating %1.dvi, %1.pdf, and %1.ps from latest.dvi
echo #
cd D:\Data\documents\Thesis\TexSources
copy latest.dvi D:\Data\documents\Thesis\TexOutput\%1.dvi
dvips -o D:\Data\documents\Thesis\TexOutput\%1.ps latest.dvi
dvipdfm -p a4 -o D:\Data\documents\Thesis\TexOutput\%1.pdf latest.dvi
cd ..
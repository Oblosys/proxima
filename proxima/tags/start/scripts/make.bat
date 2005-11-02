@echo off

rem on Windows:
rem To get rid of console window, add: -optl "-Wl,--subsystem,windows"
rem However, in that case the program may not use standard out, or the whole thing will crash

@echo on
ghc  --make ../src/main/Main.hs -o Proxima.exe -fglasgow-exts -fallow-undecidable-instances -package lang -i../src/evaluation:../src/presentation:../src/layout:../src/arrangement:../src/rendering:../src/main:../src/common                  -fallow-overlapping-instances -package util -package net -package data -i../../parsec -i../../lvm/src/lib/common:../../lvm/src/lib/common/ghc:../../lvm/src/lib/lvm:../../lvm/src/lib/asm:../../lvm/src/lib/core -i../../heliumNT/src/syntax:../../heliumNT/src/parser:../../heliumNT/src/main:../../heliumNT/src/utils:../../heliumNT/src/modulesystem:../../heliumNT/src/staticanalysis/constraints:../../heliumNT/src/staticanalysis/inferencers:../../heliumNT/src/staticanalysis/inferencers/typingstrategies:../../heliumNT/src/staticanalysis/solvers:../../heliumNT/src/staticanalysis/solvers/typegraph:../../heliumNT/src/staticanalysis/solvers/typegraphheuristics:../../heliumNT/src/staticanalysis/messages:../../heliumNT/src/staticanalysis/staticchecks:../../heliumNT/src/staticanalysis/types:../../heliumNT/src/codegeneration 

@rem ghci does not recognize functions in main if the main.o exists
del ..\Main\Main.o
@rem Compile one Proxima module (called from ultraEdit)
@
ghc %1 -c -package objectio -fglasgow-exts -fallow-undecidable-instances -package lang -i../Evaluation -i../Presentation -i../Layout -i../Arrangement -i../Rendering -i../Main -i../Common                                              -fgenerics -i../Helium/helium:../Helium/lvm/src/lib/common:../Helium/lvm/src/lib/common/ghc:../Helium/lvm/src/lib/lvm:../Helium/lvm/src/lib/asm:../Helium/lvm/src/lib/core:../Helium/helium/utils:../Helium/helium/parsec:../Helium/helium/parser:../Helium/helium/staticanalysis:../Helium/helium/uha:../Helium/helium/uhaToCore -fallow-overlapping-instances -package util -package net 

@rem ghci does not recognize functions in main if the main.o exists
del ..\Main\Main.o
rem move all object and .hi files to normal (no profiling or optimization) storage directories

move make.bat ..\Store\Normal

move ..\Rendering\*.o    ..\Store\Normal\Rendering
move ..\Arrangement\*.o  ..\Store\Normal\Arrangement
move ..\Layout\*.o       ..\Store\Normal\Layout
move ..\Presentation\*.o ..\Store\Normal\Presentation
move ..\Evaluation\*.o   ..\Store\Normal\Evaluation
move ..\Main\*.o         ..\Store\Normal\Main
move ..\Common\*.o       ..\Store\Normal\Common
move ..\Rendering\*.hi    ..\Store\Normal\Rendering
move ..\Arrangement\*.hi  ..\Store\Normal\Arrangement
move ..\Layout\*.hi       ..\Store\Normal\Layout
move ..\Presentation\*.hi ..\Store\Normal\Presentation
move ..\Evaluation\*.hi   ..\Store\Normal\Evaluation
move ..\Main\*.hi         ..\Store\Normal\Main
move ..\Common\*.hi       ..\Store\Normal\Common

move ..\Helium\lvm\src\lib\asm\*.o         ..\Store\Normal\Helium\lvm\src\lib\asm
move ..\Helium\lvm\src\lib\common\ghc\*.o  ..\Store\Normal\Helium\lvm\src\lib\common\ghc
move ..\Helium\lvm\src\lib\common\*.o      ..\Store\Normal\Helium\lvm\src\lib\common
move ..\Helium\lvm\src\lib\core\*.o        ..\Store\Normal\Helium\lvm\src\lib\core
move ..\Helium\lvm\src\lib\lvm\*.o         ..\Store\Normal\Helium\lvm\src\lib\lvm
move ..\Helium\lvm\src\lib\parsec\*.o      ..\Store\Normal\Helium\lvm\src\lib\parsec
move ..\Helium\helium\*.o                  ..\Store\Normal\Helium\helium
move ..\Helium\helium\demo\*.o             ..\Store\Normal\Helium\helium\demo
move ..\Helium\helium\parsec\*.o           ..\Store\Normal\Helium\helium\parsec
move ..\Helium\helium\parser\*.o           ..\Store\Normal\Helium\helium\parser
move ..\Helium\helium\staticanalysis\*.o   ..\Store\Normal\Helium\helium\staticanalysis
move ..\Helium\helium\uha\*.o              ..\Store\Normal\Helium\helium\uha
move ..\Helium\helium\uhaToCore\*.o        ..\Store\Normal\Helium\helium\uhaToCore
move ..\Helium\helium\utils\*.o            ..\Store\Normal\Helium\helium\utils

move ..\Helium\lvm\src\lib\asm\*.hi         ..\Store\Normal\Helium\lvm\src\lib\asm
move ..\Helium\lvm\src\lib\common\ghc\*.hi  ..\Store\Normal\Helium\lvm\src\lib\common\ghc
move ..\Helium\lvm\src\lib\common\*.hi      ..\Store\Normal\Helium\lvm\src\lib\common
move ..\Helium\lvm\src\lib\core\*.hi        ..\Store\Normal\Helium\lvm\src\lib\core
move ..\Helium\lvm\src\lib\lvm\*.hi         ..\Store\Normal\Helium\lvm\src\lib\lvm
move ..\Helium\lvm\src\lib\parsec\*.hi      ..\Store\Normal\Helium\lvm\src\lib\parsec
move ..\Helium\helium\*.hi                  ..\Store\Normal\Helium\helium
move ..\Helium\helium\demo\*.hi             ..\Store\Normal\Helium\helium\demo
move ..\Helium\helium\parsec\*.hi           ..\Store\Normal\Helium\helium\parsec
move ..\Helium\helium\parser\*.hi           ..\Store\Normal\Helium\helium\parser
move ..\Helium\helium\staticanalysis\*.hi   ..\Store\Normal\Helium\helium\staticanalysis
move ..\Helium\helium\uha\*.hi              ..\Store\Normal\Helium\helium\uha
move ..\Helium\helium\uhaToCore\*.hi        ..\Store\Normal\Helium\helium\uhaToCore
move ..\Helium\helium\utils\*.hi            ..\Store\Normal\Helium\helium\utils

move ..\Store\retrieve*.bat .
move store*.bat ..\Store

rem this line is not reached, as the batch file moves itself

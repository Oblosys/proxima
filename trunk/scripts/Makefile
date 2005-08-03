# TODO: make option for AG and generator only without ghc compiling for use with ghci

# TODO: add all Haskell sources


#
# ?? also support building generator, ag compiler, helium, lvm, .. ?
# ?? use configure/automake?

AGC = d:/bin/uuagc
AGC_OPTS_Pres = -cfspm --self
AGC_OPTS_Arr = -cfspm

HC = ghc

# --make ../src/main/Main.hs -o Proxima.exe
HC_OPTS = -fglasgow-exts -fallow-undecidable-instances -package lang -package wx -i$(Common_DIR):$(Main_DIR):$(Gui_DIR):$(Evaluation_DIR):$(Presentation_DIR):$(Layout_DIR):$(Arrangement_DIR):$(Rendering_DIR)

Arrangement_DIR  = ../src/arrangement
Common_DIR       = ../src/common
Evaluation_DIR   = ../src/evaluation
Generator_DIR    = ../src/generator
Layout_DIR       = ../src/layout
Main_DIR         = ../src/main
Presentation_DIR = ../src/presentation
Rendering_DIR    = ../src/rendering

Parsec_DIRS = ../../parsec
LVM_DIRS = ../../lvm/src/lib/common \
           ../../lvm/src/lib/common/ghc \
           ../../lvm/src/lib/lvm \
           ../../lvm/src/lib/asm \
           ../../lvm/src/lib/core
Helium_DIRS = ../../heliumNT/src/syntax \
              ../../heliumNT/src/parser \
              ../../heliumNT/src/main \
              ../../heliumNT/src/utils \
              ../../heliumNT/src/modulesystem \
              ../../heliumNT/src/staticanalysis/constraints \ 
              ../../heliumNT/src/staticanalysis/inferencers \
              ../../heliumNT/src/staticanalysis/inferencers/typingstrategies \
              ../../heliumNT/src/staticanalysis/solvers \
              ../../heliumNT/src/staticanalysis/solvers/typegraph \
              ../../heliumNT/src/staticanalysis/solvers/typegraphheuristics \
              ../../heliumNT/src/staticanalysis/messages \
              ../../heliumNT/src/staticanalysis/staticchecks \
              ../../heliumNT/src/staticanalysis/types \
              ../../heliumNT/src/codegeneration \

GENERATOR = $(Generator_DIR)/Generate

H_SRCS = $(Main_DIR)/Main.hs\
         $(Arrangement_DIR)/.hs \
         $(Common_DIR)/.hs \
         $(Evaluation_DIR)/.hs \
         $(Layout_DIR)/.hs \
         $(Main_DIR)/.hs \
         $(Presentation_DIR)/.hs \
         $(Generated_Srcs)

Generated_Srcs = $(Evaluation_DIR)/DocTypes_Generated.hs $(Evaluation_DIR)/DocumentEdit_Generated.hs $(Evaluation_DIR)/DocUtils_Generated.hs $(Presentation_DIR)/PresentationAG_Generated.ag $(Presentation_DIR)/ProxParser_Generated.hs


All:

# Code generation

$(Generated_Srcs) : $(Generator_DIR)/DocumentType.hs
	cd $(Generator_DIR); Generate DocumentType.hs

	
# AG	

$(Arrangement_DIR)/ArrangerAG.hs : $(Arrangement_DIR)/ArrangerAG.ag
	cd $(Arrangement_DIR); $(AGC) $(AGC_OPTS_Arr) ArrangerAG.ag

$(Presentation_DIR)/PresentationAG.hs : $(Presentation_DIR)/PresentationAG.ag \
					$(Presentation_DIR)/PresentationAG_Generated.ag\
					$(Presentation_DIR)/LambdaReduce.ag\
					$(Presentation_DIR)/InvPresentation.ag
	cd $(Presentation_DIR); wrap $(AGC) $(AGC_OPTS_Pres) PresentationAG.ag


# Haskell

# 
depend :
	$(HC) -M $(HC_OPTS) $(H_SRCS)

# DO NOT DELETE: Beginning of Haskell dependencies
../src/main/Main.o : ../src/main/Main.hs
../src/main/Main.o : ../src/evaluation/EvalLayerTypes.hi
../src/main/Main.o : ../src/arrangement/ArrUtils.hi
../src/main/Main.o : ../src/layout/Scanner.hi
../src/main/Main.o : ../src/layout/Layout.hi
../src/main/Main.o : ../src/presentation/PresentationParsing.hi
../src/main/Main.o : ../src/presentation/ProxParser.hi
../src/main/Main.o : ../src/arrangement/FontLib.hi
../src/main/Main.o : ../src/rendering/RenTypes.hi
../src/main/Main.o : ../src/arrangement/ArrTypes.hi
../src/main/Main.o : ../src/layout/LayTypes.hi
../src/main/Main.o : ../src/presentation/PresUtils.hi
../src/main/Main.o : ../src/presentation/PresTypes.hi
../src/main/Main.o : ../src/evaluation/EnrTypes.hi
../src/main/Main.o : ../src/evaluation/DocTypes.hi
../src/main/Main.o : ../src/presentation/PresPresent.hi
../src/main/Main.o : ../src/evaluation/DocUtils.hi
../src/main/Main.o : ../src/common/CommonTypes.hi
../src/main/Main.o : ../src/main/GUI.hi
../src/main/Main.o : ../src/main/Architecture.hi
# DO NOT DELETE: End of Haskell dependencies

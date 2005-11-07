#-----------------------------------------------------------------------
#  The Proxima generic editor
#-----------------------------------------------------------------------

# make		 - build everything that is needed
# make depend	 - build dependencies for hs files, ag dependencies are	written	by hand
# make clean	 - remove .hi and .o files


# TODO:
# add platform dependent stuff (configure?)
# add strip
# TODO: on normal make, generator is not rebuilt after sources have been edited (only on make generate) May have something to do with fact that generation affects multiple files, whereas only one dependency (PresentationAG_Generated) is explicit in makeFile

# fix make clean

# HC
# moet er in de regel voor Proxima niet een ref naar de executable zitten?
# generator is mixed with the rest, maybe should be in different makefile, but
# since there is no conflict, this solution is okay
# now, because standard rules are used, generator ifaces must be added to hcopts

# for ghc-6.4
# $proxima\proxima\src\evaluation>ghc -c DocTypes.hs-boot
# $proxima\proxima\src\evaluation>DocTypes_Generated.hs-boot
# $proxima\lvm\src\lib\common>ghc -c Set.hs
# remove $proxima\proxima\src\presentation\PresTypes.hs:26-27 instance Show FiniteMap
# remove $proxima\Top\src\Top\Solvers\SimpleSubst.hs:20-21 instance Show SimpleState

# fix generation dir (refers to ../../editor now)
# fix presentationAG dir  (refers to ../../editor/src now)


HC    = ghc
FIND  = /usr/bin/find


SHELL = /bin/sh

# clear and set suffixes
.SUFFIXES:
.SUFFIXES: .o .hs .hi .lhs .hc	.s .ag


default: all
all: proxima

.PHONY : default proxima

PROXIMADIR = ../../proxima
# when changing this, also modify the path encoded in the sed script of the 'objects' target

PROXIMABINDIR = $(PROXIMADIR)/bin
PROXIMASRCDIR = $(PROXIMADIR)/src



AGC             = $(PROXIMABINDIR)/uuagc$(EXE)
GEN	        = $(PROXIMABINDIR)/generate$(EXE)
PROXIMAOBJSFILE = $(PROXIMABINDIR)/PROXIMAOBJS.txt

HC_OPTS = -static -fglasgow-exts -fallow-undecidable-instances -fno-monomorphism-restriction\
		-package wx -package util -package net -package data -package parsec\
		 -i$(IFACES) $(EXTRA_OPTS)

IFACES = $(PROXIMAIFACES):$(HELIUMIFACES):$(GENERATORIFACES)



PROXIMASRCS = $(ARRANGEMENTSRCS) $(COMMONSRCS) $(EVALUATIONSRCS) $(LAYOUTSRCS) \
		$(MAINSRCS) $(PRESENTATIONSRCS) $(RENDERINGSRCS)

SRCS = $(PROXIMASRCS)

OBJS = $(SRCS:.hs=.o)



# Inter-module dependencies
depend :
	# Checking dependencies
	$(HC) -M $(HC_OPTS) $(SRCS) $(GENERATORSRCS)




# Proxima directories

PRESENTATIONDIR = $(PROXIMADIR)/src/presentation
ARRANGEMENTDIR  = $(PROXIMADIR)/src/arrangement
COMMONDIR       = $(PROXIMADIR)/src/common
EVALUATIONDIR   = $(PROXIMADIR)/src/evaluation
GENERATORDIR    = $(PROXIMADIR)/src/generator
LAYOUTDIR       = $(PROXIMADIR)/src/layout
MAINDIR         = $(PROXIMADIR)/src/main
PRESENTATIONDIR = $(PROXIMADIR)/src/presentation
RENDERINGDIR    = $(PROXIMADIR)/src/rendering
PROXIMAIFACES	= $(ARRANGEMENTDIR):$(COMMONDIR):$(EVALUATIONDIR):$(GENERATORDIR):$(LAYOUTDIR):$(MAINDIR):$(PRESENTATIONDIR):$(RENDERINGDIR)




# Proxima sources

ARRANGEMENTSRCS = \
	$(ARRANGEMENTDIR)/ArrLayerTypes.hs \
	$(ARRANGEMENTDIR)/ArrLayerUtils.hs \
	$(ARRANGEMENTDIR)/ArrPresent.hs \
	$(ARRANGEMENTDIR)/ArrTranslate.hs \
	$(ARRANGEMENTDIR)/ArrTypes.hs \
	$(ARRANGEMENTDIR)/ArrUtils.hs \
	$(ARRANGEMENTDIR)/Arranger.hs \
	$(ARRANGEMENTDIR)/ArrangerAG.hs \
	$(ARRANGEMENTDIR)/FontLib.hs

COMMONSRCS = \
	$(COMMONDIR)/CommonTypes.hs \
	$(COMMONDIR)/CommonUtils.hs \
	$(COMMONDIR)/DebugLevels.hs \
	$(COMMONDIR)/UU_Parsing.hs
	
EVALUATIONSRCS = \
	$(EVALUATIONDIR)/DocTypes.hs \
	$(EVALUATIONDIR)/DocUtils.hs \
	$(EVALUATIONDIR)/DocumentEdit.hs \
	$(EVALUATIONDIR)/EnrTypes.hs \
	$(EVALUATIONDIR)/EnrUtils.hs \
	$(EVALUATIONDIR)/EvalLayerTypes.hs \
	$(EVALUATIONDIR)/EvalLayerUtils.hs \
	$(EVALUATIONDIR)/EvalPresent.hs \
	$(EVALUATIONDIR)/EvalTranslate.hs
	
LAYOUTSRCS = \
	$(LAYOUTDIR)/LayLayerTypes.hs \
	$(LAYOUTDIR)/LayLayerUtils.hs \
	$(LAYOUTDIR)/LayPresent.hs \
	$(LAYOUTDIR)/LayTranslate.hs \
	$(LAYOUTDIR)/LayTypes.hs \
	$(LAYOUTDIR)/LayUtils.hs \
	$(LAYOUTDIR)/Layout.hs \
	$(LAYOUTDIR)/TreeEditPres.hs
	
MAINSRCS = \
	$(MAINDIR)/Architecture.hs \
	$(MAINDIR)/ArchitectureLibM.hs \
	$(MAINDIR)/GUI.hs \
	$(MAINDIR)/Proxima.hs

PRESENTATIONSRCS = \
	$(PRESENTATIONDIR)/PresLayerTypes.hs \
	$(PRESENTATIONDIR)/PresLayerUtils.hs \
	$(PRESENTATIONDIR)/PresPresent.hs \
	$(PRESENTATIONDIR)/PresTranslate.hs \
	$(PRESENTATIONDIR)/PresTypes.hs \
	$(PRESENTATIONDIR)/PresUtils.hs \
	$(PRESENTATIONDIR)/PresentationParsing.hs \
	$(PRESENTATIONDIR)/Presenter.hs \
	$(PRESENTATIONDIR)/XprezLib.hs \
	$(PRESENTATIONDIR)/XLatex.hs

RENDERINGSRCS = \
	$(RENDERINGDIR)/RenLayerTypes.hs \
	$(RENDERINGDIR)/RenLayerUtils.hs \
	$(RENDERINGDIR)/RenPresent.hs \
	$(RENDERINGDIR)/RenTranslate.hs \
	$(RENDERINGDIR)/RenTypes.hs \
	$(RENDERINGDIR)/RenUtils.hs \
	$(RENDERINGDIR)/Renderer.hs



# TODO: order is important???? moving BinaryTrees below CharParser causes error
# also includes the uu lib sources
GENERATORSRCS = \
	../../uust/lib/scanner/UU/Scanner/Position.hs \
	../../uust/lib/parsing/UU/Parsing/MachineInterface.hs \
	../../uust/lib/parsing/UU/Parsing/Machine.hs \
	../../uust/lib/parsing/UU/Parsing/Interface.hs \
	../../uust/lib/parsing/UU/Parsing/Derived.hs \
	../../uust/lib/parsing/UU/Parsing.hs \
	../../uust/lib/parsing/UU/Parsing/CharParser.hs	\
	../../uust/lib/util/UU/Util/BinaryTrees.hs \
	$(GENERATORDIR)/GenCommon.hs \
	$(GENERATORDIR)/GenParser.hs \
	$(GENERATORDIR)/GenAG.hs \
	$(GENERATORDIR)/GenTypes.hs \
	$(GENERATORDIR)/GenEditable.hs \
	$(GENERATORDIR)/GenProxParser.hs \
	$(GENERATORDIR)/GenDocUtils.hs \
	$(GENERATORDIR)/Generator.hs
GENERATOROBJS = $(GENERATORSRCS:.hs=.o)

GENERATORIFACES = $(GENERATORDIR):../../uust/lib/parsing:../../uust/lib/scanner:../../uust/lib/util


objectsfile: $(PROXIMAOBJSFILE)

$(PROXIMAOBJSFILE): Makefile
	@echo Creating object list in $(PROXIMAOBJSFILE)
	echo $(OBJS) | sed -e s/\.\.\\/\.\.\\/proxima/$$\(PROXIMADIR\)/g > $(PROXIMAOBJSFILE)
#	echo $(OBJS) | sed -e s/\.\./$$\(PROXIMADIR\)/g > $(PROXIMAOBJSFILE)
# puts all objects in the text file $(PROXIMAOBJSFILE), while replacing
# the path "../../proxima" by "$(PROXIMADIR)"


# All proxima objects and the text file containing them
proxima: $(OBJS) $(PROXIMAOBJSFILE)

clean:
	$(FIND) $(PROXIMASRCDIR) -name "*.hi" -delete
	$(FIND) $(PROXIMASRCDIR) -name "*.o" -delete

# The Generator
generator : $(GEN)

$(GEN): $(GENERATOROBJS)
	$(HC) -fglasgow-exts \
	-o $(PROXIMABINDIR)/generate -i$(GENERATORIFACES) $(GENERATOROBJS)



# The AG compiler
# no dependencies, uuagc is assumed not to be modified
$(AGC): 
	ghc --make -o $(AGC) ../../uust/tools/ag/Ag.hs -package util -fglasgow-exts -O2 -i../../uust/tools/ag:../../uust/lib/ddata:../../uust/lib/pretty:../../uust/lib/scanner:../../uust/lib/util:../../uust/lib/parsing




arranger: $(ARRANGEMENTDIR)/ArrangerAG.hs

# The arrangemer AG

$(ARRANGEMENTDIR)/ArrangerAG.hs : $(AGC) $(ARRANGEMENTDIR)/ArrangerAG.ag \
	
	@echo "Compiling arrangerAG.ag"
	$(AGC) -o $(ARRANGEMENTDIR)/ArrangerAG.hs \
	-cfmp --module=ArrangerAG \
	$(ARRANGEMENTDIR)/ArrangerAG.ag -P $(ARRANGEMENTDIR)

# -m is because otherwise uuagc uses ../../presentation/PresentationAG as module name
	
# Standard suffix rules
.o.hi:
	@:

.lhs.o:
	$(HC) -c $< $(HC_OPTS)

.hs.o:
	$(HC) -c $< $(HC_OPTS)

.hs.hi:
	$(HC) -c $< $(HC_OPTS) 

.lhs.hi:
	$(HC) -c $< $(HC_OPTS)

.ag.hs:
	$(AGC) -a $<



# DO NOT DELETE: Beginning of Haskell dependencies
../src/generator/GenCommon.o : ../src/generator/GenCommon.hs
../src/generator/GenAG.o : ../src/generator/GenAG.hs
../src/generator/GenAG.o : ../src/generator/GenCommon.hi
../src/generator/GenTypes.o : ../src/generator/GenTypes.hs
../src/generator/GenTypes.o : ../src/generator/GenCommon.hi
../src/generator/GenEditable.o : ../src/generator/GenEditable.hs
../src/generator/GenEditable.o : ../src/generator/GenCommon.hi
../src/generator/GenProxParser.o : ../src/generator/GenProxParser.hs
../src/generator/GenProxParser.o : ../src/generator/GenCommon.hi
../src/generator/GenDocUtils.o : ../src/generator/GenDocUtils.hs
../src/generator/GenDocUtils.o : ../src/generator/GenCommon.hi
../../uust/lib/util/UU/Util/BinaryTrees.o : ../../uust/lib/util/UU/Util/BinaryTrees.hs
../../uust/lib/parsing/UU/Parsing/MachineInterface.o : ../../uust/lib/parsing/UU/Parsing/MachineInterface.hs
../../uust/lib/parsing/UU/Parsing/Machine.o : ../../uust/lib/parsing/UU/Parsing/Machine.hs
../../uust/lib/parsing/UU/Parsing/Machine.o : ../../uust/lib/parsing/UU/Parsing/MachineInterface.hi
../../uust/lib/parsing/UU/Parsing/Machine.o : ../../uust/lib/util/UU/Util/BinaryTrees.hi
../../uust/lib/parsing/UU/Parsing/Interface.o : ../../uust/lib/parsing/UU/Parsing/Interface.hs
../../uust/lib/parsing/UU/Parsing/Interface.o : ../../uust/lib/parsing/UU/Parsing/MachineInterface.hi
../../uust/lib/parsing/UU/Parsing/Interface.o : ../../uust/lib/parsing/UU/Parsing/Machine.hi
../../uust/lib/parsing/UU/Parsing/Derived.o : ../../uust/lib/parsing/UU/Parsing/Derived.hs
../../uust/lib/parsing/UU/Parsing/Derived.o : ../../uust/lib/parsing/UU/Parsing/Interface.hi
../../uust/lib/parsing/UU/Parsing.o : ../../uust/lib/parsing/UU/Parsing.hs
../../uust/lib/parsing/UU/Parsing.o : ../../uust/lib/parsing/UU/Parsing/Interface.hi
../../uust/lib/parsing/UU/Parsing.o : ../../uust/lib/parsing/UU/Parsing/Derived.hi
../../uust/lib/scanner/UU/Scanner/Position.o : ../../uust/lib/scanner/UU/Scanner/Position.hs
../../uust/lib/parsing/UU/Parsing/CharParser.o : ../../uust/lib/parsing/UU/Parsing/CharParser.hs
../../uust/lib/parsing/UU/Parsing/CharParser.o : ../../uust/lib/scanner/UU/Scanner/Position.hi
../../uust/lib/parsing/UU/Parsing/CharParser.o : ../../uust/lib/parsing/UU/Parsing/Interface.hi
../src/generator/GenParser.o : ../src/generator/GenParser.hs
../src/generator/GenParser.o : ../../uust/lib/parsing/UU/Parsing/CharParser.hi
../src/generator/GenParser.o : ../../uust/lib/parsing/UU/Parsing.hi
../src/generator/GenParser.o : ../src/generator/GenCommon.hi
../src/generator/Generator.o : ../src/generator/Generator.hs
../src/generator/Generator.o : ../src/generator/GenProxParser.hi
../src/generator/Generator.o : ../src/generator/GenDocUtils.hi
../src/generator/Generator.o : ../src/generator/GenEditable.hi
../src/generator/Generator.o : ../src/generator/GenTypes.hi
../src/generator/Generator.o : ../src/generator/GenAG.hi
../src/generator/Generator.o : ../src/generator/GenParser.hi
../src/generator/Generator.o : ../src/generator/GenCommon.hi
../src/main/ArchitectureLibM.o : ../src/main/ArchitectureLibM.hs
../src/common/UU_Parsing.o : ../src/common/UU_Parsing.hs
../src/common/DebugLevels.o : ../src/common/DebugLevels.hs
../src/common/CommonTypes.o : ../src/common/CommonTypes.hs
../src/common/CommonTypes.o : ../src/common/DebugLevels.hi
../src/common/CommonUtils.o : ../src/common/CommonUtils.hs
../src/common/CommonUtils.o : ../src/common/CommonTypes.hi
../src/evaluation/DocTypes.o : ../src/evaluation/DocTypes.hs
../src/evaluation/DocTypes.o : ../src/common/CommonTypes.hi
../src/presentation/PresTypes.o : ../src/presentation/PresTypes.hs
../src/presentation/PresTypes.o : ../src/common/CommonUtils.hi
../src/presentation/PresTypes.o : ../src/evaluation/DocTypes.hi
../src/presentation/PresTypes.o : ../src/common/CommonTypes.hi
../src/evaluation/DocUtils.o : ../src/evaluation/DocUtils.hs
../src/evaluation/DocUtils.o : ../src/presentation/PresTypes.hi
../src/evaluation/DocUtils.o : ../src/evaluation/DocTypes.hi
../src/evaluation/EnrUtils.o : ../src/evaluation/EnrUtils.hs
../src/evaluation/EnrUtils.o : ../src/evaluation/DocUtils.hi
../src/evaluation/EnrUtils.o : ../src/evaluation/DocTypes.hi
../src/evaluation/DocumentEdit.o : ../src/evaluation/DocumentEdit.hs
../src/evaluation/DocumentEdit.o : ../src/presentation/PresTypes.hi
../src/evaluation/DocumentEdit.o : ../src/evaluation/DocUtils.hi
../src/evaluation/DocumentEdit.o : ../src/evaluation/DocTypes.hi
../src/evaluation/DocumentEdit.o : ../src/common/CommonTypes.hi
../src/evaluation/EnrTypes.o : ../src/evaluation/EnrTypes.hs
../src/evaluation/EnrTypes.o : ../src/presentation/PresTypes.hi
../src/evaluation/EnrTypes.o : ../src/common/CommonUtils.hi
../src/evaluation/EnrTypes.o : ../src/evaluation/DocTypes.hi
../src/evaluation/EnrTypes.o : ../src/common/CommonTypes.hi
../src/evaluation/EvalLayerTypes.o : ../src/evaluation/EvalLayerTypes.hs
../src/evaluation/EvalLayerTypes.o : ../src/evaluation/DocTypes.hi
../src/evaluation/EvalLayerTypes.o : ../src/evaluation/EnrTypes.hi
../src/evaluation/EvalLayerTypes.o : ../src/common/CommonTypes.hi
../src/evaluation/EvalLayerUtils.o : ../src/evaluation/EvalLayerUtils.hs
../src/evaluation/EvalLayerUtils.o : ../src/evaluation/EnrUtils.hi
../src/evaluation/EvalLayerUtils.o : ../src/common/CommonUtils.hi
../src/evaluation/EvalLayerUtils.o : ../src/evaluation/EvalLayerTypes.hi
../src/evaluation/EvalLayerUtils.o : ../src/common/CommonTypes.hi
../src/evaluation/EvalPresent.o : ../src/evaluation/EvalPresent.hs
../src/evaluation/EvalPresent.o : ../src/evaluation/EvalLayerUtils.hi
../src/evaluation/EvalPresent.o : ../src/evaluation/EvalLayerTypes.hi
../src/evaluation/EvalPresent.o : ../src/common/CommonTypes.hi
../src/evaluation/EvalTranslate.o : ../src/evaluation/EvalTranslate.hs
../src/evaluation/EvalTranslate.o : ../src/evaluation/EvalLayerTypes.hi
../src/evaluation/EvalTranslate.o : ../src/common/CommonTypes.hi
../src/layout/LayTypes.o : ../src/layout/LayTypes.hs
../src/layout/LayTypes.o : ../src/evaluation/DocTypes.hi
../src/layout/LayTypes.o : ../src/common/CommonUtils.hi
../src/layout/LayTypes.o : ../src/presentation/PresTypes.hi
../src/layout/LayTypes.o : ../src/common/CommonTypes.hi
../src/layout/LayLayerTypes.o : ../src/layout/LayLayerTypes.hs
../src/layout/LayLayerTypes.o : ../src/layout/LayTypes.hi
../src/layout/LayLayerTypes.o : ../src/common/CommonTypes.hi
../src/presentation/PresLayerTypes.o : ../src/presentation/PresLayerTypes.hs
../src/presentation/PresLayerTypes.o : ../src/presentation/PresTypes.hi
../src/presentation/PresLayerTypes.o : ../src/evaluation/EnrTypes.hi
../src/presentation/PresLayerTypes.o : ../src/common/CommonTypes.hi
../src/presentation/XprezLib.o : ../src/presentation/XprezLib.hs
../src/presentation/XprezLib.o : ../src/presentation/PresTypes.hi
../src/presentation/XprezLib.o : ../src/evaluation/DocTypes.hi
../src/presentation/XprezLib.o : ../src/common/CommonTypes.hi
../src/presentation/PresUtils.o : ../src/presentation/PresUtils.hs
../src/presentation/PresUtils.o : ../src/presentation/XprezLib.hi
../src/presentation/PresUtils.o : ../src/common/CommonUtils.hi
../src/presentation/PresUtils.o : ../src/presentation/PresTypes.hi
../src/presentation/PresUtils.o : ../src/common/CommonTypes.hi
../src/layout/LayUtils.o : ../src/layout/LayUtils.hs
../src/layout/LayUtils.o : ../src/presentation/PresUtils.hi
../src/layout/LayLayerUtils.o : ../src/layout/LayLayerUtils.hs
../src/layout/LayLayerUtils.o : ../src/layout/LayUtils.hi
../src/layout/LayLayerUtils.o : ../src/common/CommonUtils.hi
../src/layout/LayLayerUtils.o : ../src/layout/LayLayerTypes.hi
../src/layout/LayLayerUtils.o : ../src/common/CommonTypes.hi
../src/layout/Layout.o : ../src/layout/Layout.hs
../src/layout/Layout.o : ../src/layout/LayLayerUtils.hi
../src/layout/Layout.o : ../src/layout/LayLayerTypes.hi
../src/layout/Layout.o : ../src/common/CommonTypes.hi
../src/layout/TreeEditPres.o : ../src/layout/TreeEditPres.hs
../src/layout/TreeEditPres.o : ../src/presentation/XprezLib.hi
../src/layout/TreeEditPres.o : ../src/presentation/PresUtils.hi
../src/layout/TreeEditPres.o : ../src/presentation/PresTypes.hi
../src/layout/TreeEditPres.o : ../src/evaluation/DocTypes.hi
../src/layout/TreeEditPres.o : ../src/common/CommonTypes.hi
../src/layout/LayPresent.o : ../src/layout/LayPresent.hs
../src/layout/LayPresent.o : ../src/layout/TreeEditPres.hi
../src/layout/LayPresent.o : ../src/layout/Layout.hi
../src/layout/LayPresent.o : ../src/layout/LayLayerUtils.hi
../src/layout/LayPresent.o : ../src/layout/LayLayerTypes.hi
../src/layout/LayPresent.o : ../src/common/CommonTypes.hi
../src/layout/LayTranslate.o : ../src/layout/LayTranslate.hs
../src/layout/LayTranslate.o : ../src/layout/TreeEditPres.hi
../src/layout/LayTranslate.o : ../src/layout/LayLayerUtils.hi
../src/layout/LayTranslate.o : ../src/layout/LayLayerTypes.hi
../src/layout/LayTranslate.o : ../src/common/CommonTypes.hi
../src/presentation/PresLayerUtils.o : ../src/presentation/PresLayerUtils.hs
../src/presentation/PresLayerUtils.o : ../src/presentation/PresUtils.hi
../src/presentation/PresLayerUtils.o : ../src/evaluation/EnrUtils.hi
../src/presentation/PresLayerUtils.o : ../src/common/CommonUtils.hi
../src/presentation/PresLayerUtils.o : ../src/presentation/PresLayerTypes.hi
../src/presentation/PresLayerUtils.o : ../src/common/CommonTypes.hi
../src/presentation/PresTranslate.o : ../src/presentation/PresTranslate.hs
../src/presentation/PresTranslate.o : ../src/presentation/PresLayerUtils.hi
../src/presentation/PresTranslate.o : ../src/presentation/PresLayerTypes.hi
../src/presentation/PresTranslate.o : ../src/common/CommonTypes.hi
../src/presentation/PresentationParsing.o : ../src/presentation/PresentationParsing.hs
../src/presentation/PresentationParsing.o : ../src/common/UU_Parsing.hi
../src/presentation/PresentationParsing.o : ../src/common/UU_Parsing.hi
../src/presentation/PresentationParsing.o : ../src/presentation/XprezLib.hi
../src/presentation/PresentationParsing.o : ../src/evaluation/DocumentEdit.hi
../src/presentation/PresentationParsing.o : ../src/presentation/PresTypes.hi
../src/presentation/PresentationParsing.o : ../src/common/CommonTypes.hi
../src/presentation/Presenter.o : ../src/presentation/Presenter.hs
../src/presentation/Presenter.o : ../src/layout/TreeEditPres.hi
../src/presentation/Presenter.o : ../src/presentation/XprezLib.hi
../src/presentation/Presenter.o : ../src/presentation/PresLayerUtils.hi
../src/presentation/Presenter.o : ../src/presentation/PresLayerTypes.hi
../src/presentation/Presenter.o : ../src/common/CommonTypes.hi
../src/presentation/PresPresent.o : ../src/presentation/PresPresent.hs
../src/presentation/PresPresent.o : ../src/presentation/Presenter.hi
../src/presentation/PresPresent.o : ../src/presentation/PresLayerUtils.hi
../src/presentation/PresPresent.o : ../src/presentation/PresLayerTypes.hi
../src/presentation/PresPresent.o : ../src/common/CommonTypes.hi
../src/presentation/XLatex.o : ../src/presentation/XLatex.hs
../src/presentation/XLatex.o : ../src/common/CommonTypes.hi
../src/presentation/XLatex.o : ../src/presentation/PresTypes.hi
../src/presentation/XLatex.o : ../src/presentation/XprezLib.hi
../src/rendering/RenTypes.o : ../src/rendering/RenTypes.hs
../src/rendering/RenTypes.o : ../src/evaluation/DocTypes.hi
../src/rendering/RenTypes.o : ../src/common/CommonUtils.hi
../src/rendering/RenTypes.o : ../src/common/CommonTypes.hi
../src/main/GUI.o : ../src/main/GUI.hs
../src/main/GUI.o : ../src/common/CommonUtils.hi
../src/main/GUI.o : ../src/rendering/RenTypes.hi
../src/main/GUI.o : ../src/common/CommonTypes.hi
../src/main/GUI.o : ../src/common/CommonTypes.hi
../src/rendering/RenUtils.o : ../src/rendering/RenUtils.hs
../src/rendering/RenUtils.o : ../src/rendering/RenTypes.hi
../src/rendering/RenUtils.o : ../src/common/CommonTypes.hi
../src/arrangement/FontLib.o : ../src/arrangement/FontLib.hs
../src/arrangement/FontLib.o : ../src/common/CommonTypes.hi
../src/arrangement/ArrTypes.o : ../src/arrangement/ArrTypes.hs
../src/arrangement/ArrTypes.o : ../src/presentation/PresTypes.hi
../src/arrangement/ArrTypes.o : ../src/evaluation/DocTypes.hi
../src/arrangement/ArrTypes.o : ../src/common/CommonTypes.hi
../src/arrangement/ArrUtils.o : ../src/arrangement/ArrUtils.hs
../src/arrangement/ArrUtils.o : ../src/common/CommonUtils.hi
../src/arrangement/ArrUtils.o : ../src/arrangement/ArrTypes.hi
../src/arrangement/ArrUtils.o : ../src/common/CommonTypes.hi
../src/rendering/RenLayerTypes.o : ../src/rendering/RenLayerTypes.hs
../src/rendering/RenLayerTypes.o : ../src/rendering/RenTypes.hi
../src/rendering/RenLayerTypes.o : ../src/arrangement/ArrTypes.hi
../src/rendering/RenLayerTypes.o : ../src/common/CommonTypes.hi
../src/rendering/RenLayerUtils.o : ../src/rendering/RenLayerUtils.hs
../src/rendering/RenLayerUtils.o : ../src/rendering/RenUtils.hi
../src/rendering/RenLayerUtils.o : ../src/arrangement/ArrUtils.hi
../src/rendering/RenLayerUtils.o : ../src/rendering/RenLayerTypes.hi
../src/rendering/RenLayerUtils.o : ../src/common/CommonTypes.hi
../src/rendering/RenTranslate.o : ../src/rendering/RenTranslate.hs
../src/rendering/RenTranslate.o : ../src/rendering/RenLayerUtils.hi
../src/rendering/RenTranslate.o : ../src/rendering/RenLayerTypes.hi
../src/rendering/RenTranslate.o : ../src/common/CommonTypes.hi
../src/arrangement/ArrLayerTypes.o : ../src/arrangement/ArrLayerTypes.hs
../src/arrangement/ArrLayerTypes.o : ../src/arrangement/FontLib.hi
../src/arrangement/ArrLayerTypes.o : ../src/arrangement/ArrTypes.hi
../src/arrangement/ArrLayerTypes.o : ../src/layout/LayTypes.hi
../src/arrangement/ArrLayerTypes.o : ../src/common/CommonTypes.hi
../src/arrangement/ArrLayerUtils.o : ../src/arrangement/ArrLayerUtils.hs
../src/arrangement/ArrLayerUtils.o : ../src/arrangement/ArrUtils.hi
../src/arrangement/ArrLayerUtils.o : ../src/layout/LayUtils.hi
../src/arrangement/ArrLayerUtils.o : ../src/common/CommonUtils.hi
../src/arrangement/ArrLayerUtils.o : ../src/arrangement/ArrLayerTypes.hi
../src/arrangement/ArrLayerUtils.o : ../src/common/CommonTypes.hi
../src/rendering/Renderer.o : ../src/rendering/Renderer.hs
../src/rendering/Renderer.o : ../src/main/GUI.hi
../src/rendering/Renderer.o : ../src/evaluation/DocumentEdit.hi
../src/rendering/Renderer.o : ../src/evaluation/DocUtils.hi
../src/rendering/Renderer.o : ../src/evaluation/DocTypes.hi
../src/rendering/Renderer.o : ../src/presentation/PresTypes.hi
../src/rendering/Renderer.o : ../src/arrangement/ArrLayerUtils.hi
../src/rendering/Renderer.o : ../src/rendering/RenLayerUtils.hi
../src/rendering/Renderer.o : ../src/rendering/RenLayerTypes.hi
../src/rendering/Renderer.o : ../src/common/CommonTypes.hi
../src/rendering/Renderer.o : ../src/common/CommonTypes.hi
../src/rendering/RenPresent.o : ../src/rendering/RenPresent.hs
../src/rendering/RenPresent.o : ../src/evaluation/DocTypes.hi
../src/rendering/RenPresent.o : ../src/rendering/Renderer.hi
../src/rendering/RenPresent.o : ../src/rendering/RenLayerUtils.hi
../src/rendering/RenPresent.o : ../src/rendering/RenLayerTypes.hi
../src/rendering/RenPresent.o : ../src/common/CommonTypes.hi
../src/arrangement/ArrTranslate.o : ../src/arrangement/ArrTranslate.hs
../src/arrangement/ArrTranslate.o : ../src/layout/TreeEditPres.hi
../src/arrangement/ArrTranslate.o : ../src/evaluation/DocUtils.hi
../src/arrangement/ArrTranslate.o : ../src/evaluation/DocTypes.hi
../src/arrangement/ArrTranslate.o : ../src/arrangement/ArrLayerUtils.hi
../src/arrangement/ArrTranslate.o : ../src/arrangement/ArrLayerTypes.hi
../src/arrangement/ArrTranslate.o : ../src/common/CommonTypes.hi
../src/arrangement/ArrangerAG.o : ../src/arrangement/ArrangerAG.hs
../src/arrangement/ArrangerAG.o : ../src/arrangement/FontLib.hi
../src/arrangement/ArrangerAG.o : ../src/arrangement/ArrLayerUtils.hi
../src/arrangement/ArrangerAG.o : ../src/arrangement/ArrLayerTypes.hi
../src/arrangement/ArrangerAG.o : ../src/common/CommonTypes.hi
../src/arrangement/Arranger.o : ../src/arrangement/Arranger.hs
../src/arrangement/Arranger.o : ../src/arrangement/FontLib.hi
../src/arrangement/Arranger.o : ../src/arrangement/ArrangerAG.hi
../src/arrangement/Arranger.o : ../src/arrangement/ArrLayerUtils.hi
../src/arrangement/Arranger.o : ../src/arrangement/ArrLayerTypes.hi
../src/arrangement/Arranger.o : ../src/common/CommonTypes.hi
../src/arrangement/ArrPresent.o : ../src/arrangement/ArrPresent.hs
../src/arrangement/ArrPresent.o : ../src/arrangement/Arranger.hi
../src/arrangement/ArrPresent.o : ../src/arrangement/ArrLayerUtils.hi
../src/arrangement/ArrPresent.o : ../src/arrangement/ArrLayerTypes.hi
../src/arrangement/ArrPresent.o : ../src/common/CommonTypes.hi
../src/main/Architecture.o : ../src/main/Architecture.hs
../src/main/Architecture.o : ../src/rendering/RenTranslate.hi
../src/main/Architecture.o : ../src/rendering/RenPresent.hi
../src/main/Architecture.o : ../src/arrangement/ArrTranslate.hi
../src/main/Architecture.o : ../src/arrangement/ArrPresent.hi
../src/main/Architecture.o : ../src/layout/LayTranslate.hi
../src/main/Architecture.o : ../src/layout/LayPresent.hi
../src/main/Architecture.o : ../src/presentation/PresTranslate.hi
../src/main/Architecture.o : ../src/presentation/PresPresent.hi
../src/main/Architecture.o : ../src/evaluation/EvalTranslate.hi
../src/main/Architecture.o : ../src/evaluation/EvalPresent.hi
../src/main/Architecture.o : ../src/main/ArchitectureLibM.hi
../src/main/Proxima.o : ../src/main/Proxima.hs
../src/main/Proxima.o : ../src/evaluation/EvalLayerTypes.hi
../src/main/Proxima.o : ../src/arrangement/ArrUtils.hi
../src/main/Proxima.o : ../src/layout/Layout.hi
../src/main/Proxima.o : ../src/presentation/PresentationParsing.hi
../src/main/Proxima.o : ../src/layout/LayLayerTypes.hi
../src/main/Proxima.o : ../src/presentation/PresLayerTypes.hi
../src/main/Proxima.o : ../src/evaluation/EvalLayerTypes.hi
../src/main/Proxima.o : ../src/arrangement/FontLib.hi
../src/main/Proxima.o : ../src/rendering/RenTypes.hi
../src/main/Proxima.o : ../src/arrangement/ArrTypes.hi
../src/main/Proxima.o : ../src/layout/LayTypes.hi
../src/main/Proxima.o : ../src/presentation/PresUtils.hi
../src/main/Proxima.o : ../src/presentation/PresTypes.hi
../src/main/Proxima.o : ../src/evaluation/EnrTypes.hi
../src/main/Proxima.o : ../src/presentation/PresPresent.hi
../src/main/Proxima.o : ../src/evaluation/DocUtils.hi
../src/main/Proxima.o : ../src/evaluation/DocTypes.hi
../src/main/Proxima.o : ../src/common/CommonTypes.hi
../src/main/Proxima.o : ../src/main/GUI.hi
../src/main/Proxima.o : ../src/main/Architecture.hi
# DO NOT DELETE: End of Haskell dependencies

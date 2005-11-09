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
#HC    = /c/Program\ Files/Visual\ Haskell/bin/ghc
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
UUSTDIR = $(PROXIMADIR)/../uust

AGC             = $(PROXIMABINDIR)/uuagc$(EXE)
GEN	        = $(PROXIMABINDIR)/generate$(EXE)
PROXIMAOBJSFILE = $(PROXIMABINDIR)/PROXIMAOBJS.txt

HC_OPTS = -static -fglasgow-exts -fallow-undecidable-instances -fno-monomorphism-restriction\
		-package wx -package parsec\
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
	../../uust/lib/util/UU/Util/BinaryTrees.hs \
	../../uust/lib/scanner/UU/Scanner/Position.hs \
	../../uust/lib/parsing/UU/Parsing/MachineInterface.hs \
	../../uust/lib/parsing/UU/Parsing/Machine.hs \
	../../uust/lib/parsing/UU/Parsing/Interface.hs \
	../../uust/lib/parsing/UU/Parsing/Derived.hs \
	../../uust/lib/parsing/UU/Parsing.hs \
	../../uust/lib/parsing/UU/Parsing/CharParser.hs	\
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
	@echo $(OBJS) | sed -e s/\.\.\\/\.\.\\/proxima/$$\(PROXIMADIR\)/g > $(PROXIMAOBJSFILE)
#	echo $(OBJS) | sed -e s/\.\./$$\(PROXIMADIR\)/g > $(PROXIMAOBJSFILE)
# puts all objects in the text file $(PROXIMAOBJSFILE), while replacing
# the path "../../proxima" by "$(PROXIMADIR)"


# All proxima objects and the text file containing them
proxima: $(OBJS) $(PROXIMAOBJSFILE)

clean:
	$(FIND) $(PROXIMASRCDIR) -name "*.hi" -delete
	$(FIND) $(PROXIMASRCDIR) -name "*.o" -delete
	

clean-agc:
	$(FIND) $(UUSTDIR) -name "*.hi" -delete
	$(FIND) $(UUSTDIR) -name "*.o" -delete
	$(FIND) $(PROXIMABINDIR) -name "uuagc.*" -delete

clean-all: clean clean-agc

# The Generator
generator : $(GEN)

$(GEN): $(GENERATOROBJS)
	@echo GHC: compiling Generator
	@$(HC) -fglasgow-exts \
	-o $(PROXIMABINDIR)/generate -i$(GENERATORIFACES) $(GENERATOROBJS)



# The AG compiler
# no dependencies, uuagc is assumed not to be modified

agc: $(AGC)

$(AGC): 
	@echo GHC: compiling uuagc compiler with -O2
	@ghc --make -o $(AGC) $(UUSTDIR)/tools/ag/Ag.hs -package util -fglasgow-exts -O2 -i$(UUSTDIR)/tools/ag:$(UUSTDIR)/lib/ddata:$(UUSTDIR)/lib/pretty:$(UUSTDIR)/lib/scanner:$(UUSTDIR)/lib/util:$(UUSTDIR)/lib/parsing




arranger: $(ARRANGEMENTDIR)/ArrangerAG.hs

# The arrangemer AG

$(ARRANGEMENTDIR)/ArrangerAG.hs : $(AGC) $(ARRANGEMENTDIR)/ArrangerAG.ag \
	
	@echo "Compiling arrangerAG.ag"
	@$(AGC) -o $(ARRANGEMENTDIR)/ArrangerAG.hs \
	-cfmp --module=ArrangerAG \
	$(ARRANGEMENTDIR)/ArrangerAG.ag -P $(ARRANGEMENTDIR)

# -m is because otherwise uuagc uses ../../presentation/PresentationAG as module name
	
# Standard suffix rules
.o.hi:
	@:

.lhs.o:
	@echo GHC: compiling $<
	@$(HC) -c $< $(HC_OPTS)

.hs.o:
	@echo GHC: compiling $<
	@$(HC) -c $< $(HC_OPTS)

.hs.hi:
	@echo GHC: compiling $<
	@$(HC) -c $< $(HC_OPTS) 

.lhs.hi:
	@echo GHC: compiling $<
	@$(HC) -c $< $(HC_OPTS)

.ag.hs:
	@echo AGC: compiling $<
	@$(AGC) -a $<



# DO NOT DELETE: Beginning of Haskell dependencies
../../proxima/src/generator/GenCommon.o : ../../proxima/src/generator/GenCommon.hs
../../proxima/src/generator/GenAG.o : ../../proxima/src/generator/GenAG.hs
../../proxima/src/generator/GenAG.o : ../../proxima/src/generator/GenCommon.hi
../../proxima/src/generator/GenTypes.o : ../../proxima/src/generator/GenTypes.hs
../../proxima/src/generator/GenTypes.o : ../../proxima/src/generator/GenCommon.hi
../../proxima/src/generator/GenEditable.o : ../../proxima/src/generator/GenEditable.hs
../../proxima/src/generator/GenEditable.o : ../../proxima/src/generator/GenCommon.hi
../../proxima/src/generator/GenProxParser.o : ../../proxima/src/generator/GenProxParser.hs
../../proxima/src/generator/GenProxParser.o : ../../proxima/src/generator/GenCommon.hi
../../proxima/src/generator/GenDocUtils.o : ../../proxima/src/generator/GenDocUtils.hs
../../proxima/src/generator/GenDocUtils.o : ../../proxima/src/generator/GenCommon.hi
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
../../proxima/src/generator/GenParser.o : ../../proxima/src/generator/GenParser.hs
../../proxima/src/generator/GenParser.o : ../../uust/lib/parsing/UU/Parsing/CharParser.hi
../../proxima/src/generator/GenParser.o : ../../uust/lib/parsing/UU/Parsing.hi
../../proxima/src/generator/GenParser.o : ../../proxima/src/generator/GenCommon.hi
../../proxima/src/generator/Generator.o : ../../proxima/src/generator/Generator.hs
../../proxima/src/generator/Generator.o : ../../proxima/src/generator/GenProxParser.hi
../../proxima/src/generator/Generator.o : ../../proxima/src/generator/GenDocUtils.hi
../../proxima/src/generator/Generator.o : ../../proxima/src/generator/GenEditable.hi
../../proxima/src/generator/Generator.o : ../../proxima/src/generator/GenTypes.hi
../../proxima/src/generator/Generator.o : ../../proxima/src/generator/GenAG.hi
../../proxima/src/generator/Generator.o : ../../proxima/src/generator/GenParser.hi
../../proxima/src/generator/Generator.o : ../../proxima/src/generator/GenCommon.hi
../../proxima/src/main/ArchitectureLibM.o : ../../proxima/src/main/ArchitectureLibM.hs
../../proxima/src/common/UU_Parsing.o : ../../proxima/src/common/UU_Parsing.hs
../../proxima/src/common/DebugLevels.o : ../../proxima/src/common/DebugLevels.hs
../../proxima/src/common/CommonTypes.o : ../../proxima/src/common/CommonTypes.hs
../../proxima/src/common/CommonTypes.o : ../../proxima/src/common/DebugLevels.hi
../../proxima/src/common/CommonUtils.o : ../../proxima/src/common/CommonUtils.hs
../../proxima/src/common/CommonUtils.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/evaluation/DocTypes.o : ../../proxima/src/evaluation/DocTypes.hs
../../proxima/src/evaluation/DocTypes.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/presentation/PresTypes.o : ../../proxima/src/presentation/PresTypes.hs
../../proxima/src/presentation/PresTypes.o : ../../proxima/src/common/CommonUtils.hi
../../proxima/src/presentation/PresTypes.o : ../../proxima/src/evaluation/DocTypes.hi
../../proxima/src/presentation/PresTypes.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/evaluation/DocUtils.o : ../../proxima/src/evaluation/DocUtils.hs
../../proxima/src/evaluation/DocUtils.o : ../../proxima/src/presentation/PresTypes.hi
../../proxima/src/evaluation/DocUtils.o : ../../proxima/src/evaluation/DocTypes.hi
../../proxima/src/evaluation/EnrUtils.o : ../../proxima/src/evaluation/EnrUtils.hs
../../proxima/src/evaluation/EnrUtils.o : ../../proxima/src/evaluation/DocUtils.hi
../../proxima/src/evaluation/EnrUtils.o : ../../proxima/src/evaluation/DocTypes.hi
../../proxima/src/evaluation/DocumentEdit.o : ../../proxima/src/evaluation/DocumentEdit.hs
../../proxima/src/evaluation/DocumentEdit.o : ../../proxima/src/presentation/PresTypes.hi
../../proxima/src/evaluation/DocumentEdit.o : ../../proxima/src/evaluation/DocUtils.hi
../../proxima/src/evaluation/DocumentEdit.o : ../../proxima/src/evaluation/DocTypes.hi
../../proxima/src/evaluation/DocumentEdit.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/evaluation/EnrTypes.o : ../../proxima/src/evaluation/EnrTypes.hs
../../proxima/src/evaluation/EnrTypes.o : ../../proxima/src/presentation/PresTypes.hi
../../proxima/src/evaluation/EnrTypes.o : ../../proxima/src/common/CommonUtils.hi
../../proxima/src/evaluation/EnrTypes.o : ../../proxima/src/evaluation/DocTypes.hi
../../proxima/src/evaluation/EnrTypes.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/evaluation/EvalLayerTypes.o : ../../proxima/src/evaluation/EvalLayerTypes.hs
../../proxima/src/evaluation/EvalLayerTypes.o : ../../proxima/src/evaluation/DocTypes.hi
../../proxima/src/evaluation/EvalLayerTypes.o : ../../proxima/src/evaluation/EnrTypes.hi
../../proxima/src/evaluation/EvalLayerTypes.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/evaluation/EvalLayerUtils.o : ../../proxima/src/evaluation/EvalLayerUtils.hs
../../proxima/src/evaluation/EvalLayerUtils.o : ../../proxima/src/evaluation/EnrUtils.hi
../../proxima/src/evaluation/EvalLayerUtils.o : ../../proxima/src/common/CommonUtils.hi
../../proxima/src/evaluation/EvalLayerUtils.o : ../../proxima/src/evaluation/EvalLayerTypes.hi
../../proxima/src/evaluation/EvalLayerUtils.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/evaluation/EvalPresent.o : ../../proxima/src/evaluation/EvalPresent.hs
../../proxima/src/evaluation/EvalPresent.o : ../../proxima/src/evaluation/EvalLayerUtils.hi
../../proxima/src/evaluation/EvalPresent.o : ../../proxima/src/evaluation/EvalLayerTypes.hi
../../proxima/src/evaluation/EvalPresent.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/evaluation/EvalTranslate.o : ../../proxima/src/evaluation/EvalTranslate.hs
../../proxima/src/evaluation/EvalTranslate.o : ../../proxima/src/evaluation/EvalLayerTypes.hi
../../proxima/src/evaluation/EvalTranslate.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/layout/LayTypes.o : ../../proxima/src/layout/LayTypes.hs
../../proxima/src/layout/LayTypes.o : ../../proxima/src/evaluation/DocTypes.hi
../../proxima/src/layout/LayTypes.o : ../../proxima/src/common/CommonUtils.hi
../../proxima/src/layout/LayTypes.o : ../../proxima/src/presentation/PresTypes.hi
../../proxima/src/layout/LayTypes.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/layout/LayLayerTypes.o : ../../proxima/src/layout/LayLayerTypes.hs
../../proxima/src/layout/LayLayerTypes.o : ../../proxima/src/layout/LayTypes.hi
../../proxima/src/layout/LayLayerTypes.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/presentation/PresLayerTypes.o : ../../proxima/src/presentation/PresLayerTypes.hs
../../proxima/src/presentation/PresLayerTypes.o : ../../proxima/src/presentation/PresTypes.hi
../../proxima/src/presentation/PresLayerTypes.o : ../../proxima/src/evaluation/EnrTypes.hi
../../proxima/src/presentation/PresLayerTypes.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/presentation/XprezLib.o : ../../proxima/src/presentation/XprezLib.hs
../../proxima/src/presentation/XprezLib.o : ../../proxima/src/presentation/PresTypes.hi
../../proxima/src/presentation/XprezLib.o : ../../proxima/src/evaluation/DocTypes.hi
../../proxima/src/presentation/XprezLib.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/presentation/PresUtils.o : ../../proxima/src/presentation/PresUtils.hs
../../proxima/src/presentation/PresUtils.o : ../../proxima/src/presentation/XprezLib.hi
../../proxima/src/presentation/PresUtils.o : ../../proxima/src/common/CommonUtils.hi
../../proxima/src/presentation/PresUtils.o : ../../proxima/src/presentation/PresTypes.hi
../../proxima/src/presentation/PresUtils.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/layout/LayUtils.o : ../../proxima/src/layout/LayUtils.hs
../../proxima/src/layout/LayUtils.o : ../../proxima/src/presentation/PresUtils.hi
../../proxima/src/layout/LayLayerUtils.o : ../../proxima/src/layout/LayLayerUtils.hs
../../proxima/src/layout/LayLayerUtils.o : ../../proxima/src/layout/LayUtils.hi
../../proxima/src/layout/LayLayerUtils.o : ../../proxima/src/common/CommonUtils.hi
../../proxima/src/layout/LayLayerUtils.o : ../../proxima/src/layout/LayLayerTypes.hi
../../proxima/src/layout/LayLayerUtils.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/layout/Layout.o : ../../proxima/src/layout/Layout.hs
../../proxima/src/layout/Layout.o : ../../proxima/src/layout/LayLayerUtils.hi
../../proxima/src/layout/Layout.o : ../../proxima/src/layout/LayLayerTypes.hi
../../proxima/src/layout/Layout.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/layout/TreeEditPres.o : ../../proxima/src/layout/TreeEditPres.hs
../../proxima/src/layout/TreeEditPres.o : ../../proxima/src/presentation/XprezLib.hi
../../proxima/src/layout/TreeEditPres.o : ../../proxima/src/presentation/PresUtils.hi
../../proxima/src/layout/TreeEditPres.o : ../../proxima/src/presentation/PresTypes.hi
../../proxima/src/layout/TreeEditPres.o : ../../proxima/src/evaluation/DocTypes.hi
../../proxima/src/layout/TreeEditPres.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/layout/LayPresent.o : ../../proxima/src/layout/LayPresent.hs
../../proxima/src/layout/LayPresent.o : ../../proxima/src/layout/TreeEditPres.hi
../../proxima/src/layout/LayPresent.o : ../../proxima/src/layout/Layout.hi
../../proxima/src/layout/LayPresent.o : ../../proxima/src/layout/LayLayerUtils.hi
../../proxima/src/layout/LayPresent.o : ../../proxima/src/layout/LayLayerTypes.hi
../../proxima/src/layout/LayPresent.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/layout/LayTranslate.o : ../../proxima/src/layout/LayTranslate.hs
../../proxima/src/layout/LayTranslate.o : ../../proxima/src/layout/TreeEditPres.hi
../../proxima/src/layout/LayTranslate.o : ../../proxima/src/layout/LayLayerUtils.hi
../../proxima/src/layout/LayTranslate.o : ../../proxima/src/layout/LayLayerTypes.hi
../../proxima/src/layout/LayTranslate.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/presentation/PresLayerUtils.o : ../../proxima/src/presentation/PresLayerUtils.hs
../../proxima/src/presentation/PresLayerUtils.o : ../../proxima/src/presentation/PresUtils.hi
../../proxima/src/presentation/PresLayerUtils.o : ../../proxima/src/evaluation/EnrUtils.hi
../../proxima/src/presentation/PresLayerUtils.o : ../../proxima/src/common/CommonUtils.hi
../../proxima/src/presentation/PresLayerUtils.o : ../../proxima/src/presentation/PresLayerTypes.hi
../../proxima/src/presentation/PresLayerUtils.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/presentation/PresPresent.o : ../../proxima/src/presentation/PresPresent.hs
../../proxima/src/presentation/PresPresent.o : ../../proxima/src/presentation/PresLayerUtils.hi
../../proxima/src/presentation/PresPresent.o : ../../proxima/src/presentation/PresLayerTypes.hi
../../proxima/src/presentation/PresPresent.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/presentation/PresTranslate.o : ../../proxima/src/presentation/PresTranslate.hs
../../proxima/src/presentation/PresTranslate.o : ../../proxima/src/presentation/PresLayerUtils.hi
../../proxima/src/presentation/PresTranslate.o : ../../proxima/src/presentation/PresLayerTypes.hi
../../proxima/src/presentation/PresTranslate.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/presentation/PresentationParsing.o : ../../proxima/src/presentation/PresentationParsing.hs
../../proxima/src/presentation/PresentationParsing.o : ../../proxima/src/common/UU_Parsing.hi
../../proxima/src/presentation/PresentationParsing.o : ../../proxima/src/common/UU_Parsing.hi
../../proxima/src/presentation/PresentationParsing.o : ../../proxima/src/presentation/XprezLib.hi
../../proxima/src/presentation/PresentationParsing.o : ../../proxima/src/evaluation/DocumentEdit.hi
../../proxima/src/presentation/PresentationParsing.o : ../../proxima/src/presentation/PresTypes.hi
../../proxima/src/presentation/PresentationParsing.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/presentation/XLatex.o : ../../proxima/src/presentation/XLatex.hs
../../proxima/src/presentation/XLatex.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/presentation/XLatex.o : ../../proxima/src/presentation/PresTypes.hi
../../proxima/src/presentation/XLatex.o : ../../proxima/src/presentation/XprezLib.hi
../../proxima/src/rendering/RenTypes.o : ../../proxima/src/rendering/RenTypes.hs
../../proxima/src/rendering/RenTypes.o : ../../proxima/src/evaluation/DocTypes.hi
../../proxima/src/rendering/RenTypes.o : ../../proxima/src/common/CommonUtils.hi
../../proxima/src/rendering/RenTypes.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/main/GUI.o : ../../proxima/src/main/GUI.hs
../../proxima/src/main/GUI.o : ../../proxima/src/common/CommonUtils.hi
../../proxima/src/main/GUI.o : ../../proxima/src/rendering/RenTypes.hi
../../proxima/src/main/GUI.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/main/GUI.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/rendering/RenUtils.o : ../../proxima/src/rendering/RenUtils.hs
../../proxima/src/rendering/RenUtils.o : ../../proxima/src/rendering/RenTypes.hi
../../proxima/src/rendering/RenUtils.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/arrangement/FontLib.o : ../../proxima/src/arrangement/FontLib.hs
../../proxima/src/arrangement/FontLib.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/arrangement/ArrTypes.o : ../../proxima/src/arrangement/ArrTypes.hs
../../proxima/src/arrangement/ArrTypes.o : ../../proxima/src/presentation/PresTypes.hi
../../proxima/src/arrangement/ArrTypes.o : ../../proxima/src/evaluation/DocTypes.hi
../../proxima/src/arrangement/ArrTypes.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/arrangement/ArrUtils.o : ../../proxima/src/arrangement/ArrUtils.hs
../../proxima/src/arrangement/ArrUtils.o : ../../proxima/src/common/CommonUtils.hi
../../proxima/src/arrangement/ArrUtils.o : ../../proxima/src/arrangement/ArrTypes.hi
../../proxima/src/arrangement/ArrUtils.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/rendering/RenLayerTypes.o : ../../proxima/src/rendering/RenLayerTypes.hs
../../proxima/src/rendering/RenLayerTypes.o : ../../proxima/src/rendering/RenTypes.hi
../../proxima/src/rendering/RenLayerTypes.o : ../../proxima/src/arrangement/ArrTypes.hi
../../proxima/src/rendering/RenLayerTypes.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/rendering/RenLayerUtils.o : ../../proxima/src/rendering/RenLayerUtils.hs
../../proxima/src/rendering/RenLayerUtils.o : ../../proxima/src/rendering/RenUtils.hi
../../proxima/src/rendering/RenLayerUtils.o : ../../proxima/src/arrangement/ArrUtils.hi
../../proxima/src/rendering/RenLayerUtils.o : ../../proxima/src/rendering/RenLayerTypes.hi
../../proxima/src/rendering/RenLayerUtils.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/rendering/RenTranslate.o : ../../proxima/src/rendering/RenTranslate.hs
../../proxima/src/rendering/RenTranslate.o : ../../proxima/src/rendering/RenLayerUtils.hi
../../proxima/src/rendering/RenTranslate.o : ../../proxima/src/rendering/RenLayerTypes.hi
../../proxima/src/rendering/RenTranslate.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/arrangement/ArrLayerTypes.o : ../../proxima/src/arrangement/ArrLayerTypes.hs
../../proxima/src/arrangement/ArrLayerTypes.o : ../../proxima/src/arrangement/FontLib.hi
../../proxima/src/arrangement/ArrLayerTypes.o : ../../proxima/src/arrangement/ArrTypes.hi
../../proxima/src/arrangement/ArrLayerTypes.o : ../../proxima/src/layout/LayTypes.hi
../../proxima/src/arrangement/ArrLayerTypes.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/arrangement/ArrLayerUtils.o : ../../proxima/src/arrangement/ArrLayerUtils.hs
../../proxima/src/arrangement/ArrLayerUtils.o : ../../proxima/src/arrangement/ArrUtils.hi
../../proxima/src/arrangement/ArrLayerUtils.o : ../../proxima/src/layout/LayUtils.hi
../../proxima/src/arrangement/ArrLayerUtils.o : ../../proxima/src/common/CommonUtils.hi
../../proxima/src/arrangement/ArrLayerUtils.o : ../../proxima/src/arrangement/ArrLayerTypes.hi
../../proxima/src/arrangement/ArrLayerUtils.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/rendering/Renderer.o : ../../proxima/src/rendering/Renderer.hs
../../proxima/src/rendering/Renderer.o : ../../proxima/src/main/GUI.hi
../../proxima/src/rendering/Renderer.o : ../../proxima/src/evaluation/DocumentEdit.hi
../../proxima/src/rendering/Renderer.o : ../../proxima/src/evaluation/DocUtils.hi
../../proxima/src/rendering/Renderer.o : ../../proxima/src/evaluation/DocTypes.hi
../../proxima/src/rendering/Renderer.o : ../../proxima/src/presentation/PresTypes.hi
../../proxima/src/rendering/Renderer.o : ../../proxima/src/arrangement/ArrLayerUtils.hi
../../proxima/src/rendering/Renderer.o : ../../proxima/src/rendering/RenLayerUtils.hi
../../proxima/src/rendering/Renderer.o : ../../proxima/src/rendering/RenLayerTypes.hi
../../proxima/src/rendering/Renderer.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/rendering/Renderer.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/rendering/RenPresent.o : ../../proxima/src/rendering/RenPresent.hs
../../proxima/src/rendering/RenPresent.o : ../../proxima/src/evaluation/DocTypes.hi
../../proxima/src/rendering/RenPresent.o : ../../proxima/src/rendering/Renderer.hi
../../proxima/src/rendering/RenPresent.o : ../../proxima/src/rendering/RenLayerUtils.hi
../../proxima/src/rendering/RenPresent.o : ../../proxima/src/rendering/RenLayerTypes.hi
../../proxima/src/rendering/RenPresent.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/arrangement/ArrTranslate.o : ../../proxima/src/arrangement/ArrTranslate.hs
../../proxima/src/arrangement/ArrTranslate.o : ../../proxima/src/layout/TreeEditPres.hi
../../proxima/src/arrangement/ArrTranslate.o : ../../proxima/src/evaluation/DocUtils.hi
../../proxima/src/arrangement/ArrTranslate.o : ../../proxima/src/evaluation/DocTypes.hi
../../proxima/src/arrangement/ArrTranslate.o : ../../proxima/src/arrangement/ArrLayerUtils.hi
../../proxima/src/arrangement/ArrTranslate.o : ../../proxima/src/arrangement/ArrLayerTypes.hi
../../proxima/src/arrangement/ArrTranslate.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/arrangement/ArrangerAG.o : ../../proxima/src/arrangement/ArrangerAG.hs
../../proxima/src/arrangement/ArrangerAG.o : ../../proxima/src/arrangement/FontLib.hi
../../proxima/src/arrangement/ArrangerAG.o : ../../proxima/src/arrangement/ArrLayerUtils.hi
../../proxima/src/arrangement/ArrangerAG.o : ../../proxima/src/arrangement/ArrLayerTypes.hi
../../proxima/src/arrangement/ArrangerAG.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/arrangement/Arranger.o : ../../proxima/src/arrangement/Arranger.hs
../../proxima/src/arrangement/Arranger.o : ../../proxima/src/arrangement/FontLib.hi
../../proxima/src/arrangement/Arranger.o : ../../proxima/src/arrangement/ArrangerAG.hi
../../proxima/src/arrangement/Arranger.o : ../../proxima/src/arrangement/ArrLayerUtils.hi
../../proxima/src/arrangement/Arranger.o : ../../proxima/src/arrangement/ArrLayerTypes.hi
../../proxima/src/arrangement/Arranger.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/arrangement/ArrPresent.o : ../../proxima/src/arrangement/ArrPresent.hs
../../proxima/src/arrangement/ArrPresent.o : ../../proxima/src/arrangement/Arranger.hi
../../proxima/src/arrangement/ArrPresent.o : ../../proxima/src/arrangement/ArrLayerUtils.hi
../../proxima/src/arrangement/ArrPresent.o : ../../proxima/src/arrangement/ArrLayerTypes.hi
../../proxima/src/arrangement/ArrPresent.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/main/Architecture.o : ../../proxima/src/main/Architecture.hs
../../proxima/src/main/Architecture.o : ../../proxima/src/rendering/RenTranslate.hi
../../proxima/src/main/Architecture.o : ../../proxima/src/rendering/RenPresent.hi
../../proxima/src/main/Architecture.o : ../../proxima/src/arrangement/ArrTranslate.hi
../../proxima/src/main/Architecture.o : ../../proxima/src/arrangement/ArrPresent.hi
../../proxima/src/main/Architecture.o : ../../proxima/src/layout/LayTranslate.hi
../../proxima/src/main/Architecture.o : ../../proxima/src/layout/LayPresent.hi
../../proxima/src/main/Architecture.o : ../../proxima/src/presentation/PresTranslate.hi
../../proxima/src/main/Architecture.o : ../../proxima/src/presentation/PresPresent.hi
../../proxima/src/main/Architecture.o : ../../proxima/src/evaluation/EvalTranslate.hi
../../proxima/src/main/Architecture.o : ../../proxima/src/evaluation/EvalPresent.hi
../../proxima/src/main/Architecture.o : ../../proxima/src/main/ArchitectureLibM.hi
../../proxima/src/main/Proxima.o : ../../proxima/src/main/Proxima.hs
../../proxima/src/main/Proxima.o : ../../proxima/src/evaluation/EvalLayerTypes.hi
../../proxima/src/main/Proxima.o : ../../proxima/src/arrangement/ArrUtils.hi
../../proxima/src/main/Proxima.o : ../../proxima/src/layout/Layout.hi
../../proxima/src/main/Proxima.o : ../../proxima/src/presentation/PresentationParsing.hi
../../proxima/src/main/Proxima.o : ../../proxima/src/layout/LayLayerTypes.hi
../../proxima/src/main/Proxima.o : ../../proxima/src/presentation/PresLayerTypes.hi
../../proxima/src/main/Proxima.o : ../../proxima/src/evaluation/EvalLayerTypes.hi
../../proxima/src/main/Proxima.o : ../../proxima/src/arrangement/FontLib.hi
../../proxima/src/main/Proxima.o : ../../proxima/src/rendering/RenTypes.hi
../../proxima/src/main/Proxima.o : ../../proxima/src/arrangement/ArrTypes.hi
../../proxima/src/main/Proxima.o : ../../proxima/src/layout/LayTypes.hi
../../proxima/src/main/Proxima.o : ../../proxima/src/presentation/PresUtils.hi
../../proxima/src/main/Proxima.o : ../../proxima/src/presentation/PresTypes.hi
../../proxima/src/main/Proxima.o : ../../proxima/src/evaluation/EnrTypes.hi
../../proxima/src/main/Proxima.o : ../../proxima/src/presentation/PresPresent.hi
../../proxima/src/main/Proxima.o : ../../proxima/src/evaluation/DocUtils.hi
../../proxima/src/main/Proxima.o : ../../proxima/src/evaluation/DocTypes.hi
../../proxima/src/main/Proxima.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/main/Proxima.o : ../../proxima/src/main/GUI.hi
../../proxima/src/main/Proxima.o : ../../proxima/src/main/Architecture.hi
# DO NOT DELETE: End of Haskell dependencies

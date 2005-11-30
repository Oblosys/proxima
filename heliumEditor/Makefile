# editing on proxima/PresentationParsing.hs does not cause rebuilding
# doesn't know when proxima.exe is up to date
# maybe has to do with proxima and proxima.exe targets
# problem: when only sources in proxima dir have been changed, this makefile will not
# detect it. somehow, the proxima/src/Makefile must always be called even when nothing 
# needs to be done in editor.

# no make when documenttype is modified

# make trekt aan helium objecten als ze in $(OBJS) staan en evaluateTypes/compileHelium 
# in EDITORSRCS zitten. Dit kan ervoor zorgen dat deze makefile helium sources gaat
# compileren. Zodra er geen cpp dingen meer in lvm zitten kan Helium waarschijnlijk 
# gecompileerd worden vanuit de Proxima makefile. Tot die tijd misschien even make 
# op Helium aanroepen wanneer de executable niet bestaat (Helium wordt dan geacht niet te 
# veranderen.)

# Dit zou ook de oorzaak kunnen zijn van de problemen bij het compileren van Set, nl. dat
# het vanuit de Proxima makefile gebeurde.

# recursive make depend?

HC    = ghc
#HC    = /c/Program\ Files/Visual\ Haskell/bin/ghc
FIND = /usr/bin/find

PROXIMADIR = ../proxima
PROXIMASRCDIR = $(PROXIMADIR)/src
PROXIMABINDIR = $(PROXIMADIR)/bin

AGC  = $(PROXIMABINDIR)/uuagc
GEN  = $(PROXIMABINDIR)/generate

EXE            = 

MAIN           = proxima$(EXE)


SHELL = /bin/sh

# clear and set suffixes
.SUFFIXES:
.SUFFIXES: .o .hs .hi .lhs .hc	.s .ag





default: all
all: $(MAIN)
.PHONY : default proxima generate



PROXIMAOBJSFILE = $(PROXIMABINDIR)/PROXIMAOBJS.txt


# replace "$(PROXIMADIR)" in proximaObjs by "../proxima"
PROXIMAOBJS = `cat $(PROXIMAOBJSFILE) | sed -e s/$$\(PROXIMADIR\)/\.\.\\\/proxima/g`



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

	
HELIUMSRCDIR = $(PROXIMADIR)/../helium/src


HELIUMOBJSFILE = $(HELIUMSRCDIR)/heliumObjs.txt

$(HELIUMOBJSFILE): 
	cd $(HELIUMSRCDIR); make objectsfile

HELIUMOBJS = `cat $(HELIUMOBJSFILE) | sed -e s/$$\(HELIUMSRCDIR\)/\.\.\\\/helium\\\/src/g`

# replaces "$(HELIUMSRCDIR)" in heliumObjs by "../helium/src"


#directories
PARSECDIR     = ../parsec
PARSECIMPDIR  = $(PARSECDIR)/Text/ParserCombinators
LVMLIBDIR     = ../lvm/src/lib
LVMRUNTIMEDIR = ../lvm/src/runtime
TOPDIR        = ../Top/src/Top

HELIUMIFACES	= $(LVMLIBDIR)/common:$(LVMLIBDIR)/common/ghc:$(LVMLIBDIR)/lvm:$(LVMLIBDIR)/asm:$(LVMLIBDIR)/core:$(HELIUMSRCDIR)/utils:$(PARSECDIR):$(HELIUMSRCDIR)/parser:$(HELIUMSRCDIR)/staticanalysis/miscellaneous:$(HELIUMSRCDIR)/staticanalysis/inferencers:$(HELIUMSRCDIR)/staticanalysis/directives:$(HELIUMSRCDIR)/staticanalysis/staticchecks:$(HELIUMSRCDIR)/staticanalysis/heuristics:$(HELIUMSRCDIR)/staticanalysis/messages:$(HELIUMSRCDIR)/syntax:$(HELIUMSRCDIR)/codegeneration:$(HELIUMSRCDIR)/main:$(HELIUMSRCDIR)/modulesystem:$(TOPDIR):$(TOPDIR)/..

IFACES = src:$(PROXIMAIFACES):$(HELIUMIFACES)

	


HC_OPTS = -static -fglasgow-exts -fallow-undecidable-instances \
		-package wx -package parsec\
		-fno-monomorphism-restriction -i$(IFACES) $(EXTRA_OPTS)



EDITORSRCS = \
	src/DocTypes_Generated.hs \
	src/DocUtils_Generated.hs \
	src/DocumentEdit_Generated.hs \
	src/EvaluateTypesStubs.hs \
	src/Evaluator.hs \
	src/Reducer.hs \
	src/Scanner.hs \
	src/Chess.hs \
	src/ProxParser.hs \
	src/ProxParser_Generated.hs \
	src/PresentationAG.hs \
	src/Main.hs \
	src/EvaluateTypes.hs \
	src/CompileHelium.hs
	
EDITOROBJSTMP = $(EDITORSRCS:.hs=.o)
EDITOROBJS = $(EDITOROBJSTMP:.lhs=.o)


OBJS = $(PROXIMAOBJS) $(EDITOROBJS) $(HELIUMOBJS)



# Generation 

generate : src/DocTypes_Generated.hs 

$(GEN):
	cd $(PROXIMASRCDIR); make generator

src/DocTypes_Generated.hs : src/DocumentType.hs $(GEN)
	cd $(PROXIMASRCDIR); make generator
	$(GEN) src src/DocumentType.hs


# The presentation AG

# target for only compiling presentationAG, useful when working with GHCi
presenter: src/PresentationAG.hs
	
# TODO: make dependencies easy to see and to maintain (important for editor builder)

	
$(AGC):
	cd $(PROXIMASRCDIR); make agc

src/PresentationAG.hs : $(AGC) \
	src/PresentationAG.ag \
	src/PresentationAG_Generated.ag \
	src/LambdaReduce.ag \
	
	@echo "Compiling presentationAG.ag"
	@$(AGC) -o src/PresentationAG.hs \
	-cfmspw --self --module=PresentationAG \
	src/PresentationAG.ag -P src
# --module is because otherwise uuagc uses ../../presentation/PresentationAG as module name

# The main target

depend :
	echo $(EDITORSRCS)
	# Checking dependencies
	$(HC) -M -cpp $(HC_OPTS) $(EDITORSRCS)
# -cpp is necessary because the Helium modules are parsed.
# It would be nice if we could only pass the Proxima interface directories, but 
# unfortunately GHC -M requires all directories, and thus generates rules also for
# sources outside the project (making the -cpp necessary).

$(MAIN): $(EDITOROBJS) $(HELIUMOBJSFILE)
	cd $(PROXIMASRCDIR); make proxima
	@echo GHC: Linking Proxima
	@$(HC) -o $(MAIN) $(HC_OPTS) $(OBJS)


clean:
	$(FIND) src -name "*.hi" -delete
	$(FIND) src -name "*.o" -delete
	$(FIND) src -name "presentationAG.hs" -delete
	touch src/DocumentType.hs
	$(FIND) . -name "proxima.*" -delete

# cannot delete .._Generated sources, so we touch src/DocumentType.hs, causing them to be
# recompiled on the next make
	
clean-proxima: clean
	cd $(PROXIMASRCDIR); make clean

clean-all: clean-proxima
	cd $(HELIUMSRCDIR); make clean
	
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

# DO NOT DELETE: Beginning of Haskell dependencies
../proxima/../helium/src/utils/Texts.o : ../proxima/../helium/src/utils/Texts.hs
../proxima/../helium/src/parser/LexerToken.o : ../proxima/../helium/src/parser/LexerToken.hs
../proxima/../helium/src/parser/LexerToken.o : ../proxima/../helium/src/utils/Texts.hi
../proxima/../helium/src/parser/LayoutRule.o : ../proxima/../helium/src/parser/LayoutRule.hs
../proxima/../helium/src/parser/LayoutRule.o : ../proxima/../helium/src/parser/LexerToken.hi
../proxima/../helium/src/main/Version.o : ../proxima/../helium/src/main/Version.hs
../Top/src/Top/../Top/ComposedSolvers/TreeWalk.o : ../Top/src/Top/../Top/ComposedSolvers/TreeWalk.hs
../Top/src/Top/../Top/ComposedSolvers/Tree.o : ../Top/src/Top/../Top/ComposedSolvers/Tree.hs
../Top/src/Top/../Top/ComposedSolvers/Tree.o : ../Top/src/Top/../Top/ComposedSolvers/TreeWalk.hi
../Top/src/Top/../Top/States/States.o : ../Top/src/Top/../Top/States/States.hs
../proxima/../helium/src/utils/OneLiner.o : ../proxima/../helium/src/utils/OneLiner.hs
../lvm/src/lib/common/IntMap.o : ../lvm/src/lib/common/IntMap.hs
../lvm/src/lib/common/ghc/Special.o : ../lvm/src/lib/common/ghc/Special.hs
../lvm/src/lib/common/Standard.o : ../lvm/src/lib/common/Standard.hs
../lvm/src/lib/common/Standard.o : ../lvm/src/lib/common/ghc/Special.hi
../lvm/src/lib/common/TopSort.o : ../lvm/src/lib/common/TopSort.hs
../lvm/src/lib/common/TopSort.o : ../lvm/src/lib/common/ghc/Special.hi
../lvm/src/lib/common/Byte.o : ../lvm/src/lib/common/Byte.hs
../lvm/src/lib/common/Byte.o : ../lvm/src/lib/common/Standard.hi
../lvm/src/lib/common/Byte.o : ../lvm/src/lib/common/ghc/Special.hi
../lvm/src/lib/common/Id.o : ../lvm/src/lib/common/Id.hs
../lvm/src/lib/common/Id.o : ../lvm/src/lib/common/ghc/Special.hi
../lvm/src/lib/common/Id.o : ../lvm/src/lib/common/IntMap.hi
../lvm/src/lib/common/Id.o : ../lvm/src/lib/common/Standard.hi
../lvm/src/lib/common/IdMap.o : ../lvm/src/lib/common/IdMap.hs
../lvm/src/lib/common/IdMap.o : ../lvm/src/lib/common/Id.hi
../lvm/src/lib/common/IdMap.o : ../lvm/src/lib/common/IntMap.hi
../lvm/src/lib/common/IdSet.o : ../lvm/src/lib/common/IdSet.hs
../lvm/src/lib/common/IdSet.o : ../lvm/src/lib/common/IdMap.hi
../lvm/src/lib/common/IdSet.o : ../lvm/src/lib/common/Id.hi
../lvm/src/lib/lvm/Instr.o : ../lvm/src/lib/lvm/Instr.hs
../lvm/src/lib/lvm/Instr.o : ../lvm/src/lib/common/Byte.hi
../lvm/src/lib/lvm/Instr.o : ../lvm/src/lib/common/Id.hi
../lvm/src/lib/lvm/Instr.o : ../lvm/src/lib/common/Standard.hi
../lvm/src/lib/lvm/Module.o : ../lvm/src/lib/lvm/Module.hs
../lvm/src/lib/lvm/Module.o : ../lvm/src/lib/lvm/Instr.hi
../lvm/src/lib/lvm/Module.o : ../lvm/src/lib/common/IdSet.hi
../lvm/src/lib/lvm/Module.o : ../lvm/src/lib/common/Id.hi
../lvm/src/lib/lvm/Module.o : ../lvm/src/lib/common/Byte.hi
../lvm/src/lib/lvm/Module.o : ../lvm/src/lib/common/Standard.hi
../lvm/src/lib/core/Core.o : ../lvm/src/lib/core/Core.hs
../lvm/src/lib/core/Core.o : ../lvm/src/lib/common/IdSet.hi
../lvm/src/lib/core/Core.o : ../lvm/src/lib/common/IdMap.hi
../lvm/src/lib/core/Core.o : ../lvm/src/lib/lvm/Module.hi
../lvm/src/lib/core/Core.o : ../lvm/src/lib/common/Id.hi
../lvm/src/lib/core/Core.o : ../lvm/src/lib/common/Byte.hi
../proxima/../helium/src/codegeneration/CoreUtils.o : ../proxima/../helium/src/codegeneration/CoreUtils.hs
../proxima/../helium/src/codegeneration/CoreUtils.o : ../lvm/src/lib/common/Byte.hi
../proxima/../helium/src/codegeneration/CoreUtils.o : ../lvm/src/lib/common/Id.hi
../proxima/../helium/src/codegeneration/CoreUtils.o : ../lvm/src/lib/core/Core.hi
../lvm/src/lib/lvm/Lvm.o : ../lvm/src/lib/lvm/Lvm.hs
../lvm/src/lib/lvm/Lvm.o : ../lvm/src/lib/lvm/Module.hi
../lvm/src/lib/lvm/Lvm.o : ../lvm/src/lib/lvm/Instr.hi
../lvm/src/lib/lvm/Lvm.o : ../lvm/src/lib/common/Id.hi
../lvm/src/lib/lvm/Lvm.o : ../lvm/src/lib/common/Byte.hi
../lvm/src/lib/common/PPrint.o : ../lvm/src/lib/common/PPrint.hs
../lvm/src/lib/lvm/ModulePretty.o : ../lvm/src/lib/lvm/ModulePretty.hs
../lvm/src/lib/lvm/ModulePretty.o : ../lvm/src/lib/lvm/Module.hi
../lvm/src/lib/lvm/ModulePretty.o : ../lvm/src/lib/common/IdSet.hi
../lvm/src/lib/lvm/ModulePretty.o : ../lvm/src/lib/common/IdMap.hi
../lvm/src/lib/lvm/ModulePretty.o : ../lvm/src/lib/common/Id.hi
../lvm/src/lib/lvm/ModulePretty.o : ../lvm/src/lib/common/Byte.hi
../lvm/src/lib/lvm/ModulePretty.o : ../lvm/src/lib/common/PPrint.hi
../lvm/src/lib/core/CorePretty.o : ../lvm/src/lib/core/CorePretty.hs
../lvm/src/lib/core/CorePretty.o : ../lvm/src/lib/lvm/ModulePretty.hi
../lvm/src/lib/core/CorePretty.o : ../lvm/src/lib/core/Core.hi
../lvm/src/lib/core/CorePretty.o : ../lvm/src/lib/common/IdSet.hi
../lvm/src/lib/core/CorePretty.o : ../lvm/src/lib/common/Id.hi
../lvm/src/lib/core/CorePretty.o : ../lvm/src/lib/common/Byte.hi
../lvm/src/lib/core/CorePretty.o : ../lvm/src/lib/common/PPrint.hi
../lvm/src/lib/lvm/InstrPretty.o : ../lvm/src/lib/lvm/InstrPretty.hs
../lvm/src/lib/lvm/InstrPretty.o : ../lvm/src/lib/lvm/Instr.hi
../lvm/src/lib/lvm/InstrPretty.o : ../lvm/src/lib/common/Id.hi
../lvm/src/lib/lvm/InstrPretty.o : ../lvm/src/lib/common/Byte.hi
../lvm/src/lib/lvm/InstrPretty.o : ../lvm/src/lib/common/PPrint.hi
../lvm/src/lib/lvm/LvmRead.o : ../lvm/src/lib/lvm/LvmRead.hs
../lvm/src/lib/lvm/LvmRead.o : ../lvm/src/lib/lvm/ModulePretty.hi
../lvm/src/lib/lvm/LvmRead.o : ../lvm/src/lib/lvm/InstrPretty.hi
../lvm/src/lib/lvm/LvmRead.o : ../lvm/src/lib/lvm/Instr.hi
../lvm/src/lib/lvm/LvmRead.o : ../lvm/src/lib/lvm/Lvm.hi
../lvm/src/lib/lvm/LvmRead.o : ../lvm/src/lib/common/Byte.hi
../lvm/src/lib/lvm/LvmRead.o : ../lvm/src/lib/common/Byte.hi
../lvm/src/lib/lvm/LvmRead.o : ../lvm/src/lib/common/IdMap.hi
../lvm/src/lib/lvm/LvmRead.o : ../lvm/src/lib/common/Id.hi
../lvm/src/lib/lvm/LvmRead.o : ../lvm/src/lib/common/Standard.hi
../lvm/src/lib/lvm/LvmRead.o : ../lvm/src/lib/common/PPrint.hi
../lvm/src/lib/lvm/LvmImport.o : ../lvm/src/lib/lvm/LvmImport.hs
../lvm/src/lib/lvm/LvmImport.o : ../lvm/src/lib/common/Id.hi
../lvm/src/lib/lvm/LvmImport.o : ../lvm/src/lib/common/Byte.hi
../lvm/src/lib/lvm/LvmImport.o : ../lvm/src/lib/common/PPrint.hi
../lvm/src/lib/lvm/LvmImport.o : ../lvm/src/lib/lvm/InstrPretty.hi
../lvm/src/lib/lvm/LvmImport.o : ../lvm/src/lib/lvm/ModulePretty.hi
../lvm/src/lib/lvm/LvmImport.o : ../lvm/src/lib/lvm/LvmRead.hi
../lvm/src/lib/lvm/LvmImport.o : ../lvm/src/lib/lvm/Lvm.hi
../lvm/src/lib/lvm/LvmImport.o : ../lvm/src/lib/lvm/Module.hi
../lvm/src/lib/lvm/LvmImport.o : ../lvm/src/lib/common/IdMap.hi
../lvm/src/lib/lvm/LvmImport.o : ../lvm/src/lib/common/Id.hi
../lvm/src/lib/lvm/LvmImport.o : ../lvm/src/lib/common/Standard.hi
../Top/src/Top/../Top/Types/Basics.o : ../Top/src/Top/../Top/Types/Basics.hs
../proxima/../helium/src/syntax/UHA_Syntax.o : ../proxima/../helium/src/syntax/UHA_Syntax.hs
../proxima/../helium/src/staticanalysis/directives/TS_Syntax.o : ../proxima/../helium/src/staticanalysis/directives/TS_Syntax.hs
../proxima/../helium/src/staticanalysis/directives/TS_Syntax.o : ../proxima/../helium/src/syntax/UHA_Syntax.hi
../proxima/../helium/src/utils/Logger.o : ../proxima/../helium/src/utils/Logger.hs
../proxima/../helium/src/utils/Utils.o : ../proxima/../helium/src/utils/Utils.hs
../proxima/../helium/src/utils/Utils.o : ../proxima/../helium/src/utils/Logger.hi
../Top/src/Top/../Top/Types/Substitution.o : ../Top/src/Top/../Top/Types/Substitution.hs
../Top/src/Top/../Top/Types/Substitution.o : ../proxima/../helium/src/utils/Utils.hi
../Top/src/Top/../Top/Types/Substitution.o : ../Top/src/Top/../Top/Types/Basics.hi
../Top/src/Top/../Top/Types/Qualification.o : ../Top/src/Top/../Top/Types/Qualification.hs
../Top/src/Top/../Top/Types/Qualification.o : ../Top/src/Top/../Top/Types/Substitution.hi
../Top/src/Top/../Top/Types/Qualification.o : ../Top/src/Top/../Top/Types/Basics.hi
../Top/src/Top/../Top/Types/Synonyms.o : ../Top/src/Top/../Top/Types/Synonyms.hs
../Top/src/Top/../Top/Types/Synonyms.o : ../proxima/../helium/src/utils/Utils.hi
../Top/src/Top/../Top/Types/Synonyms.o : ../Top/src/Top/../Top/Types/Basics.hi
../Top/src/Top/../Top/Types/Unification.o : ../Top/src/Top/../Top/Types/Unification.hs
../Top/src/Top/../Top/Types/Unification.o : ../proxima/../helium/src/utils/Utils.hi
../Top/src/Top/../Top/Types/Unification.o : ../Top/src/Top/../Top/Types/Synonyms.hi
../Top/src/Top/../Top/Types/Unification.o : ../Top/src/Top/../Top/Types/Basics.hi
../Top/src/Top/../Top/Types/Unification.o : ../Top/src/Top/../Top/Types/Substitution.hi
../Top/src/Top/../Top/Types/Classes.o : ../Top/src/Top/../Top/Types/Classes.hs
../Top/src/Top/../Top/Types/Classes.o : ../Top/src/Top/../Top/Types/Qualification.hi
../Top/src/Top/../Top/Types/Classes.o : ../Top/src/Top/../Top/Types/Synonyms.hi
../Top/src/Top/../Top/Types/Classes.o : ../Top/src/Top/../Top/Types/Unification.hi
../Top/src/Top/../Top/Types/Classes.o : ../Top/src/Top/../Top/Types/Substitution.hi
../Top/src/Top/../Top/Types/Classes.o : ../Top/src/Top/../Top/Types/Basics.hi
../Top/src/Top/../Top/Types/Quantification.o : ../Top/src/Top/../Top/Types/Quantification.hs
../Top/src/Top/../Top/Types/Quantification.o : ../proxima/../helium/src/utils/Utils.hi
../Top/src/Top/../Top/Types/Quantification.o : ../Top/src/Top/../Top/Types/Substitution.hi
../Top/src/Top/../Top/Types/Quantification.o : ../Top/src/Top/../Top/Types/Basics.hi
../Top/src/Top/../Top/Types/Schemes.o : ../Top/src/Top/../Top/Types/Schemes.hs
../Top/src/Top/../Top/Types/Schemes.o : ../Top/src/Top/../Top/Types/Classes.hi
../Top/src/Top/../Top/Types/Schemes.o : ../Top/src/Top/../Top/Types/Unification.hi
../Top/src/Top/../Top/Types/Schemes.o : ../Top/src/Top/../Top/Types/Synonyms.hi
../Top/src/Top/../Top/Types/Schemes.o : ../Top/src/Top/../Top/Types/Substitution.hi
../Top/src/Top/../Top/Types/Schemes.o : ../Top/src/Top/../Top/Types/Qualification.hi
../Top/src/Top/../Top/Types/Schemes.o : ../Top/src/Top/../Top/Types/Quantification.hi
../Top/src/Top/../Top/Types/Schemes.o : ../Top/src/Top/../Top/Types/Basics.hi
../Top/src/Top/../Top/Types/Kinds.o : ../Top/src/Top/../Top/Types/Kinds.hs
../Top/src/Top/../Top/Types/Kinds.o : ../Top/src/Top/../Top/Types/Schemes.hi
../Top/src/Top/../Top/Types/Kinds.o : ../Top/src/Top/../Top/Types/Quantification.hi
../Top/src/Top/../Top/Types/Kinds.o : ../Top/src/Top/../Top/Types/Substitution.hi
../Top/src/Top/../Top/Types/Kinds.o : ../Top/src/Top/../Top/Types/Basics.hi
../Top/src/Top/../Top/Types.o : ../Top/src/Top/../Top/Types.hs
../Top/src/Top/../Top/Types.o : ../Top/src/Top/../Top/Types/Kinds.hi
../Top/src/Top/../Top/Types.o : ../Top/src/Top/../Top/Types/Schemes.hi
../Top/src/Top/../Top/Types.o : ../Top/src/Top/../Top/Types/Classes.hi
../Top/src/Top/../Top/Types.o : ../Top/src/Top/../Top/Types/Unification.hi
../Top/src/Top/../Top/Types.o : ../Top/src/Top/../Top/Types/Synonyms.hi
../Top/src/Top/../Top/Types.o : ../Top/src/Top/../Top/Types/Qualification.hi
../Top/src/Top/../Top/Types.o : ../Top/src/Top/../Top/Types/Quantification.hi
../Top/src/Top/../Top/Types.o : ../Top/src/Top/../Top/Types/Substitution.hi
../Top/src/Top/../Top/Types.o : ../Top/src/Top/../Top/Types/Basics.hi
../proxima/../helium/src/staticanalysis/miscellaneous/TypesToAlignedDocs.o : ../proxima/../helium/src/staticanalysis/miscellaneous/TypesToAlignedDocs.hs
../proxima/../helium/src/staticanalysis/miscellaneous/TypesToAlignedDocs.o : ../lvm/src/lib/common/PPrint.hi
../proxima/../helium/src/staticanalysis/miscellaneous/TypesToAlignedDocs.o : ../Top/src/Top/../Top/Types.hi
../proxima/../helium/src/staticanalysis/directives/TS_CoreSyntax.o : ../proxima/../helium/src/staticanalysis/directives/TS_CoreSyntax.hs
../proxima/../helium/src/staticanalysis/directives/TS_CoreSyntax.o : ../Top/src/Top/../Top/Types.hi
../Top/src/Top/../Top/Qualifiers/QualifierMap.o : ../Top/src/Top/../Top/Qualifiers/QualifierMap.hs
../Top/src/Top/../Top/Qualifiers/QualifierMap.o : ../Top/src/Top/../Top/Types.hi
../Top/src/Top/../Top/Qualifiers/QualifierMap.o : ../Top/src/Top/../Top/States/States.hi
../Top/src/Top/../Top/Constraints/Constraints.o : ../Top/src/Top/../Top/Constraints/Constraints.hs
../Top/src/Top/../Top/Constraints/Constraints.o : ../Top/src/Top/../Top/Types.hi
../Top/src/Top/../Top/Constraints/TypeConstraintInfo.o : ../Top/src/Top/../Top/Constraints/TypeConstraintInfo.hs
../Top/src/Top/../Top/Constraints/TypeConstraintInfo.o : ../Top/src/Top/../Top/Types.hi
../proxima/../helium/src/syntax/UHA_Range.o : ../proxima/../helium/src/syntax/UHA_Range.hs
../proxima/../helium/src/syntax/UHA_Range.o : ../proxima/../helium/src/utils/Utils.hi
../proxima/../helium/src/syntax/UHA_Range.o : ../lvm/src/lib/common/Id.hi
../proxima/../helium/src/syntax/UHA_Range.o : ../proxima/../helium/src/syntax/UHA_Syntax.hi
../proxima/../helium/src/utils/Similarity.o : ../proxima/../helium/src/utils/Similarity.hs
../proxima/../helium/src/utils/Similarity.o : ../proxima/../helium/src/utils/Utils.hi
../proxima/../helium/src/syntax/UHA_Utils.o : ../proxima/../helium/src/syntax/UHA_Utils.hs
../proxima/../helium/src/syntax/UHA_Utils.o : ../proxima/../helium/src/utils/Utils.hi
../proxima/../helium/src/syntax/UHA_Utils.o : ../Top/src/Top/../Top/Types.hi
../proxima/../helium/src/syntax/UHA_Utils.o : ../lvm/src/lib/common/Id.hi
../proxima/../helium/src/syntax/UHA_Utils.o : ../proxima/../helium/src/syntax/UHA_Syntax.hi
../proxima/../helium/src/syntax/UHA_Utils.o : ../proxima/../helium/src/syntax/UHA_Range.hi
../proxima/../helium/src/staticanalysis/messages/Messages.o : ../proxima/../helium/src/staticanalysis/messages/Messages.hs
../proxima/../helium/src/staticanalysis/messages/Messages.o : ../proxima/../helium/src/utils/Utils.hi
../proxima/../helium/src/staticanalysis/messages/Messages.o : ../proxima/../helium/src/utils/Similarity.hi
../proxima/../helium/src/staticanalysis/messages/Messages.o : ../proxima/../helium/src/utils/OneLiner.hi
../proxima/../helium/src/staticanalysis/messages/Messages.o : ../Top/src/Top/../Top/Types.hi
../proxima/../helium/src/staticanalysis/messages/Messages.o : ../proxima/../helium/src/syntax/UHA_Range.hi
../proxima/../helium/src/staticanalysis/messages/Messages.o : ../proxima/../helium/src/syntax/UHA_Utils.hi
../proxima/../helium/src/staticanalysis/messages/Messages.o : ../proxima/../helium/src/syntax/UHA_Syntax.hi
../proxima/../helium/src/staticanalysis/messages/HeliumMessages.o : ../proxima/../helium/src/staticanalysis/messages/HeliumMessages.hs
../proxima/../helium/src/staticanalysis/messages/HeliumMessages.o : ../proxima/../helium/src/syntax/UHA_Range.hi
../proxima/../helium/src/staticanalysis/messages/HeliumMessages.o : ../proxima/../helium/src/staticanalysis/miscellaneous/TypesToAlignedDocs.hi
../proxima/../helium/src/staticanalysis/messages/HeliumMessages.o : ../proxima/../helium/src/utils/OneLiner.hi
../proxima/../helium/src/staticanalysis/messages/HeliumMessages.o : ../lvm/src/lib/common/PPrint.hi
../proxima/../helium/src/staticanalysis/messages/HeliumMessages.o : ../Top/src/Top/../Top/Types.hi
../proxima/../helium/src/staticanalysis/messages/HeliumMessages.o : ../proxima/../helium/src/staticanalysis/messages/Messages.hi
../proxima/../helium/src/parser/LexerMessage.o : ../proxima/../helium/src/parser/LexerMessage.hs
../proxima/../helium/src/parser/LexerMessage.o : ../proxima/../helium/src/utils/Texts.hi
../proxima/../helium/src/parser/LexerMessage.o : ../proxima/../helium/src/staticanalysis/messages/Messages.hi
../proxima/../helium/src/parser/LexerMessage.o : ../proxima/../helium/src/syntax/UHA_Syntax.hi
../proxima/../helium/src/parser/LexerMonad.o : ../proxima/../helium/src/parser/LexerMonad.hs
../proxima/../helium/src/parser/LexerMonad.o : ../proxima/../helium/src/parser/LexerMessage.hi
../proxima/../helium/src/staticanalysis/directives/Matchers.o : ../proxima/../helium/src/staticanalysis/directives/Matchers.hs
../proxima/../helium/src/staticanalysis/directives/Matchers.o : ../proxima/../helium/src/staticanalysis/messages/Messages.hi
../proxima/../helium/src/staticanalysis/directives/Matchers.o : ../proxima/../helium/src/syntax/UHA_Syntax.hi
../proxima/../helium/src/parser/ParseMessage.o : ../proxima/../helium/src/parser/ParseMessage.hs
../proxima/../helium/src/parser/ParseMessage.o : ../proxima/../helium/src/utils/Texts.hi
../proxima/../helium/src/parser/ParseMessage.o : ../proxima/../helium/src/syntax/UHA_Syntax.hi
../proxima/../helium/src/parser/ParseMessage.o : ../proxima/../helium/src/staticanalysis/messages/Messages.hi
../proxima/../helium/src/parser/OperatorTable.o : ../proxima/../helium/src/parser/OperatorTable.hs
../proxima/../helium/src/parser/OperatorTable.o : ../proxima/../helium/src/syntax/UHA_Utils.hi
../proxima/../helium/src/parser/OperatorTable.o : ../proxima/../helium/src/syntax/UHA_Syntax.hi
../proxima/../helium/src/staticanalysis/miscellaneous/TypeConversion.o : ../proxima/../helium/src/staticanalysis/miscellaneous/TypeConversion.hs
../proxima/../helium/src/staticanalysis/miscellaneous/TypeConversion.o : ../Top/src/Top/../Top/Types.hi
../proxima/../helium/src/staticanalysis/miscellaneous/TypeConversion.o : ../proxima/../helium/src/syntax/UHA_Syntax.hi
../proxima/../helium/src/staticanalysis/miscellaneous/TypeConversion.o : ../proxima/../helium/src/utils/Utils.hi
../proxima/../helium/src/staticanalysis/miscellaneous/TypeConversion.o : ../proxima/../helium/src/syntax/UHA_Range.hi
../proxima/../helium/src/staticanalysis/miscellaneous/TypeConversion.o : ../proxima/../helium/src/syntax/UHA_Utils.hi
../proxima/../helium/src/syntax/UHA_OneLine.o : ../proxima/../helium/src/syntax/UHA_OneLine.hs
../proxima/../helium/src/syntax/UHA_OneLine.o : ../proxima/../helium/src/utils/Utils.hi
../proxima/../helium/src/syntax/UHA_OneLine.o : ../proxima/../helium/src/syntax/UHA_Syntax.hi
../proxima/../helium/src/syntax/UHA_OneLine.o : ../proxima/../helium/src/staticanalysis/miscellaneous/TypeConversion.hi
../proxima/../helium/src/syntax/UHA_OneLine.o : ../Top/src/Top/../Top/Types.hi
../proxima/../helium/src/syntax/UHA_OneLine.o : ../proxima/../helium/src/syntax/UHA_Utils.hi
../proxima/../helium/src/syntax/UHA_OneLine.o : ../proxima/../helium/src/utils/OneLiner.hi
../proxima/../helium/src/staticanalysis/miscellaneous/UHA_Source.o : ../proxima/../helium/src/staticanalysis/miscellaneous/UHA_Source.hs
../proxima/../helium/src/staticanalysis/miscellaneous/UHA_Source.o : ../proxima/../helium/src/syntax/UHA_OneLine.hi
../proxima/../helium/src/staticanalysis/miscellaneous/UHA_Source.o : ../proxima/../helium/src/syntax/UHA_Utils.hi
../proxima/../helium/src/staticanalysis/miscellaneous/UHA_Source.o : ../proxima/../helium/src/syntax/UHA_Syntax.hi
../proxima/../helium/src/staticanalysis/miscellaneous/UHA_Source.o : ../proxima/../helium/src/syntax/UHA_Range.hi
../proxima/../helium/src/staticanalysis/miscellaneous/UHA_Source.o : ../proxima/../helium/src/utils/OneLiner.hi
../proxima/../helium/src/staticanalysis/messages/TypeErrors.o : ../proxima/../helium/src/staticanalysis/messages/TypeErrors.hs
../proxima/../helium/src/staticanalysis/messages/TypeErrors.o : ../proxima/../helium/src/staticanalysis/miscellaneous/UHA_Source.hi
../proxima/../helium/src/staticanalysis/messages/TypeErrors.o : ../proxima/../helium/src/syntax/UHA_Range.hi
../proxima/../helium/src/staticanalysis/messages/TypeErrors.o : ../proxima/../helium/src/syntax/UHA_Syntax.hi
../proxima/../helium/src/staticanalysis/messages/TypeErrors.o : ../proxima/../helium/src/utils/OneLiner.hi
../proxima/../helium/src/staticanalysis/messages/TypeErrors.o : ../Top/src/Top/../Top/Types.hi
../proxima/../helium/src/staticanalysis/messages/TypeErrors.o : ../proxima/../helium/src/staticanalysis/messages/Messages.hi
../proxima/../helium/src/staticanalysis/messages/StaticErrors.o : ../proxima/../helium/src/staticanalysis/messages/StaticErrors.hs
../proxima/../helium/src/staticanalysis/messages/StaticErrors.o : ../Top/src/Top/../Top/Types.hi
../proxima/../helium/src/staticanalysis/messages/StaticErrors.o : ../proxima/../helium/src/utils/Utils.hi
../proxima/../helium/src/staticanalysis/messages/StaticErrors.o : ../proxima/../helium/src/staticanalysis/messages/Messages.hi
../proxima/../helium/src/staticanalysis/messages/StaticErrors.o : ../proxima/../helium/src/syntax/UHA_Range.hi
../proxima/../helium/src/staticanalysis/messages/StaticErrors.o : ../proxima/../helium/src/syntax/UHA_Utils.hi
../proxima/../helium/src/staticanalysis/messages/StaticErrors.o : ../proxima/../helium/src/syntax/UHA_Syntax.hi
../Top/src/Top/../Top/States/BasicState.o : ../Top/src/Top/../Top/States/BasicState.hs
../Top/src/Top/../Top/States/BasicState.o : ../proxima/../helium/src/utils/Utils.hi
../Top/src/Top/../Top/States/BasicState.o : ../Top/src/Top/../Top/States/States.hi
../Top/src/Top/../Top/States/BasicState.o : ../Top/src/Top/../Top/Constraints/Constraints.hi
../Top/src/Top/../Top/States/SubstState.o : ../Top/src/Top/../Top/States/SubstState.hs
../Top/src/Top/../Top/States/SubstState.o : ../Top/src/Top/../Top/States/BasicState.hi
../Top/src/Top/../Top/States/SubstState.o : ../Top/src/Top/../Top/Types.hi
../Top/src/Top/../Top/States/TIState.o : ../Top/src/Top/../Top/States/TIState.hs
../Top/src/Top/../Top/States/TIState.o : ../Top/src/Top/../Top/Qualifiers/QualifierMap.hi
../Top/src/Top/../Top/States/TIState.o : ../Top/src/Top/../Top/States/SubstState.hi
../Top/src/Top/../Top/States/TIState.o : ../Top/src/Top/../Top/States/BasicState.hi
../Top/src/Top/../Top/States/TIState.o : ../Top/src/Top/../Top/States/States.hi
../Top/src/Top/../Top/States/TIState.o : ../Top/src/Top/../Top/Constraints/TypeConstraintInfo.hi
../Top/src/Top/../Top/States/TIState.o : ../Top/src/Top/../Top/Types.hi
../Top/src/Top/../Top/Constraints/Equality.o : ../Top/src/Top/../Top/Constraints/Equality.hs
../Top/src/Top/../Top/Constraints/Equality.o : ../Top/src/Top/../Top/States/TIState.hi
../Top/src/Top/../Top/Constraints/Equality.o : ../Top/src/Top/../Top/States/SubstState.hi
../Top/src/Top/../Top/Constraints/Equality.o : ../Top/src/Top/../Top/Constraints/TypeConstraintInfo.hi
../Top/src/Top/../Top/Constraints/Equality.o : ../Top/src/Top/../Top/Constraints/Constraints.hi
../Top/src/Top/../Top/Constraints/Equality.o : ../Top/src/Top/../Top/Types.hi
../Top/src/Top/../Top/Solvers/BasicMonad.o : ../Top/src/Top/../Top/Solvers/BasicMonad.hs
../Top/src/Top/../Top/Solvers/BasicMonad.o : ../Top/src/Top/../Top/States/States.hi
../Top/src/Top/../Top/Solvers/BasicMonad.o : ../Top/src/Top/../Top/States/BasicState.hi
../Top/src/Top/../Top/Solvers/BasicMonad.o : ../Top/src/Top/../Top/Constraints/Constraints.hi
../Top/src/Top/../Top/Solvers/SimpleSubst.o : ../Top/src/Top/../Top/Solvers/SimpleSubst.hs
../Top/src/Top/../Top/Solvers/SimpleSubst.o : ../Top/src/Top/../Top/States/TIState.hi
../Top/src/Top/../Top/Solvers/SimpleSubst.o : ../Top/src/Top/../Top/States/BasicState.hi
../Top/src/Top/../Top/Solvers/SimpleSubst.o : ../Top/src/Top/../Top/States/SubstState.hi
../Top/src/Top/../Top/Solvers/SimpleSubst.o : ../Top/src/Top/../Top/States/States.hi
../Top/src/Top/../Top/Solvers/SimpleSubst.o : ../Top/src/Top/../Top/Types.hi
../Top/src/Top/../Top/States/QualifierState.o : ../Top/src/Top/../Top/States/QualifierState.hs
../Top/src/Top/../Top/States/QualifierState.o : ../proxima/../helium/src/utils/Utils.hi
../Top/src/Top/../Top/States/QualifierState.o : ../Top/src/Top/../Top/States/States.hi
../Top/src/Top/../Top/States/QualifierState.o : ../Top/src/Top/../Top/Types.hi
../Top/src/Top/../Top/Qualifiers/Qualifiers.o : ../Top/src/Top/../Top/Qualifiers/Qualifiers.hs
../Top/src/Top/../Top/Qualifiers/Qualifiers.o : ../Top/src/Top/../Top/States/BasicState.hi
../Top/src/Top/../Top/Qualifiers/Qualifiers.o : ../Top/src/Top/../Top/States/SubstState.hi
../Top/src/Top/../Top/Qualifiers/Qualifiers.o : ../Top/src/Top/../Top/States/TIState.hi
../Top/src/Top/../Top/Qualifiers/Qualifiers.o : ../Top/src/Top/../Top/Qualifiers/QualifierMap.hi
../Top/src/Top/../Top/Qualifiers/Qualifiers.o : ../Top/src/Top/../Top/Constraints/TypeConstraintInfo.hi
../Top/src/Top/../Top/Qualifiers/Qualifiers.o : ../Top/src/Top/../Top/Types.hi
../Top/src/Top/../Top/Qualifiers/Qualifiers.o : ../Top/src/Top/../Top/States/QualifierState.hi
../Top/src/Top/../Top/Qualifiers/Qualifiers.o : ../Top/src/Top/../Top/States/States.hi
../Top/src/Top/../Top/Qualifiers/TypeClasses.o : ../Top/src/Top/../Top/Qualifiers/TypeClasses.hs
../Top/src/Top/../Top/Qualifiers/TypeClasses.o : ../Top/src/Top/../Top/Constraints/Equality.hi
../Top/src/Top/../Top/Qualifiers/TypeClasses.o : ../Top/src/Top/../Top/Constraints/Constraints.hi
../Top/src/Top/../Top/Qualifiers/TypeClasses.o : ../Top/src/Top/../Top/Constraints/TypeConstraintInfo.hi
../Top/src/Top/../Top/Qualifiers/TypeClasses.o : ../Top/src/Top/../Top/Qualifiers/Qualifiers.hi
../Top/src/Top/../Top/Qualifiers/TypeClasses.o : ../Top/src/Top/../Top/States/SubstState.hi
../Top/src/Top/../Top/Qualifiers/TypeClasses.o : ../Top/src/Top/../Top/States/TIState.hi
../Top/src/Top/../Top/Qualifiers/TypeClasses.o : ../Top/src/Top/../Top/States/BasicState.hi
../Top/src/Top/../Top/Qualifiers/TypeClasses.o : ../Top/src/Top/../Top/Types.hi
../Top/src/Top/../Top/Solvers/SolveConstraints.o : ../Top/src/Top/../Top/Solvers/SolveConstraints.hs
../Top/src/Top/../Top/Solvers/SolveConstraints.o : ../Top/src/Top/../Top/Solvers/BasicMonad.hi
../Top/src/Top/../Top/Solvers/SolveConstraints.o : ../Top/src/Top/../Top/States/QualifierState.hi
../Top/src/Top/../Top/Solvers/SolveConstraints.o : ../Top/src/Top/../Top/States/TIState.hi
../Top/src/Top/../Top/Solvers/SolveConstraints.o : ../Top/src/Top/../Top/States/BasicState.hi
../Top/src/Top/../Top/Solvers/SolveConstraints.o : ../Top/src/Top/../Top/States/SubstState.hi
../Top/src/Top/../Top/Solvers/SolveConstraints.o : ../Top/src/Top/../Top/States/States.hi
../Top/src/Top/../Top/Solvers/SolveConstraints.o : ../Top/src/Top/../Top/Constraints/TypeConstraintInfo.hi
../Top/src/Top/../Top/Solvers/SolveConstraints.o : ../Top/src/Top/../Top/Qualifiers/Qualifiers.hi
../Top/src/Top/../Top/Solvers/SolveConstraints.o : ../Top/src/Top/../Top/Constraints/Constraints.hi
../Top/src/Top/../Top/Solvers/SolveConstraints.o : ../Top/src/Top/../Top/Types.hi
../Top/src/Top/../Top/ComposedSolvers/ChunkySolver.o : ../Top/src/Top/../Top/ComposedSolvers/ChunkySolver.hs
../Top/src/Top/../Top/ComposedSolvers/ChunkySolver.o : ../Top/src/Top/../Top/ComposedSolvers/Tree.hi
../Top/src/Top/../Top/ComposedSolvers/ChunkySolver.o : ../Top/src/Top/../Top/Solvers/SolveConstraints.hi
../Top/src/Top/../Top/ComposedSolvers/ChunkySolver.o : ../Top/src/Top/../Top/States/States.hi
../Top/src/Top/../Top/ComposedSolvers/ChunkySolver.o : ../Top/src/Top/../Top/Types.hi
../Top/src/Top/../Top/ComposedSolvers/CombinationSolver.o : ../Top/src/Top/../Top/ComposedSolvers/CombinationSolver.hs
../Top/src/Top/../Top/ComposedSolvers/CombinationSolver.o : ../Top/src/Top/../Top/States/BasicState.hi
../Top/src/Top/../Top/ComposedSolvers/CombinationSolver.o : ../Top/src/Top/../Top/Solvers/SolveConstraints.hi
../Top/src/Top/../Top/Solvers/SimpleSolver.o : ../Top/src/Top/../Top/Solvers/SimpleSolver.hs
../Top/src/Top/../Top/Solvers/SimpleSolver.o : ../Top/src/Top/../Top/Qualifiers/Qualifiers.hi
../Top/src/Top/../Top/Solvers/SimpleSolver.o : ../Top/src/Top/../Top/Constraints/TypeConstraintInfo.hi
../Top/src/Top/../Top/Solvers/SimpleSolver.o : ../Top/src/Top/../Top/Constraints/Constraints.hi
../Top/src/Top/../Top/Solvers/SimpleSolver.o : ../Top/src/Top/../Top/Types.hi
../Top/src/Top/../Top/Solvers/SimpleSolver.o : ../Top/src/Top/../Top/Solvers/SimpleSubst.hi
../Top/src/Top/../Top/Solvers/SimpleSolver.o : ../Top/src/Top/../Top/Solvers/SolveConstraints.hi
../Top/src/Top/../Top/Solvers/SimpleSolver.o : ../Top/src/Top/../Top/Solvers/BasicMonad.hi
../Top/src/Top/../Top/Solvers/SimpleSolver.o : ../Top/src/Top/../Top/States/States.hi
../Top/src/Top/../Top/Solvers/SimpleSolver.o : ../Top/src/Top/../Top/States/SubstState.hi
../Top/src/Top/../Top/Solvers/SimpleSolver.o : ../Top/src/Top/../Top/States/TIState.hi
../Top/src/Top/../Top/Constraints/Polymorphism.o : ../Top/src/Top/../Top/Constraints/Polymorphism.hs
../Top/src/Top/../Top/Constraints/Polymorphism.o : ../Top/src/Top/../Top/States/QualifierState.hi
../Top/src/Top/../Top/Constraints/Polymorphism.o : ../Top/src/Top/../Top/States/SubstState.hi
../Top/src/Top/../Top/Constraints/Polymorphism.o : ../Top/src/Top/../Top/States/TIState.hi
../Top/src/Top/../Top/Constraints/Polymorphism.o : ../Top/src/Top/../Top/States/BasicState.hi
../Top/src/Top/../Top/Constraints/Polymorphism.o : ../Top/src/Top/../Top/Constraints/TypeConstraintInfo.hi
../Top/src/Top/../Top/Constraints/Polymorphism.o : ../Top/src/Top/../Top/Constraints/Equality.hi
../Top/src/Top/../Top/Constraints/Polymorphism.o : ../Top/src/Top/../Top/Qualifiers/Qualifiers.hi
../Top/src/Top/../Top/Constraints/Polymorphism.o : ../Top/src/Top/../Top/Constraints/Constraints.hi
../Top/src/Top/../Top/Constraints/Polymorphism.o : ../Top/src/Top/../Top/Types.hi
../Top/src/Top/../Top/Constraints/ExtraConstraints.o : ../Top/src/Top/../Top/Constraints/ExtraConstraints.hs
../Top/src/Top/../Top/Constraints/ExtraConstraints.o : ../Top/src/Top/../Top/States/QualifierState.hi
../Top/src/Top/../Top/Constraints/ExtraConstraints.o : ../Top/src/Top/../Top/States/BasicState.hi
../Top/src/Top/../Top/Constraints/ExtraConstraints.o : ../Top/src/Top/../Top/States/SubstState.hi
../Top/src/Top/../Top/Constraints/ExtraConstraints.o : ../Top/src/Top/../Top/States/TIState.hi
../Top/src/Top/../Top/Constraints/ExtraConstraints.o : ../Top/src/Top/../Top/States/States.hi
../Top/src/Top/../Top/Constraints/ExtraConstraints.o : ../Top/src/Top/../Top/Constraints/Polymorphism.hi
../Top/src/Top/../Top/Constraints/ExtraConstraints.o : ../Top/src/Top/../Top/Qualifiers/Qualifiers.hi
../Top/src/Top/../Top/Constraints/ExtraConstraints.o : ../Top/src/Top/../Top/Constraints/TypeConstraintInfo.hi
../Top/src/Top/../Top/Constraints/ExtraConstraints.o : ../Top/src/Top/../Top/Constraints/Constraints.hi
../Top/src/Top/../Top/Constraints/ExtraConstraints.o : ../Top/src/Top/../Top/Types.hi
../proxima/../helium/src/staticanalysis/miscellaneous/TypeConstraints.o : ../proxima/../helium/src/staticanalysis/miscellaneous/TypeConstraints.hs
../proxima/../helium/src/staticanalysis/miscellaneous/TypeConstraints.o : ../Top/src/Top/../Top/Solvers/SolveConstraints.hi
../proxima/../helium/src/staticanalysis/miscellaneous/TypeConstraints.o : ../Top/src/Top/../Top/Types.hi
../proxima/../helium/src/staticanalysis/miscellaneous/TypeConstraints.o : ../Top/src/Top/../Top/Qualifiers/TypeClasses.hi
../proxima/../helium/src/staticanalysis/miscellaneous/TypeConstraints.o : ../Top/src/Top/../Top/States/QualifierState.hi
../proxima/../helium/src/staticanalysis/miscellaneous/TypeConstraints.o : ../Top/src/Top/../Top/States/SubstState.hi
../proxima/../helium/src/staticanalysis/miscellaneous/TypeConstraints.o : ../Top/src/Top/../Top/States/TIState.hi
../proxima/../helium/src/staticanalysis/miscellaneous/TypeConstraints.o : ../Top/src/Top/../Top/States/BasicState.hi
../proxima/../helium/src/staticanalysis/miscellaneous/TypeConstraints.o : ../Top/src/Top/../Top/Constraints/TypeConstraintInfo.hi
../proxima/../helium/src/staticanalysis/miscellaneous/TypeConstraints.o : ../Top/src/Top/../Top/Constraints/Polymorphism.hi
../proxima/../helium/src/staticanalysis/miscellaneous/TypeConstraints.o : ../Top/src/Top/../Top/Constraints/ExtraConstraints.hi
../proxima/../helium/src/staticanalysis/miscellaneous/TypeConstraints.o : ../Top/src/Top/../Top/Constraints/Equality.hi
../proxima/../helium/src/staticanalysis/miscellaneous/TypeConstraints.o : ../Top/src/Top/../Top/Constraints/Constraints.hi
../proxima/../helium/src/staticanalysis/miscellaneous/DoublyLinkedTree.o : ../proxima/../helium/src/staticanalysis/miscellaneous/DoublyLinkedTree.hs
../proxima/../helium/src/staticanalysis/miscellaneous/DoublyLinkedTree.o : ../proxima/../helium/src/utils/Utils.hi
../proxima/../helium/src/staticanalysis/miscellaneous/ConstraintInfo.o : ../proxima/../helium/src/staticanalysis/miscellaneous/ConstraintInfo.hs
../proxima/../helium/src/staticanalysis/miscellaneous/ConstraintInfo.o : ../proxima/../helium/src/utils/Utils.hi
../proxima/../helium/src/staticanalysis/miscellaneous/ConstraintInfo.o : ../Top/src/Top/../Top/States/TIState.hi
../proxima/../helium/src/staticanalysis/miscellaneous/ConstraintInfo.o : ../Top/src/Top/../Top/States/SubstState.hi
../proxima/../helium/src/staticanalysis/miscellaneous/ConstraintInfo.o : ../Top/src/Top/../Top/States/BasicState.hi
../proxima/../helium/src/staticanalysis/miscellaneous/ConstraintInfo.o : ../Top/src/Top/../Top/Qualifiers/TypeClasses.hi
../proxima/../helium/src/staticanalysis/miscellaneous/ConstraintInfo.o : ../Top/src/Top/../Top/Constraints/TypeConstraintInfo.hi
../proxima/../helium/src/staticanalysis/miscellaneous/ConstraintInfo.o : ../proxima/../helium/src/staticanalysis/miscellaneous/TypeConstraints.hi
../proxima/../helium/src/staticanalysis/miscellaneous/ConstraintInfo.o : ../proxima/../helium/src/staticanalysis/miscellaneous/DoublyLinkedTree.hi
../proxima/../helium/src/staticanalysis/miscellaneous/ConstraintInfo.o : ../proxima/../helium/src/staticanalysis/messages/Messages.hi
../proxima/../helium/src/staticanalysis/miscellaneous/ConstraintInfo.o : ../proxima/../helium/src/staticanalysis/messages/TypeErrors.hi
../proxima/../helium/src/staticanalysis/miscellaneous/ConstraintInfo.o : ../proxima/../helium/src/syntax/UHA_Range.hi
../proxima/../helium/src/staticanalysis/miscellaneous/ConstraintInfo.o : ../proxima/../helium/src/staticanalysis/miscellaneous/UHA_Source.hi
../proxima/../helium/src/staticanalysis/miscellaneous/ConstraintInfo.o : ../proxima/../helium/src/syntax/UHA_Syntax.hi
../proxima/../helium/src/staticanalysis/miscellaneous/ConstraintInfo.o : ../Top/src/Top/../Top/ComposedSolvers/Tree.hi
../proxima/../helium/src/staticanalysis/miscellaneous/ConstraintInfo.o : ../Top/src/Top/../Top/Types.hi
../proxima/../helium/src/staticanalysis/inferencers/BindingGroupAnalysis.o : ../proxima/../helium/src/staticanalysis/inferencers/BindingGroupAnalysis.hs
../proxima/../helium/src/staticanalysis/inferencers/BindingGroupAnalysis.o : ../Top/src/Top/../Top/ComposedSolvers/Tree.hi
../proxima/../helium/src/staticanalysis/inferencers/BindingGroupAnalysis.o : ../Top/src/Top/../Top/Types.hi
../proxima/../helium/src/staticanalysis/inferencers/BindingGroupAnalysis.o : ../lvm/src/lib/common/TopSort.hi
../proxima/../helium/src/staticanalysis/inferencers/BindingGroupAnalysis.o : ../proxima/../helium/src/staticanalysis/miscellaneous/ConstraintInfo.hi
../proxima/../helium/src/staticanalysis/inferencers/BindingGroupAnalysis.o : ../proxima/../helium/src/staticanalysis/miscellaneous/TypeConstraints.hi
../proxima/../helium/src/staticanalysis/inferencers/BindingGroupAnalysis.o : ../proxima/../helium/src/syntax/UHA_Syntax.hi
../Top/src/Top/../Top/TypeGraph/Paths.o : ../Top/src/Top/../Top/TypeGraph/Paths.hs
../Top/src/Top/../Top/TypeGraph/Paths.o : ../proxima/../helium/src/utils/Utils.hi
../Top/src/Top/../Top/TypeGraph/Basics.o : ../Top/src/Top/../Top/TypeGraph/Basics.hs
../Top/src/Top/../Top/TypeGraph/Basics.o : ../proxima/../helium/src/utils/Utils.hi
../Top/src/Top/../Top/TypeGraph/Basics.o : ../Top/src/Top/../Top/Types.hi
../Top/src/Top/../Top/TypeGraph/Basics.o : ../Top/src/Top/../Top/TypeGraph/Paths.hi
../Top/src/Top/../Top/TypeGraph/TypeGraphState.o : ../Top/src/Top/../Top/TypeGraph/TypeGraphState.hs
../Top/src/Top/../Top/TypeGraph/TypeGraphState.o : ../proxima/../helium/src/utils/Utils.hi
../Top/src/Top/../Top/TypeGraph/TypeGraphState.o : ../Top/src/Top/../Top/Types.hi
../Top/src/Top/../Top/TypeGraph/TypeGraphState.o : ../Top/src/Top/../Top/States/SubstState.hi
../Top/src/Top/../Top/TypeGraph/TypeGraphState.o : ../Top/src/Top/../Top/States/TIState.hi
../Top/src/Top/../Top/TypeGraph/TypeGraphState.o : ../Top/src/Top/../Top/States/BasicState.hi
../Top/src/Top/../Top/TypeGraph/TypeGraphState.o : ../Top/src/Top/../Top/TypeGraph/Basics.hi
../Top/src/Top/../Top/TypeGraph/Heuristics.o : ../Top/src/Top/../Top/TypeGraph/Heuristics.hs
../Top/src/Top/../Top/TypeGraph/Heuristics.o : ../proxima/../helium/src/utils/Utils.hi
../Top/src/Top/../Top/TypeGraph/Heuristics.o : ../Top/src/Top/../Top/Types.hi
../Top/src/Top/../Top/TypeGraph/Heuristics.o : ../Top/src/Top/../Top/TypeGraph/Basics.hi
../Top/src/Top/../Top/TypeGraph/Heuristics.o : ../Top/src/Top/../Top/TypeGraph/TypeGraphState.hi
../proxima/../helium/src/staticanalysis/heuristics/OnlyResultHeuristics.o : ../proxima/../helium/src/staticanalysis/heuristics/OnlyResultHeuristics.hs
../proxima/../helium/src/staticanalysis/heuristics/OnlyResultHeuristics.o : ../proxima/../helium/src/staticanalysis/miscellaneous/UHA_Source.hi
../proxima/../helium/src/staticanalysis/heuristics/OnlyResultHeuristics.o : ../proxima/../helium/src/syntax/UHA_Syntax.hi
../proxima/../helium/src/staticanalysis/heuristics/OnlyResultHeuristics.o : ../proxima/../helium/src/utils/OneLiner.hi
../proxima/../helium/src/staticanalysis/heuristics/OnlyResultHeuristics.o : ../Top/src/Top/../Top/Types.hi
../proxima/../helium/src/staticanalysis/heuristics/OnlyResultHeuristics.o : ../Top/src/Top/../Top/States/TIState.hi
../proxima/../helium/src/staticanalysis/heuristics/OnlyResultHeuristics.o : ../Top/src/Top/../Top/TypeGraph/Heuristics.hi
../proxima/../helium/src/staticanalysis/heuristics/RepairHeuristics.o : ../proxima/../helium/src/staticanalysis/heuristics/RepairHeuristics.hs
../proxima/../helium/src/staticanalysis/heuristics/RepairHeuristics.o : ../proxima/../helium/src/syntax/UHA_Syntax.hi
../proxima/../helium/src/staticanalysis/heuristics/RepairHeuristics.o : ../proxima/../helium/src/staticanalysis/miscellaneous/UHA_Source.hi
../proxima/../helium/src/staticanalysis/heuristics/RepairHeuristics.o : ../proxima/../helium/src/utils/OneLiner.hi
../proxima/../helium/src/staticanalysis/heuristics/RepairHeuristics.o : ../proxima/../helium/src/staticanalysis/messages/Messages.hi
../proxima/../helium/src/staticanalysis/heuristics/RepairHeuristics.o : ../proxima/../helium/src/staticanalysis/miscellaneous/ConstraintInfo.hi
../proxima/../helium/src/staticanalysis/heuristics/RepairHeuristics.o : ../proxima/../helium/src/staticanalysis/messages/TypeErrors.hi
../proxima/../helium/src/staticanalysis/heuristics/RepairHeuristics.o : ../proxima/../helium/src/staticanalysis/heuristics/OnlyResultHeuristics.hi
../proxima/../helium/src/staticanalysis/heuristics/RepairHeuristics.o : ../Top/src/Top/../Top/Types.hi
../proxima/../helium/src/staticanalysis/heuristics/RepairHeuristics.o : ../Top/src/Top/../Top/TypeGraph/TypeGraphState.hi
../proxima/../helium/src/staticanalysis/heuristics/RepairHeuristics.o : ../Top/src/Top/../Top/States/SubstState.hi
../proxima/../helium/src/staticanalysis/heuristics/RepairHeuristics.o : ../Top/src/Top/../Top/States/TIState.hi
../proxima/../helium/src/staticanalysis/heuristics/RepairHeuristics.o : ../Top/src/Top/../Top/TypeGraph/Basics.hi
../proxima/../helium/src/staticanalysis/heuristics/RepairHeuristics.o : ../Top/src/Top/../Top/TypeGraph/Heuristics.hi
../proxima/../helium/src/modulesystem/ImportEnvironment.o : ../proxima/../helium/src/modulesystem/ImportEnvironment.hs
../proxima/../helium/src/modulesystem/ImportEnvironment.o : ../proxima/../helium/src/staticanalysis/directives/TS_CoreSyntax.hi
../proxima/../helium/src/modulesystem/ImportEnvironment.o : ../proxima/../helium/src/staticanalysis/heuristics/RepairHeuristics.hi
../proxima/../helium/src/modulesystem/ImportEnvironment.o : ../proxima/../helium/src/staticanalysis/messages/Messages.hi
../proxima/../helium/src/modulesystem/ImportEnvironment.o : ../proxima/../helium/src/parser/OperatorTable.hi
../proxima/../helium/src/modulesystem/ImportEnvironment.o : ../Top/src/Top/../Top/Types.hi
../proxima/../helium/src/modulesystem/ImportEnvironment.o : ../proxima/../helium/src/syntax/UHA_Utils.hi
../proxima/../helium/src/modulesystem/ImportEnvironment.o : ../proxima/../helium/src/syntax/UHA_Syntax.hi
../proxima/../helium/src/modulesystem/ImportEnvironment.o : ../proxima/../helium/src/utils/Utils.hi
../proxima/../helium/src/staticanalysis/heuristics/TieBreakerHeuristics.o : ../proxima/../helium/src/staticanalysis/heuristics/TieBreakerHeuristics.hs
../proxima/../helium/src/staticanalysis/heuristics/TieBreakerHeuristics.o : ../Top/src/Top/../Top/TypeGraph/Heuristics.hi
../proxima/../helium/src/staticanalysis/heuristics/TieBreakerHeuristics.o : ../Top/src/Top/../Top/States/TIState.hi
../proxima/../helium/src/staticanalysis/heuristics/TieBreakerHeuristics.o : ../Top/src/Top/../Top/Types.hi
../proxima/../helium/src/main/Args.o : ../proxima/../helium/src/main/Args.hs
../proxima/../helium/src/main/Args.o : ../proxima/../helium/src/utils/Utils.hi
../proxima/../helium/src/main/Args.o : ../proxima/../helium/src/main/Version.hi
../proxima/../helium/src/main/CompileUtils.o : ../proxima/../helium/src/main/CompileUtils.hs
../proxima/../helium/src/main/CompileUtils.o : ../lvm/src/lib/common/Standard.hi
../proxima/../helium/src/main/CompileUtils.o : ../proxima/../helium/src/syntax/UHA_Syntax.hi
../proxima/../helium/src/main/CompileUtils.o : ../proxima/../helium/src/modulesystem/ImportEnvironment.hi
../proxima/../helium/src/main/CompileUtils.o : ../proxima/../helium/src/utils/Logger.hi
../proxima/../helium/src/main/CompileUtils.o : ../proxima/../helium/src/utils/Utils.hi
../proxima/../helium/src/main/CompileUtils.o : ../proxima/../helium/src/staticanalysis/messages/HeliumMessages.hi
../proxima/../helium/src/main/CompileUtils.o : ../proxima/../helium/src/staticanalysis/messages/Messages.hi
../proxima/../helium/src/main/CompileUtils.o : ../proxima/../helium/src/main/Args.hi
../proxima/../helium/src/staticanalysis/directives/TS_Attributes.o : ../proxima/../helium/src/staticanalysis/directives/TS_Attributes.hs
../proxima/../helium/src/staticanalysis/directives/TS_Attributes.o : ../proxima/../helium/src/utils/Utils.hi
../proxima/../helium/src/staticanalysis/directives/TS_Attributes.o : ../proxima/../helium/src/staticanalysis/miscellaneous/UHA_Source.hi
../proxima/../helium/src/staticanalysis/directives/TS_Attributes.o : ../proxima/../helium/src/syntax/UHA_Syntax.hi
../proxima/../helium/src/staticanalysis/directives/TS_Attributes.o : ../proxima/../helium/src/utils/OneLiner.hi
../proxima/../helium/src/staticanalysis/directives/TS_Attributes.o : ../Top/src/Top/../Top/Types.hi
../proxima/../helium/src/staticanalysis/directives/TS_Attributes.o : ../proxima/../helium/src/staticanalysis/messages/Messages.hi
../proxima/../helium/src/staticanalysis/directives/TS_Attributes.o : ../proxima/../helium/src/staticanalysis/miscellaneous/ConstraintInfo.hi
../proxima/../helium/src/staticanalysis/directives/TS_Attributes.o : ../proxima/../helium/src/staticanalysis/inferencers/BindingGroupAnalysis.hi
../proxima/../helium/src/parser/ResolveOperators.o : ../proxima/../helium/src/parser/ResolveOperators.hs
../proxima/../helium/src/parser/ResolveOperators.o : ../proxima/../helium/src/staticanalysis/messages/Messages.hi
../proxima/../helium/src/parser/ResolveOperators.o : ../proxima/../helium/src/utils/Utils.hi
../proxima/../helium/src/parser/ResolveOperators.o : ../proxima/../helium/src/parser/OperatorTable.hi
../proxima/../helium/src/parser/ResolveOperators.o : ../proxima/../helium/src/syntax/UHA_Range.hi
../proxima/../helium/src/parser/ResolveOperators.o : ../proxima/../helium/src/syntax/UHA_Syntax.hi
../proxima/../helium/src/parser/ResolveOperators.o : ../proxima/../helium/src/syntax/UHA_Utils.hi
../proxima/../helium/src/parser/Lexer.o : ../proxima/../helium/src/parser/Lexer.hs
../proxima/../helium/src/parser/Lexer.o : ../proxima/../helium/src/utils/Utils.hi
../proxima/../helium/src/parser/Lexer.o : ../proxima/../helium/src/parser/LexerToken.hi
../proxima/../helium/src/parser/Lexer.o : ../proxima/../helium/src/parser/LexerMessage.hi
../proxima/../helium/src/parser/Lexer.o : ../proxima/../helium/src/parser/LexerMonad.hi
../proxima/../helium/src/parser/ParseLibrary.o : ../proxima/../helium/src/parser/ParseLibrary.hs
../proxima/../helium/src/parser/ParseLibrary.o : ../proxima/../helium/src/utils/Texts.hi
../proxima/../helium/src/parser/ParseLibrary.o : ../proxima/../helium/src/syntax/UHA_Utils.hi
../proxima/../helium/src/parser/ParseLibrary.o : ../proxima/../helium/src/syntax/UHA_Syntax.hi
../proxima/../helium/src/parser/ParseLibrary.o : ../proxima/../helium/src/parser/Lexer.hi
../proxima/../helium/src/parser/CollectFunctionBindings.o : ../proxima/../helium/src/parser/CollectFunctionBindings.hs
../proxima/../helium/src/parser/CollectFunctionBindings.o : ../proxima/../helium/src/utils/Utils.hi
../proxima/../helium/src/parser/CollectFunctionBindings.o : ../proxima/../helium/src/syntax/UHA_Range.hi
../proxima/../helium/src/parser/CollectFunctionBindings.o : ../proxima/../helium/src/syntax/UHA_Utils.hi
../proxima/../helium/src/parser/CollectFunctionBindings.o : ../proxima/../helium/src/syntax/UHA_Syntax.hi
../proxima/../helium/src/parser/Parser.o : ../proxima/../helium/src/parser/Parser.hs
../proxima/../helium/src/parser/Parser.o : ../proxima/../helium/src/utils/Utils.hi
../proxima/../helium/src/parser/Parser.o : ../proxima/../helium/src/parser/CollectFunctionBindings.hi
../proxima/../helium/src/parser/Parser.o : ../proxima/../helium/src/syntax/UHA_Range.hi
../proxima/../helium/src/parser/Parser.o : ../proxima/../helium/src/syntax/UHA_Utils.hi
../proxima/../helium/src/parser/Parser.o : ../proxima/../helium/src/syntax/UHA_Syntax.hi
../proxima/../helium/src/parser/Parser.o : ../proxima/../helium/src/utils/Texts.hi
../proxima/../helium/src/parser/Parser.o : ../proxima/../helium/src/parser/LayoutRule.hi
../proxima/../helium/src/parser/Parser.o : ../proxima/../helium/src/parser/Lexer.hi
../proxima/../helium/src/parser/Parser.o : ../proxima/../helium/src/parser/ParseLibrary.hi
../proxima/../helium/src/staticanalysis/directives/TS_Apply.o : ../proxima/../helium/src/staticanalysis/directives/TS_Apply.hs
../proxima/../helium/src/staticanalysis/directives/TS_Apply.o : ../Top/src/Top/../Top/Types.hi
../proxima/../helium/src/staticanalysis/directives/TS_Apply.o : ../proxima/../helium/src/staticanalysis/miscellaneous/DoublyLinkedTree.hi
../proxima/../helium/src/staticanalysis/directives/TS_Apply.o : ../proxima/../helium/src/staticanalysis/miscellaneous/UHA_Source.hi
../proxima/../helium/src/staticanalysis/directives/TS_Apply.o : ../Top/src/Top/../Top/ComposedSolvers/Tree.hi
../proxima/../helium/src/staticanalysis/directives/TS_Apply.o : ../proxima/../helium/src/staticanalysis/directives/TS_CoreSyntax.hi
../proxima/../helium/src/staticanalysis/directives/TS_Apply.o : ../proxima/../helium/src/staticanalysis/directives/TS_Attributes.hi
../proxima/../helium/src/staticanalysis/directives/TS_Apply.o : ../proxima/../helium/src/parser/ResolveOperators.hi
../proxima/../helium/src/staticanalysis/directives/TS_Apply.o : ../proxima/../helium/src/parser/ParseLibrary.hi
../proxima/../helium/src/staticanalysis/directives/TS_Apply.o : ../proxima/../helium/src/parser/Lexer.hi
../proxima/../helium/src/staticanalysis/directives/TS_Apply.o : ../proxima/../helium/src/parser/Parser.hi
../proxima/../helium/src/staticanalysis/directives/TS_Apply.o : ../proxima/../helium/src/parser/OperatorTable.hi
../proxima/../helium/src/staticanalysis/directives/TS_Apply.o : ../proxima/../helium/src/staticanalysis/inferencers/BindingGroupAnalysis.hi
../proxima/../helium/src/staticanalysis/directives/TS_Apply.o : ../proxima/../helium/src/modulesystem/ImportEnvironment.hi
../proxima/../helium/src/staticanalysis/directives/TS_Apply.o : ../proxima/../helium/src/staticanalysis/messages/TypeErrors.hi
../proxima/../helium/src/staticanalysis/directives/TS_Apply.o : ../proxima/../helium/src/staticanalysis/messages/Messages.hi
../proxima/../helium/src/staticanalysis/directives/TS_Apply.o : ../proxima/../helium/src/utils/OneLiner.hi
../proxima/../helium/src/staticanalysis/directives/TS_Apply.o : ../proxima/../helium/src/utils/Utils.hi
../proxima/../helium/src/staticanalysis/directives/TS_Apply.o : ../proxima/../helium/src/syntax/UHA_Utils.hi
../proxima/../helium/src/staticanalysis/directives/TS_Apply.o : ../proxima/../helium/src/syntax/UHA_Range.hi
../proxima/../helium/src/staticanalysis/directives/TS_Apply.o : ../Top/src/Top/../Top/Types.hi
../proxima/../helium/src/staticanalysis/directives/TS_Apply.o : ../proxima/../helium/src/staticanalysis/miscellaneous/ConstraintInfo.hi
../proxima/../helium/src/staticanalysis/directives/TS_Apply.o : ../proxima/../helium/src/staticanalysis/miscellaneous/TypeConstraints.hi
../proxima/../helium/src/staticanalysis/directives/TS_Apply.o : ../proxima/../helium/src/syntax/UHA_Syntax.hi
../proxima/../helium/src/modulesystem/DictionaryEnvironment.o : ../proxima/../helium/src/modulesystem/DictionaryEnvironment.hs
../proxima/../helium/src/modulesystem/DictionaryEnvironment.o : ../Top/src/Top/../Top/Types.hi
../proxima/../helium/src/modulesystem/DictionaryEnvironment.o : ../proxima/../helium/src/utils/Utils.hi
../proxima/../helium/src/modulesystem/DictionaryEnvironment.o : ../proxima/../helium/src/syntax/UHA_Utils.hi
../proxima/../helium/src/modulesystem/DictionaryEnvironment.o : ../proxima/../helium/src/syntax/UHA_Syntax.hi
../proxima/../helium/src/codegeneration/DerivingShow.o : ../proxima/../helium/src/codegeneration/DerivingShow.hs
../proxima/../helium/src/codegeneration/DerivingShow.o : ../Top/src/Top/../Top/Types.hi
../proxima/../helium/src/codegeneration/DerivingShow.o : ../proxima/../helium/src/utils/Utils.hi
../proxima/../helium/src/codegeneration/DerivingShow.o : ../lvm/src/lib/common/Id.hi
../proxima/../helium/src/codegeneration/DerivingShow.o : ../lvm/src/lib/core/Core.hi
../proxima/../helium/src/codegeneration/DerivingShow.o : ../proxima/../helium/src/codegeneration/CoreUtils.hi
../proxima/../helium/src/codegeneration/DerivingShow.o : ../proxima/../helium/src/syntax/UHA_Utils.hi
../proxima/../helium/src/codegeneration/DerivingShow.o : ../proxima/../helium/src/syntax/UHA_Syntax.hi
../proxima/../helium/src/staticanalysis/heuristics/UnifierHeuristics.o : ../proxima/../helium/src/staticanalysis/heuristics/UnifierHeuristics.hs
../proxima/../helium/src/staticanalysis/heuristics/UnifierHeuristics.o : ../proxima/../helium/src/utils/Utils.hi
../proxima/../helium/src/staticanalysis/heuristics/UnifierHeuristics.o : ../proxima/../helium/src/staticanalysis/miscellaneous/ConstraintInfo.hi
../proxima/../helium/src/staticanalysis/heuristics/UnifierHeuristics.o : ../proxima/../helium/src/staticanalysis/heuristics/RepairHeuristics.hi
../proxima/../helium/src/staticanalysis/heuristics/UnifierHeuristics.o : ../Top/src/Top/../Top/TypeGraph/Heuristics.hi
../proxima/../helium/src/staticanalysis/heuristics/UnifierHeuristics.o : ../Top/src/Top/../Top/TypeGraph/TypeGraphState.hi
../proxima/../helium/src/staticanalysis/heuristics/UnifierHeuristics.o : ../Top/src/Top/../Top/TypeGraph/Basics.hi
../proxima/../helium/src/staticanalysis/heuristics/UnifierHeuristics.o : ../Top/src/Top/../Top/States/TIState.hi
../proxima/../helium/src/staticanalysis/heuristics/UnifierHeuristics.o : ../Top/src/Top/../Top/Types.hi
../proxima/../helium/src/staticanalysis/heuristics/HeuristicsInfo.o : ../proxima/../helium/src/staticanalysis/heuristics/HeuristicsInfo.hs
../proxima/../helium/src/staticanalysis/heuristics/HeuristicsInfo.o : ../Top/src/Top/../Top/TypeGraph/Heuristics.hi
../proxima/../helium/src/staticanalysis/heuristics/HeuristicsInfo.o : ../Top/src/Top/../Top/Types.hi
../proxima/../helium/src/staticanalysis/heuristics/HeuristicsInfo.o : ../proxima/../helium/src/utils/Utils.hi
../proxima/../helium/src/staticanalysis/heuristics/HeuristicsInfo.o : ../proxima/../helium/src/staticanalysis/messages/TypeErrors.hi
../proxima/../helium/src/staticanalysis/heuristics/HeuristicsInfo.o : ../proxima/../helium/src/staticanalysis/messages/HeliumMessages.hi
../proxima/../helium/src/staticanalysis/heuristics/HeuristicsInfo.o : ../proxima/../helium/src/staticanalysis/messages/Messages.hi
../proxima/../helium/src/staticanalysis/heuristics/HeuristicsInfo.o : ../proxima/../helium/src/syntax/UHA_Syntax.hi
../proxima/../helium/src/staticanalysis/heuristics/HeuristicsInfo.o : ../proxima/../helium/src/staticanalysis/miscellaneous/UHA_Source.hi
../proxima/../helium/src/staticanalysis/heuristics/HeuristicsInfo.o : ../proxima/../helium/src/utils/OneLiner.hi
../proxima/../helium/src/staticanalysis/heuristics/HeuristicsInfo.o : ../proxima/../helium/src/staticanalysis/miscellaneous/DoublyLinkedTree.hi
../proxima/../helium/src/staticanalysis/heuristics/HeuristicsInfo.o : ../proxima/../helium/src/staticanalysis/heuristics/UnifierHeuristics.hi
../proxima/../helium/src/staticanalysis/heuristics/HeuristicsInfo.o : ../proxima/../helium/src/staticanalysis/heuristics/OnlyResultHeuristics.hi
../proxima/../helium/src/staticanalysis/heuristics/HeuristicsInfo.o : ../proxima/../helium/src/staticanalysis/heuristics/TieBreakerHeuristics.hi
../proxima/../helium/src/staticanalysis/heuristics/HeuristicsInfo.o : ../proxima/../helium/src/staticanalysis/heuristics/RepairHeuristics.hi
../proxima/../helium/src/staticanalysis/heuristics/HeuristicsInfo.o : ../proxima/../helium/src/staticanalysis/miscellaneous/ConstraintInfo.hi
../Top/src/Top/../Top/TypeGraph/ApplyHeuristics.o : ../Top/src/Top/../Top/TypeGraph/ApplyHeuristics.hs
../Top/src/Top/../Top/TypeGraph/ApplyHeuristics.o : ../Top/src/Top/../Top/States/TIState.hi
../Top/src/Top/../Top/TypeGraph/ApplyHeuristics.o : ../Top/src/Top/../Top/States/BasicState.hi
../Top/src/Top/../Top/TypeGraph/ApplyHeuristics.o : ../Top/src/Top/../Top/Types.hi
../Top/src/Top/../Top/TypeGraph/ApplyHeuristics.o : ../proxima/../helium/src/utils/Utils.hi
../Top/src/Top/../Top/TypeGraph/ApplyHeuristics.o : ../Top/src/Top/../Top/TypeGraph/Paths.hi
../Top/src/Top/../Top/TypeGraph/ApplyHeuristics.o : ../Top/src/Top/../Top/TypeGraph/Basics.hi
../Top/src/Top/../Top/TypeGraph/ApplyHeuristics.o : ../Top/src/Top/../Top/TypeGraph/Heuristics.hi
../Top/src/Top/../Top/TypeGraph/ApplyHeuristics.o : ../Top/src/Top/../Top/TypeGraph/TypeGraphState.hi
../Top/src/Top/../Top/TypeGraph/DefaultHeuristics.o : ../Top/src/Top/../Top/TypeGraph/DefaultHeuristics.hs
../Top/src/Top/../Top/TypeGraph/DefaultHeuristics.o : ../Top/src/Top/../Top/TypeGraph/ApplyHeuristics.hi
../Top/src/Top/../Top/TypeGraph/DefaultHeuristics.o : ../Top/src/Top/../Top/States/BasicState.hi
../Top/src/Top/../Top/TypeGraph/DefaultHeuristics.o : ../Top/src/Top/../Top/TypeGraph/Paths.hi
../Top/src/Top/../Top/TypeGraph/DefaultHeuristics.o : ../Top/src/Top/../Top/TypeGraph/Heuristics.hi
../Top/src/Top/../Top/TypeGraph/DefaultHeuristics.o : ../Top/src/Top/../Top/TypeGraph/Basics.hi
../proxima/../helium/src/staticanalysis/heuristics/ListOfHeuristics.o : ../proxima/../helium/src/staticanalysis/heuristics/ListOfHeuristics.hs
../proxima/../helium/src/staticanalysis/heuristics/ListOfHeuristics.o : ../Top/src/Top/../Top/TypeGraph/Basics.hi
../proxima/../helium/src/staticanalysis/heuristics/ListOfHeuristics.o : ../Top/src/Top/../Top/TypeGraph/Paths.hi
../proxima/../helium/src/staticanalysis/heuristics/ListOfHeuristics.o : ../Top/src/Top/../Top/TypeGraph/TypeGraphState.hi
../proxima/../helium/src/staticanalysis/heuristics/ListOfHeuristics.o : ../proxima/../helium/src/staticanalysis/heuristics/TieBreakerHeuristics.hi
../proxima/../helium/src/staticanalysis/heuristics/ListOfHeuristics.o : ../proxima/../helium/src/staticanalysis/heuristics/OnlyResultHeuristics.hi
../proxima/../helium/src/staticanalysis/heuristics/ListOfHeuristics.o : ../proxima/../helium/src/staticanalysis/heuristics/UnifierHeuristics.hi
../proxima/../helium/src/staticanalysis/heuristics/ListOfHeuristics.o : ../proxima/../helium/src/staticanalysis/heuristics/RepairHeuristics.hi
../proxima/../helium/src/staticanalysis/heuristics/ListOfHeuristics.o : ../Top/src/Top/../Top/TypeGraph/DefaultHeuristics.hi
../proxima/../helium/src/staticanalysis/heuristics/ListOfHeuristics.o : ../Top/src/Top/../Top/TypeGraph/Heuristics.hi
../proxima/../helium/src/staticanalysis/heuristics/ListOfHeuristics.o : ../Top/src/Top/../Top/Types.hi
../proxima/../helium/src/staticanalysis/heuristics/ListOfHeuristics.o : ../proxima/../helium/src/staticanalysis/heuristics/HeuristicsInfo.hi
../proxima/../helium/src/staticanalysis/heuristics/ListOfHeuristics.o : ../proxima/../helium/src/staticanalysis/miscellaneous/ConstraintInfo.hi
../proxima/../helium/src/staticanalysis/heuristics/ListOfHeuristics.o : ../proxima/../helium/src/main/Args.hi
../Top/src/Top/../Top/TypeGraph/EquivalenceGroup.o : ../Top/src/Top/../Top/TypeGraph/EquivalenceGroup.hs
../Top/src/Top/../Top/TypeGraph/EquivalenceGroup.o : ../proxima/../helium/src/utils/Utils.hi
../Top/src/Top/../Top/TypeGraph/EquivalenceGroup.o : ../Top/src/Top/../Top/Types.hi
../Top/src/Top/../Top/TypeGraph/EquivalenceGroup.o : ../Top/src/Top/../Top/TypeGraph/Basics.hi
../Top/src/Top/../Top/TypeGraph/EquivalenceGroup.o : ../Top/src/Top/../Top/TypeGraph/Paths.hi
../Top/src/Top/../Top/TypeGraph/TypeGraphMonad.o : ../Top/src/Top/../Top/TypeGraph/TypeGraphMonad.hs
../Top/src/Top/../Top/TypeGraph/TypeGraphMonad.o : ../proxima/../helium/src/utils/Utils.hi
../Top/src/Top/../Top/TypeGraph/TypeGraphMonad.o : ../Top/src/Top/../Top/TypeGraph/DefaultHeuristics.hi
../Top/src/Top/../Top/TypeGraph/TypeGraphMonad.o : ../Top/src/Top/../Top/TypeGraph/Heuristics.hi
../Top/src/Top/../Top/TypeGraph/TypeGraphMonad.o : ../Top/src/Top/../Top/TypeGraph/Paths.hi
../Top/src/Top/../Top/TypeGraph/TypeGraphMonad.o : ../Top/src/Top/../Top/States/States.hi
../Top/src/Top/../Top/TypeGraph/TypeGraphMonad.o : ../Top/src/Top/../Top/TypeGraph/Basics.hi
../Top/src/Top/../Top/TypeGraph/TypeGraphMonad.o : ../Top/src/Top/../Top/TypeGraph/EquivalenceGroup.hi
../Top/src/Top/../Top/TypeGraph/TypeGraphSubst.o : ../Top/src/Top/../Top/TypeGraph/TypeGraphSubst.hs
../Top/src/Top/../Top/TypeGraph/TypeGraphSubst.o : ../Top/src/Top/../Top/Types.hi
../Top/src/Top/../Top/TypeGraph/TypeGraphSubst.o : ../Top/src/Top/../Top/TypeGraph/Basics.hi
../Top/src/Top/../Top/TypeGraph/TypeGraphSubst.o : ../Top/src/Top/../Top/TypeGraph/TypeGraphMonad.hi
../Top/src/Top/../Top/TypeGraph/TypeGraphSubst.o : ../Top/src/Top/../Top/TypeGraph/TypeGraphState.hi
../Top/src/Top/../Top/TypeGraph/TypeGraphSubst.o : ../Top/src/Top/../Top/States/SubstState.hi
../Top/src/Top/../Top/TypeGraph/Implementation.o : ../Top/src/Top/../Top/TypeGraph/Implementation.hs
../Top/src/Top/../Top/TypeGraph/Implementation.o : ../proxima/../helium/src/utils/Utils.hi
../Top/src/Top/../Top/TypeGraph/Implementation.o : ../Top/src/Top/../Top/Types.hi
../Top/src/Top/../Top/TypeGraph/Implementation.o : ../Top/src/Top/../Top/TypeGraph/ApplyHeuristics.hi
../Top/src/Top/../Top/TypeGraph/Implementation.o : ../Top/src/Top/../Top/TypeGraph/Basics.hi
../Top/src/Top/../Top/TypeGraph/Implementation.o : ../Top/src/Top/../Top/TypeGraph/EquivalenceGroup.hi
../Top/src/Top/../Top/TypeGraph/Implementation.o : ../Top/src/Top/../Top/TypeGraph/TypeGraphState.hi
../Top/src/Top/../Top/TypeGraph/Implementation.o : ../Top/src/Top/../Top/TypeGraph/TypeGraphMonad.hi
../Top/src/Top/../Top/TypeGraph/Implementation.o : ../Top/src/Top/../Top/States/SubstState.hi
../Top/src/Top/../Top/TypeGraph/Implementation.o : ../Top/src/Top/../Top/States/BasicState.hi
../Top/src/Top/../Top/TypeGraph/Implementation.o : ../Top/src/Top/../Top/States/TIState.hi
../Top/src/Top/../Top/TypeGraph/TypeGraphSolver.o : ../Top/src/Top/../Top/TypeGraph/TypeGraphSolver.hs
../Top/src/Top/../Top/TypeGraph/TypeGraphSolver.o : ../Top/src/Top/../Top/Types.hi
../Top/src/Top/../Top/TypeGraph/TypeGraphSolver.o : ../Top/src/Top/../Top/Qualifiers/Qualifiers.hi
../Top/src/Top/../Top/TypeGraph/TypeGraphSolver.o : ../Top/src/Top/../Top/Constraints/Constraints.hi
../Top/src/Top/../Top/TypeGraph/TypeGraphSolver.o : ../Top/src/Top/../Top/States/States.hi
../Top/src/Top/../Top/TypeGraph/TypeGraphSolver.o : ../Top/src/Top/../Top/States/SubstState.hi
../Top/src/Top/../Top/TypeGraph/TypeGraphSolver.o : ../Top/src/Top/../Top/States/TIState.hi
../Top/src/Top/../Top/TypeGraph/TypeGraphSolver.o : ../Top/src/Top/../Top/Solvers/BasicMonad.hi
../Top/src/Top/../Top/TypeGraph/TypeGraphSolver.o : ../Top/src/Top/../Top/Solvers/SolveConstraints.hi
../Top/src/Top/../Top/TypeGraph/TypeGraphSolver.o : ../Top/src/Top/../Top/TypeGraph/Implementation.hi
../Top/src/Top/../Top/TypeGraph/TypeGraphSolver.o : ../Top/src/Top/../Top/TypeGraph/TypeGraphState.hi
../Top/src/Top/../Top/TypeGraph/TypeGraphSolver.o : ../Top/src/Top/../Top/TypeGraph/TypeGraphSubst.hi
../Top/src/Top/../Top/TypeGraph/TypeGraphSolver.o : ../Top/src/Top/../Top/TypeGraph/TypeGraphMonad.hi
../Top/src/Top/../Top/Solvers/GreedySubst.o : ../Top/src/Top/../Top/Solvers/GreedySubst.hs
../Top/src/Top/../Top/Solvers/GreedySubst.o : ../proxima/../helium/src/utils/Utils.hi
../Top/src/Top/../Top/Solvers/GreedySubst.o : ../Top/src/Top/../Top/Types.hi
../Top/src/Top/../Top/Solvers/GreedySubst.o : ../Top/src/Top/../Top/States/TIState.hi
../Top/src/Top/../Top/Solvers/GreedySubst.o : ../Top/src/Top/../Top/States/BasicState.hi
../Top/src/Top/../Top/Solvers/GreedySubst.o : ../Top/src/Top/../Top/States/SubstState.hi
../Top/src/Top/../Top/Solvers/GreedySubst.o : ../Top/src/Top/../Top/States/States.hi
../Top/src/Top/../Top/Solvers/GreedySolver.o : ../Top/src/Top/../Top/Solvers/GreedySolver.hs
../Top/src/Top/../Top/Solvers/GreedySolver.o : ../Top/src/Top/../Top/Qualifiers/Qualifiers.hi
../Top/src/Top/../Top/Solvers/GreedySolver.o : ../Top/src/Top/../Top/Constraints/TypeConstraintInfo.hi
../Top/src/Top/../Top/Solvers/GreedySolver.o : ../Top/src/Top/../Top/Constraints/Constraints.hi
../Top/src/Top/../Top/Solvers/GreedySolver.o : ../Top/src/Top/../Top/Types.hi
../Top/src/Top/../Top/Solvers/GreedySolver.o : ../Top/src/Top/../Top/States/States.hi
../Top/src/Top/../Top/Solvers/GreedySolver.o : ../Top/src/Top/../Top/States/SubstState.hi
../Top/src/Top/../Top/Solvers/GreedySolver.o : ../Top/src/Top/../Top/States/TIState.hi
../Top/src/Top/../Top/Solvers/GreedySolver.o : ../Top/src/Top/../Top/Solvers/BasicMonad.hi
../Top/src/Top/../Top/Solvers/GreedySolver.o : ../Top/src/Top/../Top/Solvers/GreedySubst.hi
../Top/src/Top/../Top/Solvers/GreedySolver.o : ../Top/src/Top/../Top/Solvers/SolveConstraints.hi
../proxima/../helium/src/staticanalysis/directives/TS_Messages.o : ../proxima/../helium/src/staticanalysis/directives/TS_Messages.hs
../proxima/../helium/src/staticanalysis/directives/TS_Messages.o : ../proxima/../helium/src/syntax/UHA_Syntax.hi
../proxima/../helium/src/staticanalysis/directives/TS_Messages.o : ../proxima/../helium/src/utils/Utils.hi
../proxima/../helium/src/staticanalysis/directives/TS_Messages.o : ../Top/src/Top/../Top/Types.hi
../proxima/../helium/src/staticanalysis/directives/TS_Messages.o : ../proxima/../helium/src/staticanalysis/messages/HeliumMessages.hi
../proxima/../helium/src/staticanalysis/directives/TS_Messages.o : ../proxima/../helium/src/staticanalysis/miscellaneous/ConstraintInfo.hi
../proxima/../helium/src/staticanalysis/directives/TS_Messages.o : ../proxima/../helium/src/staticanalysis/messages/TypeErrors.hi
../proxima/../helium/src/staticanalysis/directives/TS_Messages.o : ../proxima/../helium/src/staticanalysis/messages/Messages.hi
../proxima/../helium/src/modulesystem/ExtractImportDecls.o : ../proxima/../helium/src/modulesystem/ExtractImportDecls.hs
../proxima/../helium/src/modulesystem/ExtractImportDecls.o : ../proxima/../helium/src/utils/Utils.hi
../proxima/../helium/src/modulesystem/ExtractImportDecls.o : ../lvm/src/lib/core/Core.hi
../proxima/../helium/src/modulesystem/ExtractImportDecls.o : ../lvm/src/lib/common/Id.hi
../proxima/../helium/src/modulesystem/ExtractImportDecls.o : ../proxima/../helium/src/syntax/UHA_Utils.hi
../proxima/../helium/src/modulesystem/ExtractImportDecls.o : ../proxima/../helium/src/syntax/UHA_Syntax.hi
../proxima/../helium/src/modulesystem/CoreToImportEnv.o : ../proxima/../helium/src/modulesystem/CoreToImportEnv.hs
../proxima/../helium/src/modulesystem/CoreToImportEnv.o : ../proxima/../helium/src/syntax/UHA_Range.hi
../proxima/../helium/src/modulesystem/CoreToImportEnv.o : ../lvm/src/lib/common/Byte.hi
../proxima/../helium/src/modulesystem/CoreToImportEnv.o : ../Top/src/Top/../Top/Types.hi
../proxima/../helium/src/modulesystem/CoreToImportEnv.o : ../proxima/../helium/src/parser/OperatorTable.hi
../proxima/../helium/src/modulesystem/CoreToImportEnv.o : ../proxima/../helium/src/syntax/UHA_Syntax.hi
../proxima/../helium/src/modulesystem/CoreToImportEnv.o : ../lvm/src/lib/common/Id.hi
../proxima/../helium/src/modulesystem/CoreToImportEnv.o : ../proxima/../helium/src/syntax/UHA_Utils.hi
../proxima/../helium/src/modulesystem/CoreToImportEnv.o : ../proxima/../helium/src/modulesystem/ImportEnvironment.hi
../proxima/../helium/src/modulesystem/CoreToImportEnv.o : ../proxima/../helium/src/parser/Parser.hi
../proxima/../helium/src/modulesystem/CoreToImportEnv.o : ../proxima/../helium/src/parser/Lexer.hi
../proxima/../helium/src/modulesystem/CoreToImportEnv.o : ../proxima/../helium/src/parser/ParseLibrary.hi
../proxima/../helium/src/modulesystem/CoreToImportEnv.o : ../proxima/../helium/src/staticanalysis/miscellaneous/TypeConversion.hi
../proxima/../helium/src/modulesystem/CoreToImportEnv.o : ../proxima/../helium/src/utils/Utils.hi
../proxima/../helium/src/modulesystem/CoreToImportEnv.o : ../lvm/src/lib/core/Core.hi
../proxima/../helium/src/main/PhaseImport.o : ../proxima/../helium/src/main/PhaseImport.hs
../proxima/../helium/src/main/PhaseImport.o : ../lvm/src/lib/core/CorePretty.hi
../proxima/../helium/src/main/PhaseImport.o : ../proxima/../helium/src/modulesystem/ExtractImportDecls.hi
../proxima/../helium/src/main/PhaseImport.o : ../proxima/../helium/src/modulesystem/CoreToImportEnv.hi
../proxima/../helium/src/main/PhaseImport.o : ../lvm/src/lib/common/Id.hi
../proxima/../helium/src/main/PhaseImport.o : ../lvm/src/lib/lvm/LvmImport.hi
../proxima/../helium/src/main/PhaseImport.o : ../lvm/src/lib/common/Standard.hi
../proxima/../helium/src/main/PhaseImport.o : ../proxima/../helium/src/syntax/UHA_Range.hi
../proxima/../helium/src/main/PhaseImport.o : ../proxima/../helium/src/syntax/UHA_Utils.hi
../proxima/../helium/src/main/PhaseImport.o : ../proxima/../helium/src/syntax/UHA_Syntax.hi
../proxima/../helium/src/main/PhaseImport.o : ../lvm/src/lib/common/Id.hi
../proxima/../helium/src/main/PhaseImport.o : ../lvm/src/lib/core/Core.hi
../proxima/../helium/src/main/PhaseImport.o : ../proxima/../helium/src/main/CompileUtils.hi
../proxima/../helium/src/syntax/UHA_Pretty.o : ../proxima/../helium/src/syntax/UHA_Pretty.hs
../proxima/../helium/src/syntax/UHA_Pretty.o : ../proxima/../helium/src/utils/Utils.hi
../proxima/../helium/src/syntax/UHA_Pretty.o : ../proxima/../helium/src/syntax/UHA_Syntax.hi
../proxima/../helium/src/syntax/UHA_Pretty.o : ../Top/src/Top/../Top/Types.hi
../proxima/../helium/src/syntax/UHA_Pretty.o : ../lvm/src/lib/common/PPrint.hi
../proxima/../helium/src/staticanalysis/messages/Warnings.o : ../proxima/../helium/src/staticanalysis/messages/Warnings.hs
../proxima/../helium/src/staticanalysis/messages/Warnings.o : ../proxima/../helium/src/syntax/UHA_Pretty.hi
../proxima/../helium/src/staticanalysis/messages/Warnings.o : ../proxima/../helium/src/utils/Utils.hi
../proxima/../helium/src/staticanalysis/messages/Warnings.o : ../proxima/../helium/src/staticanalysis/messages/Messages.hi
../proxima/../helium/src/staticanalysis/messages/Warnings.o : ../Top/src/Top/../Top/Types.hi
../proxima/../helium/src/staticanalysis/messages/Warnings.o : ../proxima/../helium/src/syntax/UHA_Utils.hi
../proxima/../helium/src/staticanalysis/messages/Warnings.o : ../proxima/../helium/src/syntax/UHA_Syntax.hi
../proxima/../helium/src/staticanalysis/messages/Warnings.o : ../proxima/../helium/src/syntax/UHA_Range.hi
../proxima/../helium/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../proxima/../helium/src/staticanalysis/inferencers/SelectConstraintSolver.hs
../proxima/../helium/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../proxima/../helium/src/staticanalysis/messages/Warnings.hi
../proxima/../helium/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../Top/src/Top/../Top/Constraints/Polymorphism.hi
../proxima/../helium/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../proxima/../helium/src/syntax/UHA_Utils.hi
../proxima/../helium/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../proxima/../helium/src/syntax/UHA_Syntax.hi
../proxima/../helium/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../Top/src/Top/../Top/ComposedSolvers/ChunkySolver.hi
../proxima/../helium/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../Top/src/Top/../Top/ComposedSolvers/CombinationSolver.hi
../proxima/../helium/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../Top/src/Top/../Top/ComposedSolvers/TreeWalk.hi
../proxima/../helium/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../Top/src/Top/../Top/ComposedSolvers/Tree.hi
../proxima/../helium/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../Top/src/Top/../Top/TypeGraph/TypeGraphMonad.hi
../proxima/../helium/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../Top/src/Top/../Top/TypeGraph/TypeGraphSolver.hi
../proxima/../helium/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../Top/src/Top/../Top/Solvers/GreedySolver.hi
../proxima/../helium/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../Top/src/Top/../Top/Solvers/SimpleSolver.hi
../proxima/../helium/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../Top/src/Top/../Top/States/States.hi
../proxima/../helium/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../Top/src/Top/../Top/Solvers/SolveConstraints.hi
../proxima/../helium/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../Top/src/Top/../Top/States/SubstState.hi
../proxima/../helium/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../Top/src/Top/../Top/States/BasicState.hi
../proxima/../helium/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../Top/src/Top/../Top/Types.hi
../proxima/../helium/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../proxima/../helium/src/modulesystem/ImportEnvironment.hi
../proxima/../helium/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../proxima/../helium/src/staticanalysis/heuristics/ListOfHeuristics.hi
../proxima/../helium/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../proxima/../helium/src/staticanalysis/miscellaneous/TypeConstraints.hi
../proxima/../helium/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../proxima/../helium/src/staticanalysis/miscellaneous/ConstraintInfo.hi
../proxima/../helium/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../proxima/../helium/src/main/Args.hi
../proxima/../helium/src/staticanalysis/inferencers/TypeInferencing.o : ../proxima/../helium/src/staticanalysis/inferencers/TypeInferencing.hs
../proxima/../helium/src/staticanalysis/inferencers/TypeInferencing.o : ../proxima/../helium/src/syntax/UHA_Utils.hi
../proxima/../helium/src/staticanalysis/inferencers/TypeInferencing.o : ../proxima/../helium/src/staticanalysis/directives/TS_Attributes.hi
../proxima/../helium/src/staticanalysis/inferencers/TypeInferencing.o : ../proxima/../helium/src/staticanalysis/directives/TS_CoreSyntax.hi
../proxima/../helium/src/staticanalysis/inferencers/TypeInferencing.o : ../proxima/../helium/src/staticanalysis/directives/TS_Apply.hi
../proxima/../helium/src/staticanalysis/inferencers/TypeInferencing.o : ../proxima/../helium/src/staticanalysis/directives/Matchers.hi
../proxima/../helium/src/staticanalysis/inferencers/TypeInferencing.o : ../proxima/../helium/src/main/Args.hi
../proxima/../helium/src/staticanalysis/inferencers/TypeInferencing.o : ../proxima/../helium/src/modulesystem/DictionaryEnvironment.hi
../proxima/../helium/src/staticanalysis/inferencers/TypeInferencing.o : ../proxima/../helium/src/modulesystem/ImportEnvironment.hi
../proxima/../helium/src/staticanalysis/inferencers/TypeInferencing.o : ../proxima/../helium/src/codegeneration/DerivingShow.hi
../proxima/../helium/src/staticanalysis/inferencers/TypeInferencing.o : ../proxima/../helium/src/utils/Utils.hi
../proxima/../helium/src/staticanalysis/inferencers/TypeInferencing.o : ../proxima/../helium/src/syntax/UHA_Utils.hi
../proxima/../helium/src/staticanalysis/inferencers/TypeInferencing.o : ../proxima/../helium/src/syntax/UHA_Range.hi
../proxima/../helium/src/staticanalysis/inferencers/TypeInferencing.o : ../proxima/../helium/src/syntax/UHA_Syntax.hi
../proxima/../helium/src/staticanalysis/inferencers/TypeInferencing.o : ../proxima/../helium/src/staticanalysis/inferencers/BindingGroupAnalysis.hi
../proxima/../helium/src/staticanalysis/inferencers/TypeInferencing.o : ../proxima/../helium/src/staticanalysis/heuristics/HeuristicsInfo.hi
../proxima/../helium/src/staticanalysis/inferencers/TypeInferencing.o : ../Top/src/Top/../Top/Solvers/SolveConstraints.hi
../proxima/../helium/src/staticanalysis/inferencers/TypeInferencing.o : ../proxima/../helium/src/staticanalysis/inferencers/SelectConstraintSolver.hi
../proxima/../helium/src/staticanalysis/inferencers/TypeInferencing.o : ../Top/src/Top/../Top/ComposedSolvers/Tree.hi
../proxima/../helium/src/staticanalysis/inferencers/TypeInferencing.o : ../proxima/../helium/src/staticanalysis/miscellaneous/TypeConstraints.hi
../proxima/../helium/src/staticanalysis/inferencers/TypeInferencing.o : ../proxima/../helium/src/staticanalysis/miscellaneous/UHA_Source.hi
../proxima/../helium/src/staticanalysis/inferencers/TypeInferencing.o : ../proxima/../helium/src/staticanalysis/miscellaneous/DoublyLinkedTree.hi
../proxima/../helium/src/staticanalysis/inferencers/TypeInferencing.o : ../proxima/../helium/src/staticanalysis/miscellaneous/ConstraintInfo.hi
../proxima/../helium/src/staticanalysis/inferencers/TypeInferencing.o : ../proxima/../helium/src/staticanalysis/messages/Warnings.hi
../proxima/../helium/src/staticanalysis/inferencers/TypeInferencing.o : ../proxima/../helium/src/staticanalysis/messages/TypeErrors.hi
../proxima/../helium/src/staticanalysis/inferencers/TypeInferencing.o : ../proxima/../helium/src/staticanalysis/messages/Messages.hi
../proxima/../helium/src/staticanalysis/inferencers/TypeInferencing.o : ../proxima/../helium/src/staticanalysis/miscellaneous/TypeConversion.hi
../proxima/../helium/src/staticanalysis/inferencers/TypeInferencing.o : ../Top/src/Top/../Top/Types.hi
../proxima/../helium/src/staticanalysis/inferencers/ExpressionTypeInferencer.o : ../proxima/../helium/src/staticanalysis/inferencers/ExpressionTypeInferencer.hs
../proxima/../helium/src/staticanalysis/inferencers/ExpressionTypeInferencer.o : ../proxima/../helium/src/syntax/UHA_Syntax.hi
../proxima/../helium/src/staticanalysis/inferencers/ExpressionTypeInferencer.o : ../proxima/../helium/src/utils/Utils.hi
../proxima/../helium/src/staticanalysis/inferencers/ExpressionTypeInferencer.o : ../proxima/../helium/src/syntax/UHA_Range.hi
../proxima/../helium/src/staticanalysis/inferencers/ExpressionTypeInferencer.o : ../proxima/../helium/src/syntax/UHA_Utils.hi
../proxima/../helium/src/staticanalysis/inferencers/ExpressionTypeInferencer.o : ../Top/src/Top/../Top/Types.hi
../proxima/../helium/src/staticanalysis/inferencers/ExpressionTypeInferencer.o : ../proxima/../helium/src/staticanalysis/messages/TypeErrors.hi
../proxima/../helium/src/staticanalysis/inferencers/ExpressionTypeInferencer.o : ../proxima/../helium/src/staticanalysis/inferencers/BindingGroupAnalysis.hi
../proxima/../helium/src/staticanalysis/inferencers/ExpressionTypeInferencer.o : ../proxima/../helium/src/modulesystem/ImportEnvironment.hi
../proxima/../helium/src/staticanalysis/inferencers/ExpressionTypeInferencer.o : ../proxima/../helium/src/staticanalysis/inferencers/TypeInferencing.hi
../proxima/../helium/src/staticanalysis/directives/TS_ToCore.o : ../proxima/../helium/src/staticanalysis/directives/TS_ToCore.hs
../proxima/../helium/src/staticanalysis/directives/TS_ToCore.o : ../proxima/../helium/src/staticanalysis/inferencers/ExpressionTypeInferencer.hi
../proxima/../helium/src/staticanalysis/directives/TS_ToCore.o : ../proxima/../helium/src/syntax/UHA_Syntax.hi
../proxima/../helium/src/staticanalysis/directives/TS_ToCore.o : ../proxima/../helium/src/staticanalysis/miscellaneous/TypeConstraints.hi
../proxima/../helium/src/staticanalysis/directives/TS_ToCore.o : ../proxima/../helium/src/staticanalysis/directives/TS_Attributes.hi
../proxima/../helium/src/staticanalysis/directives/TS_ToCore.o : ../proxima/../helium/src/utils/OneLiner.hi
../proxima/../helium/src/staticanalysis/directives/TS_ToCore.o : ../proxima/../helium/src/staticanalysis/messages/TypeErrors.hi
../proxima/../helium/src/staticanalysis/directives/TS_ToCore.o : ../proxima/../helium/src/utils/Utils.hi
../proxima/../helium/src/staticanalysis/directives/TS_ToCore.o : ../proxima/../helium/src/staticanalysis/miscellaneous/TypeConversion.hi
../proxima/../helium/src/staticanalysis/directives/TS_ToCore.o : ../proxima/../helium/src/staticanalysis/miscellaneous/UHA_Source.hi
../proxima/../helium/src/staticanalysis/directives/TS_ToCore.o : ../proxima/../helium/src/syntax/UHA_OneLine.hi
../proxima/../helium/src/staticanalysis/directives/TS_ToCore.o : ../proxima/../helium/src/syntax/UHA_Utils.hi
../proxima/../helium/src/staticanalysis/directives/TS_ToCore.o : ../proxima/../helium/src/syntax/UHA_Range.hi
../proxima/../helium/src/staticanalysis/directives/TS_ToCore.o : ../proxima/../helium/src/staticanalysis/miscellaneous/ConstraintInfo.hi
../proxima/../helium/src/staticanalysis/directives/TS_ToCore.o : ../Top/src/Top/../Top/Types.hi
../proxima/../helium/src/staticanalysis/directives/TS_ToCore.o : ../proxima/../helium/src/staticanalysis/messages/Messages.hi
../proxima/../helium/src/staticanalysis/directives/TS_ToCore.o : ../proxima/../helium/src/modulesystem/ImportEnvironment.hi
../proxima/../helium/src/staticanalysis/directives/TS_ToCore.o : ../proxima/../helium/src/staticanalysis/directives/TS_CoreSyntax.hi
../proxima/../helium/src/staticanalysis/directives/TS_ToCore.o : ../proxima/../helium/src/staticanalysis/directives/TS_Syntax.hi
../proxima/../helium/src/staticanalysis/staticchecks/StaticChecks.o : ../proxima/../helium/src/staticanalysis/staticchecks/StaticChecks.hs
../proxima/../helium/src/staticanalysis/staticchecks/StaticChecks.o : ../proxima/../helium/src/parser/OperatorTable.hi
../proxima/../helium/src/staticanalysis/staticchecks/StaticChecks.o : ../proxima/../helium/src/modulesystem/ImportEnvironment.hi
../proxima/../helium/src/staticanalysis/staticchecks/StaticChecks.o : ../proxima/../helium/src/staticanalysis/miscellaneous/TypeConstraints.hi
../proxima/../helium/src/staticanalysis/staticchecks/StaticChecks.o : ../proxima/../helium/src/codegeneration/DerivingShow.hi
../proxima/../helium/src/staticanalysis/staticchecks/StaticChecks.o : ../proxima/../helium/src/staticanalysis/miscellaneous/TypeConversion.hi
../proxima/../helium/src/staticanalysis/staticchecks/StaticChecks.o : ../proxima/../helium/src/utils/Utils.hi
../proxima/../helium/src/staticanalysis/staticchecks/StaticChecks.o : ../lvm/src/lib/common/TopSort.hi
../proxima/../helium/src/staticanalysis/staticchecks/StaticChecks.o : ../proxima/../helium/src/staticanalysis/messages/Messages.hi
../proxima/../helium/src/staticanalysis/staticchecks/StaticChecks.o : ../proxima/../helium/src/staticanalysis/messages/Warnings.hi
../proxima/../helium/src/staticanalysis/staticchecks/StaticChecks.o : ../proxima/../helium/src/staticanalysis/messages/StaticErrors.hi
../proxima/../helium/src/staticanalysis/staticchecks/StaticChecks.o : ../Top/src/Top/../Top/Types.hi
../proxima/../helium/src/staticanalysis/staticchecks/StaticChecks.o : ../proxima/../helium/src/syntax/UHA_Range.hi
../proxima/../helium/src/staticanalysis/staticchecks/StaticChecks.o : ../proxima/../helium/src/syntax/UHA_Utils.hi
../proxima/../helium/src/staticanalysis/staticchecks/StaticChecks.o : ../proxima/../helium/src/syntax/UHA_Syntax.hi
../proxima/../helium/src/staticanalysis/staticchecks/StaticChecks.o : ../proxima/../helium/src/main/Args.hi
../proxima/../helium/src/staticanalysis/staticchecks/StaticChecks.o : ../proxima/../helium/src/utils/Similarity.hi
../proxima/../helium/src/main/PhaseStaticChecks.o : ../proxima/../helium/src/main/PhaseStaticChecks.hs
../proxima/../helium/src/main/PhaseStaticChecks.o : ../proxima/../helium/src/staticanalysis/messages/StaticErrors.hi
../proxima/../helium/src/main/PhaseStaticChecks.o : ../Top/src/Top/../Top/Types.hi
../proxima/../helium/src/main/PhaseStaticChecks.o : ../proxima/../helium/src/syntax/UHA_Syntax.hi
../proxima/../helium/src/main/PhaseStaticChecks.o : ../proxima/../helium/src/staticanalysis/staticchecks/StaticChecks.hi
../proxima/../helium/src/main/PhaseStaticChecks.o : ../proxima/../helium/src/staticanalysis/messages/Warnings.hi
../proxima/../helium/src/main/PhaseStaticChecks.o : ../proxima/../helium/src/main/CompileUtils.hi
../proxima/../helium/src/staticanalysis/directives/TS_Analyse.o : ../proxima/../helium/src/staticanalysis/directives/TS_Analyse.hs
../proxima/../helium/src/staticanalysis/directives/TS_Analyse.o : ../proxima/../helium/src/staticanalysis/inferencers/ExpressionTypeInferencer.hi
../proxima/../helium/src/staticanalysis/directives/TS_Analyse.o : ../proxima/../helium/src/syntax/UHA_Syntax.hi
../proxima/../helium/src/staticanalysis/directives/TS_Analyse.o : ../proxima/../helium/src/syntax/UHA_Pretty.hi
../proxima/../helium/src/staticanalysis/directives/TS_Analyse.o : ../proxima/../helium/src/utils/Utils.hi
../proxima/../helium/src/staticanalysis/directives/TS_Analyse.o : ../proxima/../helium/src/staticanalysis/inferencers/ExpressionTypeInferencer.hi
../proxima/../helium/src/staticanalysis/directives/TS_Analyse.o : ../proxima/../helium/src/modulesystem/ImportEnvironment.hi
../proxima/../helium/src/staticanalysis/directives/TS_Analyse.o : ../proxima/../helium/src/staticanalysis/directives/TS_Messages.hi
../proxima/../helium/src/staticanalysis/directives/TS_Analyse.o : ../proxima/../helium/src/staticanalysis/messages/TypeErrors.hi
../proxima/../helium/src/staticanalysis/directives/TS_Analyse.o : ../proxima/../helium/src/staticanalysis/messages/Messages.hi
../proxima/../helium/src/staticanalysis/directives/TS_Analyse.o : ../proxima/../helium/src/staticanalysis/miscellaneous/UHA_Source.hi
../proxima/../helium/src/staticanalysis/directives/TS_Analyse.o : ../proxima/../helium/src/syntax/UHA_Range.hi
../proxima/../helium/src/staticanalysis/directives/TS_Analyse.o : ../proxima/../helium/src/syntax/UHA_Utils.hi
../proxima/../helium/src/staticanalysis/directives/TS_Analyse.o : ../proxima/../helium/src/staticanalysis/miscellaneous/TypeConversion.hi
../proxima/../helium/src/staticanalysis/directives/TS_Analyse.o : ../proxima/../helium/src/staticanalysis/directives/TS_Syntax.hi
../proxima/../helium/src/staticanalysis/directives/TS_Analyse.o : ../proxima/../helium/src/staticanalysis/miscellaneous/ConstraintInfo.hi
../proxima/../helium/src/staticanalysis/directives/TS_Analyse.o : ../proxima/../helium/src/staticanalysis/miscellaneous/TypeConstraints.hi
../proxima/../helium/src/staticanalysis/directives/TS_Analyse.o : ../Top/src/Top/../Top/States/TIState.hi
../proxima/../helium/src/staticanalysis/directives/TS_Analyse.o : ../Top/src/Top/../Top/Solvers/SolveConstraints.hi
../proxima/../helium/src/staticanalysis/directives/TS_Analyse.o : ../Top/src/Top/../Top/Solvers/GreedySolver.hi
../proxima/../helium/src/staticanalysis/directives/TS_Analyse.o : ../Top/src/Top/../Top/Types.hi
../proxima/../helium/src/staticanalysis/directives/TS_Parser.o : ../proxima/../helium/src/staticanalysis/directives/TS_Parser.hs
../proxima/../helium/src/staticanalysis/directives/TS_Parser.o : ../proxima/../helium/src/utils/Utils.hi
../proxima/../helium/src/staticanalysis/directives/TS_Parser.o : ../proxima/../helium/src/parser/OperatorTable.hi
../proxima/../helium/src/staticanalysis/directives/TS_Parser.o : ../proxima/../helium/src/parser/ResolveOperators.hi
../proxima/../helium/src/staticanalysis/directives/TS_Parser.o : ../proxima/../helium/src/parser/Parser.hi
../proxima/../helium/src/staticanalysis/directives/TS_Parser.o : ../proxima/../helium/src/parser/ParseLibrary.hi
../proxima/../helium/src/staticanalysis/directives/TS_Parser.o : ../proxima/../helium/src/parser/Lexer.hi
../proxima/../helium/src/staticanalysis/directives/TS_Parser.o : ../proxima/../helium/src/staticanalysis/directives/TS_ToCore.hi
../proxima/../helium/src/staticanalysis/directives/TS_Parser.o : ../proxima/../helium/src/staticanalysis/directives/TS_CoreSyntax.hi
../proxima/../helium/src/staticanalysis/directives/TS_Parser.o : ../proxima/../helium/src/staticanalysis/directives/TS_Syntax.hi
../proxima/../helium/src/staticanalysis/directives/TS_Parser.o : ../proxima/../helium/src/syntax/UHA_Pretty.hi
../proxima/../helium/src/staticanalysis/directives/TS_Parser.o : ../proxima/../helium/src/syntax/UHA_Utils.hi
../proxima/../helium/src/staticanalysis/directives/TS_Parser.o : ../proxima/../helium/src/syntax/UHA_Syntax.hi
../proxima/../helium/src/staticanalysis/directives/TS_Compile.o : ../proxima/../helium/src/staticanalysis/directives/TS_Compile.hs
../proxima/../helium/src/staticanalysis/directives/TS_Compile.o : ../lvm/src/lib/core/Core.hi
../proxima/../helium/src/staticanalysis/directives/TS_Compile.o : ../proxima/../helium/src/codegeneration/CoreUtils.hi
../proxima/../helium/src/staticanalysis/directives/TS_Compile.o : ../proxima/../helium/src/parser/ParseMessage.hi
../proxima/../helium/src/staticanalysis/directives/TS_Compile.o : ../proxima/../helium/src/main/Args.hi
../proxima/../helium/src/staticanalysis/directives/TS_Compile.o : ../proxima/../helium/src/staticanalysis/messages/HeliumMessages.hi
../proxima/../helium/src/staticanalysis/directives/TS_Compile.o : ../proxima/../helium/src/staticanalysis/directives/TS_Analyse.hi
../proxima/../helium/src/staticanalysis/directives/TS_Compile.o : ../proxima/../helium/src/parser/Lexer.hi
../proxima/../helium/src/staticanalysis/directives/TS_Compile.o : ../proxima/../helium/src/staticanalysis/directives/TS_Parser.hi
../proxima/../helium/src/staticanalysis/directives/TS_Compile.o : ../proxima/../helium/src/staticanalysis/directives/TS_ToCore.hi
../proxima/../helium/src/staticanalysis/directives/TS_Compile.o : ../proxima/../helium/src/modulesystem/ImportEnvironment.hi
../proxima/../helium/src/staticanalysis/directives/TS_Compile.o : ../proxima/../helium/src/staticanalysis/directives/TS_CoreSyntax.hi
../proxima/../helium/src/main/PhaseTypingStrategies.o : ../proxima/../helium/src/main/PhaseTypingStrategies.hs
../proxima/../helium/src/main/PhaseTypingStrategies.o : ../Top/src/Top/../Top/Types.hi
../proxima/../helium/src/main/PhaseTypingStrategies.o : ../proxima/../helium/src/syntax/UHA_Syntax.hi
../proxima/../helium/src/main/PhaseTypingStrategies.o : ../proxima/../helium/src/staticanalysis/directives/TS_Compile.hi
../proxima/../helium/src/main/PhaseTypingStrategies.o : ../lvm/src/lib/core/Core.hi
../proxima/../helium/src/main/PhaseTypingStrategies.o : ../proxima/../helium/src/main/CompileUtils.hi
../proxima/src/main/ArchitectureLibM.o : ../proxima/src/main/ArchitectureLibM.hs
../proxima/src/common/UU_Parsing.o : ../proxima/src/common/UU_Parsing.hs
../proxima/src/common/DebugLevels.o : ../proxima/src/common/DebugLevels.hs
../proxima/src/common/CommonTypes.o : ../proxima/src/common/CommonTypes.hs
../proxima/src/common/CommonTypes.o : ../proxima/src/common/DebugLevels.hi
../proxima/src/evaluation/DocTypes.o : ../proxima/src/evaluation/DocTypes.hs
../proxima/src/evaluation/DocTypes.o : ../proxima/src/common/CommonTypes.hi
../proxima/src/arrangement/FontLib.o : ../proxima/src/arrangement/FontLib.hs
../proxima/src/arrangement/FontLib.o : ../proxima/src/common/CommonTypes.hi
../proxima/src/common/CommonUtils.o : ../proxima/src/common/CommonUtils.hs
../proxima/src/common/CommonUtils.o : ../proxima/src/common/CommonTypes.hi
../proxima/src/rendering/RenTypes.o : ../proxima/src/rendering/RenTypes.hs
../proxima/src/rendering/RenTypes.o : ../proxima/src/evaluation/DocTypes.hi
../proxima/src/rendering/RenTypes.o : ../proxima/src/common/CommonUtils.hi
../proxima/src/rendering/RenTypes.o : ../proxima/src/common/CommonTypes.hi
../proxima/src/rendering/RenUtils.o : ../proxima/src/rendering/RenUtils.hs
../proxima/src/rendering/RenUtils.o : ../proxima/src/rendering/RenTypes.hi
../proxima/src/rendering/RenUtils.o : ../proxima/src/common/CommonTypes.hi
../proxima/src/main/GUI.o : ../proxima/src/main/GUI.hs
../proxima/src/main/GUI.o : ../proxima/src/common/CommonUtils.hi
../proxima/src/main/GUI.o : ../proxima/src/rendering/RenTypes.hi
../proxima/src/main/GUI.o : ../proxima/src/common/CommonTypes.hi
../proxima/src/main/GUI.o : ../proxima/src/common/CommonTypes.hi
../proxima/src/presentation/PresTypes.o : ../proxima/src/presentation/PresTypes.hs
../proxima/src/presentation/PresTypes.o : ../proxima/src/common/CommonUtils.hi
../proxima/src/presentation/PresTypes.o : ../proxima/src/evaluation/DocTypes.hi
../proxima/src/presentation/PresTypes.o : ../proxima/src/common/CommonTypes.hi
../proxima/src/evaluation/DocUtils.o : ../proxima/src/evaluation/DocUtils.hs
../proxima/src/evaluation/DocUtils.o : ../proxima/src/presentation/PresTypes.hi
../proxima/src/evaluation/DocUtils.o : ../proxima/src/evaluation/DocTypes.hi
../proxima/src/evaluation/EnrUtils.o : ../proxima/src/evaluation/EnrUtils.hs
../proxima/src/evaluation/EnrUtils.o : ../proxima/src/evaluation/DocUtils.hi
../proxima/src/evaluation/EnrUtils.o : ../proxima/src/evaluation/DocTypes.hi
../proxima/src/evaluation/DocumentEdit.o : ../proxima/src/evaluation/DocumentEdit.hs
../proxima/src/evaluation/DocumentEdit.o : ../proxima/src/presentation/PresTypes.hi
../proxima/src/evaluation/DocumentEdit.o : ../proxima/src/evaluation/DocUtils.hi
../proxima/src/evaluation/DocumentEdit.o : ../proxima/src/evaluation/DocTypes.hi
../proxima/src/evaluation/DocumentEdit.o : ../proxima/src/common/CommonTypes.hi
../proxima/src/evaluation/EnrTypes.o : ../proxima/src/evaluation/EnrTypes.hs
../proxima/src/evaluation/EnrTypes.o : ../proxima/src/presentation/PresTypes.hi
../proxima/src/evaluation/EnrTypes.o : ../proxima/src/common/CommonUtils.hi
../proxima/src/evaluation/EnrTypes.o : ../proxima/src/evaluation/DocTypes.hi
../proxima/src/evaluation/EnrTypes.o : ../proxima/src/common/CommonTypes.hi
../proxima/src/evaluation/EvalLayerTypes.o : ../proxima/src/evaluation/EvalLayerTypes.hs
../proxima/src/evaluation/EvalLayerTypes.o : ../proxima/src/evaluation/DocTypes.hi
../proxima/src/evaluation/EvalLayerTypes.o : ../proxima/src/evaluation/EnrTypes.hi
../proxima/src/evaluation/EvalLayerTypes.o : ../proxima/src/common/CommonTypes.hi
../proxima/src/evaluation/EvalLayerUtils.o : ../proxima/src/evaluation/EvalLayerUtils.hs
../proxima/src/evaluation/EvalLayerUtils.o : ../proxima/src/evaluation/EnrUtils.hi
../proxima/src/evaluation/EvalLayerUtils.o : ../proxima/src/common/CommonUtils.hi
../proxima/src/evaluation/EvalLayerUtils.o : ../proxima/src/evaluation/EvalLayerTypes.hi
../proxima/src/evaluation/EvalLayerUtils.o : ../proxima/src/common/CommonTypes.hi
../proxima/src/evaluation/EvalTranslate.o : ../proxima/src/evaluation/EvalTranslate.hs
../proxima/src/evaluation/EvalTranslate.o : ../proxima/src/evaluation/EvalLayerTypes.hi
../proxima/src/evaluation/EvalTranslate.o : ../proxima/src/common/CommonTypes.hi
../proxima/src/evaluation/EvalPresent.o : ../proxima/src/evaluation/EvalPresent.hs
../proxima/src/evaluation/EvalPresent.o : ../proxima/src/evaluation/EvalLayerUtils.hi
../proxima/src/evaluation/EvalPresent.o : ../proxima/src/evaluation/EvalLayerTypes.hi
../proxima/src/evaluation/EvalPresent.o : ../proxima/src/common/CommonTypes.hi
../proxima/src/presentation/XprezLib.o : ../proxima/src/presentation/XprezLib.hs
../proxima/src/presentation/XprezLib.o : ../proxima/src/presentation/PresTypes.hi
../proxima/src/presentation/XprezLib.o : ../proxima/src/evaluation/DocTypes.hi
../proxima/src/presentation/XprezLib.o : ../proxima/src/common/CommonTypes.hi
../proxima/src/presentation/PresUtils.o : ../proxima/src/presentation/PresUtils.hs
../proxima/src/presentation/PresUtils.o : ../proxima/src/presentation/XprezLib.hi
../proxima/src/presentation/PresUtils.o : ../proxima/src/common/CommonUtils.hi
../proxima/src/presentation/PresUtils.o : ../proxima/src/presentation/PresTypes.hi
../proxima/src/presentation/PresUtils.o : ../proxima/src/common/CommonTypes.hi
../proxima/src/layout/LayUtils.o : ../proxima/src/layout/LayUtils.hs
../proxima/src/layout/LayUtils.o : ../proxima/src/presentation/PresUtils.hi
../proxima/src/layout/LayTypes.o : ../proxima/src/layout/LayTypes.hs
../proxima/src/layout/LayTypes.o : ../proxima/src/evaluation/DocTypes.hi
../proxima/src/layout/LayTypes.o : ../proxima/src/common/CommonUtils.hi
../proxima/src/layout/LayTypes.o : ../proxima/src/presentation/PresTypes.hi
../proxima/src/layout/LayTypes.o : ../proxima/src/common/CommonTypes.hi
../proxima/src/layout/LayLayerTypes.o : ../proxima/src/layout/LayLayerTypes.hs
../proxima/src/layout/LayLayerTypes.o : ../proxima/src/layout/LayTypes.hi
../proxima/src/layout/LayLayerTypes.o : ../proxima/src/common/CommonTypes.hi
../proxima/src/layout/LayLayerUtils.o : ../proxima/src/layout/LayLayerUtils.hs
../proxima/src/layout/LayLayerUtils.o : ../proxima/src/layout/LayUtils.hi
../proxima/src/layout/LayLayerUtils.o : ../proxima/src/common/CommonUtils.hi
../proxima/src/layout/LayLayerUtils.o : ../proxima/src/layout/LayLayerTypes.hi
../proxima/src/layout/LayLayerUtils.o : ../proxima/src/common/CommonTypes.hi
../proxima/src/layout/Layout.o : ../proxima/src/layout/Layout.hs
../proxima/src/layout/Layout.o : ../proxima/src/layout/LayLayerUtils.hi
../proxima/src/layout/Layout.o : ../proxima/src/layout/LayLayerTypes.hi
../proxima/src/layout/Layout.o : ../proxima/src/common/CommonTypes.hi
../proxima/src/presentation/PresentationParsing.o : ../proxima/src/presentation/PresentationParsing.hs
../proxima/src/presentation/PresentationParsing.o : ../proxima/src/common/UU_Parsing.hi
../proxima/src/presentation/PresentationParsing.o : ../proxima/src/common/UU_Parsing.hi
../proxima/src/presentation/PresentationParsing.o : ../proxima/src/presentation/XprezLib.hi
../proxima/src/presentation/PresentationParsing.o : ../proxima/src/evaluation/DocumentEdit.hi
../proxima/src/presentation/PresentationParsing.o : ../proxima/src/presentation/PresTypes.hi
../proxima/src/presentation/PresentationParsing.o : ../proxima/src/common/CommonTypes.hi
../proxima/src/presentation/PresLayerTypes.o : ../proxima/src/presentation/PresLayerTypes.hs
../proxima/src/presentation/PresLayerTypes.o : ../proxima/src/presentation/PresTypes.hi
../proxima/src/presentation/PresLayerTypes.o : ../proxima/src/evaluation/EnrTypes.hi
../proxima/src/presentation/PresLayerTypes.o : ../proxima/src/common/CommonTypes.hi
../proxima/src/presentation/PresLayerUtils.o : ../proxima/src/presentation/PresLayerUtils.hs
../proxima/src/presentation/PresLayerUtils.o : ../proxima/src/presentation/PresUtils.hi
../proxima/src/presentation/PresLayerUtils.o : ../proxima/src/evaluation/EnrUtils.hi
../proxima/src/presentation/PresLayerUtils.o : ../proxima/src/common/CommonUtils.hi
../proxima/src/presentation/PresLayerUtils.o : ../proxima/src/presentation/PresLayerTypes.hi
../proxima/src/presentation/PresLayerUtils.o : ../proxima/src/common/CommonTypes.hi
../proxima/src/presentation/PresPresent.o : ../proxima/src/presentation/PresPresent.hs
../proxima/src/presentation/PresPresent.o : ../proxima/src/presentation/PresLayerUtils.hi
../proxima/src/presentation/PresPresent.o : ../proxima/src/presentation/PresLayerTypes.hi
../proxima/src/presentation/PresPresent.o : ../proxima/src/common/CommonTypes.hi
../proxima/src/presentation/PresTranslate.o : ../proxima/src/presentation/PresTranslate.hs
../proxima/src/presentation/PresTranslate.o : ../proxima/src/presentation/PresLayerUtils.hi
../proxima/src/presentation/PresTranslate.o : ../proxima/src/presentation/PresLayerTypes.hi
../proxima/src/presentation/PresTranslate.o : ../proxima/src/common/CommonTypes.hi
../proxima/src/presentation/XLatex.o : ../proxima/src/presentation/XLatex.hs
../proxima/src/presentation/XLatex.o : ../proxima/src/common/CommonTypes.hi
../proxima/src/presentation/XLatex.o : ../proxima/src/presentation/PresTypes.hi
../proxima/src/presentation/XLatex.o : ../proxima/src/presentation/XprezLib.hi
../proxima/src/arrangement/ArrTypes.o : ../proxima/src/arrangement/ArrTypes.hs
../proxima/src/arrangement/ArrTypes.o : ../proxima/src/presentation/PresTypes.hi
../proxima/src/arrangement/ArrTypes.o : ../proxima/src/evaluation/DocTypes.hi
../proxima/src/arrangement/ArrTypes.o : ../proxima/src/common/CommonTypes.hi
../proxima/src/arrangement/ArrUtils.o : ../proxima/src/arrangement/ArrUtils.hs
../proxima/src/arrangement/ArrUtils.o : ../proxima/src/common/CommonUtils.hi
../proxima/src/arrangement/ArrUtils.o : ../proxima/src/arrangement/ArrTypes.hi
../proxima/src/arrangement/ArrUtils.o : ../proxima/src/common/CommonTypes.hi
../proxima/src/rendering/RenLayerTypes.o : ../proxima/src/rendering/RenLayerTypes.hs
../proxima/src/rendering/RenLayerTypes.o : ../proxima/src/rendering/RenTypes.hi
../proxima/src/rendering/RenLayerTypes.o : ../proxima/src/arrangement/ArrTypes.hi
../proxima/src/rendering/RenLayerTypes.o : ../proxima/src/common/CommonTypes.hi
../proxima/src/rendering/RenLayerUtils.o : ../proxima/src/rendering/RenLayerUtils.hs
../proxima/src/rendering/RenLayerUtils.o : ../proxima/src/rendering/RenUtils.hi
../proxima/src/rendering/RenLayerUtils.o : ../proxima/src/arrangement/ArrUtils.hi
../proxima/src/rendering/RenLayerUtils.o : ../proxima/src/common/CommonUtils.hi
../proxima/src/rendering/RenLayerUtils.o : ../proxima/src/rendering/RenLayerTypes.hi
../proxima/src/rendering/RenLayerUtils.o : ../proxima/src/common/CommonTypes.hi
../proxima/src/rendering/RenTranslate.o : ../proxima/src/rendering/RenTranslate.hs
../proxima/src/rendering/RenTranslate.o : ../proxima/src/rendering/RenLayerUtils.hi
../proxima/src/rendering/RenTranslate.o : ../proxima/src/rendering/RenLayerTypes.hi
../proxima/src/rendering/RenTranslate.o : ../proxima/src/common/CommonTypes.hi
../proxima/src/arrangement/ArrLayerTypes.o : ../proxima/src/arrangement/ArrLayerTypes.hs
../proxima/src/arrangement/ArrLayerTypes.o : ../proxima/src/arrangement/FontLib.hi
../proxima/src/arrangement/ArrLayerTypes.o : ../proxima/src/arrangement/ArrTypes.hi
../proxima/src/arrangement/ArrLayerTypes.o : ../proxima/src/layout/LayTypes.hi
../proxima/src/arrangement/ArrLayerTypes.o : ../proxima/src/common/CommonTypes.hi
../proxima/src/arrangement/ArrLayerUtils.o : ../proxima/src/arrangement/ArrLayerUtils.hs
../proxima/src/arrangement/ArrLayerUtils.o : ../proxima/src/arrangement/ArrUtils.hi
../proxima/src/arrangement/ArrLayerUtils.o : ../proxima/src/layout/LayUtils.hi
../proxima/src/arrangement/ArrLayerUtils.o : ../proxima/src/common/CommonUtils.hi
../proxima/src/arrangement/ArrLayerUtils.o : ../proxima/src/arrangement/ArrLayerTypes.hi
../proxima/src/arrangement/ArrLayerUtils.o : ../proxima/src/common/CommonTypes.hi
../proxima/src/arrangement/ArrangerAG.o : ../proxima/src/arrangement/ArrangerAG.hs
../proxima/src/arrangement/ArrangerAG.o : ../proxima/src/arrangement/FontLib.hi
../proxima/src/arrangement/ArrangerAG.o : ../proxima/src/arrangement/ArrLayerUtils.hi
../proxima/src/arrangement/ArrangerAG.o : ../proxima/src/arrangement/ArrLayerTypes.hi
../proxima/src/arrangement/ArrangerAG.o : ../proxima/src/common/CommonTypes.hi
../proxima/src/arrangement/Arranger.o : ../proxima/src/arrangement/Arranger.hs
../proxima/src/arrangement/Arranger.o : ../proxima/src/arrangement/FontLib.hi
../proxima/src/arrangement/Arranger.o : ../proxima/src/arrangement/ArrangerAG.hi
../proxima/src/arrangement/Arranger.o : ../proxima/src/arrangement/ArrLayerUtils.hi
../proxima/src/arrangement/Arranger.o : ../proxima/src/arrangement/ArrLayerTypes.hi
../proxima/src/arrangement/Arranger.o : ../proxima/src/common/CommonTypes.hi
../proxima/src/arrangement/ArrPresent.o : ../proxima/src/arrangement/ArrPresent.hs
../proxima/src/arrangement/ArrPresent.o : ../proxima/src/arrangement/Arranger.hi
../proxima/src/arrangement/ArrPresent.o : ../proxima/src/arrangement/ArrLayerUtils.hi
../proxima/src/arrangement/ArrPresent.o : ../proxima/src/arrangement/ArrLayerTypes.hi
../proxima/src/arrangement/ArrPresent.o : ../proxima/src/common/CommonTypes.hi
../proxima/src/layout/TreeEditPres.o : ../proxima/src/layout/TreeEditPres.hs
../proxima/src/layout/TreeEditPres.o : ../proxima/src/presentation/XprezLib.hi
../proxima/src/layout/TreeEditPres.o : ../proxima/src/presentation/PresUtils.hi
../proxima/src/layout/TreeEditPres.o : ../proxima/src/presentation/PresTypes.hi
../proxima/src/layout/TreeEditPres.o : ../proxima/src/evaluation/DocTypes.hi
../proxima/src/layout/TreeEditPres.o : ../proxima/src/common/CommonTypes.hi
../proxima/src/arrangement/ArrTranslate.o : ../proxima/src/arrangement/ArrTranslate.hs
../proxima/src/arrangement/ArrTranslate.o : ../proxima/src/layout/TreeEditPres.hi
../proxima/src/arrangement/ArrTranslate.o : ../proxima/src/evaluation/DocUtils.hi
../proxima/src/arrangement/ArrTranslate.o : ../proxima/src/evaluation/DocTypes.hi
../proxima/src/arrangement/ArrTranslate.o : ../proxima/src/arrangement/ArrLayerUtils.hi
../proxima/src/arrangement/ArrTranslate.o : ../proxima/src/arrangement/ArrLayerTypes.hi
../proxima/src/arrangement/ArrTranslate.o : ../proxima/src/common/CommonTypes.hi
../proxima/src/layout/LayTranslate.o : ../proxima/src/layout/LayTranslate.hs
../proxima/src/layout/LayTranslate.o : ../proxima/src/layout/TreeEditPres.hi
../proxima/src/layout/LayTranslate.o : ../proxima/src/layout/LayLayerUtils.hi
../proxima/src/layout/LayTranslate.o : ../proxima/src/layout/LayLayerTypes.hi
../proxima/src/layout/LayTranslate.o : ../proxima/src/common/CommonTypes.hi
../proxima/src/layout/LayPresent.o : ../proxima/src/layout/LayPresent.hs
../proxima/src/layout/LayPresent.o : ../proxima/src/layout/TreeEditPres.hi
../proxima/src/layout/LayPresent.o : ../proxima/src/layout/Layout.hi
../proxima/src/layout/LayPresent.o : ../proxima/src/layout/LayLayerUtils.hi
../proxima/src/layout/LayPresent.o : ../proxima/src/layout/LayLayerTypes.hi
../proxima/src/layout/LayPresent.o : ../proxima/src/common/CommonTypes.hi
src/CompileHelium.o : src/CompileHelium.hs
src/CompileHelium.o : ../proxima/../helium/src/main/CompileUtils.hi
src/CompileHelium.o : ../proxima/../helium/src/main/Args.hi
src/CompileHelium.o : ../Top/src/Top/../Top/Types.hi
src/CompileHelium.o : ../proxima/../helium/src/syntax/UHA_Syntax.hi
src/CompileHelium.o : ../proxima/../helium/src/staticanalysis/messages/StaticErrors.hi
src/CompileHelium.o : ../proxima/../helium/src/staticanalysis/messages/TypeErrors.hi
src/CompileHelium.o : ../proxima/../helium/src/staticanalysis/inferencers/TypeInferencing.hi
src/CompileHelium.o : ../proxima/../helium/src/main/PhaseTypingStrategies.hi
src/CompileHelium.o : ../proxima/../helium/src/main/PhaseStaticChecks.hi
src/CompileHelium.o : ../proxima/../helium/src/main/PhaseImport.hi
src/DocTypes_Generated.o : src/DocTypes_Generated.hs
src/DocTypes_Generated.o : ../proxima/src/presentation/PresTypes.hi
src/DocTypes_Generated.o : ../proxima/src/evaluation/DocTypes.hi
src/DocTypes_Generated.o : ../proxima/src/common/CommonTypes.hi
src/DocumentEdit_Generated.o : src/DocumentEdit_Generated.hs
src/DocumentEdit_Generated.o : ../proxima/src/presentation/PresTypes.hi
src/DocumentEdit_Generated.o : ../proxima/src/evaluation/DocUtils.hi
src/DocumentEdit_Generated.o : ../proxima/src/evaluation/DocumentEdit.hi
src/DocumentEdit_Generated.o : src/DocTypes_Generated.hi
src/DocumentEdit_Generated.o : ../proxima/src/evaluation/DocTypes.hi
src/DocumentEdit_Generated.o : ../proxima/src/common/CommonTypes.hi
src/EvaluateTypesStubs.o : src/EvaluateTypesStubs.hs
src/EvaluateTypesStubs.o : ../proxima/src/common/DebugLevels.hi
src/EvaluateTypesStubs.o : src/DocTypes_Generated.hi
src/EvaluateTypesStubs.o : ../proxima/src/evaluation/DocTypes.hi
src/Scanner.o : src/Scanner.hs
src/Scanner.o : src/DocTypes_Generated.hi
src/Scanner.o : ../proxima/src/layout/LayLayerUtils.hi
src/Scanner.o : ../proxima/src/layout/LayLayerTypes.hi
src/Scanner.o : ../proxima/src/common/CommonTypes.hi
src/ProxParser_Generated.o : src/ProxParser_Generated.hs
src/ProxParser_Generated.o : src/DocTypes_Generated.hi
src/ProxParser_Generated.o : ../proxima/src/evaluation/DocTypes.hi
src/ProxParser_Generated.o : src/DocumentEdit_Generated.hi
src/ProxParser_Generated.o : ../proxima/src/evaluation/DocumentEdit.hi
src/ProxParser_Generated.o : ../proxima/src/presentation/PresLayerUtils.hi
src/ProxParser_Generated.o : ../proxima/src/presentation/PresLayerTypes.hi
src/ProxParser_Generated.o : ../proxima/src/common/CommonTypes.hi
src/EvaluateTypes.o : src/EvaluateTypes.hs
src/EvaluateTypes.o : ../proxima/../helium/src/syntax/UHA_Pretty.hi
src/EvaluateTypes.o : ../proxima/../helium/src/staticanalysis/messages/HeliumMessages.hi
src/EvaluateTypes.o : ../proxima/../helium/src/syntax/UHA_Range.hi
src/EvaluateTypes.o : ../proxima/../helium/src/syntax/UHA_Utils.hi
src/EvaluateTypes.o : ../Top/src/Top/../Top/Types.hi
src/EvaluateTypes.o : ../proxima/../helium/src/staticanalysis/messages/TypeErrors.hi
src/EvaluateTypes.o : ../proxima/../helium/src/staticanalysis/messages/StaticErrors.hi
src/EvaluateTypes.o : ../proxima/../helium/src/utils/Utils.hi
src/EvaluateTypes.o : src/CompileHelium.hi
src/EvaluateTypes.o : ../proxima/../helium/src/syntax/UHA_Syntax.hi
src/EvaluateTypes.o : src/DocumentEdit_Generated.hi
src/EvaluateTypes.o : ../proxima/src/evaluation/DocumentEdit.hi
src/EvaluateTypes.o : ../proxima/src/common/DebugLevels.hi
src/EvaluateTypes.o : src/DocTypes_Generated.hi
src/EvaluateTypes.o : ../proxima/src/evaluation/DocTypes.hi
src/Evaluator.o : src/Evaluator.hs
src/Evaluator.o : src/EvaluateTypes.hi
src/Evaluator.o : src/DocTypes_Generated.hi
src/Evaluator.o : src/DocumentEdit_Generated.hi
src/Evaluator.o : ../proxima/src/evaluation/DocumentEdit.hi
src/Evaluator.o : ../proxima/src/evaluation/EvalLayerUtils.hi
src/Evaluator.o : ../proxima/src/evaluation/EvalLayerTypes.hi
src/Evaluator.o : ../proxima/src/common/CommonTypes.hi
src/DocUtils_Generated.o : src/DocUtils_Generated.hs
src/DocUtils_Generated.o : ../proxima/src/presentation/PresTypes.hi
src/DocUtils_Generated.o : src/DocTypes_Generated.hi
src/DocUtils_Generated.o : ../proxima/src/evaluation/DocTypes.hi
src/Reducer.o : src/Reducer.hs
src/Reducer.o : src/DocUtils_Generated.hi
src/Reducer.o : src/DocTypes_Generated.hi
src/Reducer.o : ../proxima/src/presentation/PresTypes.hi
src/Reducer.o : ../proxima/src/evaluation/EvalLayerUtils.hi
src/Reducer.o : ../proxima/src/evaluation/EvalLayerTypes.hi
src/Reducer.o : ../proxima/src/common/CommonTypes.hi
src/Chess.o : src/Chess.hs
src/Chess.o : ../proxima/src/evaluation/DocumentEdit.hi
src/Chess.o : ../proxima/src/presentation/PresTypes.hi
src/Chess.o : ../proxima/src/presentation/XprezLib.hi
src/Chess.o : src/DocUtils_Generated.hi
src/Chess.o : ../proxima/src/evaluation/DocUtils.hi
src/Chess.o : ../proxima/src/evaluation/DocTypes.hi
src/Chess.o : src/DocTypes_Generated.hi
src/Chess.o : ../proxima/src/common/CommonTypes.hi
src/ProxParser.o : src/ProxParser.hs
src/ProxParser.o : src/DocUtils_Generated.hi
src/ProxParser.o : src/DocTypes_Generated.hi
src/ProxParser.o : ../proxima/src/common/UU_Parsing.hi
src/ProxParser.o : src/DocumentEdit_Generated.hi
src/ProxParser.o : ../proxima/src/evaluation/DocumentEdit.hi
src/ProxParser.o : src/ProxParser_Generated.hi
src/ProxParser.o : ../proxima/src/common/UU_Parsing.hi
src/ProxParser.o : ../proxima/src/presentation/XprezLib.hi
src/ProxParser.o : ../proxima/src/presentation/PresentationParsing.hi
src/ProxParser.o : ../proxima/src/presentation/PresLayerUtils.hi
src/ProxParser.o : ../proxima/src/presentation/PresLayerTypes.hi
src/ProxParser.o : ../proxima/src/common/CommonTypes.hi
src/PresentationAG.o : src/PresentationAG.hs
src/PresentationAG.o : src/DocumentEdit_Generated.hi
src/PresentationAG.o : src/DocUtils_Generated.hi
src/PresentationAG.o : src/DocTypes_Generated.hi
src/PresentationAG.o : ../proxima/src/evaluation/DocTypes.hi
src/PresentationAG.o : src/Chess.hi
src/PresentationAG.o : ../proxima/src/evaluation/DocumentEdit.hi
src/PresentationAG.o : ../proxima/src/presentation/XLatex.hi
src/PresentationAG.o : ../proxima/src/presentation/XprezLib.hi
src/PresentationAG.o : ../proxima/src/presentation/PresLayerUtils.hi
src/PresentationAG.o : ../proxima/src/presentation/PresLayerTypes.hi
src/PresentationAG.o : ../proxima/src/common/CommonTypes.hi
../proxima/src/rendering/Renderer.o : ../proxima/src/rendering/Renderer.hs
../proxima/src/rendering/Renderer.o : src/DocumentEdit_Generated.hi
../proxima/src/rendering/Renderer.o : ../proxima/src/evaluation/DocumentEdit.hi
../proxima/src/rendering/Renderer.o : src/DocUtils_Generated.hi
../proxima/src/rendering/Renderer.o : src/DocTypes_Generated.hi
../proxima/src/rendering/Renderer.o : ../proxima/src/main/GUI.hi
../proxima/src/rendering/Renderer.o : ../proxima/src/evaluation/DocumentEdit.hi
../proxima/src/rendering/Renderer.o : ../proxima/src/evaluation/DocUtils.hi
../proxima/src/rendering/Renderer.o : ../proxima/src/evaluation/DocTypes.hi
../proxima/src/rendering/Renderer.o : ../proxima/src/presentation/PresTypes.hi
../proxima/src/rendering/Renderer.o : ../proxima/src/arrangement/ArrLayerUtils.hi
../proxima/src/rendering/Renderer.o : ../proxima/src/rendering/RenLayerUtils.hi
../proxima/src/rendering/Renderer.o : ../proxima/src/rendering/RenLayerTypes.hi
../proxima/src/rendering/Renderer.o : ../proxima/src/common/CommonTypes.hi
../proxima/src/rendering/Renderer.o : ../proxima/src/common/CommonTypes.hi
../proxima/src/rendering/RenPresent.o : ../proxima/src/rendering/RenPresent.hs
../proxima/src/rendering/RenPresent.o : ../proxima/src/evaluation/DocTypes.hi
../proxima/src/rendering/RenPresent.o : ../proxima/src/rendering/Renderer.hi
../proxima/src/rendering/RenPresent.o : ../proxima/src/rendering/RenLayerUtils.hi
../proxima/src/rendering/RenPresent.o : ../proxima/src/rendering/RenLayerTypes.hi
../proxima/src/rendering/RenPresent.o : ../proxima/src/common/CommonTypes.hi
../proxima/src/main/Architecture.o : ../proxima/src/main/Architecture.hs
../proxima/src/main/Architecture.o : ../proxima/src/rendering/RenTranslate.hi
../proxima/src/main/Architecture.o : ../proxima/src/rendering/RenPresent.hi
../proxima/src/main/Architecture.o : ../proxima/src/arrangement/ArrTranslate.hi
../proxima/src/main/Architecture.o : ../proxima/src/arrangement/ArrPresent.hi
../proxima/src/main/Architecture.o : ../proxima/src/layout/LayTranslate.hi
../proxima/src/main/Architecture.o : ../proxima/src/layout/LayPresent.hi
../proxima/src/main/Architecture.o : ../proxima/src/presentation/PresTranslate.hi
../proxima/src/main/Architecture.o : ../proxima/src/presentation/PresPresent.hi
../proxima/src/main/Architecture.o : ../proxima/src/evaluation/EvalTranslate.hi
../proxima/src/main/Architecture.o : ../proxima/src/evaluation/EvalPresent.hi
../proxima/src/main/Architecture.o : ../proxima/src/main/ArchitectureLibM.hi
../proxima/src/main/Proxima.o : ../proxima/src/main/Proxima.hs
../proxima/src/main/Proxima.o : ../proxima/src/evaluation/EvalLayerTypes.hi
../proxima/src/main/Proxima.o : ../proxima/src/arrangement/ArrUtils.hi
../proxima/src/main/Proxima.o : ../proxima/src/layout/Layout.hi
../proxima/src/main/Proxima.o : ../proxima/src/presentation/PresentationParsing.hi
../proxima/src/main/Proxima.o : ../proxima/src/layout/LayLayerTypes.hi
../proxima/src/main/Proxima.o : ../proxima/src/presentation/PresLayerTypes.hi
../proxima/src/main/Proxima.o : ../proxima/src/evaluation/EvalLayerTypes.hi
../proxima/src/main/Proxima.o : ../proxima/src/arrangement/FontLib.hi
../proxima/src/main/Proxima.o : ../proxima/src/rendering/RenTypes.hi
../proxima/src/main/Proxima.o : ../proxima/src/arrangement/ArrTypes.hi
../proxima/src/main/Proxima.o : ../proxima/src/layout/LayTypes.hi
../proxima/src/main/Proxima.o : ../proxima/src/presentation/PresUtils.hi
../proxima/src/main/Proxima.o : ../proxima/src/presentation/PresTypes.hi
../proxima/src/main/Proxima.o : ../proxima/src/evaluation/EnrTypes.hi
../proxima/src/main/Proxima.o : ../proxima/src/presentation/PresPresent.hi
../proxima/src/main/Proxima.o : ../proxima/src/evaluation/DocUtils.hi
../proxima/src/main/Proxima.o : ../proxima/src/evaluation/DocTypes.hi
../proxima/src/main/Proxima.o : ../proxima/src/common/CommonTypes.hi
../proxima/src/main/Proxima.o : ../proxima/src/main/GUI.hi
../proxima/src/main/Proxima.o : ../proxima/src/main/Architecture.hi
src/Main.o : src/Main.hs
src/Main.o : ../proxima/src/common/CommonTypes.hi
src/Main.o : ../proxima/src/arrangement/ArrTypes.hi
src/Main.o : ../proxima/src/layout/LayTypes.hi
src/Main.o : ../proxima/src/presentation/PresTypes.hi
src/Main.o : ../proxima/src/evaluation/EnrTypes.hi
src/Main.o : src/DocTypes_Generated.hi
src/Main.o : ../proxima/src/evaluation/DocTypes.hi
src/Main.o : src/Scanner.hi
src/Main.o : src/ProxParser.hi
src/Main.o : src/PresentationAG.hi
src/Main.o : src/Reducer.hi
src/Main.o : src/Evaluator.hi
src/Main.o : ../proxima/src/main/Proxima.hi
# DO NOT DELETE: End of Haskell dependencies

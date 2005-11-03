# doesn't know when proxima.exe is up to date
# maybe has to do with proxima and proxima.exe targets

# no make when documenttype is modified

SHELL = /bin/sh

# clear and set suffixes
.SUFFIXES:
.SUFFIXES: .o .hs .hi .lhs .hc	.s .ag



EXE            = 
MAIN           = proxima$(EXE)

default: all
all: $(MAIN)
.PHONY : default proxima generate


PROXIMADIR = ../proxima
PROXIMASRCDIR = $(PROXIMADIR)/src
PROXIMABINDIR = $(PROXIMADIR)/bin

AGC       = $(PROXIMABINDIR)/uuagc$(EXE)
GENERATOR = $(PROXIMABINDIR)/generate


# replace "$(PROXIMADIR)" in proximaObjs by "../proxima"
PROXIMAOBJS = `cat $(PROXIMABINDIR)/proximaObjs.txt | sed -e s/$$\(PROXIMADIR\)/\.\.\\\/proxima/g`



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

	
HELIUMSRCDIR = ../heliumNT/src

# replace "$(HELIUMSRCDIR)" in heliumObjs by "../heliumNT/src"
HELIUMOBJS = `cat $(HELIUMSRCDIR)/heliumObjs.txt | sed -e s/$$\(HELIUMSRCDIR\)/\.\.\\\/heliumNT\\\/src/g`

#directories
PARSECDIR     = ../parsec
PARSECIMPDIR  = $(PARSECDIR)/Text/ParserCombinators
LVMLIBDIR     = ../lvm/src/lib
LVMRUNTIMEDIR = ../lvm/src/runtime
TOPDIR        = ../Top/src/Top

HELIUMIFACES	= $(LVMLIBDIR)/common:$(LVMLIBDIR)/common/ghc:$(LVMLIBDIR)/lvm:$(LVMLIBDIR)/asm:$(LVMLIBDIR)/core:$(HELIUMSRCDIR)/utils:$(PARSECDIR):$(HELIUMSRCDIR)/parser:$(HELIUMSRCDIR)/staticanalysis/miscellaneous:$(HELIUMSRCDIR)/staticanalysis/inferencers:$(HELIUMSRCDIR)/staticanalysis/directives:$(HELIUMSRCDIR)/staticanalysis/staticchecks:$(HELIUMSRCDIR)/staticanalysis/heuristics:$(HELIUMSRCDIR)/staticanalysis/messages:$(HELIUMSRCDIR)/syntax:$(HELIUMSRCDIR)/codegeneration:$(HELIUMSRCDIR)/main:$(HELIUMSRCDIR)/modulesystem:$(TOPDIR):$(TOPDIR)/..

IFACES = src:$(PROXIMAIFACES):$(HELIUMIFACES)

	


HC	= ghc
HC_OPTS = -static -fglasgow-exts -fallow-undecidable-instances \
		-package lang -package wx -package util -package net -package data -package parsec\
		-fno-monomorphism-restriction -i$(IFACES) $(EXTRA_OPTS)
#		-ignore-package network-1.0



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
	cd $(PROXIMASRCDIR); make generator

src/DocTypes_Generated.hs : src/DocumentType.hs
	cd $(PROXIMASRCDIR); make generator
	$(GENERATOR) src src/DocumentType.hs


# The presentation AG

# target for only compiling presentationAG, useful when working with GHCi
presenter: src/PresentationAG.hs
	
# TODO: make dependencies easy to see and to maintain (important for editor builder)

src/PresentationAG.hs : $(AGC) \
	src/PresentationAG.ag \
	src/PresentationAG_Generated.ag \
	src/LambdaReduce.ag \
	
	@echo "Compiling presentationAG.ag"
	$(AGC) -o src/PresentationAG.hs \
	-cfmspw --self --module=PresentationAG \
	src/PresentationAG.ag -P src

# --module is because otherwise uuagc uses ../../presentation/PresentationAG as module name

# The main target

# Inter-module dependencies
depend :
	echo $(EDITORSRCS)
	# Checking dependencies
	$(HC) -M $(HC_OPTS) $(EDITORSRCS)


$(MAIN): $(EDITOROBJS) 
	@echo "****************************************************"
	@echo "  Proxima: the Proxima generic editor"
	@echo "****************************************************"
	cd $(PROXIMASRCDIR); make proxima
	$(HC) -o $(MAIN) $(HC_OPTS) $(OBJS)

	

	
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
../parsec/Text/ParserCombinators/ParsecSpecial/Pos.o : ../parsec/Text/ParserCombinators/ParsecSpecial/Pos.hs
../parsec/Text/ParserCombinators/ParsecSpecial/Error.o : ../parsec/Text/ParserCombinators/ParsecSpecial/Error.hs
../parsec/Text/ParserCombinators/ParsecSpecial/Error.o : ../parsec/Text/ParserCombinators/ParsecSpecial/Pos.hi
../parsec/Text/ParserCombinators/ParsecSpecial/Prim.o : ../parsec/Text/ParserCombinators/ParsecSpecial/Prim.hs
../parsec/Text/ParserCombinators/ParsecSpecial/Prim.o : ../parsec/Text/ParserCombinators/ParsecSpecial/Error.hi
../parsec/Text/ParserCombinators/ParsecSpecial/Prim.o : ../parsec/Text/ParserCombinators/ParsecSpecial/Pos.hi
../parsec/Text/ParserCombinators/ParsecSpecial/Char.o : ../parsec/Text/ParserCombinators/ParsecSpecial/Char.hs
../parsec/Text/ParserCombinators/ParsecSpecial/Char.o : ../parsec/Text/ParserCombinators/ParsecSpecial/Prim.hi
../parsec/Text/ParserCombinators/ParsecSpecial/Char.o : ../parsec/Text/ParserCombinators/ParsecSpecial/Pos.hi
../parsec/Text/ParserCombinators/ParsecSpecial/Combinator.o : ../parsec/Text/ParserCombinators/ParsecSpecial/Combinator.hs
../parsec/Text/ParserCombinators/ParsecSpecial/Combinator.o : ../parsec/Text/ParserCombinators/ParsecSpecial/Prim.hi
../parsec/Text/ParserCombinators/ParsecSpecial.o : ../parsec/Text/ParserCombinators/ParsecSpecial.hs
../parsec/Text/ParserCombinators/ParsecSpecial.o : ../parsec/Text/ParserCombinators/ParsecSpecial/Char.hi
../parsec/Text/ParserCombinators/ParsecSpecial.o : ../parsec/Text/ParserCombinators/ParsecSpecial/Combinator.hi
../parsec/Text/ParserCombinators/ParsecSpecial.o : ../parsec/Text/ParserCombinators/ParsecSpecial/Prim.hi
../parsec/Text/ParserCombinators/ParsecSpecial.o : ../parsec/Text/ParserCombinators/ParsecSpecial/Error.hi
../parsec/Text/ParserCombinators/ParsecSpecial.o : ../parsec/Text/ParserCombinators/ParsecSpecial/Pos.hi
../heliumNT/src/utils/Texts.o : ../heliumNT/src/utils/Texts.hs
../heliumNT/src/parser/LexerToken.o : ../heliumNT/src/parser/LexerToken.hs
../heliumNT/src/parser/LexerToken.o : ../heliumNT/src/utils/Texts.hi
../heliumNT/src/parser/LexerToken.o : ../parsec/Text/ParserCombinators/ParsecSpecial/Pos.hi
../heliumNT/src/parser/LayoutRule.o : ../heliumNT/src/parser/LayoutRule.hs
../heliumNT/src/parser/LayoutRule.o : ../parsec/Text/ParserCombinators/ParsecSpecial/Pos.hi
../heliumNT/src/parser/LayoutRule.o : ../heliumNT/src/parser/LexerToken.hi
../heliumNT/src/main/Version.o : ../heliumNT/src/main/Version.hs
../Top/src/Top/../Top/ComposedSolvers/TreeWalk.o : ../Top/src/Top/../Top/ComposedSolvers/TreeWalk.hs
../Top/src/Top/../Top/ComposedSolvers/Tree.o : ../Top/src/Top/../Top/ComposedSolvers/Tree.hs
../Top/src/Top/../Top/ComposedSolvers/Tree.o : ../Top/src/Top/../Top/ComposedSolvers/TreeWalk.hi
../Top/src/Top/../Top/States/States.o : ../Top/src/Top/../Top/States/States.hs
../heliumNT/src/utils/OneLiner.o : ../heliumNT/src/utils/OneLiner.hs
../lvm/src/lib/common/ghc/Special.o : ../lvm/src/lib/common/ghc/Special.hs
../lvm/src/lib/common/TopSort.o : ../lvm/src/lib/common/TopSort.hs
../lvm/src/lib/common/TopSort.o : ../lvm/src/lib/common/ghc/Special.hi
../lvm/src/lib/common/Standard.o : ../lvm/src/lib/common/Standard.hs
../lvm/src/lib/common/Standard.o : ../lvm/src/lib/common/ghc/Special.hi
../lvm/src/lib/common/Byte.o : ../lvm/src/lib/common/Byte.hs
../lvm/src/lib/common/Byte.o : ../lvm/src/lib/common/Standard.hi
../lvm/src/lib/common/Byte.o : ../lvm/src/lib/common/ghc/Special.hi
../lvm/src/lib/common/IntMap.o : ../lvm/src/lib/common/IntMap.hs
../lvm/src/lib/common/Id.o : ../lvm/src/lib/common/Id.hs
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
../heliumNT/src/codegeneration/CoreUtils.o : ../heliumNT/src/codegeneration/CoreUtils.hs
../heliumNT/src/codegeneration/CoreUtils.o : ../lvm/src/lib/common/Byte.hi
../heliumNT/src/codegeneration/CoreUtils.o : ../lvm/src/lib/common/Id.hi
../heliumNT/src/codegeneration/CoreUtils.o : ../lvm/src/lib/core/Core.hi
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
../Top/src/Top/../Graph.o : ../Top/src/Top/../Graph.hs
../Top/src/Top/../Top/Types/Basics.o : ../Top/src/Top/../Top/Types/Basics.hs
../heliumNT/src/syntax/UHA_Syntax.o : ../heliumNT/src/syntax/UHA_Syntax.hs
../heliumNT/src/staticanalysis/directives/TS_Syntax.o : ../heliumNT/src/staticanalysis/directives/TS_Syntax.hs
../heliumNT/src/staticanalysis/directives/TS_Syntax.o : ../heliumNT/src/syntax/UHA_Syntax.hi
../heliumNT/src/utils/Logger.o : ../heliumNT/src/utils/Logger.hs
../heliumNT/src/utils/Utils.o : ../heliumNT/src/utils/Utils.hs
../heliumNT/src/utils/Utils.o : ../heliumNT/src/utils/Logger.hi
../Top/src/Top/../Top/Types/Substitution.o : ../Top/src/Top/../Top/Types/Substitution.hs
../Top/src/Top/../Top/Types/Substitution.o : ../heliumNT/src/utils/Utils.hi
../Top/src/Top/../Top/Types/Substitution.o : ../Top/src/Top/../Top/Types/Basics.hi
../Top/src/Top/../Top/Types/Qualification.o : ../Top/src/Top/../Top/Types/Qualification.hs
../Top/src/Top/../Top/Types/Qualification.o : ../Top/src/Top/../Top/Types/Substitution.hi
../Top/src/Top/../Top/Types/Qualification.o : ../Top/src/Top/../Top/Types/Basics.hi
../Top/src/Top/../Top/Types/Synonyms.o : ../Top/src/Top/../Top/Types/Synonyms.hs
../Top/src/Top/../Top/Types/Synonyms.o : ../heliumNT/src/utils/Utils.hi
../Top/src/Top/../Top/Types/Synonyms.o : ../Top/src/Top/../Graph.hi
../Top/src/Top/../Top/Types/Synonyms.o : ../Top/src/Top/../Top/Types/Basics.hi
../Top/src/Top/../Top/Types/Unification.o : ../Top/src/Top/../Top/Types/Unification.hs
../Top/src/Top/../Top/Types/Unification.o : ../heliumNT/src/utils/Utils.hi
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
../Top/src/Top/../Top/Types/Quantification.o : ../heliumNT/src/utils/Utils.hi
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
../heliumNT/src/staticanalysis/miscellaneous/TypesToAlignedDocs.o : ../heliumNT/src/staticanalysis/miscellaneous/TypesToAlignedDocs.hs
../heliumNT/src/staticanalysis/miscellaneous/TypesToAlignedDocs.o : ../lvm/src/lib/common/PPrint.hi
../heliumNT/src/staticanalysis/miscellaneous/TypesToAlignedDocs.o : ../Top/src/Top/../Top/Types.hi
../heliumNT/src/staticanalysis/directives/TS_CoreSyntax.o : ../heliumNT/src/staticanalysis/directives/TS_CoreSyntax.hs
../heliumNT/src/staticanalysis/directives/TS_CoreSyntax.o : ../Top/src/Top/../Top/Types.hi
../Top/src/Top/../Top/Qualifiers/QualifierMap.o : ../Top/src/Top/../Top/Qualifiers/QualifierMap.hs
../Top/src/Top/../Top/Qualifiers/QualifierMap.o : ../Top/src/Top/../Top/Types.hi
../Top/src/Top/../Top/Qualifiers/QualifierMap.o : ../Top/src/Top/../Top/States/States.hi
../Top/src/Top/../Top/Constraints/Constraints.o : ../Top/src/Top/../Top/Constraints/Constraints.hs
../Top/src/Top/../Top/Constraints/Constraints.o : ../Top/src/Top/../Top/Types.hi
../Top/src/Top/../Top/Constraints/TypeConstraintInfo.o : ../Top/src/Top/../Top/Constraints/TypeConstraintInfo.hs
../Top/src/Top/../Top/Constraints/TypeConstraintInfo.o : ../Top/src/Top/../Top/Types.hi
../heliumNT/src/staticanalysis/miscellaneous/TypeDebugInfo.o : ../heliumNT/src/staticanalysis/miscellaneous/TypeDebugInfo.hs
../heliumNT/src/staticanalysis/miscellaneous/TypeDebugInfo.o : ../Top/src/Top/../Top/Types.hi
../heliumNT/src/syntax/UHA_Range.o : ../heliumNT/src/syntax/UHA_Range.hs
../heliumNT/src/syntax/UHA_Range.o : ../heliumNT/src/utils/Utils.hi
../heliumNT/src/syntax/UHA_Range.o : ../lvm/src/lib/common/Id.hi
../heliumNT/src/syntax/UHA_Range.o : ../heliumNT/src/syntax/UHA_Syntax.hi
../heliumNT/src/utils/Similarity.o : ../heliumNT/src/utils/Similarity.hs
../heliumNT/src/utils/Similarity.o : ../heliumNT/src/utils/Utils.hi
../heliumNT/src/syntax/UHA_Utils.o : ../heliumNT/src/syntax/UHA_Utils.hs
../heliumNT/src/syntax/UHA_Utils.o : ../heliumNT/src/utils/Utils.hi
../heliumNT/src/syntax/UHA_Utils.o : ../Top/src/Top/../Top/Types.hi
../heliumNT/src/syntax/UHA_Utils.o : ../lvm/src/lib/common/Id.hi
../heliumNT/src/syntax/UHA_Utils.o : ../heliumNT/src/syntax/UHA_Syntax.hi
../heliumNT/src/syntax/UHA_Utils.o : ../heliumNT/src/syntax/UHA_Range.hi
../heliumNT/src/staticanalysis/messages/Messages.o : ../heliumNT/src/staticanalysis/messages/Messages.hs
../heliumNT/src/staticanalysis/messages/Messages.o : ../heliumNT/src/utils/Utils.hi
../heliumNT/src/staticanalysis/messages/Messages.o : ../heliumNT/src/utils/Similarity.hi
../heliumNT/src/staticanalysis/messages/Messages.o : ../heliumNT/src/utils/OneLiner.hi
../heliumNT/src/staticanalysis/messages/Messages.o : ../Top/src/Top/../Top/Types.hi
../heliumNT/src/staticanalysis/messages/Messages.o : ../heliumNT/src/syntax/UHA_Range.hi
../heliumNT/src/staticanalysis/messages/Messages.o : ../heliumNT/src/syntax/UHA_Utils.hi
../heliumNT/src/staticanalysis/messages/Messages.o : ../heliumNT/src/syntax/UHA_Syntax.hi
../heliumNT/src/staticanalysis/messages/HeliumMessages.o : ../heliumNT/src/staticanalysis/messages/HeliumMessages.hs
../heliumNT/src/staticanalysis/messages/HeliumMessages.o : ../heliumNT/src/syntax/UHA_Range.hi
../heliumNT/src/staticanalysis/messages/HeliumMessages.o : ../heliumNT/src/staticanalysis/miscellaneous/TypesToAlignedDocs.hi
../heliumNT/src/staticanalysis/messages/HeliumMessages.o : ../heliumNT/src/utils/OneLiner.hi
../heliumNT/src/staticanalysis/messages/HeliumMessages.o : ../lvm/src/lib/common/PPrint.hi
../heliumNT/src/staticanalysis/messages/HeliumMessages.o : ../Top/src/Top/../Top/Types.hi
../heliumNT/src/staticanalysis/messages/HeliumMessages.o : ../heliumNT/src/staticanalysis/messages/Messages.hi
../heliumNT/src/parser/LexerMessage.o : ../heliumNT/src/parser/LexerMessage.hs
../heliumNT/src/parser/LexerMessage.o : ../heliumNT/src/utils/Texts.hi
../heliumNT/src/parser/LexerMessage.o : ../heliumNT/src/staticanalysis/messages/Messages.hi
../heliumNT/src/parser/LexerMessage.o : ../heliumNT/src/syntax/UHA_Syntax.hi
../heliumNT/src/parser/LexerMessage.o : ../parsec/Text/ParserCombinators/ParsecSpecial/Pos.hi
../heliumNT/src/parser/LexerMonad.o : ../heliumNT/src/parser/LexerMonad.hs
../heliumNT/src/parser/LexerMonad.o : ../parsec/Text/ParserCombinators/ParsecSpecial/Pos.hi
../heliumNT/src/parser/LexerMonad.o : ../heliumNT/src/parser/LexerMessage.hi
../heliumNT/src/staticanalysis/directives/Matchers.o : ../heliumNT/src/staticanalysis/directives/Matchers.hs
../heliumNT/src/staticanalysis/directives/Matchers.o : ../heliumNT/src/staticanalysis/messages/Messages.hi
../heliumNT/src/staticanalysis/directives/Matchers.o : ../heliumNT/src/syntax/UHA_Syntax.hi
../heliumNT/src/parser/ParseMessage.o : ../heliumNT/src/parser/ParseMessage.hs
../heliumNT/src/parser/ParseMessage.o : ../parsec/Text/ParserCombinators/ParsecSpecial/Pos.hi
../heliumNT/src/parser/ParseMessage.o : ../parsec/Text/ParserCombinators/ParsecSpecial/Error.hi
../heliumNT/src/parser/ParseMessage.o : ../heliumNT/src/utils/Texts.hi
../heliumNT/src/parser/ParseMessage.o : ../heliumNT/src/syntax/UHA_Syntax.hi
../heliumNT/src/parser/ParseMessage.o : ../heliumNT/src/staticanalysis/messages/Messages.hi
../heliumNT/src/parser/OperatorTable.o : ../heliumNT/src/parser/OperatorTable.hs
../heliumNT/src/parser/OperatorTable.o : ../heliumNT/src/syntax/UHA_Utils.hi
../heliumNT/src/parser/OperatorTable.o : ../heliumNT/src/syntax/UHA_Syntax.hi
../heliumNT/src/staticanalysis/miscellaneous/TypeConversion.o : ../heliumNT/src/staticanalysis/miscellaneous/TypeConversion.hs
../heliumNT/src/staticanalysis/miscellaneous/TypeConversion.o : ../Top/src/Top/../Top/Types.hi
../heliumNT/src/staticanalysis/miscellaneous/TypeConversion.o : ../heliumNT/src/syntax/UHA_Syntax.hi
../heliumNT/src/staticanalysis/miscellaneous/TypeConversion.o : ../heliumNT/src/utils/Utils.hi
../heliumNT/src/staticanalysis/miscellaneous/TypeConversion.o : ../heliumNT/src/syntax/UHA_Range.hi
../heliumNT/src/staticanalysis/miscellaneous/TypeConversion.o : ../heliumNT/src/syntax/UHA_Utils.hi
../heliumNT/src/syntax/UHA_OneLine.o : ../heliumNT/src/syntax/UHA_OneLine.hs
../heliumNT/src/syntax/UHA_OneLine.o : ../heliumNT/src/utils/Utils.hi
../heliumNT/src/syntax/UHA_OneLine.o : ../heliumNT/src/syntax/UHA_Syntax.hi
../heliumNT/src/syntax/UHA_OneLine.o : ../heliumNT/src/staticanalysis/miscellaneous/TypeConversion.hi
../heliumNT/src/syntax/UHA_OneLine.o : ../Top/src/Top/../Top/Types.hi
../heliumNT/src/syntax/UHA_OneLine.o : ../heliumNT/src/syntax/UHA_Utils.hi
../heliumNT/src/syntax/UHA_OneLine.o : ../heliumNT/src/utils/OneLiner.hi
../heliumNT/src/staticanalysis/miscellaneous/UHA_Source.o : ../heliumNT/src/staticanalysis/miscellaneous/UHA_Source.hs
../heliumNT/src/staticanalysis/miscellaneous/UHA_Source.o : ../heliumNT/src/syntax/UHA_OneLine.hi
../heliumNT/src/staticanalysis/miscellaneous/UHA_Source.o : ../heliumNT/src/syntax/UHA_Utils.hi
../heliumNT/src/staticanalysis/miscellaneous/UHA_Source.o : ../heliumNT/src/syntax/UHA_Syntax.hi
../heliumNT/src/staticanalysis/miscellaneous/UHA_Source.o : ../heliumNT/src/syntax/UHA_Range.hi
../heliumNT/src/staticanalysis/miscellaneous/UHA_Source.o : ../heliumNT/src/utils/OneLiner.hi
../heliumNT/src/staticanalysis/messages/TypeErrors.o : ../heliumNT/src/staticanalysis/messages/TypeErrors.hs
../heliumNT/src/staticanalysis/messages/TypeErrors.o : ../heliumNT/src/staticanalysis/miscellaneous/UHA_Source.hi
../heliumNT/src/staticanalysis/messages/TypeErrors.o : ../heliumNT/src/syntax/UHA_Range.hi
../heliumNT/src/staticanalysis/messages/TypeErrors.o : ../heliumNT/src/syntax/UHA_Syntax.hi
../heliumNT/src/staticanalysis/messages/TypeErrors.o : ../heliumNT/src/utils/OneLiner.hi
../heliumNT/src/staticanalysis/messages/TypeErrors.o : ../Top/src/Top/../Top/Types.hi
../heliumNT/src/staticanalysis/messages/TypeErrors.o : ../heliumNT/src/staticanalysis/messages/Messages.hi
../heliumNT/src/staticanalysis/messages/StaticErrors.o : ../heliumNT/src/staticanalysis/messages/StaticErrors.hs
../heliumNT/src/staticanalysis/messages/StaticErrors.o : ../Top/src/Top/../Top/Types.hi
../heliumNT/src/staticanalysis/messages/StaticErrors.o : ../heliumNT/src/utils/Utils.hi
../heliumNT/src/staticanalysis/messages/StaticErrors.o : ../heliumNT/src/staticanalysis/messages/Messages.hi
../heliumNT/src/staticanalysis/messages/StaticErrors.o : ../heliumNT/src/syntax/UHA_Range.hi
../heliumNT/src/staticanalysis/messages/StaticErrors.o : ../heliumNT/src/syntax/UHA_Utils.hi
../heliumNT/src/staticanalysis/messages/StaticErrors.o : ../heliumNT/src/syntax/UHA_Syntax.hi
../Top/src/Top/../Top/States/BasicState.o : ../Top/src/Top/../Top/States/BasicState.hs
../Top/src/Top/../Top/States/BasicState.o : ../heliumNT/src/utils/Utils.hi
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
../Top/src/Top/../Top/States/QualifierState.o : ../heliumNT/src/utils/Utils.hi
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
../heliumNT/src/staticanalysis/miscellaneous/TypeConstraints.o : ../heliumNT/src/staticanalysis/miscellaneous/TypeConstraints.hs
../heliumNT/src/staticanalysis/miscellaneous/TypeConstraints.o : ../Top/src/Top/../Top/Solvers/SolveConstraints.hi
../heliumNT/src/staticanalysis/miscellaneous/TypeConstraints.o : ../Top/src/Top/../Top/Types.hi
../heliumNT/src/staticanalysis/miscellaneous/TypeConstraints.o : ../Top/src/Top/../Top/Qualifiers/TypeClasses.hi
../heliumNT/src/staticanalysis/miscellaneous/TypeConstraints.o : ../Top/src/Top/../Top/States/QualifierState.hi
../heliumNT/src/staticanalysis/miscellaneous/TypeConstraints.o : ../Top/src/Top/../Top/States/SubstState.hi
../heliumNT/src/staticanalysis/miscellaneous/TypeConstraints.o : ../Top/src/Top/../Top/States/TIState.hi
../heliumNT/src/staticanalysis/miscellaneous/TypeConstraints.o : ../Top/src/Top/../Top/States/BasicState.hi
../heliumNT/src/staticanalysis/miscellaneous/TypeConstraints.o : ../Top/src/Top/../Top/Constraints/TypeConstraintInfo.hi
../heliumNT/src/staticanalysis/miscellaneous/TypeConstraints.o : ../Top/src/Top/../Top/Constraints/Polymorphism.hi
../heliumNT/src/staticanalysis/miscellaneous/TypeConstraints.o : ../Top/src/Top/../Top/Constraints/ExtraConstraints.hi
../heliumNT/src/staticanalysis/miscellaneous/TypeConstraints.o : ../Top/src/Top/../Top/Constraints/Equality.hi
../heliumNT/src/staticanalysis/miscellaneous/TypeConstraints.o : ../Top/src/Top/../Top/Constraints/Constraints.hi
../heliumNT/src/staticanalysis/miscellaneous/DoublyLinkedTree.o : ../heliumNT/src/staticanalysis/miscellaneous/DoublyLinkedTree.hs
../heliumNT/src/staticanalysis/miscellaneous/DoublyLinkedTree.o : ../heliumNT/src/utils/Utils.hi
../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.o : ../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.hs
../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.o : ../heliumNT/src/utils/Utils.hi
../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.o : ../Top/src/Top/../Top/States/TIState.hi
../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.o : ../Top/src/Top/../Top/States/SubstState.hi
../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.o : ../Top/src/Top/../Top/States/BasicState.hi
../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.o : ../Top/src/Top/../Top/Qualifiers/TypeClasses.hi
../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.o : ../Top/src/Top/../Top/Constraints/TypeConstraintInfo.hi
../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.o : ../heliumNT/src/staticanalysis/miscellaneous/TypeConstraints.hi
../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.o : ../heliumNT/src/staticanalysis/miscellaneous/DoublyLinkedTree.hi
../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.o : ../heliumNT/src/staticanalysis/messages/Messages.hi
../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.o : ../heliumNT/src/staticanalysis/messages/TypeErrors.hi
../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.o : ../heliumNT/src/syntax/UHA_Range.hi
../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.o : ../heliumNT/src/staticanalysis/miscellaneous/UHA_Source.hi
../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.o : ../heliumNT/src/syntax/UHA_Syntax.hi
../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.o : ../Top/src/Top/../Top/ComposedSolvers/Tree.hi
../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.o : ../Top/src/Top/../Top/Types.hi
../heliumNT/src/staticanalysis/inferencers/BindingGroupAnalysis.o : ../heliumNT/src/staticanalysis/inferencers/BindingGroupAnalysis.hs
../heliumNT/src/staticanalysis/inferencers/BindingGroupAnalysis.o : ../Top/src/Top/../Top/ComposedSolvers/Tree.hi
../heliumNT/src/staticanalysis/inferencers/BindingGroupAnalysis.o : ../Top/src/Top/../Top/Types.hi
../heliumNT/src/staticanalysis/inferencers/BindingGroupAnalysis.o : ../lvm/src/lib/common/TopSort.hi
../heliumNT/src/staticanalysis/inferencers/BindingGroupAnalysis.o : ../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.hi
../heliumNT/src/staticanalysis/inferencers/BindingGroupAnalysis.o : ../heliumNT/src/staticanalysis/miscellaneous/TypeConstraints.hi
../heliumNT/src/staticanalysis/inferencers/BindingGroupAnalysis.o : ../heliumNT/src/syntax/UHA_Syntax.hi
../Top/src/Top/../Top/TypeGraph/Paths.o : ../Top/src/Top/../Top/TypeGraph/Paths.hs
../Top/src/Top/../Top/TypeGraph/Paths.o : ../heliumNT/src/utils/Utils.hi
../Top/src/Top/../Top/TypeGraph/Basics.o : ../Top/src/Top/../Top/TypeGraph/Basics.hs
../Top/src/Top/../Top/TypeGraph/Basics.o : ../heliumNT/src/utils/Utils.hi
../Top/src/Top/../Top/TypeGraph/Basics.o : ../Top/src/Top/../Top/Types.hi
../Top/src/Top/../Top/TypeGraph/Basics.o : ../Top/src/Top/../Top/TypeGraph/Paths.hi
../Top/src/Top/../Top/TypeGraph/TypeGraphState.o : ../Top/src/Top/../Top/TypeGraph/TypeGraphState.hs
../Top/src/Top/../Top/TypeGraph/TypeGraphState.o : ../heliumNT/src/utils/Utils.hi
../Top/src/Top/../Top/TypeGraph/TypeGraphState.o : ../Top/src/Top/../Top/Types.hi
../Top/src/Top/../Top/TypeGraph/TypeGraphState.o : ../Top/src/Top/../Top/States/SubstState.hi
../Top/src/Top/../Top/TypeGraph/TypeGraphState.o : ../Top/src/Top/../Top/States/TIState.hi
../Top/src/Top/../Top/TypeGraph/TypeGraphState.o : ../Top/src/Top/../Top/States/BasicState.hi
../Top/src/Top/../Top/TypeGraph/TypeGraphState.o : ../Top/src/Top/../Top/TypeGraph/Basics.hi
../Top/src/Top/../Top/TypeGraph/Heuristics.o : ../Top/src/Top/../Top/TypeGraph/Heuristics.hs
../Top/src/Top/../Top/TypeGraph/Heuristics.o : ../heliumNT/src/utils/Utils.hi
../Top/src/Top/../Top/TypeGraph/Heuristics.o : ../Top/src/Top/../Top/Types.hi
../Top/src/Top/../Top/TypeGraph/Heuristics.o : ../Top/src/Top/../Top/TypeGraph/Basics.hi
../Top/src/Top/../Top/TypeGraph/Heuristics.o : ../Top/src/Top/../Top/TypeGraph/TypeGraphState.hi
../heliumNT/src/staticanalysis/heuristics/OnlyResultHeuristics.o : ../heliumNT/src/staticanalysis/heuristics/OnlyResultHeuristics.hs
../heliumNT/src/staticanalysis/heuristics/OnlyResultHeuristics.o : ../heliumNT/src/staticanalysis/miscellaneous/UHA_Source.hi
../heliumNT/src/staticanalysis/heuristics/OnlyResultHeuristics.o : ../heliumNT/src/syntax/UHA_Syntax.hi
../heliumNT/src/staticanalysis/heuristics/OnlyResultHeuristics.o : ../heliumNT/src/utils/OneLiner.hi
../heliumNT/src/staticanalysis/heuristics/OnlyResultHeuristics.o : ../Top/src/Top/../Top/Types.hi
../heliumNT/src/staticanalysis/heuristics/OnlyResultHeuristics.o : ../Top/src/Top/../Top/States/TIState.hi
../heliumNT/src/staticanalysis/heuristics/OnlyResultHeuristics.o : ../Top/src/Top/../Top/TypeGraph/Heuristics.hi
../heliumNT/src/staticanalysis/heuristics/RepairHeuristics.o : ../heliumNT/src/staticanalysis/heuristics/RepairHeuristics.hs
../heliumNT/src/staticanalysis/heuristics/RepairHeuristics.o : ../heliumNT/src/syntax/UHA_Syntax.hi
../heliumNT/src/staticanalysis/heuristics/RepairHeuristics.o : ../heliumNT/src/staticanalysis/miscellaneous/UHA_Source.hi
../heliumNT/src/staticanalysis/heuristics/RepairHeuristics.o : ../heliumNT/src/utils/OneLiner.hi
../heliumNT/src/staticanalysis/heuristics/RepairHeuristics.o : ../heliumNT/src/staticanalysis/messages/Messages.hi
../heliumNT/src/staticanalysis/heuristics/RepairHeuristics.o : ../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.hi
../heliumNT/src/staticanalysis/heuristics/RepairHeuristics.o : ../heliumNT/src/staticanalysis/messages/TypeErrors.hi
../heliumNT/src/staticanalysis/heuristics/RepairHeuristics.o : ../heliumNT/src/staticanalysis/heuristics/OnlyResultHeuristics.hi
../heliumNT/src/staticanalysis/heuristics/RepairHeuristics.o : ../Top/src/Top/../Top/Types.hi
../heliumNT/src/staticanalysis/heuristics/RepairHeuristics.o : ../Top/src/Top/../Top/TypeGraph/TypeGraphState.hi
../heliumNT/src/staticanalysis/heuristics/RepairHeuristics.o : ../Top/src/Top/../Top/States/SubstState.hi
../heliumNT/src/staticanalysis/heuristics/RepairHeuristics.o : ../Top/src/Top/../Top/States/TIState.hi
../heliumNT/src/staticanalysis/heuristics/RepairHeuristics.o : ../Top/src/Top/../Top/TypeGraph/Basics.hi
../heliumNT/src/staticanalysis/heuristics/RepairHeuristics.o : ../Top/src/Top/../Top/TypeGraph/Heuristics.hi
../heliumNT/src/modulesystem/ImportEnvironment.o : ../heliumNT/src/modulesystem/ImportEnvironment.hs
../heliumNT/src/modulesystem/ImportEnvironment.o : ../heliumNT/src/staticanalysis/directives/TS_CoreSyntax.hi
../heliumNT/src/modulesystem/ImportEnvironment.o : ../heliumNT/src/staticanalysis/heuristics/RepairHeuristics.hi
../heliumNT/src/modulesystem/ImportEnvironment.o : ../heliumNT/src/staticanalysis/messages/Messages.hi
../heliumNT/src/modulesystem/ImportEnvironment.o : ../heliumNT/src/parser/OperatorTable.hi
../heliumNT/src/modulesystem/ImportEnvironment.o : ../Top/src/Top/../Top/Types.hi
../heliumNT/src/modulesystem/ImportEnvironment.o : ../heliumNT/src/syntax/UHA_Utils.hi
../heliumNT/src/modulesystem/ImportEnvironment.o : ../heliumNT/src/syntax/UHA_Syntax.hi
../heliumNT/src/modulesystem/ImportEnvironment.o : ../heliumNT/src/utils/Utils.hi
../heliumNT/src/staticanalysis/heuristics/TieBreakerHeuristics.o : ../heliumNT/src/staticanalysis/heuristics/TieBreakerHeuristics.hs
../heliumNT/src/staticanalysis/heuristics/TieBreakerHeuristics.o : ../Top/src/Top/../Top/TypeGraph/Heuristics.hi
../heliumNT/src/staticanalysis/heuristics/TieBreakerHeuristics.o : ../Top/src/Top/../Top/States/TIState.hi
../heliumNT/src/staticanalysis/heuristics/TieBreakerHeuristics.o : ../Top/src/Top/../Top/Types.hi
../heliumNT/src/main/Args.o : ../heliumNT/src/main/Args.hs
../heliumNT/src/main/Args.o : ../heliumNT/src/utils/Utils.hi
../heliumNT/src/main/Args.o : ../heliumNT/src/main/Version.hi
../heliumNT/src/main/CompileUtils.o : ../heliumNT/src/main/CompileUtils.hs
../heliumNT/src/main/CompileUtils.o : ../lvm/src/lib/common/Standard.hi
../heliumNT/src/main/CompileUtils.o : ../heliumNT/src/syntax/UHA_Syntax.hi
../heliumNT/src/main/CompileUtils.o : ../heliumNT/src/modulesystem/ImportEnvironment.hi
../heliumNT/src/main/CompileUtils.o : ../heliumNT/src/utils/Logger.hi
../heliumNT/src/main/CompileUtils.o : ../heliumNT/src/utils/Utils.hi
../heliumNT/src/main/CompileUtils.o : ../heliumNT/src/staticanalysis/messages/HeliumMessages.hi
../heliumNT/src/main/CompileUtils.o : ../heliumNT/src/staticanalysis/messages/Messages.hi
../heliumNT/src/main/CompileUtils.o : ../heliumNT/src/main/Args.hi
../heliumNT/src/staticanalysis/directives/TS_Attributes.o : ../heliumNT/src/staticanalysis/directives/TS_Attributes.hs
../heliumNT/src/staticanalysis/directives/TS_Attributes.o : ../heliumNT/src/utils/Utils.hi
../heliumNT/src/staticanalysis/directives/TS_Attributes.o : ../heliumNT/src/staticanalysis/miscellaneous/UHA_Source.hi
../heliumNT/src/staticanalysis/directives/TS_Attributes.o : ../heliumNT/src/syntax/UHA_Syntax.hi
../heliumNT/src/staticanalysis/directives/TS_Attributes.o : ../heliumNT/src/utils/OneLiner.hi
../heliumNT/src/staticanalysis/directives/TS_Attributes.o : ../Top/src/Top/../Top/Types.hi
../heliumNT/src/staticanalysis/directives/TS_Attributes.o : ../heliumNT/src/staticanalysis/messages/Messages.hi
../heliumNT/src/staticanalysis/directives/TS_Attributes.o : ../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.hi
../heliumNT/src/staticanalysis/directives/TS_Attributes.o : ../heliumNT/src/staticanalysis/inferencers/BindingGroupAnalysis.hi
../heliumNT/src/parser/ResolveOperators.o : ../heliumNT/src/parser/ResolveOperators.hs
../heliumNT/src/parser/ResolveOperators.o : ../heliumNT/src/staticanalysis/messages/Messages.hi
../heliumNT/src/parser/ResolveOperators.o : ../heliumNT/src/utils/Utils.hi
../heliumNT/src/parser/ResolveOperators.o : ../heliumNT/src/parser/OperatorTable.hi
../heliumNT/src/parser/ResolveOperators.o : ../heliumNT/src/syntax/UHA_Range.hi
../heliumNT/src/parser/ResolveOperators.o : ../heliumNT/src/syntax/UHA_Syntax.hi
../heliumNT/src/parser/ResolveOperators.o : ../heliumNT/src/syntax/UHA_Utils.hi
../heliumNT/src/parser/Lexer.o : ../heliumNT/src/parser/Lexer.hs
../heliumNT/src/parser/Lexer.o : ../heliumNT/src/utils/Utils.hi
../heliumNT/src/parser/Lexer.o : ../parsec/Text/ParserCombinators/ParsecSpecial/Pos.hi
../heliumNT/src/parser/Lexer.o : ../heliumNT/src/parser/LexerToken.hi
../heliumNT/src/parser/Lexer.o : ../heliumNT/src/parser/LexerMessage.hi
../heliumNT/src/parser/Lexer.o : ../heliumNT/src/parser/LexerMonad.hi
../heliumNT/src/parser/ParseLibrary.o : ../heliumNT/src/parser/ParseLibrary.hs
../heliumNT/src/parser/ParseLibrary.o : ../heliumNT/src/utils/Texts.hi
../heliumNT/src/parser/ParseLibrary.o : ../heliumNT/src/syntax/UHA_Utils.hi
../heliumNT/src/parser/ParseLibrary.o : ../heliumNT/src/syntax/UHA_Syntax.hi
../heliumNT/src/parser/ParseLibrary.o : ../heliumNT/src/parser/Lexer.hi
../heliumNT/src/parser/ParseLibrary.o : ../parsec/Text/ParserCombinators/ParsecSpecial/Pos.hi
../heliumNT/src/parser/ParseLibrary.o : ../parsec/Text/ParserCombinators/ParsecSpecial.hi
../heliumNT/src/parser/CollectFunctionBindings.o : ../heliumNT/src/parser/CollectFunctionBindings.hs
../heliumNT/src/parser/CollectFunctionBindings.o : ../heliumNT/src/utils/Utils.hi
../heliumNT/src/parser/CollectFunctionBindings.o : ../heliumNT/src/syntax/UHA_Range.hi
../heliumNT/src/parser/CollectFunctionBindings.o : ../heliumNT/src/syntax/UHA_Utils.hi
../heliumNT/src/parser/CollectFunctionBindings.o : ../heliumNT/src/syntax/UHA_Syntax.hi
../heliumNT/src/parser/Parser.o : ../heliumNT/src/parser/Parser.hs
../heliumNT/src/parser/Parser.o : ../heliumNT/src/utils/Utils.hi
../heliumNT/src/parser/Parser.o : ../heliumNT/src/parser/CollectFunctionBindings.hi
../heliumNT/src/parser/Parser.o : ../heliumNT/src/syntax/UHA_Range.hi
../heliumNT/src/parser/Parser.o : ../heliumNT/src/syntax/UHA_Utils.hi
../heliumNT/src/parser/Parser.o : ../heliumNT/src/syntax/UHA_Syntax.hi
../heliumNT/src/parser/Parser.o : ../heliumNT/src/utils/Texts.hi
../heliumNT/src/parser/Parser.o : ../heliumNT/src/parser/LayoutRule.hi
../heliumNT/src/parser/Parser.o : ../heliumNT/src/parser/Lexer.hi
../heliumNT/src/parser/Parser.o : ../parsec/Text/ParserCombinators/ParsecSpecial/Pos.hi
../heliumNT/src/parser/Parser.o : ../parsec/Text/ParserCombinators/ParsecSpecial.hi
../heliumNT/src/parser/Parser.o : ../heliumNT/src/parser/ParseLibrary.hi
../heliumNT/src/staticanalysis/directives/TS_Apply.o : ../heliumNT/src/staticanalysis/directives/TS_Apply.hs
../heliumNT/src/staticanalysis/directives/TS_Apply.o : ../Top/src/Top/../Top/Types.hi
../heliumNT/src/staticanalysis/directives/TS_Apply.o : ../heliumNT/src/staticanalysis/miscellaneous/DoublyLinkedTree.hi
../heliumNT/src/staticanalysis/directives/TS_Apply.o : ../heliumNT/src/staticanalysis/miscellaneous/UHA_Source.hi
../heliumNT/src/staticanalysis/directives/TS_Apply.o : ../Top/src/Top/../Top/ComposedSolvers/Tree.hi
../heliumNT/src/staticanalysis/directives/TS_Apply.o : ../heliumNT/src/staticanalysis/directives/TS_CoreSyntax.hi
../heliumNT/src/staticanalysis/directives/TS_Apply.o : ../heliumNT/src/staticanalysis/directives/TS_Attributes.hi
../heliumNT/src/staticanalysis/directives/TS_Apply.o : ../heliumNT/src/parser/ResolveOperators.hi
../heliumNT/src/staticanalysis/directives/TS_Apply.o : ../heliumNT/src/parser/ParseLibrary.hi
../heliumNT/src/staticanalysis/directives/TS_Apply.o : ../heliumNT/src/parser/Lexer.hi
../heliumNT/src/staticanalysis/directives/TS_Apply.o : ../heliumNT/src/parser/Parser.hi
../heliumNT/src/staticanalysis/directives/TS_Apply.o : ../heliumNT/src/parser/OperatorTable.hi
../heliumNT/src/staticanalysis/directives/TS_Apply.o : ../heliumNT/src/staticanalysis/inferencers/BindingGroupAnalysis.hi
../heliumNT/src/staticanalysis/directives/TS_Apply.o : ../heliumNT/src/modulesystem/ImportEnvironment.hi
../heliumNT/src/staticanalysis/directives/TS_Apply.o : ../heliumNT/src/staticanalysis/messages/TypeErrors.hi
../heliumNT/src/staticanalysis/directives/TS_Apply.o : ../heliumNT/src/staticanalysis/messages/Messages.hi
../heliumNT/src/staticanalysis/directives/TS_Apply.o : ../heliumNT/src/utils/OneLiner.hi
../heliumNT/src/staticanalysis/directives/TS_Apply.o : ../heliumNT/src/utils/Utils.hi
../heliumNT/src/staticanalysis/directives/TS_Apply.o : ../heliumNT/src/syntax/UHA_Utils.hi
../heliumNT/src/staticanalysis/directives/TS_Apply.o : ../heliumNT/src/syntax/UHA_Range.hi
../heliumNT/src/staticanalysis/directives/TS_Apply.o : ../Top/src/Top/../Top/Types.hi
../heliumNT/src/staticanalysis/directives/TS_Apply.o : ../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.hi
../heliumNT/src/staticanalysis/directives/TS_Apply.o : ../heliumNT/src/staticanalysis/miscellaneous/TypeConstraints.hi
../heliumNT/src/staticanalysis/directives/TS_Apply.o : ../heliumNT/src/syntax/UHA_Syntax.hi
../heliumNT/src/modulesystem/DictionaryEnvironment.o : ../heliumNT/src/modulesystem/DictionaryEnvironment.hs
../heliumNT/src/modulesystem/DictionaryEnvironment.o : ../Top/src/Top/../Top/Types.hi
../heliumNT/src/modulesystem/DictionaryEnvironment.o : ../heliumNT/src/utils/Utils.hi
../heliumNT/src/modulesystem/DictionaryEnvironment.o : ../heliumNT/src/syntax/UHA_Utils.hi
../heliumNT/src/modulesystem/DictionaryEnvironment.o : ../heliumNT/src/syntax/UHA_Syntax.hi
../heliumNT/src/staticanalysis/miscellaneous/TypeDebugInfoWriter.o : ../heliumNT/src/staticanalysis/miscellaneous/TypeDebugInfoWriter.hs
../heliumNT/src/staticanalysis/miscellaneous/TypeDebugInfoWriter.o : ../heliumNT/src/utils/Utils.hi
../heliumNT/src/staticanalysis/miscellaneous/TypeDebugInfoWriter.o : ../heliumNT/src/utils/OneLiner.hi
../heliumNT/src/staticanalysis/miscellaneous/TypeDebugInfoWriter.o : ../heliumNT/src/staticanalysis/messages/HeliumMessages.hi
../heliumNT/src/staticanalysis/miscellaneous/TypeDebugInfoWriter.o : ../heliumNT/src/staticanalysis/messages/Messages.hi
../heliumNT/src/staticanalysis/miscellaneous/TypeDebugInfoWriter.o : ../heliumNT/src/staticanalysis/miscellaneous/UHA_Source.hi
../heliumNT/src/staticanalysis/miscellaneous/TypeDebugInfoWriter.o : ../heliumNT/src/syntax/UHA_Syntax.hi
../heliumNT/src/staticanalysis/miscellaneous/TypeDebugInfoWriter.o : ../heliumNT/src/staticanalysis/miscellaneous/TypeDebugInfo.hi
../heliumNT/src/staticanalysis/miscellaneous/TypeDebugInfoWriter.o : ../heliumNT/src/staticanalysis/miscellaneous/DoublyLinkedTree.hi
../heliumNT/src/staticanalysis/miscellaneous/TypeDebugInfoWriter.o : ../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.hi
../heliumNT/src/staticanalysis/miscellaneous/TypeDebugInfoWriter.o : ../heliumNT/src/staticanalysis/messages/TypeErrors.hi
../heliumNT/src/staticanalysis/miscellaneous/TypeDebugInfoWriter.o : ../heliumNT/src/staticanalysis/miscellaneous/TypeConstraints.hi
../heliumNT/src/staticanalysis/miscellaneous/TypeDebugInfoWriter.o : ../Top/src/Top/../Top/Constraints/Polymorphism.hi
../heliumNT/src/staticanalysis/miscellaneous/TypeDebugInfoWriter.o : ../Top/src/Top/../Top/ComposedSolvers/TreeWalk.hi
../heliumNT/src/staticanalysis/miscellaneous/TypeDebugInfoWriter.o : ../Top/src/Top/../Top/ComposedSolvers/Tree.hi
../heliumNT/src/staticanalysis/miscellaneous/TypeDebugInfoWriter.o : ../Top/src/Top/../Top/Types.hi
../heliumNT/src/codegeneration/DerivingShow.o : ../heliumNT/src/codegeneration/DerivingShow.hs
../heliumNT/src/codegeneration/DerivingShow.o : ../Top/src/Top/../Top/Types.hi
../heliumNT/src/codegeneration/DerivingShow.o : ../heliumNT/src/utils/Utils.hi
../heliumNT/src/codegeneration/DerivingShow.o : ../lvm/src/lib/common/Id.hi
../heliumNT/src/codegeneration/DerivingShow.o : ../lvm/src/lib/core/Core.hi
../heliumNT/src/codegeneration/DerivingShow.o : ../heliumNT/src/codegeneration/CoreUtils.hi
../heliumNT/src/codegeneration/DerivingShow.o : ../heliumNT/src/syntax/UHA_Utils.hi
../heliumNT/src/codegeneration/DerivingShow.o : ../heliumNT/src/syntax/UHA_Syntax.hi
../heliumNT/src/staticanalysis/heuristics/UnifierHeuristics.o : ../heliumNT/src/staticanalysis/heuristics/UnifierHeuristics.hs
../heliumNT/src/staticanalysis/heuristics/UnifierHeuristics.o : ../heliumNT/src/utils/Utils.hi
../heliumNT/src/staticanalysis/heuristics/UnifierHeuristics.o : ../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.hi
../heliumNT/src/staticanalysis/heuristics/UnifierHeuristics.o : ../heliumNT/src/staticanalysis/heuristics/RepairHeuristics.hi
../heliumNT/src/staticanalysis/heuristics/UnifierHeuristics.o : ../Top/src/Top/../Top/TypeGraph/Heuristics.hi
../heliumNT/src/staticanalysis/heuristics/UnifierHeuristics.o : ../Top/src/Top/../Top/TypeGraph/TypeGraphState.hi
../heliumNT/src/staticanalysis/heuristics/UnifierHeuristics.o : ../Top/src/Top/../Top/TypeGraph/Basics.hi
../heliumNT/src/staticanalysis/heuristics/UnifierHeuristics.o : ../Top/src/Top/../Top/States/TIState.hi
../heliumNT/src/staticanalysis/heuristics/UnifierHeuristics.o : ../Top/src/Top/../Top/Types.hi
../heliumNT/src/staticanalysis/heuristics/HeuristicsInfo.o : ../heliumNT/src/staticanalysis/heuristics/HeuristicsInfo.hs
../heliumNT/src/staticanalysis/heuristics/HeuristicsInfo.o : ../Top/src/Top/../Top/TypeGraph/Heuristics.hi
../heliumNT/src/staticanalysis/heuristics/HeuristicsInfo.o : ../Top/src/Top/../Top/Types.hi
../heliumNT/src/staticanalysis/heuristics/HeuristicsInfo.o : ../heliumNT/src/utils/Utils.hi
../heliumNT/src/staticanalysis/heuristics/HeuristicsInfo.o : ../heliumNT/src/staticanalysis/messages/TypeErrors.hi
../heliumNT/src/staticanalysis/heuristics/HeuristicsInfo.o : ../heliumNT/src/staticanalysis/messages/HeliumMessages.hi
../heliumNT/src/staticanalysis/heuristics/HeuristicsInfo.o : ../heliumNT/src/staticanalysis/messages/Messages.hi
../heliumNT/src/staticanalysis/heuristics/HeuristicsInfo.o : ../heliumNT/src/syntax/UHA_Syntax.hi
../heliumNT/src/staticanalysis/heuristics/HeuristicsInfo.o : ../heliumNT/src/staticanalysis/miscellaneous/UHA_Source.hi
../heliumNT/src/staticanalysis/heuristics/HeuristicsInfo.o : ../heliumNT/src/utils/OneLiner.hi
../heliumNT/src/staticanalysis/heuristics/HeuristicsInfo.o : ../heliumNT/src/staticanalysis/miscellaneous/DoublyLinkedTree.hi
../heliumNT/src/staticanalysis/heuristics/HeuristicsInfo.o : ../heliumNT/src/staticanalysis/heuristics/UnifierHeuristics.hi
../heliumNT/src/staticanalysis/heuristics/HeuristicsInfo.o : ../heliumNT/src/staticanalysis/heuristics/OnlyResultHeuristics.hi
../heliumNT/src/staticanalysis/heuristics/HeuristicsInfo.o : ../heliumNT/src/staticanalysis/heuristics/TieBreakerHeuristics.hi
../heliumNT/src/staticanalysis/heuristics/HeuristicsInfo.o : ../heliumNT/src/staticanalysis/heuristics/RepairHeuristics.hi
../heliumNT/src/staticanalysis/heuristics/HeuristicsInfo.o : ../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.hi
../Top/src/Top/../Top/TypeGraph/ApplyHeuristics.o : ../Top/src/Top/../Top/TypeGraph/ApplyHeuristics.hs
../Top/src/Top/../Top/TypeGraph/ApplyHeuristics.o : ../Top/src/Top/../Top/States/TIState.hi
../Top/src/Top/../Top/TypeGraph/ApplyHeuristics.o : ../Top/src/Top/../Top/States/BasicState.hi
../Top/src/Top/../Top/TypeGraph/ApplyHeuristics.o : ../Top/src/Top/../Graph.hi
../Top/src/Top/../Top/TypeGraph/ApplyHeuristics.o : ../Top/src/Top/../Top/Types.hi
../Top/src/Top/../Top/TypeGraph/ApplyHeuristics.o : ../heliumNT/src/utils/Utils.hi
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
../heliumNT/src/staticanalysis/heuristics/ListOfHeuristics.o : ../heliumNT/src/staticanalysis/heuristics/ListOfHeuristics.hs
../heliumNT/src/staticanalysis/heuristics/ListOfHeuristics.o : ../Top/src/Top/../Top/TypeGraph/Basics.hi
../heliumNT/src/staticanalysis/heuristics/ListOfHeuristics.o : ../Top/src/Top/../Top/TypeGraph/Paths.hi
../heliumNT/src/staticanalysis/heuristics/ListOfHeuristics.o : ../Top/src/Top/../Top/TypeGraph/TypeGraphState.hi
../heliumNT/src/staticanalysis/heuristics/ListOfHeuristics.o : ../heliumNT/src/staticanalysis/heuristics/TieBreakerHeuristics.hi
../heliumNT/src/staticanalysis/heuristics/ListOfHeuristics.o : ../heliumNT/src/staticanalysis/heuristics/OnlyResultHeuristics.hi
../heliumNT/src/staticanalysis/heuristics/ListOfHeuristics.o : ../heliumNT/src/staticanalysis/heuristics/UnifierHeuristics.hi
../heliumNT/src/staticanalysis/heuristics/ListOfHeuristics.o : ../heliumNT/src/staticanalysis/heuristics/RepairHeuristics.hi
../heliumNT/src/staticanalysis/heuristics/ListOfHeuristics.o : ../Top/src/Top/../Top/TypeGraph/DefaultHeuristics.hi
../heliumNT/src/staticanalysis/heuristics/ListOfHeuristics.o : ../Top/src/Top/../Top/TypeGraph/Heuristics.hi
../heliumNT/src/staticanalysis/heuristics/ListOfHeuristics.o : ../Top/src/Top/../Top/Types.hi
../heliumNT/src/staticanalysis/heuristics/ListOfHeuristics.o : ../heliumNT/src/staticanalysis/heuristics/HeuristicsInfo.hi
../heliumNT/src/staticanalysis/heuristics/ListOfHeuristics.o : ../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.hi
../heliumNT/src/staticanalysis/heuristics/ListOfHeuristics.o : ../heliumNT/src/main/Args.hi
../Top/src/Top/../Top/TypeGraph/EquivalenceGroup.o : ../Top/src/Top/../Top/TypeGraph/EquivalenceGroup.hs
../Top/src/Top/../Top/TypeGraph/EquivalenceGroup.o : ../heliumNT/src/utils/Utils.hi
../Top/src/Top/../Top/TypeGraph/EquivalenceGroup.o : ../Top/src/Top/../Top/Types.hi
../Top/src/Top/../Top/TypeGraph/EquivalenceGroup.o : ../Top/src/Top/../Top/TypeGraph/Basics.hi
../Top/src/Top/../Top/TypeGraph/EquivalenceGroup.o : ../Top/src/Top/../Top/TypeGraph/Paths.hi
../Top/src/Top/../Top/TypeGraph/TypeGraphMonad.o : ../Top/src/Top/../Top/TypeGraph/TypeGraphMonad.hs
../Top/src/Top/../Top/TypeGraph/TypeGraphMonad.o : ../heliumNT/src/utils/Utils.hi
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
../Top/src/Top/../Top/TypeGraph/Implementation.o : ../heliumNT/src/utils/Utils.hi
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
../Top/src/Top/../Top/Solvers/GreedySubst.o : ../heliumNT/src/utils/Utils.hi
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
../heliumNT/src/staticanalysis/directives/TS_Messages.o : ../heliumNT/src/staticanalysis/directives/TS_Messages.hs
../heliumNT/src/staticanalysis/directives/TS_Messages.o : ../heliumNT/src/syntax/UHA_Syntax.hi
../heliumNT/src/staticanalysis/directives/TS_Messages.o : ../heliumNT/src/utils/Utils.hi
../heliumNT/src/staticanalysis/directives/TS_Messages.o : ../Top/src/Top/../Top/Types.hi
../heliumNT/src/staticanalysis/directives/TS_Messages.o : ../heliumNT/src/staticanalysis/messages/HeliumMessages.hi
../heliumNT/src/staticanalysis/directives/TS_Messages.o : ../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.hi
../heliumNT/src/staticanalysis/directives/TS_Messages.o : ../heliumNT/src/staticanalysis/messages/TypeErrors.hi
../heliumNT/src/staticanalysis/directives/TS_Messages.o : ../heliumNT/src/staticanalysis/messages/Messages.hi
../heliumNT/src/modulesystem/ExtractImportDecls.o : ../heliumNT/src/modulesystem/ExtractImportDecls.hs
../heliumNT/src/modulesystem/ExtractImportDecls.o : ../heliumNT/src/utils/Utils.hi
../heliumNT/src/modulesystem/ExtractImportDecls.o : ../lvm/src/lib/core/Core.hi
../heliumNT/src/modulesystem/ExtractImportDecls.o : ../lvm/src/lib/common/Id.hi
../heliumNT/src/modulesystem/ExtractImportDecls.o : ../heliumNT/src/syntax/UHA_Utils.hi
../heliumNT/src/modulesystem/ExtractImportDecls.o : ../heliumNT/src/syntax/UHA_Syntax.hi
../heliumNT/src/modulesystem/CoreToImportEnv.o : ../heliumNT/src/modulesystem/CoreToImportEnv.hs
../heliumNT/src/modulesystem/CoreToImportEnv.o : ../heliumNT/src/syntax/UHA_Range.hi
../heliumNT/src/modulesystem/CoreToImportEnv.o : ../lvm/src/lib/common/Byte.hi
../heliumNT/src/modulesystem/CoreToImportEnv.o : ../Top/src/Top/../Top/Types.hi
../heliumNT/src/modulesystem/CoreToImportEnv.o : ../heliumNT/src/parser/OperatorTable.hi
../heliumNT/src/modulesystem/CoreToImportEnv.o : ../heliumNT/src/syntax/UHA_Syntax.hi
../heliumNT/src/modulesystem/CoreToImportEnv.o : ../lvm/src/lib/common/Id.hi
../heliumNT/src/modulesystem/CoreToImportEnv.o : ../heliumNT/src/syntax/UHA_Utils.hi
../heliumNT/src/modulesystem/CoreToImportEnv.o : ../heliumNT/src/modulesystem/ImportEnvironment.hi
../heliumNT/src/modulesystem/CoreToImportEnv.o : ../heliumNT/src/parser/Parser.hi
../heliumNT/src/modulesystem/CoreToImportEnv.o : ../heliumNT/src/parser/Lexer.hi
../heliumNT/src/modulesystem/CoreToImportEnv.o : ../heliumNT/src/parser/ParseLibrary.hi
../heliumNT/src/modulesystem/CoreToImportEnv.o : ../heliumNT/src/staticanalysis/miscellaneous/TypeConversion.hi
../heliumNT/src/modulesystem/CoreToImportEnv.o : ../heliumNT/src/utils/Utils.hi
../heliumNT/src/modulesystem/CoreToImportEnv.o : ../lvm/src/lib/core/Core.hi
../heliumNT/src/main/PhaseImport.o : ../heliumNT/src/main/PhaseImport.hs
../heliumNT/src/main/PhaseImport.o : ../lvm/src/lib/core/CorePretty.hi
../heliumNT/src/main/PhaseImport.o : ../heliumNT/src/modulesystem/ExtractImportDecls.hi
../heliumNT/src/main/PhaseImport.o : ../heliumNT/src/modulesystem/CoreToImportEnv.hi
../heliumNT/src/main/PhaseImport.o : ../lvm/src/lib/common/Id.hi
../heliumNT/src/main/PhaseImport.o : ../lvm/src/lib/lvm/LvmImport.hi
../heliumNT/src/main/PhaseImport.o : ../lvm/src/lib/common/Standard.hi
../heliumNT/src/main/PhaseImport.o : ../heliumNT/src/syntax/UHA_Range.hi
../heliumNT/src/main/PhaseImport.o : ../heliumNT/src/syntax/UHA_Utils.hi
../heliumNT/src/main/PhaseImport.o : ../heliumNT/src/syntax/UHA_Syntax.hi
../heliumNT/src/main/PhaseImport.o : ../lvm/src/lib/common/Id.hi
../heliumNT/src/main/PhaseImport.o : ../lvm/src/lib/core/Core.hi
../heliumNT/src/main/PhaseImport.o : ../heliumNT/src/main/CompileUtils.hi
../heliumNT/src/syntax/UHA_Pretty.o : ../heliumNT/src/syntax/UHA_Pretty.hs
../heliumNT/src/syntax/UHA_Pretty.o : ../heliumNT/src/utils/Utils.hi
../heliumNT/src/syntax/UHA_Pretty.o : ../heliumNT/src/syntax/UHA_Syntax.hi
../heliumNT/src/syntax/UHA_Pretty.o : ../Top/src/Top/../Top/Types.hi
../heliumNT/src/syntax/UHA_Pretty.o : ../lvm/src/lib/common/PPrint.hi
../heliumNT/src/staticanalysis/messages/Warnings.o : ../heliumNT/src/staticanalysis/messages/Warnings.hs
../heliumNT/src/staticanalysis/messages/Warnings.o : ../heliumNT/src/syntax/UHA_Pretty.hi
../heliumNT/src/staticanalysis/messages/Warnings.o : ../heliumNT/src/utils/Utils.hi
../heliumNT/src/staticanalysis/messages/Warnings.o : ../heliumNT/src/staticanalysis/messages/Messages.hi
../heliumNT/src/staticanalysis/messages/Warnings.o : ../Top/src/Top/../Top/Types.hi
../heliumNT/src/staticanalysis/messages/Warnings.o : ../heliumNT/src/syntax/UHA_Utils.hi
../heliumNT/src/staticanalysis/messages/Warnings.o : ../heliumNT/src/syntax/UHA_Syntax.hi
../heliumNT/src/staticanalysis/messages/Warnings.o : ../heliumNT/src/syntax/UHA_Range.hi
../heliumNT/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../heliumNT/src/staticanalysis/inferencers/SelectConstraintSolver.hs
../heliumNT/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../heliumNT/src/staticanalysis/messages/Warnings.hi
../heliumNT/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../Top/src/Top/../Top/Constraints/Polymorphism.hi
../heliumNT/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../heliumNT/src/syntax/UHA_Utils.hi
../heliumNT/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../heliumNT/src/syntax/UHA_Syntax.hi
../heliumNT/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../Top/src/Top/../Top/ComposedSolvers/ChunkySolver.hi
../heliumNT/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../Top/src/Top/../Top/ComposedSolvers/CombinationSolver.hi
../heliumNT/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../Top/src/Top/../Top/ComposedSolvers/TreeWalk.hi
../heliumNT/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../Top/src/Top/../Top/ComposedSolvers/Tree.hi
../heliumNT/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../Top/src/Top/../Top/TypeGraph/TypeGraphMonad.hi
../heliumNT/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../Top/src/Top/../Top/TypeGraph/TypeGraphSolver.hi
../heliumNT/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../Top/src/Top/../Top/Solvers/GreedySolver.hi
../heliumNT/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../Top/src/Top/../Top/Solvers/SimpleSolver.hi
../heliumNT/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../Top/src/Top/../Top/States/States.hi
../heliumNT/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../Top/src/Top/../Top/Solvers/SolveConstraints.hi
../heliumNT/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../Top/src/Top/../Top/States/SubstState.hi
../heliumNT/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../Top/src/Top/../Top/States/BasicState.hi
../heliumNT/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../Top/src/Top/../Top/Types.hi
../heliumNT/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../heliumNT/src/modulesystem/ImportEnvironment.hi
../heliumNT/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../heliumNT/src/staticanalysis/heuristics/ListOfHeuristics.hi
../heliumNT/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../heliumNT/src/staticanalysis/miscellaneous/TypeConstraints.hi
../heliumNT/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.hi
../heliumNT/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../heliumNT/src/main/Args.hi
../heliumNT/src/staticanalysis/inferencers/TypeInferencing.o : ../heliumNT/src/staticanalysis/inferencers/TypeInferencing.hs
../heliumNT/src/staticanalysis/inferencers/TypeInferencing.o : ../heliumNT/src/syntax/UHA_Utils.hi
../heliumNT/src/staticanalysis/inferencers/TypeInferencing.o : ../heliumNT/src/staticanalysis/directives/TS_Attributes.hi
../heliumNT/src/staticanalysis/inferencers/TypeInferencing.o : ../heliumNT/src/staticanalysis/directives/TS_CoreSyntax.hi
../heliumNT/src/staticanalysis/inferencers/TypeInferencing.o : ../heliumNT/src/staticanalysis/directives/TS_Apply.hi
../heliumNT/src/staticanalysis/inferencers/TypeInferencing.o : ../heliumNT/src/staticanalysis/directives/Matchers.hi
../heliumNT/src/staticanalysis/inferencers/TypeInferencing.o : ../heliumNT/src/main/Args.hi
../heliumNT/src/staticanalysis/inferencers/TypeInferencing.o : ../heliumNT/src/modulesystem/DictionaryEnvironment.hi
../heliumNT/src/staticanalysis/inferencers/TypeInferencing.o : ../heliumNT/src/modulesystem/ImportEnvironment.hi
../heliumNT/src/staticanalysis/inferencers/TypeInferencing.o : ../heliumNT/src/staticanalysis/miscellaneous/TypeDebugInfoWriter.hi
../heliumNT/src/staticanalysis/inferencers/TypeInferencing.o : ../heliumNT/src/codegeneration/DerivingShow.hi
../heliumNT/src/staticanalysis/inferencers/TypeInferencing.o : ../heliumNT/src/utils/Utils.hi
../heliumNT/src/staticanalysis/inferencers/TypeInferencing.o : ../heliumNT/src/syntax/UHA_Utils.hi
../heliumNT/src/staticanalysis/inferencers/TypeInferencing.o : ../heliumNT/src/syntax/UHA_Range.hi
../heliumNT/src/staticanalysis/inferencers/TypeInferencing.o : ../heliumNT/src/syntax/UHA_Syntax.hi
../heliumNT/src/staticanalysis/inferencers/TypeInferencing.o : ../heliumNT/src/staticanalysis/inferencers/BindingGroupAnalysis.hi
../heliumNT/src/staticanalysis/inferencers/TypeInferencing.o : ../heliumNT/src/staticanalysis/heuristics/HeuristicsInfo.hi
../heliumNT/src/staticanalysis/inferencers/TypeInferencing.o : ../Top/src/Top/../Top/Solvers/SolveConstraints.hi
../heliumNT/src/staticanalysis/inferencers/TypeInferencing.o : ../heliumNT/src/staticanalysis/inferencers/SelectConstraintSolver.hi
../heliumNT/src/staticanalysis/inferencers/TypeInferencing.o : ../Top/src/Top/../Top/ComposedSolvers/Tree.hi
../heliumNT/src/staticanalysis/inferencers/TypeInferencing.o : ../heliumNT/src/staticanalysis/miscellaneous/TypeConstraints.hi
../heliumNT/src/staticanalysis/inferencers/TypeInferencing.o : ../heliumNT/src/staticanalysis/miscellaneous/UHA_Source.hi
../heliumNT/src/staticanalysis/inferencers/TypeInferencing.o : ../heliumNT/src/staticanalysis/miscellaneous/DoublyLinkedTree.hi
../heliumNT/src/staticanalysis/inferencers/TypeInferencing.o : ../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.hi
../heliumNT/src/staticanalysis/inferencers/TypeInferencing.o : ../heliumNT/src/staticanalysis/messages/Warnings.hi
../heliumNT/src/staticanalysis/inferencers/TypeInferencing.o : ../heliumNT/src/staticanalysis/messages/TypeErrors.hi
../heliumNT/src/staticanalysis/inferencers/TypeInferencing.o : ../heliumNT/src/staticanalysis/messages/Messages.hi
../heliumNT/src/staticanalysis/inferencers/TypeInferencing.o : ../heliumNT/src/staticanalysis/miscellaneous/TypeConversion.hi
../heliumNT/src/staticanalysis/inferencers/TypeInferencing.o : ../Top/src/Top/../Top/Types.hi
../heliumNT/src/staticanalysis/inferencers/ExpressionTypeInferencer.o : ../heliumNT/src/staticanalysis/inferencers/ExpressionTypeInferencer.hs
../heliumNT/src/staticanalysis/inferencers/ExpressionTypeInferencer.o : ../heliumNT/src/syntax/UHA_Syntax.hi
../heliumNT/src/staticanalysis/inferencers/ExpressionTypeInferencer.o : ../heliumNT/src/utils/Utils.hi
../heliumNT/src/staticanalysis/inferencers/ExpressionTypeInferencer.o : ../heliumNT/src/syntax/UHA_Range.hi
../heliumNT/src/staticanalysis/inferencers/ExpressionTypeInferencer.o : ../heliumNT/src/syntax/UHA_Utils.hi
../heliumNT/src/staticanalysis/inferencers/ExpressionTypeInferencer.o : ../Top/src/Top/../Top/Types.hi
../heliumNT/src/staticanalysis/inferencers/ExpressionTypeInferencer.o : ../heliumNT/src/staticanalysis/messages/TypeErrors.hi
../heliumNT/src/staticanalysis/inferencers/ExpressionTypeInferencer.o : ../heliumNT/src/staticanalysis/inferencers/BindingGroupAnalysis.hi
../heliumNT/src/staticanalysis/inferencers/ExpressionTypeInferencer.o : ../heliumNT/src/modulesystem/ImportEnvironment.hi
../heliumNT/src/staticanalysis/inferencers/ExpressionTypeInferencer.o : ../heliumNT/src/staticanalysis/inferencers/TypeInferencing.hi
../heliumNT/src/staticanalysis/directives/TS_ToCore.o : ../heliumNT/src/staticanalysis/directives/TS_ToCore.hs
../heliumNT/src/staticanalysis/directives/TS_ToCore.o : ../heliumNT/src/staticanalysis/inferencers/ExpressionTypeInferencer.hi
../heliumNT/src/staticanalysis/directives/TS_ToCore.o : ../heliumNT/src/syntax/UHA_Syntax.hi
../heliumNT/src/staticanalysis/directives/TS_ToCore.o : ../heliumNT/src/staticanalysis/miscellaneous/TypeConstraints.hi
../heliumNT/src/staticanalysis/directives/TS_ToCore.o : ../heliumNT/src/staticanalysis/directives/TS_Attributes.hi
../heliumNT/src/staticanalysis/directives/TS_ToCore.o : ../heliumNT/src/utils/OneLiner.hi
../heliumNT/src/staticanalysis/directives/TS_ToCore.o : ../heliumNT/src/staticanalysis/messages/TypeErrors.hi
../heliumNT/src/staticanalysis/directives/TS_ToCore.o : ../heliumNT/src/utils/Utils.hi
../heliumNT/src/staticanalysis/directives/TS_ToCore.o : ../heliumNT/src/staticanalysis/miscellaneous/TypeConversion.hi
../heliumNT/src/staticanalysis/directives/TS_ToCore.o : ../heliumNT/src/staticanalysis/miscellaneous/UHA_Source.hi
../heliumNT/src/staticanalysis/directives/TS_ToCore.o : ../heliumNT/src/syntax/UHA_OneLine.hi
../heliumNT/src/staticanalysis/directives/TS_ToCore.o : ../heliumNT/src/syntax/UHA_Utils.hi
../heliumNT/src/staticanalysis/directives/TS_ToCore.o : ../heliumNT/src/syntax/UHA_Range.hi
../heliumNT/src/staticanalysis/directives/TS_ToCore.o : ../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.hi
../heliumNT/src/staticanalysis/directives/TS_ToCore.o : ../Top/src/Top/../Top/Types.hi
../heliumNT/src/staticanalysis/directives/TS_ToCore.o : ../heliumNT/src/staticanalysis/messages/Messages.hi
../heliumNT/src/staticanalysis/directives/TS_ToCore.o : ../heliumNT/src/modulesystem/ImportEnvironment.hi
../heliumNT/src/staticanalysis/directives/TS_ToCore.o : ../heliumNT/src/staticanalysis/directives/TS_CoreSyntax.hi
../heliumNT/src/staticanalysis/directives/TS_ToCore.o : ../heliumNT/src/staticanalysis/directives/TS_Syntax.hi
../heliumNT/src/staticanalysis/staticchecks/StaticChecks.o : ../heliumNT/src/staticanalysis/staticchecks/StaticChecks.hs
../heliumNT/src/staticanalysis/staticchecks/StaticChecks.o : ../heliumNT/src/parser/OperatorTable.hi
../heliumNT/src/staticanalysis/staticchecks/StaticChecks.o : ../heliumNT/src/modulesystem/ImportEnvironment.hi
../heliumNT/src/staticanalysis/staticchecks/StaticChecks.o : ../heliumNT/src/staticanalysis/miscellaneous/TypeConstraints.hi
../heliumNT/src/staticanalysis/staticchecks/StaticChecks.o : ../heliumNT/src/codegeneration/DerivingShow.hi
../heliumNT/src/staticanalysis/staticchecks/StaticChecks.o : ../heliumNT/src/staticanalysis/miscellaneous/TypeConversion.hi
../heliumNT/src/staticanalysis/staticchecks/StaticChecks.o : ../heliumNT/src/utils/Utils.hi
../heliumNT/src/staticanalysis/staticchecks/StaticChecks.o : ../lvm/src/lib/common/TopSort.hi
../heliumNT/src/staticanalysis/staticchecks/StaticChecks.o : ../heliumNT/src/staticanalysis/messages/Messages.hi
../heliumNT/src/staticanalysis/staticchecks/StaticChecks.o : ../heliumNT/src/staticanalysis/messages/Warnings.hi
../heliumNT/src/staticanalysis/staticchecks/StaticChecks.o : ../heliumNT/src/staticanalysis/messages/StaticErrors.hi
../heliumNT/src/staticanalysis/staticchecks/StaticChecks.o : ../Top/src/Top/../Top/Types.hi
../heliumNT/src/staticanalysis/staticchecks/StaticChecks.o : ../heliumNT/src/syntax/UHA_Range.hi
../heliumNT/src/staticanalysis/staticchecks/StaticChecks.o : ../heliumNT/src/syntax/UHA_Utils.hi
../heliumNT/src/staticanalysis/staticchecks/StaticChecks.o : ../heliumNT/src/syntax/UHA_Syntax.hi
../heliumNT/src/staticanalysis/staticchecks/StaticChecks.o : ../heliumNT/src/main/Args.hi
../heliumNT/src/staticanalysis/staticchecks/StaticChecks.o : ../heliumNT/src/utils/Similarity.hi
../heliumNT/src/main/PhaseStaticChecks.o : ../heliumNT/src/main/PhaseStaticChecks.hs
../heliumNT/src/main/PhaseStaticChecks.o : ../heliumNT/src/staticanalysis/messages/StaticErrors.hi
../heliumNT/src/main/PhaseStaticChecks.o : ../Top/src/Top/../Top/Types.hi
../heliumNT/src/main/PhaseStaticChecks.o : ../heliumNT/src/syntax/UHA_Syntax.hi
../heliumNT/src/main/PhaseStaticChecks.o : ../heliumNT/src/staticanalysis/staticchecks/StaticChecks.hi
../heliumNT/src/main/PhaseStaticChecks.o : ../heliumNT/src/staticanalysis/messages/Warnings.hi
../heliumNT/src/main/PhaseStaticChecks.o : ../heliumNT/src/main/CompileUtils.hi
../heliumNT/src/staticanalysis/directives/TS_Analyse.o : ../heliumNT/src/staticanalysis/directives/TS_Analyse.hs
../heliumNT/src/staticanalysis/directives/TS_Analyse.o : ../heliumNT/src/staticanalysis/inferencers/ExpressionTypeInferencer.hi
../heliumNT/src/staticanalysis/directives/TS_Analyse.o : ../heliumNT/src/syntax/UHA_Syntax.hi
../heliumNT/src/staticanalysis/directives/TS_Analyse.o : ../heliumNT/src/syntax/UHA_Pretty.hi
../heliumNT/src/staticanalysis/directives/TS_Analyse.o : ../heliumNT/src/utils/Utils.hi
../heliumNT/src/staticanalysis/directives/TS_Analyse.o : ../heliumNT/src/staticanalysis/inferencers/ExpressionTypeInferencer.hi
../heliumNT/src/staticanalysis/directives/TS_Analyse.o : ../heliumNT/src/modulesystem/ImportEnvironment.hi
../heliumNT/src/staticanalysis/directives/TS_Analyse.o : ../heliumNT/src/staticanalysis/directives/TS_Messages.hi
../heliumNT/src/staticanalysis/directives/TS_Analyse.o : ../heliumNT/src/staticanalysis/messages/TypeErrors.hi
../heliumNT/src/staticanalysis/directives/TS_Analyse.o : ../heliumNT/src/staticanalysis/messages/Messages.hi
../heliumNT/src/staticanalysis/directives/TS_Analyse.o : ../heliumNT/src/staticanalysis/miscellaneous/UHA_Source.hi
../heliumNT/src/staticanalysis/directives/TS_Analyse.o : ../heliumNT/src/syntax/UHA_Range.hi
../heliumNT/src/staticanalysis/directives/TS_Analyse.o : ../heliumNT/src/syntax/UHA_Utils.hi
../heliumNT/src/staticanalysis/directives/TS_Analyse.o : ../heliumNT/src/staticanalysis/miscellaneous/TypeConversion.hi
../heliumNT/src/staticanalysis/directives/TS_Analyse.o : ../heliumNT/src/staticanalysis/directives/TS_Syntax.hi
../heliumNT/src/staticanalysis/directives/TS_Analyse.o : ../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.hi
../heliumNT/src/staticanalysis/directives/TS_Analyse.o : ../heliumNT/src/staticanalysis/miscellaneous/TypeConstraints.hi
../heliumNT/src/staticanalysis/directives/TS_Analyse.o : ../Top/src/Top/../Top/States/TIState.hi
../heliumNT/src/staticanalysis/directives/TS_Analyse.o : ../Top/src/Top/../Top/Solvers/SolveConstraints.hi
../heliumNT/src/staticanalysis/directives/TS_Analyse.o : ../Top/src/Top/../Top/Solvers/GreedySolver.hi
../heliumNT/src/staticanalysis/directives/TS_Analyse.o : ../Top/src/Top/../Top/Types.hi
../heliumNT/src/staticanalysis/directives/TS_Parser.o : ../heliumNT/src/staticanalysis/directives/TS_Parser.hs
../heliumNT/src/staticanalysis/directives/TS_Parser.o : ../heliumNT/src/utils/Utils.hi
../heliumNT/src/staticanalysis/directives/TS_Parser.o : ../heliumNT/src/parser/OperatorTable.hi
../heliumNT/src/staticanalysis/directives/TS_Parser.o : ../parsec/Text/ParserCombinators/ParsecSpecial.hi
../heliumNT/src/staticanalysis/directives/TS_Parser.o : ../heliumNT/src/parser/ResolveOperators.hi
../heliumNT/src/staticanalysis/directives/TS_Parser.o : ../heliumNT/src/parser/Parser.hi
../heliumNT/src/staticanalysis/directives/TS_Parser.o : ../heliumNT/src/parser/ParseLibrary.hi
../heliumNT/src/staticanalysis/directives/TS_Parser.o : ../heliumNT/src/parser/Lexer.hi
../heliumNT/src/staticanalysis/directives/TS_Parser.o : ../heliumNT/src/staticanalysis/directives/TS_ToCore.hi
../heliumNT/src/staticanalysis/directives/TS_Parser.o : ../heliumNT/src/staticanalysis/directives/TS_CoreSyntax.hi
../heliumNT/src/staticanalysis/directives/TS_Parser.o : ../heliumNT/src/staticanalysis/directives/TS_Syntax.hi
../heliumNT/src/staticanalysis/directives/TS_Parser.o : ../heliumNT/src/syntax/UHA_Pretty.hi
../heliumNT/src/staticanalysis/directives/TS_Parser.o : ../heliumNT/src/syntax/UHA_Utils.hi
../heliumNT/src/staticanalysis/directives/TS_Parser.o : ../heliumNT/src/syntax/UHA_Syntax.hi
../heliumNT/src/staticanalysis/directives/TS_Compile.o : ../heliumNT/src/staticanalysis/directives/TS_Compile.hs
../heliumNT/src/staticanalysis/directives/TS_Compile.o : ../lvm/src/lib/core/Core.hi
../heliumNT/src/staticanalysis/directives/TS_Compile.o : ../heliumNT/src/codegeneration/CoreUtils.hi
../heliumNT/src/staticanalysis/directives/TS_Compile.o : ../heliumNT/src/parser/ParseMessage.hi
../heliumNT/src/staticanalysis/directives/TS_Compile.o : ../heliumNT/src/main/Args.hi
../heliumNT/src/staticanalysis/directives/TS_Compile.o : ../heliumNT/src/staticanalysis/messages/HeliumMessages.hi
../heliumNT/src/staticanalysis/directives/TS_Compile.o : ../heliumNT/src/staticanalysis/directives/TS_Analyse.hi
../heliumNT/src/staticanalysis/directives/TS_Compile.o : ../heliumNT/src/parser/Lexer.hi
../heliumNT/src/staticanalysis/directives/TS_Compile.o : ../heliumNT/src/staticanalysis/directives/TS_Parser.hi
../heliumNT/src/staticanalysis/directives/TS_Compile.o : ../heliumNT/src/staticanalysis/directives/TS_ToCore.hi
../heliumNT/src/staticanalysis/directives/TS_Compile.o : ../heliumNT/src/modulesystem/ImportEnvironment.hi
../heliumNT/src/staticanalysis/directives/TS_Compile.o : ../heliumNT/src/staticanalysis/directives/TS_CoreSyntax.hi
../heliumNT/src/main/PhaseTypingStrategies.o : ../heliumNT/src/main/PhaseTypingStrategies.hs
../heliumNT/src/main/PhaseTypingStrategies.o : ../Top/src/Top/../Top/Types.hi
../heliumNT/src/main/PhaseTypingStrategies.o : ../heliumNT/src/syntax/UHA_Syntax.hi
../heliumNT/src/main/PhaseTypingStrategies.o : ../heliumNT/src/staticanalysis/directives/TS_Compile.hi
../heliumNT/src/main/PhaseTypingStrategies.o : ../lvm/src/lib/core/Core.hi
../heliumNT/src/main/PhaseTypingStrategies.o : ../heliumNT/src/main/CompileUtils.hi
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
../proxima/src/presentation/Presenter.o : ../proxima/src/presentation/Presenter.hs
../proxima/src/presentation/Presenter.o : ../proxima/src/layout/TreeEditPres.hi
../proxima/src/presentation/Presenter.o : ../proxima/src/presentation/XprezLib.hi
../proxima/src/presentation/Presenter.o : ../proxima/src/presentation/PresLayerUtils.hi
../proxima/src/presentation/Presenter.o : ../proxima/src/presentation/PresLayerTypes.hi
../proxima/src/presentation/Presenter.o : ../proxima/src/common/CommonTypes.hi
../proxima/src/presentation/PresPresent.o : ../proxima/src/presentation/PresPresent.hs
../proxima/src/presentation/PresPresent.o : ../proxima/src/presentation/Presenter.hi
../proxima/src/presentation/PresPresent.o : ../proxima/src/presentation/PresLayerUtils.hi
../proxima/src/presentation/PresPresent.o : ../proxima/src/presentation/PresLayerTypes.hi
../proxima/src/presentation/PresPresent.o : ../proxima/src/common/CommonTypes.hi
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
../proxima/src/rendering/Renderer.o : ../proxima/src/rendering/Renderer.hs
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
src/CompileHelium.o : src/CompileHelium.hs
src/CompileHelium.o : ../heliumNT/src/main/CompileUtils.hi
src/CompileHelium.o : ../heliumNT/src/main/Args.hi
src/CompileHelium.o : ../Top/src/Top/../Top/Types.hi
src/CompileHelium.o : ../heliumNT/src/syntax/UHA_Syntax.hi
src/CompileHelium.o : ../heliumNT/src/staticanalysis/messages/StaticErrors.hi
src/CompileHelium.o : ../heliumNT/src/staticanalysis/messages/TypeErrors.hi
src/CompileHelium.o : ../heliumNT/src/staticanalysis/inferencers/TypeInferencing.hi
src/CompileHelium.o : ../heliumNT/src/main/PhaseTypingStrategies.hi
src/CompileHelium.o : ../heliumNT/src/main/PhaseStaticChecks.hi
src/CompileHelium.o : ../heliumNT/src/main/PhaseImport.hi
src/DocTypes_Generated.o : src/DocTypes_Generated.hs
src/DocTypes_Generated.o : ../proxima/src/presentation/PresTypes.hi
src/DocTypes_Generated.o : ../proxima/src/evaluation/DocTypes.hi
src/DocTypes_Generated.o : ../proxima/src/common/CommonTypes.hi
src/DocUtils_Generated.o : src/DocUtils_Generated.hs
src/DocUtils_Generated.o : ../proxima/src/presentation/PresTypes.hi
src/DocUtils_Generated.o : src/DocTypes_Generated.hi
src/DocUtils_Generated.o : ../proxima/src/evaluation/DocTypes.hi
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
src/Evaluator.o : src/Evaluator.hs
src/Evaluator.o : src/EvaluateTypesStubs.hi
src/Evaluator.o : src/DocTypes_Generated.hi
src/Evaluator.o : src/DocumentEdit_Generated.hi
src/Evaluator.o : ../proxima/src/evaluation/DocumentEdit.hi
src/Evaluator.o : ../proxima/src/evaluation/EvalLayerUtils.hi
src/Evaluator.o : ../proxima/src/evaluation/EvalLayerTypes.hi
src/Evaluator.o : ../proxima/src/common/CommonTypes.hi
src/Reducer.o : src/Reducer.hs
src/Reducer.o : src/DocUtils_Generated.hi
src/Reducer.o : src/DocTypes_Generated.hi
src/Reducer.o : ../proxima/src/presentation/PresTypes.hi
src/Reducer.o : ../proxima/src/evaluation/EvalLayerUtils.hi
src/Reducer.o : ../proxima/src/evaluation/EvalLayerTypes.hi
src/Reducer.o : ../proxima/src/common/CommonTypes.hi
src/Scanner.o : src/Scanner.hs
src/Scanner.o : src/DocTypes_Generated.hi
src/Scanner.o : ../proxima/src/layout/LayLayerUtils.hi
src/Scanner.o : ../proxima/src/layout/LayLayerTypes.hi
src/Scanner.o : ../proxima/src/common/CommonTypes.hi
src/Chess.o : src/Chess.hs
src/Chess.o : ../proxima/src/evaluation/DocumentEdit.hi
src/Chess.o : ../proxima/src/presentation/PresTypes.hi
src/Chess.o : ../proxima/src/presentation/XprezLib.hi
src/Chess.o : src/DocUtils_Generated.hi
src/Chess.o : ../proxima/src/evaluation/DocUtils.hi
src/Chess.o : ../proxima/src/evaluation/DocTypes.hi
src/Chess.o : src/DocTypes_Generated.hi
src/Chess.o : ../proxima/src/common/CommonTypes.hi
src/ProxParser_Generated.o : src/ProxParser_Generated.hs
src/ProxParser_Generated.o : src/DocTypes_Generated.hi
src/ProxParser_Generated.o : ../proxima/src/evaluation/DocTypes.hi
src/ProxParser_Generated.o : src/DocumentEdit_Generated.hi
src/ProxParser_Generated.o : ../proxima/src/evaluation/DocumentEdit.hi
src/ProxParser_Generated.o : ../proxima/src/presentation/PresLayerUtils.hi
src/ProxParser_Generated.o : ../proxima/src/presentation/PresLayerTypes.hi
src/ProxParser_Generated.o : ../proxima/src/common/CommonTypes.hi
src/ProxParser.o : src/ProxParser.hs
src/ProxParser.o : src/DocUtils_Generated.hi
src/ProxParser.o : src/DocTypes_Generated.hi
src/ProxParser.o : ../proxima/src/common/UU_Parsing.hi
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
src/EvaluateTypes.o : src/EvaluateTypes.hs
src/EvaluateTypes.o : ../heliumNT/src/syntax/UHA_Pretty.hi
src/EvaluateTypes.o : ../heliumNT/src/staticanalysis/messages/HeliumMessages.hi
src/EvaluateTypes.o : ../heliumNT/src/syntax/UHA_Range.hi
src/EvaluateTypes.o : ../heliumNT/src/syntax/UHA_Utils.hi
src/EvaluateTypes.o : ../Top/src/Top/../Top/Types.hi
src/EvaluateTypes.o : ../heliumNT/src/staticanalysis/messages/TypeErrors.hi
src/EvaluateTypes.o : ../heliumNT/src/staticanalysis/messages/StaticErrors.hi
src/EvaluateTypes.o : ../heliumNT/src/utils/Utils.hi
src/EvaluateTypes.o : src/CompileHelium.hi
src/EvaluateTypes.o : ../heliumNT/src/syntax/UHA_Syntax.hi
src/EvaluateTypes.o : src/DocumentEdit_Generated.hi
src/EvaluateTypes.o : ../proxima/src/evaluation/DocumentEdit.hi
src/EvaluateTypes.o : ../proxima/src/common/DebugLevels.hi
src/EvaluateTypes.o : src/DocTypes_Generated.hi
src/EvaluateTypes.o : ../proxima/src/evaluation/DocTypes.hi
# DO NOT DELETE: End of Haskell dependencies



######### HELIUM CRAP

######################### HELIUM

# Helium sources

PARSER	= \
	$(HELIUMSRCDIR)/parser/CollectFunctionBindings.hs $(HELIUMSRCDIR)/parser/OperatorTable.hs \
	$(HELIUMSRCDIR)/parser/Parser.hs $(HELIUMSRCDIR)/parser/ResolveOperators.hs $(HELIUMSRCDIR)/parser/Lexer.hs \
	$(HELIUMSRCDIR)/parser/ParseLibrary.hs $(HELIUMSRCDIR)/parser/ParseMessage.hs \
	$(HELIUMSRCDIR)/parser/LexerMonad.hs $(HELIUMSRCDIR)/parser/LexerMessage.hs $(HELIUMSRCDIR)/parser/LayoutRule.hs \
	$(HELIUMSRCDIR)/parser/LexerToken.hs

MODULESYSTEM = \
	$(HELIUMSRCDIR)/modulesystem/ImportEnvironment.hs \
	$(HELIUMSRCDIR)/modulesystem/DictionaryEnvironment.hs \
	$(HELIUMSRCDIR)/modulesystem/CoreToImportEnv.hs $(HELIUMSRCDIR)/modulesystem/ExtractImportDecls.hs

STATICANALYSIS = \
	$(HELIUMSRCDIR)/staticanalysis/messages/Messages.hs \
	$(HELIUMSRCDIR)/staticanalysis/messages/HeliumMessages.hs \
	$(HELIUMSRCDIR)/staticanalysis/messages/StaticErrors.hs \
	$(HELIUMSRCDIR)/staticanalysis/messages/TypeErrors.hs \
	$(HELIUMSRCDIR)/staticanalysis/messages/KindErrors.hs \
	$(HELIUMSRCDIR)/staticanalysis/messages/Warnings.hs \
	$(HELIUMSRCDIR)/staticanalysis/heuristics/HeuristicsInfo.hs \
	$(HELIUMSRCDIR)/staticanalysis/heuristics/ListOfHeuristics.hs \
	$(HELIUMSRCDIR)/staticanalysis/heuristics/OnlyResultHeuristics.hs \
	$(HELIUMSRCDIR)/staticanalysis/heuristics/RepairHeuristics.hs \
	$(HELIUMSRCDIR)/staticanalysis/heuristics/TieBreakerHeuristics.hs \
	$(HELIUMSRCDIR)/staticanalysis/heuristics/UnifierHeuristics.hs \
	$(HELIUMSRCDIR)/staticanalysis/heuristics/RepairSystem.hs \
	$(HELIUMSRCDIR)/staticanalysis/staticchecks/StaticChecks.hs \
	$(HELIUMSRCDIR)/staticanalysis/inferencers/TypeInferencing.hs \
	$(HELIUMSRCDIR)/staticanalysis/inferencers/KindInferencing.hs \
	$(HELIUMSRCDIR)/staticanalysis/inferencers/ExpressionTypeInferencer.hs \
	$(HELIUMSRCDIR)/staticanalysis/inferencers/SelectConstraintSolver.hs \
	$(HELIUMSRCDIR)/staticanalysis/inferencers/BindingGroupAnalysis.hs \
	$(HELIUMSRCDIR)/staticanalysis/directives/TS_Parser.hs \
	$(HELIUMSRCDIR)/staticanalysis/directives/TS_Syntax.hs \
	$(HELIUMSRCDIR)/staticanalysis/directives/Matchers.hs \
	$(HELIUMSRCDIR)/staticanalysis/directives/TS_Apply.hs \
	$(HELIUMSRCDIR)/staticanalysis/directives/TS_Messages.hs \
	$(HELIUMSRCDIR)/staticanalysis/directives/TS_Analyse.hs \
	$(HELIUMSRCDIR)/staticanalysis/directives/TS_CoreSyntax.hs \
	$(HELIUMSRCDIR)/staticanalysis/directives/TS_ToCore.hs \
	$(HELIUMSRCDIR)/staticanalysis/directives/TS_Compile.hs \
	$(HELIUMSRCDIR)/staticanalysis/directives/TS_Attributes.hs \
	$(HELIUMSRCDIR)/staticanalysis/miscellaneous/ConstraintInfo.hs \
	$(HELIUMSRCDIR)/staticanalysis/miscellaneous/DoublyLinkedTree.hs \
	$(HELIUMSRCDIR)/staticanalysis/miscellaneous/TypeConstraints.hs \
	$(HELIUMSRCDIR)/staticanalysis/miscellaneous/TypesToAlignedDocs.hs \
	$(HELIUMSRCDIR)/staticanalysis/miscellaneous/TypeConversion.hs \
	$(HELIUMSRCDIR)/staticanalysis/miscellaneous/UHA_Source.hs \
	$(HELIUMSRCDIR)/staticanalysis/miscellaneous/TypeDebugInfo.hs \
	$(HELIUMSRCDIR)/staticanalysis/miscellaneous/TypeDebugInfoWriter.hs \
	$(TOPDIR)/Types/Basics.hs \
	$(TOPDIR)/Types/Quantification.hs \
	$(TOPDIR)/Types/Qualification.hs \
	$(TOPDIR)/Types/Schemes.hs \
	$(TOPDIR)/Types/Classes.hs \
	$(TOPDIR)/Types/Substitution.hs \
	$(TOPDIR)/Types/Kinds.hs \
	$(TOPDIR)/Types/Synonyms.hs \
	$(TOPDIR)/Types/Unification.hs \
	$(TOPDIR)/Types.hs \
	$(TOPDIR)/../Graph.hs \
	$(TOPDIR)/Constraints/Constraints.hs \
	$(TOPDIR)/Constraints/ExtraConstraints.hs \
	$(TOPDIR)/Constraints/TypeConstraintInfo.hs \
	$(TOPDIR)/Constraints/Equality.hs \
	$(TOPDIR)/Constraints/Polymorphism.hs \
	$(TOPDIR)/States/States.hs \
	$(TOPDIR)/States/BasicState.hs \
	$(TOPDIR)/States/SubstState.hs \
	$(TOPDIR)/States/TIState.hs \
	$(TOPDIR)/States/QualifierState.hs \
	$(TOPDIR)/Qualifiers/Qualifiers.hs \
	$(TOPDIR)/Qualifiers/QualifierMap.hs \
	$(TOPDIR)/Qualifiers/TypeClasses.hs \
	$(TOPDIR)/Solvers/BasicMonad.hs \
	$(TOPDIR)/Solvers/GreedySolver.hs \
	$(TOPDIR)/Solvers/GreedySubst.hs \
	$(TOPDIR)/Solvers/SimpleSolver.hs \
	$(TOPDIR)/Solvers/SimpleSubst.hs \
	$(TOPDIR)/Solvers/SolveConstraints.hs \
	$(TOPDIR)/ComposedSolvers/CombinationSolver.hs \
	$(TOPDIR)/ComposedSolvers/ChunkySolver.hs \
	$(TOPDIR)/ComposedSolvers/Tree.hs \
	$(TOPDIR)/ComposedSolvers/TreeWalk.hs \
	$(TOPDIR)/TypeGraph/ApplyHeuristics.hs \
	$(TOPDIR)/TypeGraph/Basics.hs \
	$(TOPDIR)/TypeGraph/DefaultHeuristics.hs \
	$(TOPDIR)/TypeGraph/EquivalenceGroup.hs \
	$(TOPDIR)/TypeGraph/Heuristics.hs \
	$(TOPDIR)/TypeGraph/Paths.hs \
	$(TOPDIR)/TypeGraph/TypeGraphMonad.hs \
	$(TOPDIR)/TypeGraph/TypeGraphSolver.hs \
	$(TOPDIR)/TypeGraph/TypeGraphState.hs \
	$(TOPDIR)/TypeGraph/Implementation.hs \
	$(TOPDIR)/TypeGraph/TypeGraphSubst.hs \
	$(TOPDIR)/Repair/AExpr.hs \
	$(TOPDIR)/Repair/Transformations.hs \
	$(TOPDIR)/Repair/Repair.hs \
	
UHA = \
	$(HELIUMSRCDIR)/syntax/UHA_Syntax.hs $(HELIUMSRCDIR)/syntax/UHA_Utils.hs $(HELIUMSRCDIR)/syntax/UHA_Range.hs \
	$(HELIUMSRCDIR)/syntax/UHA_Pretty.hs $(HELIUMSRCDIR)/syntax/UHA_OneLine.hs


CODEGENERATION = \
	$(HELIUMSRCDIR)/codegeneration/CoreToLvm.hs $(HELIUMSRCDIR)/codegeneration/PatternMatch.hs \
	$(HELIUMSRCDIR)/codegeneration/DerivingShow.hs $(HELIUMSRCDIR)/codegeneration/DerivingEq.hs $(HELIUMSRCDIR)/codegeneration/CoreUtils.hs \
	$(HELIUMSRCDIR)/codegeneration/CodeGeneration.hs

UTILS = \
	$(HELIUMSRCDIR)/utils/Utils.hs $(HELIUMSRCDIR)/utils/Logger.hs $(HELIUMSRCDIR)/utils/OSSpecific.hs $(HELIUMSRCDIR)/utils/OneLiner.hs \
	$(HELIUMSRCDIR)/utils/Similarity.hs $(HELIUMSRCDIR)/utils/Texts.hs

MAINFILES = \
	$(HELIUMSRCDIR)/main/Version.hs $(HELIUMSRCDIR)/main/Args.hs $(HELIUMSRCDIR)/main/Compile.hs \
	$(HELIUMSRCDIR)/main/PhaseLexer.hs $(HELIUMSRCDIR)/main/PhaseParser.hs $(HELIUMSRCDIR)/main/PhaseImport.hs $(HELIUMSRCDIR)/main/PhaseResolveOperators.hs \
	$(HELIUMSRCDIR)/main/PhaseStaticChecks.hs $(HELIUMSRCDIR)/main/PhaseTypingStrategies.hs $(HELIUMSRCDIR)/main/PhaseTypeInferencer.hs \
	$(HELIUMSRCDIR)/main/PhaseDesugarer.hs $(HELIUMSRCDIR)/main/PhaseCodeGenerator.hs $(HELIUMSRCDIR)/main/CompileUtils.hs \
	$(HELIUMSRCDIR)/main/PhaseKindInferencer.hs	
# Parsec sources

PARSEC = \
	$(PARSECIMPDIR)/ParsecSpecial.hs $(PARSECIMPDIR)/ParsecSpecial/Char.hs $(PARSECIMPDIR)/ParsecSpecial/Combinator.hs $(PARSECIMPDIR)/ParsecSpecial/Error.hs $(PARSECIMPDIR)/ParsecSpecial/Expr.hs \
	$(PARSECIMPDIR)/ParsecSpecial/Language.hs $(PARSECIMPDIR)/ParsecSpecial/Perm.hs $(PARSECIMPDIR)/ParsecSpecial/Pos.hs $(PARSECIMPDIR)/ParsecSpecial/Prim.hs $(PARSECIMPDIR)/ParsecSpecial/Token.hs


# LVM sources

LVMCOMMON = \
	  $(LVMLIBDIR)/common/Id.hs $(LVMLIBDIR)/common/IdMap.hs $(LVMLIBDIR)/common/IdSet.hs $(LVMLIBDIR)/common/IntMap.hs \
	  $(LVMLIBDIR)/common/Byte.hs $(LVMLIBDIR)/common/Standard.hs \
	  $(LVMLIBDIR)/common/TopSort.hs $(LVMLIBDIR)/common/Set.hs \
	  $(LVMLIBDIR)/common/ghc/Special.hs $(LVMLIBDIR)/common/PPrint.hs

LVM	= $(LVMLIBDIR)/lvm/Lvm.hs $(LVMLIBDIR)/lvm/LvmPretty.hs $(LVMLIBDIR)/lvm/LvmWrite.hs $(LVMLIBDIR)/lvm/LvmRead.hs \
	  $(LVMLIBDIR)/lvm/LvmImport.hs $(LVMLIBDIR)/lvm/Module.hs $(LVMLIBDIR)/lvm/ModulePretty.hs \
	  $(LVMLIBDIR)/lvm/Instr.hs $(LVMLIBDIR)/lvm/InstrPretty.hs $(LVMLIBDIR)/lvm/InstrResolve.hs $(LVMLIBDIR)/lvm/InstrRewrite.hs

ASM	= $(LVMLIBDIR)/asm/AsmOptimize.hs $(LVMLIBDIR)/asm/AsmInline.hs $(LVMLIBDIR)/asm/AsmOccur.hs \
	  $(LVMLIBDIR)/asm/Asm.hs $(LVMLIBDIR)/asm/AsmPretty.hs $(LVMLIBDIR)/asm/AsmToLvm.hs

CORE	= $(LVMLIBDIR)/core/Core.hs $(LVMLIBDIR)/core/CorePretty.hs $(LVMLIBDIR)/core/CoreToAsm.hs \
	  $(LVMLIBDIR)/core/CoreParse.hs $(LVMLIBDIR)/core/CoreLexer.hs \
	  $(LVMLIBDIR)/core/CoreNormalize.hs $(LVMLIBDIR)/core/CoreRemoveDead.hs \
	  $(LVMLIBDIR)/core/CoreNoShadow.hs $(LVMLIBDIR)/core/CoreFreeVar.hs $(LVMLIBDIR)/core/CoreLetSort.hs \
	  $(LVMLIBDIR)/core/CoreLift.hs $(LVMLIBDIR)/core/CoreSaturate.hs


# AG sources
AGSOURCES = \
     syntax/UHA_Pretty.hs syntax/UHA_OneLine.hs \
     $(HELIUMSRCDIR)/staticanalysis/staticchecks/StaticChecks.hs $(HELIUMSRCDIR)/staticanalysis/inferencers/TypeInferencing.hs \
     $(HELIUMSRCDIR)/staticanalysis/inferencers/KindInferencing.hs \
     $(HELIUMSRCDIR)/codegeneration/CodeGeneration.hs \
     $(HELIUMSRCDIR)/syntax/UHA_Syntax.hs $(HELIUMSRCDIR)/modulesystem/ExtractImportDecls.hs $(HELIUMSRCDIR)/staticanalysis/directives/TS_Syntax.hs \
     $(HELIUMSRCDIR)/staticanalysis/directives/TS_Apply.hs $(HELIUMSRCDIR)/staticanalysis/directives/TS_Analyse.hs \
     $(HELIUMSRCDIR)/staticanalysis/directives/TS_CoreSyntax.hs \
     $(HELIUMSRCDIR)/staticanalysis/directives/TS_ToCore.hs $(HELIUMSRCDIR)/parser/ResolveOperators.hs



#Helium Sources and object files
HELIUMSRCS = $(PARSER) $(STATICANALYSIS) $(UHA) $(CODEGENERATION) $(UTILS) $(LVMCOMMON) \
	     $(LVM) $(ASM) $(CORE) $(MAINFILES) $(PARSEC) $(MODULESYSTEM)

# deze moet met -cpp gecompileerd worden
$(LVMLIBDIR)/common/ghc/Special.hi: $(LVMLIBDIR)/common/ghc/Special.hs
	# GHC Special.hs
	$(HC) -c $< -cpp -fglasgow-exts $(HC_OPTS) 



###################################

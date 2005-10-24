#-----------------------------------------------------------------------
#  The Proxima generic editor
#-----------------------------------------------------------------------

# make		 - build everything that is needed
# make depend	 - build dependencies for hs files, ag dependencies are	written	by hand
# make clean	 - remove .hi and .o files


# TODO:
# add platform dependent stuff (configure?)
# add strip
# TODO: on normal make, generator is not rebuilt after sources have been edited (only on make generate)
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

default: all
all: proxima

.PHONY : default proxima


PROXIMATESTDIR = $(PROXIMADIR)/test
EXE            = 
MAIN           = proxima$(EXE)

AGC     = $(PROXIMATESTDIR)/uuagc$(EXE)
GEN	= $(PROXIMATESTDIR)/generate$(EXE)
HC	= ghc
HC_OPTS = -static -fglasgow-exts -fallow-undecidable-instances \
		-package lang -package wx -package util -package net -package data -package parsec\
		-fno-monomorphism-restriction -i$(IFACES) $(EXTRA_OPTS)
#		-ignore-package network-1.0

IFACES = $(PROXIMAIFACES):$(HELIUMIFACES):$(GENERATORIFACES)


OBJS = $(SRCS:.hs=.o)

# TODO: PROXIMASRCS and SRCS may not go below proxima: ?????
PROXIMASRCS = $(ARRANGEMENTSRCS) $(COMMONSRCS) $(EVALUATIONSRCS) $(LAYOUTSRCS) \
		$(MAINSRCS) $(PRESENTATIONSRCS) $(RENDERINGSRCS)

SRCS = $(PROXIMASRCS) $(HELIUMSRCS)





.SUFFIXES : .o .hs .hi .lhs .hc	.s .ag



# Inter-module dependencies
depend :
	# Checking dependencies
	$(HC) -M $(HC_OPTS) $(SRCS) $(GENERATORSRCS)






# Proxima directories

PROXIMADIR = ../../proxima
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
	$(EVALUATIONDIR)/CompileHelium.hs \
	$(EVALUATIONDIR)/DocTypes.hs \
	$(EVALUATIONDIR)/DocTypes_Generated.hs \
	$(EVALUATIONDIR)/DocUtils.hs \
	$(EVALUATIONDIR)/DocUtils_Generated.hs \
	$(EVALUATIONDIR)/DocumentEdit.hs \
	$(EVALUATIONDIR)/DocumentEdit_Generated.hs \
	$(EVALUATIONDIR)/EnrTypes.hs \
	$(EVALUATIONDIR)/EnrUtils.hs \
	$(EVALUATIONDIR)/Error.lhs \
	$(EVALUATIONDIR)/EvalLayerTypes.hs \
	$(EVALUATIONDIR)/EvalLayerUtils.hs \
	$(EVALUATIONDIR)/EvalPresent.hs \
	$(EVALUATIONDIR)/EvalTranslate.hs \
	$(EVALUATIONDIR)/EvaluateInv.lhs \
	$(EVALUATIONDIR)/EvaluateTypesStubs.hs \
	$(EVALUATIONDIR)/EvaluateTypes.hs \
	$(EVALUATIONDIR)/GetPut.lhs \
	$(EVALUATIONDIR)/View.lhs \
	$(EVALUATIONDIR)/X.lhs \
	$(EVALUATIONDIR)/XParse.lhs \
	$(EVALUATIONDIR)/XPrelude.lhs
	
LAYOUTSRCS = \
	$(LAYOUTDIR)/LayLayerTypes.hs \
	$(LAYOUTDIR)/LayLayerUtils.hs \
	$(LAYOUTDIR)/LayPresent.hs \
	$(LAYOUTDIR)/LayTranslate.hs \
	$(LAYOUTDIR)/LayTypes.hs \
	$(LAYOUTDIR)/LayUtils.hs \
	$(LAYOUTDIR)/Layout.hs \
	$(LAYOUTDIR)/Scanner.hs \
	$(LAYOUTDIR)/TreeEditPres.hs
	
MAINSRCS = \
	$(MAINDIR)/Architecture.hs \
	$(MAINDIR)/ArchitectureLibM.hs \
	$(MAINDIR)/GUI.hs \
	$(MAINDIR)/Main.hs

PRESENTATIONSRCS = \
	$(PRESENTATIONDIR)/Chess.hs \
	$(PRESENTATIONDIR)/PresLayerTypes.hs \
	$(PRESENTATIONDIR)/PresLayerUtils.hs \
	$(PRESENTATIONDIR)/PresPresent.hs \
	$(PRESENTATIONDIR)/PresTranslate.hs \
	$(PRESENTATIONDIR)/PresTypes.hs \
	$(PRESENTATIONDIR)/PresUtils.hs \
	$(PRESENTATIONDIR)/PresentationAG.hs \
	$(PRESENTATIONDIR)/PresentationParsing.hs \
	$(PRESENTATIONDIR)/Presenter.hs \
	$(PRESENTATIONDIR)/ProxParser.hs \
	$(PRESENTATIONDIR)/ProxParser_Generated.hs \
	$(PRESENTATIONDIR)/XLatex.hs \
	$(PRESENTATIONDIR)/XprezLib.hs
	
RENDERINGSRCS = \
	$(RENDERINGDIR)/RenLayerTypes.hs \
	$(RENDERINGDIR)/RenLayerUtils.hs \
	$(RENDERINGDIR)/RenPresent.hs \
	$(RENDERINGDIR)/RenTranslate.hs \
	$(RENDERINGDIR)/RenTypes.hs \
	$(RENDERINGDIR)/RenUtils.hs \
	$(RENDERINGDIR)/Renderer.hs












# HELIUM



# -add heliumdir to other dirs? otherwise, we must make from the same depth in proxima as we would in helium (for ../.. to work)
# remove Main.hs from helium sources
# Parsec -> ParsecSpecial

HELIUMSRCDIR = ../../heliumNT/src

#directories
PARSECDIR = ../../parsec
PARSECIMPDIR = $(PARSECDIR)/Text/ParserCombinators
LVMLIBDIR  = ../../lvm/src/lib
LVMRUNTIMEDIR  = ../../lvm/src/runtime
TOPDIR = ../../Top/src/Top



HELIUMIFACES	= $(LVMLIBDIR)/common:$(LVMLIBDIR)/common/ghc:$(LVMLIBDIR)/lvm:$(LVMLIBDIR)/asm:$(LVMLIBDIR)/core:$(HELIUMSRCDIR)/utils:$(PARSECDIR):$(HELIUMSRCDIR)/parser:$(HELIUMSRCDIR)/staticanalysis/miscellaneous:$(HELIUMSRCDIR)/staticanalysis/inferencers:$(HELIUMSRCDIR)/staticanalysis/directives:$(HELIUMSRCDIR)/staticanalysis/staticchecks:$(HELIUMSRCDIR)/staticanalysis/heuristics:$(HELIUMSRCDIR)/staticanalysis/messages:$(HELIUMSRCDIR)/syntax:$(HELIUMSRCDIR)/codegeneration:$(HELIUMSRCDIR)/main:$(HELIUMSRCDIR)/modulesystem:$(TOPDIR):../../Top/src




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



# The main target
proxima: $(OBJS) $(GEN)
	@echo "****************************************************"
	@echo "  Proxima: the Proxima generic editor"
	@echo "****************************************************"
	$(HC)	-o $(PROXIMATESTDIR)/$(MAIN) $(HC_OPTS) $(OBJS)


# The Generator
$(GEN): $(GENERATOROBJS)
	ghc -package lang -fglasgow-exts \
	-o $(PROXIMATESTDIR)/generate -i$(GENERATORIFACES) $(GENERATOROBJS)

# Generation 
generate : $(PRESENTATIONDIR)/PresentationAG_Generated.hs

$(PRESENTATIONDIR)/PresentationAG_Generated.hs : $(GEN) $(GENERATORDIR)/DocumentType.hs
	$(GEN) $(PROXIMADIR)/src $(GENERATORDIR)/DocumentType.hs
	
# The AG compiler
# no dependencies, uuagc is assumed not to be modified
$(AGC): 
	ghc --make -o $(AGC) ../../uust/tools/ag/Ag.hs -package util -fglasgow-exts -O2 -i../../uust/tools/ag:../../uust/lib/ddata:../../uust/lib/pretty:../../uust/lib/scanner:../../uust/lib/util:../../uust/lib/parsing


# The presentation AG

# target for only compiling presentationAG, useful when working with GHCi
presenter: $(PRESENTATIONDIR)/PresentationAG.hs
	
# TODO: make dependencies easy to see and to maintain (important for editor builder)

$(PRESENTATIONDIR)/PresentationAG.hs : $(AGC) \
	$(PRESENTATIONDIR)/PresentationAG.ag \
	$(PRESENTATIONDIR)/PresentationAG_Generated.ag \
	$(PRESENTATIONDIR)/InvPresentation.ag \
	$(PRESENTATIONDIR)/LambdaReduce.ag \
	
	@echo "Compiling presentationAG.ag"
	$(AGC) -o $(PRESENTATIONDIR)/PresentationAG.hs \
	-cfmspw --self --module=PresentationAG \
	$(PRESENTATIONDIR)/PresentationAG.ag -P $(PRESENTATIONDIR)

# -m is because otherwise uuagc uses ../../presentation/PresentationAG as module name


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
../../proxima/src/arrangement/ArrPresent.o : ../../proxima/src/arrangement/ArrPresent.hs
../../proxima/src/arrangement/ArrPresent.o : ../../proxima/src/arrangement/Arranger.hi
../../proxima/src/arrangement/ArrPresent.o : ../../proxima/src/arrangement/ArrLayerUtils.hi
../../proxima/src/arrangement/ArrPresent.o : ../../proxima/src/arrangement/ArrLayerTypes.hi
../../proxima/src/arrangement/ArrPresent.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/arrangement/ArrTranslate.o : ../../proxima/src/arrangement/ArrTranslate.hs
../../proxima/src/arrangement/ArrTranslate.o : ../../proxima/src/layout/TreeEditPres.hi
../../proxima/src/arrangement/ArrTranslate.o : ../../proxima/src/evaluation/DocUtils.hi
../../proxima/src/arrangement/ArrTranslate.o : ../../proxima/src/evaluation/DocTypes_Generated.hi
../../proxima/src/arrangement/ArrTranslate.o : ../../proxima/src/evaluation/DocTypes.hi
../../proxima/src/arrangement/ArrTranslate.o : ../../proxima/src/arrangement/ArrLayerUtils.hi
../../proxima/src/arrangement/ArrTranslate.o : ../../proxima/src/arrangement/ArrLayerTypes.hi
../../proxima/src/arrangement/ArrTranslate.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/arrangement/ArrTypes.o : ../../proxima/src/arrangement/ArrTypes.hs
../../proxima/src/arrangement/ArrTypes.o : ../../proxima/src/presentation/PresTypes.hi
../../proxima/src/arrangement/ArrTypes.o : ../../proxima/src/evaluation/DocTypes.hi
../../proxima/src/arrangement/ArrTypes.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/arrangement/ArrUtils.o : ../../proxima/src/arrangement/ArrUtils.hs
../../proxima/src/arrangement/ArrUtils.o : ../../proxima/src/common/CommonUtils.hi
../../proxima/src/arrangement/ArrUtils.o : ../../proxima/src/arrangement/ArrTypes.hi
../../proxima/src/arrangement/ArrUtils.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/arrangement/Arranger.o : ../../proxima/src/arrangement/Arranger.hs
../../proxima/src/arrangement/Arranger.o : ../../proxima/src/arrangement/FontLib.hi
../../proxima/src/arrangement/Arranger.o : ../../proxima/src/arrangement/ArrangerAG.hi
../../proxima/src/arrangement/Arranger.o : ../../proxima/src/arrangement/ArrLayerUtils.hi
../../proxima/src/arrangement/Arranger.o : ../../proxima/src/arrangement/ArrLayerTypes.hi
../../proxima/src/arrangement/Arranger.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/arrangement/ArrangerAG.o : ../../proxima/src/arrangement/ArrangerAG.hs
../../proxima/src/arrangement/ArrangerAG.o : ../../proxima/src/arrangement/FontLib.hi
../../proxima/src/arrangement/ArrangerAG.o : ../../proxima/src/arrangement/ArrLayerUtils.hi
../../proxima/src/arrangement/ArrangerAG.o : ../../proxima/src/arrangement/ArrLayerTypes.hi
../../proxima/src/arrangement/ArrangerAG.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/arrangement/FontLib.o : ../../proxima/src/arrangement/FontLib.hs
../../proxima/src/arrangement/FontLib.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/common/CommonTypes.o : ../../proxima/src/common/CommonTypes.hs
../../proxima/src/common/CommonTypes.o : ../../proxima/src/common/DebugLevels.hi
../../proxima/src/common/CommonUtils.o : ../../proxima/src/common/CommonUtils.hs
../../proxima/src/common/CommonUtils.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/common/DebugLevels.o : ../../proxima/src/common/DebugLevels.hs
../../proxima/src/common/UU_Parsing.o : ../../proxima/src/common/UU_Parsing.hs
../../proxima/src/evaluation/CompileHelium.o : ../../proxima/src/evaluation/CompileHelium.hs
../../proxima/src/evaluation/CompileHelium.o : ../../heliumNT/src/main/CompileUtils.hi
../../proxima/src/evaluation/CompileHelium.o : ../../heliumNT/src/main/Args.hi
../../proxima/src/evaluation/CompileHelium.o : ../../Top/src/Top/Types.hi
../../proxima/src/evaluation/CompileHelium.o : ../../heliumNT/src/syntax/UHA_Syntax.hi
../../proxima/src/evaluation/CompileHelium.o : ../../heliumNT/src/staticanalysis/messages/StaticErrors.hi
../../proxima/src/evaluation/CompileHelium.o : ../../heliumNT/src/staticanalysis/messages/TypeErrors.hi
../../proxima/src/evaluation/CompileHelium.o : ../../heliumNT/src/staticanalysis/inferencers/TypeInferencing.hi
../../proxima/src/evaluation/CompileHelium.o : ../../heliumNT/src/main/PhaseTypingStrategies.hi
../../proxima/src/evaluation/CompileHelium.o : ../../heliumNT/src/main/PhaseStaticChecks.hi
../../proxima/src/evaluation/CompileHelium.o : ../../heliumNT/src/main/PhaseImport.hi
../../proxima/src/evaluation/DocTypes.o : ../../proxima/src/evaluation/DocTypes.hs
../../proxima/src/evaluation/DocTypes.o : ../../proxima/src/presentation/PresTypes.hi
../../proxima/src/evaluation/DocTypes.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/evaluation/DocTypes.o : ../../proxima/src/evaluation/DocTypes_Generated.hi
../../proxima/src/evaluation/DocTypes_Generated.o : ../../proxima/src/evaluation/DocTypes_Generated.hs
../../proxima/src/evaluation/DocTypes_Generated.o : ../../proxima/src/evaluation/DocTypes.hi-boot
../../proxima/src/evaluation/DocTypes_Generated.o : ../../proxima/src/presentation/PresTypes.hi
../../proxima/src/evaluation/DocTypes_Generated.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/evaluation/DocUtils.o : ../../proxima/src/evaluation/DocUtils.hs
../../proxima/src/evaluation/DocUtils.o : ../../proxima/src/presentation/PresTypes.hi
../../proxima/src/evaluation/DocUtils.o : ../../proxima/src/evaluation/DocTypes.hi
../../proxima/src/evaluation/DocUtils.o : ../../proxima/src/evaluation/DocUtils_Generated.hi
../../proxima/src/evaluation/DocUtils_Generated.o : ../../proxima/src/evaluation/DocUtils_Generated.hs
../../proxima/src/evaluation/DocUtils_Generated.o : ../../proxima/src/presentation/PresTypes.hi
../../proxima/src/evaluation/DocUtils_Generated.o : ../../proxima/src/evaluation/DocTypes_Generated.hi
../../proxima/src/evaluation/DocUtils_Generated.o : ../../proxima/src/evaluation/DocTypes.hi
../../proxima/src/evaluation/DocumentEdit.o : ../../proxima/src/evaluation/DocumentEdit.hs
../../proxima/src/evaluation/DocumentEdit.o : ../../proxima/src/evaluation/DocumentEdit_Generated.hi
../../proxima/src/evaluation/DocumentEdit.o : ../../proxima/src/presentation/PresTypes.hi
../../proxima/src/evaluation/DocumentEdit.o : ../../proxima/src/evaluation/DocUtils.hi
../../proxima/src/evaluation/DocumentEdit.o : ../../proxima/src/evaluation/DocTypes_Generated.hi
../../proxima/src/evaluation/DocumentEdit.o : ../../proxima/src/evaluation/DocTypes.hi
../../proxima/src/evaluation/DocumentEdit.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/evaluation/DocumentEdit_Generated.o : ../../proxima/src/evaluation/DocumentEdit_Generated.hs
../../proxima/src/evaluation/DocumentEdit_Generated.o : ../../proxima/src/presentation/PresTypes.hi
../../proxima/src/evaluation/DocumentEdit_Generated.o : ../../proxima/src/evaluation/DocUtils.hi
../../proxima/src/evaluation/DocumentEdit_Generated.o : ../../proxima/src/evaluation/DocTypes_Generated.hi
../../proxima/src/evaluation/DocumentEdit_Generated.o : ../../proxima/src/evaluation/DocTypes.hi
../../proxima/src/evaluation/DocumentEdit_Generated.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/evaluation/EnrTypes.o : ../../proxima/src/evaluation/EnrTypes.hs
../../proxima/src/evaluation/EnrTypes.o : ../../proxima/src/presentation/PresTypes.hi
../../proxima/src/evaluation/EnrTypes.o : ../../proxima/src/common/CommonUtils.hi
../../proxima/src/evaluation/EnrTypes.o : ../../proxima/src/evaluation/DocTypes.hi
../../proxima/src/evaluation/EnrTypes.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/evaluation/EnrUtils.o : ../../proxima/src/evaluation/EnrUtils.hs
../../proxima/src/evaluation/EnrUtils.o : ../../proxima/src/evaluation/DocUtils.hi
../../proxima/src/evaluation/EnrUtils.o : ../../proxima/src/evaluation/DocTypes_Generated.hi
../../proxima/src/evaluation/EnrUtils.o : ../../proxima/src/evaluation/DocTypes.hi
../../proxima/src/evaluation/Error.o : ../../proxima/src/evaluation/Error.lhs
../../proxima/src/evaluation/Error.o : ../../proxima/src/evaluation/X.hi
../../proxima/src/evaluation/EvalLayerTypes.o : ../../proxima/src/evaluation/EvalLayerTypes.hs
../../proxima/src/evaluation/EvalLayerTypes.o : ../../proxima/src/evaluation/EnrTypes.hi
../../proxima/src/evaluation/EvalLayerTypes.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/evaluation/EvalLayerUtils.o : ../../proxima/src/evaluation/EvalLayerUtils.hs
../../proxima/src/evaluation/EvalLayerUtils.o : ../../proxima/src/evaluation/EnrUtils.hi
../../proxima/src/evaluation/EvalLayerUtils.o : ../../proxima/src/common/CommonUtils.hi
../../proxima/src/evaluation/EvalLayerUtils.o : ../../proxima/src/evaluation/EvalLayerTypes.hi
../../proxima/src/evaluation/EvalLayerUtils.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/evaluation/EvalPresent.o : ../../proxima/src/evaluation/EvalPresent.hs
../../proxima/src/evaluation/EvalPresent.o : ../../proxima/src/evaluation/EvaluateInv.hi
../../proxima/src/evaluation/EvalPresent.o : ../../proxima/src/evaluation/EvaluateTypesStubs.hi
../../proxima/src/evaluation/EvalPresent.o : ../../proxima/src/evaluation/DocumentEdit.hi
../../proxima/src/evaluation/EvalPresent.o : ../../proxima/src/evaluation/EvalLayerUtils.hi
../../proxima/src/evaluation/EvalPresent.o : ../../proxima/src/evaluation/EvalLayerTypes.hi
../../proxima/src/evaluation/EvalPresent.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/evaluation/EvalTranslate.o : ../../proxima/src/evaluation/EvalTranslate.hs
../../proxima/src/evaluation/EvalTranslate.o : ../../proxima/src/presentation/PresTypes.hi
../../proxima/src/evaluation/EvalTranslate.o : ../../proxima/src/evaluation/EvalLayerUtils.hi
../../proxima/src/evaluation/EvalTranslate.o : ../../proxima/src/evaluation/EvalLayerTypes.hi
../../proxima/src/evaluation/EvalTranslate.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/evaluation/EvaluateInv.o : ../../proxima/src/evaluation/EvaluateInv.lhs
../../proxima/src/evaluation/EvaluateInv.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/evaluation/EvaluateInv.o : ../../proxima/src/evaluation/DocTypes_Generated.hi
../../proxima/src/evaluation/EvaluateInv.o : ../../proxima/src/evaluation/GetPut.hi
../../proxima/src/evaluation/EvaluateInv.o : ../../proxima/src/evaluation/XPrelude.hi
../../proxima/src/evaluation/EvaluateInv.o : ../../proxima/src/evaluation/XParse.hi
../../proxima/src/evaluation/EvaluateInv.o : ../../proxima/src/evaluation/X.hi
../../proxima/src/evaluation/EvaluateInv.o : ../../proxima/src/evaluation/View.hi
../../proxima/src/evaluation/EvaluateTypesStubs.o : ../../proxima/src/evaluation/EvaluateTypesStubs.hs
../../proxima/src/evaluation/EvaluateTypesStubs.o : ../../heliumNT/src/syntax/UHA_Syntax.hi
../../proxima/src/evaluation/EvaluateTypesStubs.o : ../../proxima/src/common/DebugLevels.hi
../../proxima/src/evaluation/EvaluateTypesStubs.o : ../../proxima/src/evaluation/DocTypes_Generated.hi
../../proxima/src/evaluation/EvaluateTypesStubs.o : ../../proxima/src/evaluation/DocTypes.hi
../../proxima/src/evaluation/EvaluateTypes.o : ../../proxima/src/evaluation/EvaluateTypes.hs
../../proxima/src/evaluation/EvaluateTypes.o : ../../heliumNT/src/syntax/UHA_Pretty.hi
../../proxima/src/evaluation/EvaluateTypes.o : ../../heliumNT/src/staticanalysis/messages/HeliumMessages.hi
../../proxima/src/evaluation/EvaluateTypes.o : ../../heliumNT/src/syntax/UHA_Range.hi
../../proxima/src/evaluation/EvaluateTypes.o : ../../heliumNT/src/syntax/UHA_Utils.hi
../../proxima/src/evaluation/EvaluateTypes.o : ../../Top/src/Top/Types.hi
../../proxima/src/evaluation/EvaluateTypes.o : ../../heliumNT/src/staticanalysis/messages/TypeErrors.hi
../../proxima/src/evaluation/EvaluateTypes.o : ../../heliumNT/src/staticanalysis/messages/StaticErrors.hi
../../proxima/src/evaluation/EvaluateTypes.o : ../../heliumNT/src/utils/Utils.hi
../../proxima/src/evaluation/EvaluateTypes.o : ../../proxima/src/evaluation/CompileHelium.hi
../../proxima/src/evaluation/EvaluateTypes.o : ../../heliumNT/src/syntax/UHA_Syntax.hi
../../proxima/src/evaluation/EvaluateTypes.o : ../../proxima/src/evaluation/DocumentEdit.hi
../../proxima/src/evaluation/EvaluateTypes.o : ../../proxima/src/common/DebugLevels.hi
../../proxima/src/evaluation/EvaluateTypes.o : ../../proxima/src/evaluation/DocTypes_Generated.hi
../../proxima/src/evaluation/EvaluateTypes.o : ../../proxima/src/evaluation/DocTypes.hi
../../proxima/src/evaluation/GetPut.o : ../../proxima/src/evaluation/GetPut.lhs
../../proxima/src/evaluation/GetPut.o : ../../proxima/src/evaluation/Error.hi
../../proxima/src/evaluation/GetPut.o : ../../proxima/src/evaluation/X.hi
../../proxima/src/evaluation/GetPut.o : ../../proxima/src/evaluation/View.hi
../../proxima/src/evaluation/View.o : ../../proxima/src/evaluation/View.lhs
../../proxima/src/evaluation/X.o : ../../proxima/src/evaluation/X.lhs
../../proxima/src/evaluation/XParse.o : ../../proxima/src/evaluation/XParse.lhs
../../proxima/src/evaluation/XParse.o : ../../proxima/src/evaluation/View.hi
../../proxima/src/evaluation/XParse.o : ../../proxima/src/evaluation/X.hi
../../proxima/src/evaluation/XPrelude.o : ../../proxima/src/evaluation/XPrelude.lhs
../../proxima/src/evaluation/XPrelude.o : ../../proxima/src/evaluation/X.hi
../../proxima/src/evaluation/XPrelude.o : ../../proxima/src/evaluation/View.hi
../../proxima/src/layout/LayLayerTypes.o : ../../proxima/src/layout/LayLayerTypes.hs
../../proxima/src/layout/LayLayerTypes.o : ../../proxima/src/layout/LayTypes.hi
../../proxima/src/layout/LayLayerTypes.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/layout/LayLayerUtils.o : ../../proxima/src/layout/LayLayerUtils.hs
../../proxima/src/layout/LayLayerUtils.o : ../../proxima/src/layout/LayUtils.hi
../../proxima/src/layout/LayLayerUtils.o : ../../proxima/src/common/CommonUtils.hi
../../proxima/src/layout/LayLayerUtils.o : ../../proxima/src/layout/LayLayerTypes.hi
../../proxima/src/layout/LayLayerUtils.o : ../../proxima/src/common/CommonTypes.hi
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
../../proxima/src/layout/LayTypes.o : ../../proxima/src/layout/LayTypes.hs
../../proxima/src/layout/LayTypes.o : ../../proxima/src/evaluation/DocTypes.hi
../../proxima/src/layout/LayTypes.o : ../../proxima/src/common/CommonUtils.hi
../../proxima/src/layout/LayTypes.o : ../../proxima/src/presentation/PresTypes.hi
../../proxima/src/layout/LayTypes.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/layout/LayUtils.o : ../../proxima/src/layout/LayUtils.hs
../../proxima/src/layout/LayUtils.o : ../../proxima/src/presentation/PresUtils.hi
../../proxima/src/layout/Layout.o : ../../proxima/src/layout/Layout.hs
../../proxima/src/layout/Layout.o : ../../proxima/src/layout/LayLayerUtils.hi
../../proxima/src/layout/Layout.o : ../../proxima/src/layout/LayLayerTypes.hi
../../proxima/src/layout/Layout.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/layout/Scanner.o : ../../proxima/src/layout/Scanner.hs
../../proxima/src/layout/Scanner.o : ../../proxima/src/evaluation/DocTypes_Generated.hi
../../proxima/src/layout/Scanner.o : ../../proxima/src/layout/LayLayerUtils.hi
../../proxima/src/layout/Scanner.o : ../../proxima/src/layout/LayLayerTypes.hi
../../proxima/src/layout/Scanner.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/layout/TreeEditPres.o : ../../proxima/src/layout/TreeEditPres.hs
../../proxima/src/layout/TreeEditPres.o : ../../proxima/src/presentation/XprezLib.hi
../../proxima/src/layout/TreeEditPres.o : ../../proxima/src/presentation/PresUtils.hi
../../proxima/src/layout/TreeEditPres.o : ../../proxima/src/presentation/PresTypes.hi
../../proxima/src/layout/TreeEditPres.o : ../../proxima/src/evaluation/DocTypes.hi
../../proxima/src/layout/TreeEditPres.o : ../../proxima/src/common/CommonTypes.hi
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
../../proxima/src/main/Architecture.o : ../../proxima/src/presentation/PresentationAG.hi
../../proxima/src/main/ArchitectureLibM.o : ../../proxima/src/main/ArchitectureLibM.hs
../../proxima/src/main/GUI.o : ../../proxima/src/main/GUI.hs
../../proxima/src/main/GUI.o : ../../proxima/src/common/CommonUtils.hi
../../proxima/src/main/GUI.o : ../../proxima/src/rendering/RenTypes.hi
../../proxima/src/main/GUI.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/main/Main.o : ../../proxima/src/main/Main.hs
../../proxima/src/main/Main.o : ../../proxima/src/evaluation/EvalLayerTypes.hi
../../proxima/src/main/Main.o : ../../proxima/src/arrangement/ArrUtils.hi
../../proxima/src/main/Main.o : ../../proxima/src/layout/Scanner.hi
../../proxima/src/main/Main.o : ../../proxima/src/layout/Layout.hi
../../proxima/src/main/Main.o : ../../proxima/src/presentation/PresentationParsing.hi
../../proxima/src/main/Main.o : ../../proxima/src/presentation/PresentationAG.hi
../../proxima/src/main/Main.o : ../../proxima/src/presentation/ProxParser.hi
../../proxima/src/main/Main.o : ../../proxima/src/arrangement/FontLib.hi
../../proxima/src/main/Main.o : ../../proxima/src/rendering/RenTypes.hi
../../proxima/src/main/Main.o : ../../proxima/src/arrangement/ArrTypes.hi
../../proxima/src/main/Main.o : ../../proxima/src/layout/LayTypes.hi
../../proxima/src/main/Main.o : ../../proxima/src/presentation/PresUtils.hi
../../proxima/src/main/Main.o : ../../proxima/src/presentation/PresTypes.hi
../../proxima/src/main/Main.o : ../../proxima/src/evaluation/EnrTypes.hi
../../proxima/src/main/Main.o : ../../proxima/src/evaluation/DocTypes.hi
../../proxima/src/main/Main.o : ../../proxima/src/presentation/PresPresent.hi
../../proxima/src/main/Main.o : ../../proxima/src/evaluation/DocUtils.hi
../../proxima/src/main/Main.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/main/Main.o : ../../proxima/src/main/GUI.hi
../../proxima/src/main/Main.o : ../../proxima/src/main/Architecture.hi
../../proxima/src/presentation/Chess.o : ../../proxima/src/presentation/Chess.hs
../../proxima/src/presentation/Chess.o : ../../proxima/src/evaluation/DocumentEdit.hi
../../proxima/src/presentation/Chess.o : ../../proxima/src/presentation/PresTypes.hi
../../proxima/src/presentation/Chess.o : ../../proxima/src/presentation/XprezLib.hi
../../proxima/src/presentation/Chess.o : ../../proxima/src/evaluation/DocUtils.hi
../../proxima/src/presentation/Chess.o : ../../proxima/src/evaluation/DocTypes.hi
../../proxima/src/presentation/Chess.o : ../../proxima/src/evaluation/DocTypes_Generated.hi
../../proxima/src/presentation/Chess.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/presentation/PresLayerTypes.o : ../../proxima/src/presentation/PresLayerTypes.hs
../../proxima/src/presentation/PresLayerTypes.o : ../../proxima/src/presentation/PresTypes.hi
../../proxima/src/presentation/PresLayerTypes.o : ../../proxima/src/evaluation/EnrTypes.hi
../../proxima/src/presentation/PresLayerTypes.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/presentation/PresLayerUtils.o : ../../proxima/src/presentation/PresLayerUtils.hs
../../proxima/src/presentation/PresLayerUtils.o : ../../proxima/src/presentation/PresUtils.hi
../../proxima/src/presentation/PresLayerUtils.o : ../../proxima/src/evaluation/EnrUtils.hi
../../proxima/src/presentation/PresLayerUtils.o : ../../proxima/src/common/CommonUtils.hi
../../proxima/src/presentation/PresLayerUtils.o : ../../proxima/src/presentation/PresLayerTypes.hi
../../proxima/src/presentation/PresLayerUtils.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/presentation/PresPresent.o : ../../proxima/src/presentation/PresPresent.hs
../../proxima/src/presentation/PresPresent.o : ../../proxima/src/presentation/Presenter.hi
../../proxima/src/presentation/PresPresent.o : ../../proxima/src/presentation/PresLayerUtils.hi
../../proxima/src/presentation/PresPresent.o : ../../proxima/src/presentation/PresLayerTypes.hi
../../proxima/src/presentation/PresPresent.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/presentation/PresTranslate.o : ../../proxima/src/presentation/PresTranslate.hs
../../proxima/src/presentation/PresTranslate.o : ../../proxima/src/presentation/PresLayerUtils.hi
../../proxima/src/presentation/PresTranslate.o : ../../proxima/src/presentation/PresLayerTypes.hi
../../proxima/src/presentation/PresTranslate.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/presentation/PresTypes.o : ../../proxima/src/presentation/PresTypes.hs
../../proxima/src/presentation/PresTypes.o : ../../proxima/src/evaluation/DocTypes.hi-boot
../../proxima/src/presentation/PresTypes.o : ../../proxima/src/common/CommonUtils.hi
../../proxima/src/presentation/PresTypes.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/presentation/PresUtils.o : ../../proxima/src/presentation/PresUtils.hs
../../proxima/src/presentation/PresUtils.o : ../../proxima/src/presentation/XprezLib.hi
../../proxima/src/presentation/PresUtils.o : ../../proxima/src/common/CommonUtils.hi
../../proxima/src/presentation/PresUtils.o : ../../proxima/src/presentation/PresTypes.hi
../../proxima/src/presentation/PresUtils.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/presentation/PresentationAG.o : ../../proxima/src/presentation/PresentationAG.hs
../../proxima/src/presentation/PresentationAG.o : ../../proxima/src/evaluation/DocTypes_Generated.hi
../../proxima/src/presentation/PresentationAG.o : ../../proxima/src/evaluation/DocTypes.hi
../../proxima/src/presentation/PresentationAG.o : ../../proxima/src/presentation/Chess.hi
../../proxima/src/presentation/PresentationAG.o : ../../proxima/src/evaluation/DocumentEdit.hi
../../proxima/src/presentation/PresentationAG.o : ../../proxima/src/presentation/XLatex.hi
../../proxima/src/presentation/PresentationAG.o : ../../proxima/src/presentation/XprezLib.hi
../../proxima/src/presentation/PresentationAG.o : ../../proxima/src/presentation/PresLayerUtils.hi
../../proxima/src/presentation/PresentationAG.o : ../../proxima/src/presentation/PresLayerTypes.hi
../../proxima/src/presentation/PresentationAG.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/presentation/PresentationParsing.o : ../../proxima/src/presentation/PresentationParsing.hs
../../proxima/src/presentation/PresentationParsing.o : ../../proxima/src/common/UU_Parsing.hi
../../proxima/src/presentation/PresentationParsing.o : ../../proxima/src/presentation/XprezLib.hi
../../proxima/src/presentation/PresentationParsing.o : ../../proxima/src/evaluation/DocumentEdit.hi
../../proxima/src/presentation/PresentationParsing.o : ../../proxima/src/presentation/PresTypes.hi
../../proxima/src/presentation/PresentationParsing.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/presentation/Presenter.o : ../../proxima/src/presentation/Presenter.hs
../../proxima/src/presentation/Presenter.o : ../../proxima/src/layout/TreeEditPres.hi
../../proxima/src/presentation/Presenter.o : ../../proxima/src/presentation/XprezLib.hi
../../proxima/src/presentation/Presenter.o : ../../proxima/src/presentation/PresLayerUtils.hi
../../proxima/src/presentation/Presenter.o : ../../proxima/src/presentation/PresLayerTypes.hi
../../proxima/src/presentation/Presenter.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/presentation/ProxParser.o : ../../proxima/src/presentation/ProxParser.hs
../../proxima/src/presentation/ProxParser.o : ../../proxima/src/evaluation/DocTypes_Generated.hi
../../proxima/src/presentation/ProxParser.o : ../../proxima/src/common/UU_Parsing.hi
../../proxima/src/presentation/ProxParser.o : ../../proxima/src/evaluation/DocumentEdit.hi
../../proxima/src/presentation/ProxParser.o : ../../proxima/src/presentation/ProxParser_Generated.hi
../../proxima/src/presentation/ProxParser.o : ../../proxima/src/presentation/XprezLib.hi
../../proxima/src/presentation/ProxParser.o : ../../proxima/src/presentation/PresentationParsing.hi
../../proxima/src/presentation/ProxParser.o : ../../proxima/src/presentation/PresLayerUtils.hi
../../proxima/src/presentation/ProxParser.o : ../../proxima/src/presentation/PresLayerTypes.hi
../../proxima/src/presentation/ProxParser.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/presentation/ProxParser_Generated.o : ../../proxima/src/presentation/ProxParser_Generated.hs
../../proxima/src/presentation/ProxParser_Generated.o : ../../proxima/src/evaluation/DocTypes_Generated.hi
../../proxima/src/presentation/ProxParser_Generated.o : ../../proxima/src/evaluation/DocTypes.hi
../../proxima/src/presentation/ProxParser_Generated.o : ../../proxima/src/evaluation/DocumentEdit.hi
../../proxima/src/presentation/ProxParser_Generated.o : ../../proxima/src/presentation/PresLayerUtils.hi
../../proxima/src/presentation/ProxParser_Generated.o : ../../proxima/src/presentation/PresLayerTypes.hi
../../proxima/src/presentation/ProxParser_Generated.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/presentation/XLatex.o : ../../proxima/src/presentation/XLatex.hs
../../proxima/src/presentation/XLatex.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/presentation/XLatex.o : ../../proxima/src/presentation/PresTypes.hi
../../proxima/src/presentation/XLatex.o : ../../proxima/src/presentation/XprezLib.hi
../../proxima/src/presentation/XprezLib.o : ../../proxima/src/presentation/XprezLib.hs
../../proxima/src/presentation/XprezLib.o : ../../proxima/src/presentation/PresTypes.hi
../../proxima/src/presentation/XprezLib.o : ../../proxima/src/evaluation/DocTypes.hi
../../proxima/src/presentation/XprezLib.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/rendering/RenLayerTypes.o : ../../proxima/src/rendering/RenLayerTypes.hs
../../proxima/src/rendering/RenLayerTypes.o : ../../proxima/src/rendering/RenTypes.hi
../../proxima/src/rendering/RenLayerTypes.o : ../../proxima/src/arrangement/ArrTypes.hi
../../proxima/src/rendering/RenLayerTypes.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/rendering/RenLayerUtils.o : ../../proxima/src/rendering/RenLayerUtils.hs
../../proxima/src/rendering/RenLayerUtils.o : ../../proxima/src/rendering/RenUtils.hi
../../proxima/src/rendering/RenLayerUtils.o : ../../proxima/src/arrangement/ArrUtils.hi
../../proxima/src/rendering/RenLayerUtils.o : ../../proxima/src/rendering/RenLayerTypes.hi
../../proxima/src/rendering/RenLayerUtils.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/rendering/RenPresent.o : ../../proxima/src/rendering/RenPresent.hs
../../proxima/src/rendering/RenPresent.o : ../../proxima/src/evaluation/DocTypes.hi
../../proxima/src/rendering/RenPresent.o : ../../proxima/src/evaluation/DocTypes_Generated.hi
../../proxima/src/rendering/RenPresent.o : ../../proxima/src/rendering/Renderer.hi
../../proxima/src/rendering/RenPresent.o : ../../proxima/src/rendering/RenLayerUtils.hi
../../proxima/src/rendering/RenPresent.o : ../../proxima/src/rendering/RenLayerTypes.hi
../../proxima/src/rendering/RenPresent.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/rendering/RenTranslate.o : ../../proxima/src/rendering/RenTranslate.hs
../../proxima/src/rendering/RenTranslate.o : ../../proxima/src/rendering/RenLayerUtils.hi
../../proxima/src/rendering/RenTranslate.o : ../../proxima/src/rendering/RenLayerTypes.hi
../../proxima/src/rendering/RenTranslate.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/rendering/RenTypes.o : ../../proxima/src/rendering/RenTypes.hs
../../proxima/src/rendering/RenTypes.o : ../../proxima/src/evaluation/DocTypes.hi
../../proxima/src/rendering/RenTypes.o : ../../proxima/src/common/CommonUtils.hi
../../proxima/src/rendering/RenTypes.o : ../../proxima/src/common/CommonTypes.hi
../../proxima/src/rendering/RenUtils.o : ../../proxima/src/rendering/RenUtils.hs
../../proxima/src/rendering/RenUtils.o : ../../proxima/src/rendering/RenTypes.hi
../../proxima/src/rendering/RenUtils.o : ../../proxima/src/common/CommonTypes.hi
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
../../heliumNT/src/parser/CollectFunctionBindings.o : ../../heliumNT/src/parser/CollectFunctionBindings.hs
../../heliumNT/src/parser/CollectFunctionBindings.o : ../../heliumNT/src/utils/Utils.hi
../../heliumNT/src/parser/CollectFunctionBindings.o : ../../heliumNT/src/syntax/UHA_Range.hi
../../heliumNT/src/parser/CollectFunctionBindings.o : ../../heliumNT/src/syntax/UHA_Utils.hi
../../heliumNT/src/parser/CollectFunctionBindings.o : ../../heliumNT/src/syntax/UHA_Syntax.hi
../../heliumNT/src/parser/OperatorTable.o : ../../heliumNT/src/parser/OperatorTable.hs
../../heliumNT/src/parser/OperatorTable.o : ../../heliumNT/src/syntax/UHA_Utils.hi
../../heliumNT/src/parser/OperatorTable.o : ../../heliumNT/src/syntax/UHA_Syntax.hi
../../heliumNT/src/parser/Parser.o : ../../heliumNT/src/parser/Parser.hs
../../heliumNT/src/parser/Parser.o : ../../heliumNT/src/utils/Utils.hi
../../heliumNT/src/parser/Parser.o : ../../heliumNT/src/parser/CollectFunctionBindings.hi
../../heliumNT/src/parser/Parser.o : ../../heliumNT/src/syntax/UHA_Range.hi
../../heliumNT/src/parser/Parser.o : ../../heliumNT/src/syntax/UHA_Utils.hi
../../heliumNT/src/parser/Parser.o : ../../heliumNT/src/syntax/UHA_Syntax.hi
../../heliumNT/src/parser/Parser.o : ../../heliumNT/src/utils/Texts.hi
../../heliumNT/src/parser/Parser.o : ../../heliumNT/src/parser/LayoutRule.hi
../../heliumNT/src/parser/Parser.o : ../../heliumNT/src/parser/Lexer.hi
../../heliumNT/src/parser/Parser.o : ../../parsec/Text/ParserCombinators/ParsecSpecial/Pos.hi
../../heliumNT/src/parser/Parser.o : ../../parsec/Text/ParserCombinators/ParsecSpecial.hi
../../heliumNT/src/parser/Parser.o : ../../heliumNT/src/parser/ParseLibrary.hi
../../heliumNT/src/parser/ResolveOperators.o : ../../heliumNT/src/parser/ResolveOperators.hs
../../heliumNT/src/parser/ResolveOperators.o : ../../heliumNT/src/staticanalysis/messages/Messages.hi
../../heliumNT/src/parser/ResolveOperators.o : ../../heliumNT/src/utils/Utils.hi
../../heliumNT/src/parser/ResolveOperators.o : ../../heliumNT/src/parser/OperatorTable.hi
../../heliumNT/src/parser/ResolveOperators.o : ../../heliumNT/src/syntax/UHA_Range.hi
../../heliumNT/src/parser/ResolveOperators.o : ../../heliumNT/src/syntax/UHA_Syntax.hi
../../heliumNT/src/parser/ResolveOperators.o : ../../heliumNT/src/syntax/UHA_Utils.hi
../../heliumNT/src/parser/Lexer.o : ../../heliumNT/src/parser/Lexer.hs
../../heliumNT/src/parser/Lexer.o : ../../heliumNT/src/utils/Utils.hi
../../heliumNT/src/parser/Lexer.o : ../../parsec/Text/ParserCombinators/ParsecSpecial/Pos.hi
../../heliumNT/src/parser/Lexer.o : ../../heliumNT/src/parser/LexerToken.hi
../../heliumNT/src/parser/Lexer.o : ../../heliumNT/src/parser/LexerMessage.hi
../../heliumNT/src/parser/Lexer.o : ../../heliumNT/src/parser/LexerMonad.hi
../../heliumNT/src/parser/ParseLibrary.o : ../../heliumNT/src/parser/ParseLibrary.hs
../../heliumNT/src/parser/ParseLibrary.o : ../../heliumNT/src/utils/Texts.hi
../../heliumNT/src/parser/ParseLibrary.o : ../../heliumNT/src/syntax/UHA_Utils.hi
../../heliumNT/src/parser/ParseLibrary.o : ../../heliumNT/src/syntax/UHA_Syntax.hi
../../heliumNT/src/parser/ParseLibrary.o : ../../heliumNT/src/parser/Lexer.hi
../../heliumNT/src/parser/ParseLibrary.o : ../../parsec/Text/ParserCombinators/ParsecSpecial/Pos.hi
../../heliumNT/src/parser/ParseLibrary.o : ../../parsec/Text/ParserCombinators/ParsecSpecial.hi
../../heliumNT/src/parser/ParseMessage.o : ../../heliumNT/src/parser/ParseMessage.hs
../../heliumNT/src/parser/ParseMessage.o : ../../parsec/Text/ParserCombinators/ParsecSpecial/Pos.hi
../../heliumNT/src/parser/ParseMessage.o : ../../parsec/Text/ParserCombinators/ParsecSpecial/Error.hi
../../heliumNT/src/parser/ParseMessage.o : ../../heliumNT/src/utils/Texts.hi
../../heliumNT/src/parser/ParseMessage.o : ../../heliumNT/src/syntax/UHA_Syntax.hi
../../heliumNT/src/parser/ParseMessage.o : ../../heliumNT/src/staticanalysis/messages/Messages.hi
../../heliumNT/src/parser/LexerMonad.o : ../../heliumNT/src/parser/LexerMonad.hs
../../heliumNT/src/parser/LexerMonad.o : ../../parsec/Text/ParserCombinators/ParsecSpecial/Pos.hi
../../heliumNT/src/parser/LexerMonad.o : ../../heliumNT/src/parser/LexerMessage.hi
../../heliumNT/src/parser/LexerMessage.o : ../../heliumNT/src/parser/LexerMessage.hs
../../heliumNT/src/parser/LexerMessage.o : ../../heliumNT/src/utils/Texts.hi
../../heliumNT/src/parser/LexerMessage.o : ../../heliumNT/src/staticanalysis/messages/Messages.hi
../../heliumNT/src/parser/LexerMessage.o : ../../heliumNT/src/syntax/UHA_Syntax.hi
../../heliumNT/src/parser/LexerMessage.o : ../../parsec/Text/ParserCombinators/ParsecSpecial/Pos.hi
../../heliumNT/src/parser/LayoutRule.o : ../../heliumNT/src/parser/LayoutRule.hs
../../heliumNT/src/parser/LayoutRule.o : ../../parsec/Text/ParserCombinators/ParsecSpecial/Pos.hi
../../heliumNT/src/parser/LayoutRule.o : ../../heliumNT/src/parser/LexerToken.hi
../../heliumNT/src/parser/LexerToken.o : ../../heliumNT/src/parser/LexerToken.hs
../../heliumNT/src/parser/LexerToken.o : ../../heliumNT/src/utils/Texts.hi
../../heliumNT/src/parser/LexerToken.o : ../../parsec/Text/ParserCombinators/ParsecSpecial/Pos.hi
../../heliumNT/src/staticanalysis/messages/Messages.o : ../../heliumNT/src/staticanalysis/messages/Messages.hs
../../heliumNT/src/staticanalysis/messages/Messages.o : ../../heliumNT/src/utils/Utils.hi
../../heliumNT/src/staticanalysis/messages/Messages.o : ../../heliumNT/src/utils/Similarity.hi
../../heliumNT/src/staticanalysis/messages/Messages.o : ../../heliumNT/src/utils/OneLiner.hi
../../heliumNT/src/staticanalysis/messages/Messages.o : ../../Top/src/Top/Types.hi
../../heliumNT/src/staticanalysis/messages/Messages.o : ../../heliumNT/src/syntax/UHA_Range.hi
../../heliumNT/src/staticanalysis/messages/Messages.o : ../../heliumNT/src/syntax/UHA_Utils.hi
../../heliumNT/src/staticanalysis/messages/Messages.o : ../../heliumNT/src/syntax/UHA_Syntax.hi
../../heliumNT/src/staticanalysis/messages/HeliumMessages.o : ../../heliumNT/src/staticanalysis/messages/HeliumMessages.hs
../../heliumNT/src/staticanalysis/messages/HeliumMessages.o : ../../heliumNT/src/syntax/UHA_Range.hi
../../heliumNT/src/staticanalysis/messages/HeliumMessages.o : ../../heliumNT/src/staticanalysis/miscellaneous/TypesToAlignedDocs.hi
../../heliumNT/src/staticanalysis/messages/HeliumMessages.o : ../../heliumNT/src/utils/OneLiner.hi
../../heliumNT/src/staticanalysis/messages/HeliumMessages.o : ../../lvm/src/lib/common/PPrint.hi
../../heliumNT/src/staticanalysis/messages/HeliumMessages.o : ../../Top/src/Top/Types.hi
../../heliumNT/src/staticanalysis/messages/HeliumMessages.o : ../../heliumNT/src/staticanalysis/messages/Messages.hi
../../heliumNT/src/staticanalysis/messages/StaticErrors.o : ../../heliumNT/src/staticanalysis/messages/StaticErrors.hs
../../heliumNT/src/staticanalysis/messages/StaticErrors.o : ../../Top/src/Top/Types.hi
../../heliumNT/src/staticanalysis/messages/StaticErrors.o : ../../heliumNT/src/utils/Utils.hi
../../heliumNT/src/staticanalysis/messages/StaticErrors.o : ../../heliumNT/src/staticanalysis/messages/Messages.hi
../../heliumNT/src/staticanalysis/messages/StaticErrors.o : ../../heliumNT/src/syntax/UHA_Range.hi
../../heliumNT/src/staticanalysis/messages/StaticErrors.o : ../../heliumNT/src/syntax/UHA_Utils.hi
../../heliumNT/src/staticanalysis/messages/StaticErrors.o : ../../heliumNT/src/syntax/UHA_Syntax.hi
../../heliumNT/src/staticanalysis/messages/TypeErrors.o : ../../heliumNT/src/staticanalysis/messages/TypeErrors.hs
../../heliumNT/src/staticanalysis/messages/TypeErrors.o : ../../heliumNT/src/staticanalysis/miscellaneous/UHA_Source.hi
../../heliumNT/src/staticanalysis/messages/TypeErrors.o : ../../heliumNT/src/syntax/UHA_Range.hi
../../heliumNT/src/staticanalysis/messages/TypeErrors.o : ../../heliumNT/src/syntax/UHA_Syntax.hi
../../heliumNT/src/staticanalysis/messages/TypeErrors.o : ../../heliumNT/src/utils/OneLiner.hi
../../heliumNT/src/staticanalysis/messages/TypeErrors.o : ../../Top/src/Top/Types.hi
../../heliumNT/src/staticanalysis/messages/TypeErrors.o : ../../heliumNT/src/staticanalysis/messages/Messages.hi
../../heliumNT/src/staticanalysis/messages/KindErrors.o : ../../heliumNT/src/staticanalysis/messages/KindErrors.hs
../../heliumNT/src/staticanalysis/messages/KindErrors.o : ../../lvm/src/lib/common/PPrint.hi
../../heliumNT/src/staticanalysis/messages/KindErrors.o : ../../heliumNT/src/syntax/UHA_Pretty.hi
../../heliumNT/src/staticanalysis/messages/KindErrors.o : ../../heliumNT/src/staticanalysis/messages/Messages.hi
../../heliumNT/src/staticanalysis/messages/KindErrors.o : ../../heliumNT/src/syntax/UHA_Syntax.hi
../../heliumNT/src/staticanalysis/messages/KindErrors.o : ../../Top/src/Top/Types.hi
../../heliumNT/src/staticanalysis/messages/Warnings.o : ../../heliumNT/src/staticanalysis/messages/Warnings.hs
../../heliumNT/src/staticanalysis/messages/Warnings.o : ../../heliumNT/src/syntax/UHA_Pretty.hi
../../heliumNT/src/staticanalysis/messages/Warnings.o : ../../heliumNT/src/utils/Utils.hi
../../heliumNT/src/staticanalysis/messages/Warnings.o : ../../heliumNT/src/staticanalysis/messages/Messages.hi
../../heliumNT/src/staticanalysis/messages/Warnings.o : ../../Top/src/Top/Types.hi
../../heliumNT/src/staticanalysis/messages/Warnings.o : ../../heliumNT/src/syntax/UHA_Utils.hi
../../heliumNT/src/staticanalysis/messages/Warnings.o : ../../heliumNT/src/syntax/UHA_Syntax.hi
../../heliumNT/src/staticanalysis/messages/Warnings.o : ../../heliumNT/src/syntax/UHA_Range.hi
../../heliumNT/src/staticanalysis/heuristics/HeuristicsInfo.o : ../../heliumNT/src/staticanalysis/heuristics/HeuristicsInfo.hs
../../heliumNT/src/staticanalysis/heuristics/HeuristicsInfo.o : ../../Top/src/Top/TypeGraph/Heuristics.hi
../../heliumNT/src/staticanalysis/heuristics/HeuristicsInfo.o : ../../Top/src/Top/Types.hi
../../heliumNT/src/staticanalysis/heuristics/HeuristicsInfo.o : ../../heliumNT/src/utils/Utils.hi
../../heliumNT/src/staticanalysis/heuristics/HeuristicsInfo.o : ../../heliumNT/src/staticanalysis/messages/TypeErrors.hi
../../heliumNT/src/staticanalysis/heuristics/HeuristicsInfo.o : ../../heliumNT/src/staticanalysis/messages/HeliumMessages.hi
../../heliumNT/src/staticanalysis/heuristics/HeuristicsInfo.o : ../../heliumNT/src/staticanalysis/messages/Messages.hi
../../heliumNT/src/staticanalysis/heuristics/HeuristicsInfo.o : ../../heliumNT/src/syntax/UHA_Syntax.hi
../../heliumNT/src/staticanalysis/heuristics/HeuristicsInfo.o : ../../heliumNT/src/staticanalysis/miscellaneous/UHA_Source.hi
../../heliumNT/src/staticanalysis/heuristics/HeuristicsInfo.o : ../../heliumNT/src/utils/OneLiner.hi
../../heliumNT/src/staticanalysis/heuristics/HeuristicsInfo.o : ../../heliumNT/src/staticanalysis/miscellaneous/DoublyLinkedTree.hi
../../heliumNT/src/staticanalysis/heuristics/HeuristicsInfo.o : ../../heliumNT/src/staticanalysis/heuristics/UnifierHeuristics.hi
../../heliumNT/src/staticanalysis/heuristics/HeuristicsInfo.o : ../../heliumNT/src/staticanalysis/heuristics/OnlyResultHeuristics.hi
../../heliumNT/src/staticanalysis/heuristics/HeuristicsInfo.o : ../../heliumNT/src/staticanalysis/heuristics/TieBreakerHeuristics.hi
../../heliumNT/src/staticanalysis/heuristics/HeuristicsInfo.o : ../../heliumNT/src/staticanalysis/heuristics/RepairHeuristics.hi
../../heliumNT/src/staticanalysis/heuristics/HeuristicsInfo.o : ../../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.hi
../../heliumNT/src/staticanalysis/heuristics/ListOfHeuristics.o : ../../heliumNT/src/staticanalysis/heuristics/ListOfHeuristics.hs
../../heliumNT/src/staticanalysis/heuristics/ListOfHeuristics.o : ../../Top/src/Top/TypeGraph/Basics.hi
../../heliumNT/src/staticanalysis/heuristics/ListOfHeuristics.o : ../../Top/src/Top/TypeGraph/Paths.hi
../../heliumNT/src/staticanalysis/heuristics/ListOfHeuristics.o : ../../Top/src/Top/TypeGraph/TypeGraphState.hi
../../heliumNT/src/staticanalysis/heuristics/ListOfHeuristics.o : ../../heliumNT/src/staticanalysis/heuristics/TieBreakerHeuristics.hi
../../heliumNT/src/staticanalysis/heuristics/ListOfHeuristics.o : ../../heliumNT/src/staticanalysis/heuristics/OnlyResultHeuristics.hi
../../heliumNT/src/staticanalysis/heuristics/ListOfHeuristics.o : ../../heliumNT/src/staticanalysis/heuristics/UnifierHeuristics.hi
../../heliumNT/src/staticanalysis/heuristics/ListOfHeuristics.o : ../../heliumNT/src/staticanalysis/heuristics/RepairHeuristics.hi
../../heliumNT/src/staticanalysis/heuristics/ListOfHeuristics.o : ../../Top/src/Top/TypeGraph/DefaultHeuristics.hi
../../heliumNT/src/staticanalysis/heuristics/ListOfHeuristics.o : ../../Top/src/Top/TypeGraph/Heuristics.hi
../../heliumNT/src/staticanalysis/heuristics/ListOfHeuristics.o : ../../Top/src/Top/Types.hi
../../heliumNT/src/staticanalysis/heuristics/ListOfHeuristics.o : ../../heliumNT/src/staticanalysis/heuristics/HeuristicsInfo.hi
../../heliumNT/src/staticanalysis/heuristics/ListOfHeuristics.o : ../../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.hi
../../heliumNT/src/staticanalysis/heuristics/ListOfHeuristics.o : ../../heliumNT/src/main/Args.hi
../../heliumNT/src/staticanalysis/heuristics/OnlyResultHeuristics.o : ../../heliumNT/src/staticanalysis/heuristics/OnlyResultHeuristics.hs
../../heliumNT/src/staticanalysis/heuristics/OnlyResultHeuristics.o : ../../heliumNT/src/staticanalysis/miscellaneous/UHA_Source.hi
../../heliumNT/src/staticanalysis/heuristics/OnlyResultHeuristics.o : ../../heliumNT/src/syntax/UHA_Syntax.hi
../../heliumNT/src/staticanalysis/heuristics/OnlyResultHeuristics.o : ../../heliumNT/src/utils/OneLiner.hi
../../heliumNT/src/staticanalysis/heuristics/OnlyResultHeuristics.o : ../../Top/src/Top/Types.hi
../../heliumNT/src/staticanalysis/heuristics/OnlyResultHeuristics.o : ../../Top/src/Top/States/TIState.hi
../../heliumNT/src/staticanalysis/heuristics/OnlyResultHeuristics.o : ../../Top/src/Top/TypeGraph/Heuristics.hi
../../heliumNT/src/staticanalysis/heuristics/RepairHeuristics.o : ../../heliumNT/src/staticanalysis/heuristics/RepairHeuristics.hs
../../heliumNT/src/staticanalysis/heuristics/RepairHeuristics.o : ../../heliumNT/src/syntax/UHA_Syntax.hi
../../heliumNT/src/staticanalysis/heuristics/RepairHeuristics.o : ../../heliumNT/src/staticanalysis/miscellaneous/UHA_Source.hi
../../heliumNT/src/staticanalysis/heuristics/RepairHeuristics.o : ../../heliumNT/src/utils/OneLiner.hi
../../heliumNT/src/staticanalysis/heuristics/RepairHeuristics.o : ../../heliumNT/src/staticanalysis/messages/Messages.hi
../../heliumNT/src/staticanalysis/heuristics/RepairHeuristics.o : ../../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.hi
../../heliumNT/src/staticanalysis/heuristics/RepairHeuristics.o : ../../heliumNT/src/staticanalysis/messages/TypeErrors.hi
../../heliumNT/src/staticanalysis/heuristics/RepairHeuristics.o : ../../heliumNT/src/staticanalysis/heuristics/OnlyResultHeuristics.hi
../../heliumNT/src/staticanalysis/heuristics/RepairHeuristics.o : ../../Top/src/Top/Types.hi
../../heliumNT/src/staticanalysis/heuristics/RepairHeuristics.o : ../../Top/src/Top/TypeGraph/TypeGraphState.hi
../../heliumNT/src/staticanalysis/heuristics/RepairHeuristics.o : ../../Top/src/Top/States/SubstState.hi
../../heliumNT/src/staticanalysis/heuristics/RepairHeuristics.o : ../../Top/src/Top/States/TIState.hi
../../heliumNT/src/staticanalysis/heuristics/RepairHeuristics.o : ../../Top/src/Top/TypeGraph/Basics.hi
../../heliumNT/src/staticanalysis/heuristics/RepairHeuristics.o : ../../Top/src/Top/TypeGraph/Heuristics.hi
../../heliumNT/src/staticanalysis/heuristics/TieBreakerHeuristics.o : ../../heliumNT/src/staticanalysis/heuristics/TieBreakerHeuristics.hs
../../heliumNT/src/staticanalysis/heuristics/TieBreakerHeuristics.o : ../../Top/src/Top/TypeGraph/Heuristics.hi
../../heliumNT/src/staticanalysis/heuristics/TieBreakerHeuristics.o : ../../Top/src/Top/States/TIState.hi
../../heliumNT/src/staticanalysis/heuristics/TieBreakerHeuristics.o : ../../Top/src/Top/Types.hi
../../heliumNT/src/staticanalysis/heuristics/UnifierHeuristics.o : ../../heliumNT/src/staticanalysis/heuristics/UnifierHeuristics.hs
../../heliumNT/src/staticanalysis/heuristics/UnifierHeuristics.o : ../../heliumNT/src/utils/Utils.hi
../../heliumNT/src/staticanalysis/heuristics/UnifierHeuristics.o : ../../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.hi
../../heliumNT/src/staticanalysis/heuristics/UnifierHeuristics.o : ../../heliumNT/src/staticanalysis/heuristics/RepairHeuristics.hi
../../heliumNT/src/staticanalysis/heuristics/UnifierHeuristics.o : ../../Top/src/Top/TypeGraph/Heuristics.hi
../../heliumNT/src/staticanalysis/heuristics/UnifierHeuristics.o : ../../Top/src/Top/TypeGraph/TypeGraphState.hi
../../heliumNT/src/staticanalysis/heuristics/UnifierHeuristics.o : ../../Top/src/Top/TypeGraph/Basics.hi
../../heliumNT/src/staticanalysis/heuristics/UnifierHeuristics.o : ../../Top/src/Top/States/TIState.hi
../../heliumNT/src/staticanalysis/heuristics/UnifierHeuristics.o : ../../Top/src/Top/Types.hi
../../heliumNT/src/staticanalysis/heuristics/RepairSystem.o : ../../heliumNT/src/staticanalysis/heuristics/RepairSystem.hs
../../heliumNT/src/staticanalysis/heuristics/RepairSystem.o : ../../heliumNT/src/utils/Utils.hi
../../heliumNT/src/staticanalysis/heuristics/RepairSystem.o : ../../heliumNT/src/staticanalysis/miscellaneous/UHA_Source.hi
../../heliumNT/src/staticanalysis/heuristics/RepairSystem.o : ../../heliumNT/src/syntax/UHA_Syntax.hi
../../heliumNT/src/staticanalysis/heuristics/RepairSystem.o : ../../heliumNT/src/staticanalysis/miscellaneous/DoublyLinkedTree.hi
../../heliumNT/src/staticanalysis/heuristics/RepairSystem.o : ../../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.hi
../../heliumNT/src/staticanalysis/heuristics/RepairSystem.o : ../../Top/src/Top/Repair/AExpr.hi
../../heliumNT/src/staticanalysis/heuristics/RepairSystem.o : ../../Top/src/Top/Repair/Repair.hi
../../heliumNT/src/staticanalysis/heuristics/RepairSystem.o : ../../Top/src/Top/TypeGraph/TypeGraphState.hi
../../heliumNT/src/staticanalysis/heuristics/RepairSystem.o : ../../Top/src/Top/TypeGraph/Heuristics.hi
../../heliumNT/src/staticanalysis/heuristics/RepairSystem.o : ../../Top/src/Top/TypeGraph/Basics.hi
../../heliumNT/src/staticanalysis/heuristics/RepairSystem.o : ../../Top/src/Top/States/BasicState.hi
../../heliumNT/src/staticanalysis/heuristics/RepairSystem.o : ../../Top/src/Top/Types.hi
../../heliumNT/src/staticanalysis/staticchecks/StaticChecks.o : ../../heliumNT/src/staticanalysis/staticchecks/StaticChecks.hs
../../heliumNT/src/staticanalysis/staticchecks/StaticChecks.o : ../../heliumNT/src/parser/OperatorTable.hi
../../heliumNT/src/staticanalysis/staticchecks/StaticChecks.o : ../../heliumNT/src/modulesystem/ImportEnvironment.hi
../../heliumNT/src/staticanalysis/staticchecks/StaticChecks.o : ../../heliumNT/src/staticanalysis/miscellaneous/TypeConstraints.hi
../../heliumNT/src/staticanalysis/staticchecks/StaticChecks.o : ../../heliumNT/src/codegeneration/DerivingShow.hi
../../heliumNT/src/staticanalysis/staticchecks/StaticChecks.o : ../../heliumNT/src/staticanalysis/miscellaneous/TypeConversion.hi
../../heliumNT/src/staticanalysis/staticchecks/StaticChecks.o : ../../heliumNT/src/utils/Utils.hi
../../heliumNT/src/staticanalysis/staticchecks/StaticChecks.o : ../../lvm/src/lib/common/TopSort.hi
../../heliumNT/src/staticanalysis/staticchecks/StaticChecks.o : ../../heliumNT/src/staticanalysis/messages/Messages.hi
../../heliumNT/src/staticanalysis/staticchecks/StaticChecks.o : ../../heliumNT/src/staticanalysis/messages/Warnings.hi
../../heliumNT/src/staticanalysis/staticchecks/StaticChecks.o : ../../heliumNT/src/staticanalysis/messages/StaticErrors.hi
../../heliumNT/src/staticanalysis/staticchecks/StaticChecks.o : ../../Top/src/Top/Types.hi
../../heliumNT/src/staticanalysis/staticchecks/StaticChecks.o : ../../heliumNT/src/syntax/UHA_Range.hi
../../heliumNT/src/staticanalysis/staticchecks/StaticChecks.o : ../../heliumNT/src/syntax/UHA_Utils.hi
../../heliumNT/src/staticanalysis/staticchecks/StaticChecks.o : ../../heliumNT/src/syntax/UHA_Syntax.hi
../../heliumNT/src/staticanalysis/staticchecks/StaticChecks.o : ../../heliumNT/src/main/Args.hi
../../heliumNT/src/staticanalysis/staticchecks/StaticChecks.o : ../../heliumNT/src/utils/Similarity.hi
../../heliumNT/src/staticanalysis/inferencers/TypeInferencing.o : ../../heliumNT/src/staticanalysis/inferencers/TypeInferencing.hs
../../heliumNT/src/staticanalysis/inferencers/TypeInferencing.o : ../../heliumNT/src/syntax/UHA_Utils.hi
../../heliumNT/src/staticanalysis/inferencers/TypeInferencing.o : ../../heliumNT/src/staticanalysis/directives/TS_Attributes.hi
../../heliumNT/src/staticanalysis/inferencers/TypeInferencing.o : ../../heliumNT/src/staticanalysis/directives/TS_CoreSyntax.hi
../../heliumNT/src/staticanalysis/inferencers/TypeInferencing.o : ../../heliumNT/src/staticanalysis/directives/TS_Apply.hi
../../heliumNT/src/staticanalysis/inferencers/TypeInferencing.o : ../../heliumNT/src/staticanalysis/directives/Matchers.hi
../../heliumNT/src/staticanalysis/inferencers/TypeInferencing.o : ../../heliumNT/src/main/Args.hi
../../heliumNT/src/staticanalysis/inferencers/TypeInferencing.o : ../../heliumNT/src/modulesystem/DictionaryEnvironment.hi
../../heliumNT/src/staticanalysis/inferencers/TypeInferencing.o : ../../heliumNT/src/modulesystem/ImportEnvironment.hi
../../heliumNT/src/staticanalysis/inferencers/TypeInferencing.o : ../../heliumNT/src/staticanalysis/miscellaneous/TypeDebugInfoWriter.hi
../../heliumNT/src/staticanalysis/inferencers/TypeInferencing.o : ../../heliumNT/src/codegeneration/DerivingShow.hi
../../heliumNT/src/staticanalysis/inferencers/TypeInferencing.o : ../../heliumNT/src/utils/Utils.hi
../../heliumNT/src/staticanalysis/inferencers/TypeInferencing.o : ../../heliumNT/src/syntax/UHA_Range.hi
../../heliumNT/src/staticanalysis/inferencers/TypeInferencing.o : ../../heliumNT/src/syntax/UHA_Syntax.hi
../../heliumNT/src/staticanalysis/inferencers/TypeInferencing.o : ../../heliumNT/src/staticanalysis/inferencers/BindingGroupAnalysis.hi
../../heliumNT/src/staticanalysis/inferencers/TypeInferencing.o : ../../heliumNT/src/staticanalysis/heuristics/HeuristicsInfo.hi
../../heliumNT/src/staticanalysis/inferencers/TypeInferencing.o : ../../Top/src/Top/Solvers/SolveConstraints.hi
../../heliumNT/src/staticanalysis/inferencers/TypeInferencing.o : ../../heliumNT/src/staticanalysis/inferencers/SelectConstraintSolver.hi
../../heliumNT/src/staticanalysis/inferencers/TypeInferencing.o : ../../Top/src/Top/ComposedSolvers/Tree.hi
../../heliumNT/src/staticanalysis/inferencers/TypeInferencing.o : ../../heliumNT/src/staticanalysis/miscellaneous/TypeConstraints.hi
../../heliumNT/src/staticanalysis/inferencers/TypeInferencing.o : ../../heliumNT/src/staticanalysis/miscellaneous/UHA_Source.hi
../../heliumNT/src/staticanalysis/inferencers/TypeInferencing.o : ../../heliumNT/src/staticanalysis/miscellaneous/DoublyLinkedTree.hi
../../heliumNT/src/staticanalysis/inferencers/TypeInferencing.o : ../../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.hi
../../heliumNT/src/staticanalysis/inferencers/TypeInferencing.o : ../../heliumNT/src/staticanalysis/messages/Warnings.hi
../../heliumNT/src/staticanalysis/inferencers/TypeInferencing.o : ../../heliumNT/src/staticanalysis/messages/TypeErrors.hi
../../heliumNT/src/staticanalysis/inferencers/TypeInferencing.o : ../../heliumNT/src/staticanalysis/messages/Messages.hi
../../heliumNT/src/staticanalysis/inferencers/TypeInferencing.o : ../../heliumNT/src/staticanalysis/miscellaneous/TypeConversion.hi
../../heliumNT/src/staticanalysis/inferencers/TypeInferencing.o : ../../Top/src/Top/Types.hi
../../heliumNT/src/staticanalysis/inferencers/KindInferencing.o : ../../heliumNT/src/staticanalysis/inferencers/KindInferencing.hs
../../heliumNT/src/staticanalysis/inferencers/KindInferencing.o : ../../heliumNT/src/staticanalysis/inferencers/BindingGroupAnalysis.hi
../../heliumNT/src/staticanalysis/inferencers/KindInferencing.o : ../../heliumNT/src/staticanalysis/messages/KindErrors.hi
../../heliumNT/src/staticanalysis/inferencers/KindInferencing.o : ../../heliumNT/src/modulesystem/ImportEnvironment.hi
../../heliumNT/src/staticanalysis/inferencers/KindInferencing.o : ../../lvm/src/lib/common/TopSort.hi
../../heliumNT/src/staticanalysis/inferencers/KindInferencing.o : ../../heliumNT/src/utils/Utils.hi
../../heliumNT/src/staticanalysis/inferencers/KindInferencing.o : ../../heliumNT/src/staticanalysis/miscellaneous/TypeConstraints.hi
../../heliumNT/src/staticanalysis/inferencers/KindInferencing.o : ../../heliumNT/src/main/Args.hi
../../heliumNT/src/staticanalysis/inferencers/KindInferencing.o : ../../heliumNT/src/syntax/UHA_Syntax.hi
../../heliumNT/src/staticanalysis/inferencers/KindInferencing.o : ../../Top/src/Top/Constraints/TypeConstraintInfo.hi
../../heliumNT/src/staticanalysis/inferencers/KindInferencing.o : ../../Top/src/Top/Solvers/SolveConstraints.hi
../../heliumNT/src/staticanalysis/inferencers/KindInferencing.o : ../../Top/src/Top/Solvers/GreedySolver.hi
../../heliumNT/src/staticanalysis/inferencers/KindInferencing.o : ../../Top/src/Top/Types.hi
../../heliumNT/src/staticanalysis/inferencers/ExpressionTypeInferencer.o : ../../heliumNT/src/staticanalysis/inferencers/ExpressionTypeInferencer.hs
../../heliumNT/src/staticanalysis/inferencers/ExpressionTypeInferencer.o : ../../heliumNT/src/syntax/UHA_Syntax.hi
../../heliumNT/src/staticanalysis/inferencers/ExpressionTypeInferencer.o : ../../heliumNT/src/utils/Utils.hi
../../heliumNT/src/staticanalysis/inferencers/ExpressionTypeInferencer.o : ../../heliumNT/src/syntax/UHA_Range.hi
../../heliumNT/src/staticanalysis/inferencers/ExpressionTypeInferencer.o : ../../heliumNT/src/syntax/UHA_Utils.hi
../../heliumNT/src/staticanalysis/inferencers/ExpressionTypeInferencer.o : ../../Top/src/Top/Types.hi
../../heliumNT/src/staticanalysis/inferencers/ExpressionTypeInferencer.o : ../../heliumNT/src/staticanalysis/messages/TypeErrors.hi
../../heliumNT/src/staticanalysis/inferencers/ExpressionTypeInferencer.o : ../../heliumNT/src/staticanalysis/inferencers/BindingGroupAnalysis.hi
../../heliumNT/src/staticanalysis/inferencers/ExpressionTypeInferencer.o : ../../heliumNT/src/modulesystem/ImportEnvironment.hi
../../heliumNT/src/staticanalysis/inferencers/ExpressionTypeInferencer.o : ../../heliumNT/src/staticanalysis/inferencers/TypeInferencing.hi
../../heliumNT/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../../heliumNT/src/staticanalysis/inferencers/SelectConstraintSolver.hs
../../heliumNT/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../../heliumNT/src/staticanalysis/messages/Warnings.hi
../../heliumNT/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../../Top/src/Top/Constraints/Polymorphism.hi
../../heliumNT/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../../heliumNT/src/syntax/UHA_Utils.hi
../../heliumNT/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../../heliumNT/src/syntax/UHA_Syntax.hi
../../heliumNT/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../../Top/src/Top/ComposedSolvers/ChunkySolver.hi
../../heliumNT/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../../Top/src/Top/ComposedSolvers/CombinationSolver.hi
../../heliumNT/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../../Top/src/Top/ComposedSolvers/TreeWalk.hi
../../heliumNT/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../../Top/src/Top/ComposedSolvers/Tree.hi
../../heliumNT/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../../Top/src/Top/TypeGraph/TypeGraphMonad.hi
../../heliumNT/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../../Top/src/Top/TypeGraph/TypeGraphSolver.hi
../../heliumNT/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../../Top/src/Top/Solvers/GreedySolver.hi
../../heliumNT/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../../Top/src/Top/Solvers/SimpleSolver.hi
../../heliumNT/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../../Top/src/Top/States/States.hi
../../heliumNT/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../../Top/src/Top/Solvers/SolveConstraints.hi
../../heliumNT/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../../Top/src/Top/States/SubstState.hi
../../heliumNT/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../../Top/src/Top/States/BasicState.hi
../../heliumNT/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../../Top/src/Top/Types.hi
../../heliumNT/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../../heliumNT/src/modulesystem/ImportEnvironment.hi
../../heliumNT/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../../heliumNT/src/staticanalysis/heuristics/ListOfHeuristics.hi
../../heliumNT/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../../heliumNT/src/staticanalysis/miscellaneous/TypeConstraints.hi
../../heliumNT/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.hi
../../heliumNT/src/staticanalysis/inferencers/SelectConstraintSolver.o : ../../heliumNT/src/main/Args.hi
../../heliumNT/src/staticanalysis/inferencers/BindingGroupAnalysis.o : ../../heliumNT/src/staticanalysis/inferencers/BindingGroupAnalysis.hs
../../heliumNT/src/staticanalysis/inferencers/BindingGroupAnalysis.o : ../../Top/src/Top/ComposedSolvers/Tree.hi
../../heliumNT/src/staticanalysis/inferencers/BindingGroupAnalysis.o : ../../Top/src/Top/Types.hi
../../heliumNT/src/staticanalysis/inferencers/BindingGroupAnalysis.o : ../../lvm/src/lib/common/TopSort.hi
../../heliumNT/src/staticanalysis/inferencers/BindingGroupAnalysis.o : ../../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.hi
../../heliumNT/src/staticanalysis/inferencers/BindingGroupAnalysis.o : ../../heliumNT/src/staticanalysis/miscellaneous/TypeConstraints.hi
../../heliumNT/src/staticanalysis/inferencers/BindingGroupAnalysis.o : ../../heliumNT/src/syntax/UHA_Syntax.hi
../../heliumNT/src/staticanalysis/directives/TS_Parser.o : ../../heliumNT/src/staticanalysis/directives/TS_Parser.hs
../../heliumNT/src/staticanalysis/directives/TS_Parser.o : ../../heliumNT/src/utils/Utils.hi
../../heliumNT/src/staticanalysis/directives/TS_Parser.o : ../../heliumNT/src/parser/OperatorTable.hi
../../heliumNT/src/staticanalysis/directives/TS_Parser.o : ../../parsec/Text/ParserCombinators/ParsecSpecial.hi
../../heliumNT/src/staticanalysis/directives/TS_Parser.o : ../../heliumNT/src/parser/ResolveOperators.hi
../../heliumNT/src/staticanalysis/directives/TS_Parser.o : ../../heliumNT/src/parser/Parser.hi
../../heliumNT/src/staticanalysis/directives/TS_Parser.o : ../../heliumNT/src/parser/ParseLibrary.hi
../../heliumNT/src/staticanalysis/directives/TS_Parser.o : ../../heliumNT/src/parser/Lexer.hi
../../heliumNT/src/staticanalysis/directives/TS_Parser.o : ../../heliumNT/src/staticanalysis/directives/TS_ToCore.hi
../../heliumNT/src/staticanalysis/directives/TS_Parser.o : ../../heliumNT/src/staticanalysis/directives/TS_CoreSyntax.hi
../../heliumNT/src/staticanalysis/directives/TS_Parser.o : ../../heliumNT/src/staticanalysis/directives/TS_Syntax.hi
../../heliumNT/src/staticanalysis/directives/TS_Parser.o : ../../heliumNT/src/syntax/UHA_Pretty.hi
../../heliumNT/src/staticanalysis/directives/TS_Parser.o : ../../heliumNT/src/syntax/UHA_Utils.hi
../../heliumNT/src/staticanalysis/directives/TS_Parser.o : ../../heliumNT/src/syntax/UHA_Syntax.hi
../../heliumNT/src/staticanalysis/directives/TS_Syntax.o : ../../heliumNT/src/staticanalysis/directives/TS_Syntax.hs
../../heliumNT/src/staticanalysis/directives/TS_Syntax.o : ../../heliumNT/src/syntax/UHA_Syntax.hi
../../heliumNT/src/staticanalysis/directives/Matchers.o : ../../heliumNT/src/staticanalysis/directives/Matchers.hs
../../heliumNT/src/staticanalysis/directives/Matchers.o : ../../heliumNT/src/staticanalysis/messages/Messages.hi
../../heliumNT/src/staticanalysis/directives/Matchers.o : ../../heliumNT/src/syntax/UHA_Syntax.hi
../../heliumNT/src/staticanalysis/directives/TS_Apply.o : ../../heliumNT/src/staticanalysis/directives/TS_Apply.hs
../../heliumNT/src/staticanalysis/directives/TS_Apply.o : ../../Top/src/Top/Types.hi
../../heliumNT/src/staticanalysis/directives/TS_Apply.o : ../../heliumNT/src/staticanalysis/miscellaneous/DoublyLinkedTree.hi
../../heliumNT/src/staticanalysis/directives/TS_Apply.o : ../../heliumNT/src/staticanalysis/miscellaneous/UHA_Source.hi
../../heliumNT/src/staticanalysis/directives/TS_Apply.o : ../../Top/src/Top/ComposedSolvers/Tree.hi
../../heliumNT/src/staticanalysis/directives/TS_Apply.o : ../../heliumNT/src/staticanalysis/directives/TS_CoreSyntax.hi
../../heliumNT/src/staticanalysis/directives/TS_Apply.o : ../../heliumNT/src/staticanalysis/directives/TS_Attributes.hi
../../heliumNT/src/staticanalysis/directives/TS_Apply.o : ../../heliumNT/src/parser/ResolveOperators.hi
../../heliumNT/src/staticanalysis/directives/TS_Apply.o : ../../heliumNT/src/parser/ParseLibrary.hi
../../heliumNT/src/staticanalysis/directives/TS_Apply.o : ../../heliumNT/src/parser/Lexer.hi
../../heliumNT/src/staticanalysis/directives/TS_Apply.o : ../../heliumNT/src/parser/Parser.hi
../../heliumNT/src/staticanalysis/directives/TS_Apply.o : ../../heliumNT/src/parser/OperatorTable.hi
../../heliumNT/src/staticanalysis/directives/TS_Apply.o : ../../heliumNT/src/staticanalysis/inferencers/BindingGroupAnalysis.hi
../../heliumNT/src/staticanalysis/directives/TS_Apply.o : ../../heliumNT/src/modulesystem/ImportEnvironment.hi
../../heliumNT/src/staticanalysis/directives/TS_Apply.o : ../../heliumNT/src/staticanalysis/messages/TypeErrors.hi
../../heliumNT/src/staticanalysis/directives/TS_Apply.o : ../../heliumNT/src/staticanalysis/messages/Messages.hi
../../heliumNT/src/staticanalysis/directives/TS_Apply.o : ../../heliumNT/src/utils/OneLiner.hi
../../heliumNT/src/staticanalysis/directives/TS_Apply.o : ../../heliumNT/src/utils/Utils.hi
../../heliumNT/src/staticanalysis/directives/TS_Apply.o : ../../heliumNT/src/syntax/UHA_Utils.hi
../../heliumNT/src/staticanalysis/directives/TS_Apply.o : ../../heliumNT/src/syntax/UHA_Range.hi
../../heliumNT/src/staticanalysis/directives/TS_Apply.o : ../../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.hi
../../heliumNT/src/staticanalysis/directives/TS_Apply.o : ../../heliumNT/src/staticanalysis/miscellaneous/TypeConstraints.hi
../../heliumNT/src/staticanalysis/directives/TS_Apply.o : ../../heliumNT/src/syntax/UHA_Syntax.hi
../../heliumNT/src/staticanalysis/directives/TS_Messages.o : ../../heliumNT/src/staticanalysis/directives/TS_Messages.hs
../../heliumNT/src/staticanalysis/directives/TS_Messages.o : ../../heliumNT/src/syntax/UHA_Syntax.hi
../../heliumNT/src/staticanalysis/directives/TS_Messages.o : ../../heliumNT/src/utils/Utils.hi
../../heliumNT/src/staticanalysis/directives/TS_Messages.o : ../../Top/src/Top/Types.hi
../../heliumNT/src/staticanalysis/directives/TS_Messages.o : ../../heliumNT/src/staticanalysis/messages/HeliumMessages.hi
../../heliumNT/src/staticanalysis/directives/TS_Messages.o : ../../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.hi
../../heliumNT/src/staticanalysis/directives/TS_Messages.o : ../../heliumNT/src/staticanalysis/messages/TypeErrors.hi
../../heliumNT/src/staticanalysis/directives/TS_Messages.o : ../../heliumNT/src/staticanalysis/messages/Messages.hi
../../heliumNT/src/staticanalysis/directives/TS_Analyse.o : ../../heliumNT/src/staticanalysis/directives/TS_Analyse.hs
../../heliumNT/src/staticanalysis/directives/TS_Analyse.o : ../../heliumNT/src/staticanalysis/inferencers/ExpressionTypeInferencer.hi
../../heliumNT/src/staticanalysis/directives/TS_Analyse.o : ../../heliumNT/src/syntax/UHA_Syntax.hi
../../heliumNT/src/staticanalysis/directives/TS_Analyse.o : ../../heliumNT/src/syntax/UHA_Pretty.hi
../../heliumNT/src/staticanalysis/directives/TS_Analyse.o : ../../heliumNT/src/utils/Utils.hi
../../heliumNT/src/staticanalysis/directives/TS_Analyse.o : ../../heliumNT/src/modulesystem/ImportEnvironment.hi
../../heliumNT/src/staticanalysis/directives/TS_Analyse.o : ../../heliumNT/src/staticanalysis/directives/TS_Messages.hi
../../heliumNT/src/staticanalysis/directives/TS_Analyse.o : ../../heliumNT/src/staticanalysis/messages/TypeErrors.hi
../../heliumNT/src/staticanalysis/directives/TS_Analyse.o : ../../heliumNT/src/staticanalysis/messages/Messages.hi
../../heliumNT/src/staticanalysis/directives/TS_Analyse.o : ../../heliumNT/src/staticanalysis/miscellaneous/UHA_Source.hi
../../heliumNT/src/staticanalysis/directives/TS_Analyse.o : ../../heliumNT/src/syntax/UHA_Range.hi
../../heliumNT/src/staticanalysis/directives/TS_Analyse.o : ../../heliumNT/src/syntax/UHA_Utils.hi
../../heliumNT/src/staticanalysis/directives/TS_Analyse.o : ../../heliumNT/src/staticanalysis/miscellaneous/TypeConversion.hi
../../heliumNT/src/staticanalysis/directives/TS_Analyse.o : ../../heliumNT/src/staticanalysis/directives/TS_Syntax.hi
../../heliumNT/src/staticanalysis/directives/TS_Analyse.o : ../../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.hi
../../heliumNT/src/staticanalysis/directives/TS_Analyse.o : ../../heliumNT/src/staticanalysis/miscellaneous/TypeConstraints.hi
../../heliumNT/src/staticanalysis/directives/TS_Analyse.o : ../../Top/src/Top/States/TIState.hi
../../heliumNT/src/staticanalysis/directives/TS_Analyse.o : ../../Top/src/Top/Solvers/SolveConstraints.hi
../../heliumNT/src/staticanalysis/directives/TS_Analyse.o : ../../Top/src/Top/Solvers/GreedySolver.hi
../../heliumNT/src/staticanalysis/directives/TS_Analyse.o : ../../Top/src/Top/Types.hi
../../heliumNT/src/staticanalysis/directives/TS_CoreSyntax.o : ../../heliumNT/src/staticanalysis/directives/TS_CoreSyntax.hs
../../heliumNT/src/staticanalysis/directives/TS_CoreSyntax.o : ../../Top/src/Top/Types.hi
../../heliumNT/src/staticanalysis/directives/TS_ToCore.o : ../../heliumNT/src/staticanalysis/directives/TS_ToCore.hs
../../heliumNT/src/staticanalysis/directives/TS_ToCore.o : ../../heliumNT/src/staticanalysis/inferencers/ExpressionTypeInferencer.hi
../../heliumNT/src/staticanalysis/directives/TS_ToCore.o : ../../heliumNT/src/syntax/UHA_Syntax.hi
../../heliumNT/src/staticanalysis/directives/TS_ToCore.o : ../../heliumNT/src/staticanalysis/miscellaneous/TypeConstraints.hi
../../heliumNT/src/staticanalysis/directives/TS_ToCore.o : ../../heliumNT/src/staticanalysis/directives/TS_Attributes.hi
../../heliumNT/src/staticanalysis/directives/TS_ToCore.o : ../../heliumNT/src/utils/OneLiner.hi
../../heliumNT/src/staticanalysis/directives/TS_ToCore.o : ../../heliumNT/src/staticanalysis/messages/TypeErrors.hi
../../heliumNT/src/staticanalysis/directives/TS_ToCore.o : ../../heliumNT/src/utils/Utils.hi
../../heliumNT/src/staticanalysis/directives/TS_ToCore.o : ../../heliumNT/src/staticanalysis/miscellaneous/TypeConversion.hi
../../heliumNT/src/staticanalysis/directives/TS_ToCore.o : ../../heliumNT/src/staticanalysis/miscellaneous/UHA_Source.hi
../../heliumNT/src/staticanalysis/directives/TS_ToCore.o : ../../heliumNT/src/syntax/UHA_OneLine.hi
../../heliumNT/src/staticanalysis/directives/TS_ToCore.o : ../../heliumNT/src/syntax/UHA_Utils.hi
../../heliumNT/src/staticanalysis/directives/TS_ToCore.o : ../../heliumNT/src/syntax/UHA_Range.hi
../../heliumNT/src/staticanalysis/directives/TS_ToCore.o : ../../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.hi
../../heliumNT/src/staticanalysis/directives/TS_ToCore.o : ../../Top/src/Top/Types.hi
../../heliumNT/src/staticanalysis/directives/TS_ToCore.o : ../../heliumNT/src/staticanalysis/messages/Messages.hi
../../heliumNT/src/staticanalysis/directives/TS_ToCore.o : ../../heliumNT/src/modulesystem/ImportEnvironment.hi
../../heliumNT/src/staticanalysis/directives/TS_ToCore.o : ../../heliumNT/src/staticanalysis/directives/TS_CoreSyntax.hi
../../heliumNT/src/staticanalysis/directives/TS_ToCore.o : ../../heliumNT/src/staticanalysis/directives/TS_Syntax.hi
../../heliumNT/src/staticanalysis/directives/TS_Compile.o : ../../heliumNT/src/staticanalysis/directives/TS_Compile.hs
../../heliumNT/src/staticanalysis/directives/TS_Compile.o : ../../lvm/src/lib/core/Core.hi
../../heliumNT/src/staticanalysis/directives/TS_Compile.o : ../../heliumNT/src/codegeneration/CoreUtils.hi
../../heliumNT/src/staticanalysis/directives/TS_Compile.o : ../../heliumNT/src/parser/ParseMessage.hi
../../heliumNT/src/staticanalysis/directives/TS_Compile.o : ../../heliumNT/src/main/Args.hi
../../heliumNT/src/staticanalysis/directives/TS_Compile.o : ../../heliumNT/src/staticanalysis/messages/HeliumMessages.hi
../../heliumNT/src/staticanalysis/directives/TS_Compile.o : ../../heliumNT/src/staticanalysis/directives/TS_Analyse.hi
../../heliumNT/src/staticanalysis/directives/TS_Compile.o : ../../heliumNT/src/parser/Lexer.hi
../../heliumNT/src/staticanalysis/directives/TS_Compile.o : ../../heliumNT/src/staticanalysis/directives/TS_Parser.hi
../../heliumNT/src/staticanalysis/directives/TS_Compile.o : ../../heliumNT/src/staticanalysis/directives/TS_ToCore.hi
../../heliumNT/src/staticanalysis/directives/TS_Compile.o : ../../heliumNT/src/modulesystem/ImportEnvironment.hi
../../heliumNT/src/staticanalysis/directives/TS_Compile.o : ../../heliumNT/src/staticanalysis/directives/TS_CoreSyntax.hi
../../heliumNT/src/staticanalysis/directives/TS_Attributes.o : ../../heliumNT/src/staticanalysis/directives/TS_Attributes.hs
../../heliumNT/src/staticanalysis/directives/TS_Attributes.o : ../../heliumNT/src/utils/Utils.hi
../../heliumNT/src/staticanalysis/directives/TS_Attributes.o : ../../heliumNT/src/staticanalysis/miscellaneous/UHA_Source.hi
../../heliumNT/src/staticanalysis/directives/TS_Attributes.o : ../../heliumNT/src/syntax/UHA_Syntax.hi
../../heliumNT/src/staticanalysis/directives/TS_Attributes.o : ../../heliumNT/src/utils/OneLiner.hi
../../heliumNT/src/staticanalysis/directives/TS_Attributes.o : ../../Top/src/Top/Types.hi
../../heliumNT/src/staticanalysis/directives/TS_Attributes.o : ../../heliumNT/src/staticanalysis/messages/Messages.hi
../../heliumNT/src/staticanalysis/directives/TS_Attributes.o : ../../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.hi
../../heliumNT/src/staticanalysis/directives/TS_Attributes.o : ../../heliumNT/src/staticanalysis/inferencers/BindingGroupAnalysis.hi
../../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.o : ../../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.hs
../../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.o : ../../heliumNT/src/utils/Utils.hi
../../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.o : ../../Top/src/Top/States/TIState.hi
../../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.o : ../../Top/src/Top/States/SubstState.hi
../../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.o : ../../Top/src/Top/States/BasicState.hi
../../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.o : ../../Top/src/Top/Qualifiers/TypeClasses.hi
../../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.o : ../../Top/src/Top/Constraints/TypeConstraintInfo.hi
../../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.o : ../../heliumNT/src/staticanalysis/miscellaneous/TypeConstraints.hi
../../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.o : ../../heliumNT/src/staticanalysis/miscellaneous/DoublyLinkedTree.hi
../../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.o : ../../heliumNT/src/staticanalysis/messages/Messages.hi
../../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.o : ../../heliumNT/src/staticanalysis/messages/TypeErrors.hi
../../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.o : ../../heliumNT/src/syntax/UHA_Range.hi
../../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.o : ../../heliumNT/src/staticanalysis/miscellaneous/UHA_Source.hi
../../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.o : ../../heliumNT/src/syntax/UHA_Syntax.hi
../../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.o : ../../Top/src/Top/ComposedSolvers/Tree.hi
../../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.o : ../../Top/src/Top/Types.hi
../../heliumNT/src/staticanalysis/miscellaneous/DoublyLinkedTree.o : ../../heliumNT/src/staticanalysis/miscellaneous/DoublyLinkedTree.hs
../../heliumNT/src/staticanalysis/miscellaneous/DoublyLinkedTree.o : ../../heliumNT/src/utils/Utils.hi
../../heliumNT/src/staticanalysis/miscellaneous/TypeConstraints.o : ../../heliumNT/src/staticanalysis/miscellaneous/TypeConstraints.hs
../../heliumNT/src/staticanalysis/miscellaneous/TypeConstraints.o : ../../Top/src/Top/Solvers/SolveConstraints.hi
../../heliumNT/src/staticanalysis/miscellaneous/TypeConstraints.o : ../../Top/src/Top/Types.hi
../../heliumNT/src/staticanalysis/miscellaneous/TypeConstraints.o : ../../Top/src/Top/Qualifiers/TypeClasses.hi
../../heliumNT/src/staticanalysis/miscellaneous/TypeConstraints.o : ../../Top/src/Top/States/QualifierState.hi
../../heliumNT/src/staticanalysis/miscellaneous/TypeConstraints.o : ../../Top/src/Top/States/SubstState.hi
../../heliumNT/src/staticanalysis/miscellaneous/TypeConstraints.o : ../../Top/src/Top/States/TIState.hi
../../heliumNT/src/staticanalysis/miscellaneous/TypeConstraints.o : ../../Top/src/Top/States/BasicState.hi
../../heliumNT/src/staticanalysis/miscellaneous/TypeConstraints.o : ../../Top/src/Top/Constraints/TypeConstraintInfo.hi
../../heliumNT/src/staticanalysis/miscellaneous/TypeConstraints.o : ../../Top/src/Top/Constraints/Polymorphism.hi
../../heliumNT/src/staticanalysis/miscellaneous/TypeConstraints.o : ../../Top/src/Top/Constraints/ExtraConstraints.hi
../../heliumNT/src/staticanalysis/miscellaneous/TypeConstraints.o : ../../Top/src/Top/Constraints/Equality.hi
../../heliumNT/src/staticanalysis/miscellaneous/TypeConstraints.o : ../../Top/src/Top/Constraints/Constraints.hi
../../heliumNT/src/staticanalysis/miscellaneous/TypesToAlignedDocs.o : ../../heliumNT/src/staticanalysis/miscellaneous/TypesToAlignedDocs.hs
../../heliumNT/src/staticanalysis/miscellaneous/TypesToAlignedDocs.o : ../../lvm/src/lib/common/PPrint.hi
../../heliumNT/src/staticanalysis/miscellaneous/TypesToAlignedDocs.o : ../../Top/src/Top/Types.hi
../../heliumNT/src/staticanalysis/miscellaneous/TypeConversion.o : ../../heliumNT/src/staticanalysis/miscellaneous/TypeConversion.hs
../../heliumNT/src/staticanalysis/miscellaneous/TypeConversion.o : ../../Top/src/Top/Types.hi
../../heliumNT/src/staticanalysis/miscellaneous/TypeConversion.o : ../../heliumNT/src/syntax/UHA_Syntax.hi
../../heliumNT/src/staticanalysis/miscellaneous/TypeConversion.o : ../../heliumNT/src/utils/Utils.hi
../../heliumNT/src/staticanalysis/miscellaneous/TypeConversion.o : ../../heliumNT/src/syntax/UHA_Range.hi
../../heliumNT/src/staticanalysis/miscellaneous/TypeConversion.o : ../../heliumNT/src/syntax/UHA_Utils.hi
../../heliumNT/src/staticanalysis/miscellaneous/UHA_Source.o : ../../heliumNT/src/staticanalysis/miscellaneous/UHA_Source.hs
../../heliumNT/src/staticanalysis/miscellaneous/UHA_Source.o : ../../heliumNT/src/syntax/UHA_OneLine.hi
../../heliumNT/src/staticanalysis/miscellaneous/UHA_Source.o : ../../heliumNT/src/syntax/UHA_Utils.hi
../../heliumNT/src/staticanalysis/miscellaneous/UHA_Source.o : ../../heliumNT/src/syntax/UHA_Syntax.hi
../../heliumNT/src/staticanalysis/miscellaneous/UHA_Source.o : ../../heliumNT/src/syntax/UHA_Range.hi
../../heliumNT/src/staticanalysis/miscellaneous/UHA_Source.o : ../../heliumNT/src/utils/OneLiner.hi
../../heliumNT/src/staticanalysis/miscellaneous/TypeDebugInfo.o : ../../heliumNT/src/staticanalysis/miscellaneous/TypeDebugInfo.hs
../../heliumNT/src/staticanalysis/miscellaneous/TypeDebugInfo.o : ../../Top/src/Top/Types.hi
../../heliumNT/src/staticanalysis/miscellaneous/TypeDebugInfoWriter.o : ../../heliumNT/src/staticanalysis/miscellaneous/TypeDebugInfoWriter.hs
../../heliumNT/src/staticanalysis/miscellaneous/TypeDebugInfoWriter.o : ../../heliumNT/src/utils/Utils.hi
../../heliumNT/src/staticanalysis/miscellaneous/TypeDebugInfoWriter.o : ../../heliumNT/src/utils/OneLiner.hi
../../heliumNT/src/staticanalysis/miscellaneous/TypeDebugInfoWriter.o : ../../heliumNT/src/staticanalysis/messages/HeliumMessages.hi
../../heliumNT/src/staticanalysis/miscellaneous/TypeDebugInfoWriter.o : ../../heliumNT/src/staticanalysis/messages/Messages.hi
../../heliumNT/src/staticanalysis/miscellaneous/TypeDebugInfoWriter.o : ../../heliumNT/src/staticanalysis/miscellaneous/UHA_Source.hi
../../heliumNT/src/staticanalysis/miscellaneous/TypeDebugInfoWriter.o : ../../heliumNT/src/syntax/UHA_Syntax.hi
../../heliumNT/src/staticanalysis/miscellaneous/TypeDebugInfoWriter.o : ../../heliumNT/src/staticanalysis/miscellaneous/TypeDebugInfo.hi
../../heliumNT/src/staticanalysis/miscellaneous/TypeDebugInfoWriter.o : ../../heliumNT/src/staticanalysis/miscellaneous/DoublyLinkedTree.hi
../../heliumNT/src/staticanalysis/miscellaneous/TypeDebugInfoWriter.o : ../../heliumNT/src/staticanalysis/miscellaneous/ConstraintInfo.hi
../../heliumNT/src/staticanalysis/miscellaneous/TypeDebugInfoWriter.o : ../../heliumNT/src/staticanalysis/messages/TypeErrors.hi
../../heliumNT/src/staticanalysis/miscellaneous/TypeDebugInfoWriter.o : ../../heliumNT/src/staticanalysis/miscellaneous/TypeConstraints.hi
../../heliumNT/src/staticanalysis/miscellaneous/TypeDebugInfoWriter.o : ../../Top/src/Top/Constraints/Polymorphism.hi
../../heliumNT/src/staticanalysis/miscellaneous/TypeDebugInfoWriter.o : ../../Top/src/Top/ComposedSolvers/TreeWalk.hi
../../heliumNT/src/staticanalysis/miscellaneous/TypeDebugInfoWriter.o : ../../Top/src/Top/ComposedSolvers/Tree.hi
../../heliumNT/src/staticanalysis/miscellaneous/TypeDebugInfoWriter.o : ../../Top/src/Top/Types.hi
../../Top/src/Top/Types/Basics.o : ../../Top/src/Top/Types/Basics.hs
../../Top/src/Top/Types/Quantification.o : ../../Top/src/Top/Types/Quantification.hs
../../Top/src/Top/Types/Quantification.o : ../../heliumNT/src/utils/Utils.hi
../../Top/src/Top/Types/Quantification.o : ../../Top/src/Top/Types/Substitution.hi
../../Top/src/Top/Types/Quantification.o : ../../Top/src/Top/Types/Basics.hi
../../Top/src/Top/Types/Qualification.o : ../../Top/src/Top/Types/Qualification.hs
../../Top/src/Top/Types/Qualification.o : ../../Top/src/Top/Types/Substitution.hi
../../Top/src/Top/Types/Qualification.o : ../../Top/src/Top/Types/Basics.hi
../../Top/src/Top/Types/Schemes.o : ../../Top/src/Top/Types/Schemes.hs
../../Top/src/Top/Types/Schemes.o : ../../Top/src/Top/Types/Classes.hi
../../Top/src/Top/Types/Schemes.o : ../../Top/src/Top/Types/Unification.hi
../../Top/src/Top/Types/Schemes.o : ../../Top/src/Top/Types/Synonyms.hi
../../Top/src/Top/Types/Schemes.o : ../../Top/src/Top/Types/Substitution.hi
../../Top/src/Top/Types/Schemes.o : ../../Top/src/Top/Types/Qualification.hi
../../Top/src/Top/Types/Schemes.o : ../../Top/src/Top/Types/Quantification.hi
../../Top/src/Top/Types/Schemes.o : ../../Top/src/Top/Types/Basics.hi
../../Top/src/Top/Types/Classes.o : ../../Top/src/Top/Types/Classes.hs
../../Top/src/Top/Types/Classes.o : ../../Top/src/Top/Types/Qualification.hi
../../Top/src/Top/Types/Classes.o : ../../Top/src/Top/Types/Synonyms.hi
../../Top/src/Top/Types/Classes.o : ../../Top/src/Top/Types/Unification.hi
../../Top/src/Top/Types/Classes.o : ../../Top/src/Top/Types/Substitution.hi
../../Top/src/Top/Types/Classes.o : ../../Top/src/Top/Types/Basics.hi
../../Top/src/Top/Types/Substitution.o : ../../Top/src/Top/Types/Substitution.hs
../../Top/src/Top/Types/Substitution.o : ../../heliumNT/src/utils/Utils.hi
../../Top/src/Top/Types/Substitution.o : ../../Top/src/Top/Types/Basics.hi
../../Top/src/Top/Types/Kinds.o : ../../Top/src/Top/Types/Kinds.hs
../../Top/src/Top/Types/Kinds.o : ../../Top/src/Top/Types/Schemes.hi
../../Top/src/Top/Types/Kinds.o : ../../Top/src/Top/Types/Quantification.hi
../../Top/src/Top/Types/Kinds.o : ../../Top/src/Top/Types/Substitution.hi
../../Top/src/Top/Types/Kinds.o : ../../Top/src/Top/Types/Basics.hi
../../Top/src/Top/Types/Synonyms.o : ../../Top/src/Top/Types/Synonyms.hs
../../Top/src/Top/Types/Synonyms.o : ../../heliumNT/src/utils/Utils.hi
../../Top/src/Top/Types/Synonyms.o : ../../Top/src/Graph.hi
../../Top/src/Top/Types/Synonyms.o : ../../Top/src/Top/Types/Basics.hi
../../Top/src/Top/Types/Unification.o : ../../Top/src/Top/Types/Unification.hs
../../Top/src/Top/Types/Unification.o : ../../heliumNT/src/utils/Utils.hi
../../Top/src/Top/Types/Unification.o : ../../Top/src/Top/Types/Synonyms.hi
../../Top/src/Top/Types/Unification.o : ../../Top/src/Top/Types/Basics.hi
../../Top/src/Top/Types/Unification.o : ../../Top/src/Top/Types/Substitution.hi
../../Top/src/Top/Types.o : ../../Top/src/Top/Types.hs
../../Top/src/Top/Types.o : ../../Top/src/Top/Types/Kinds.hi
../../Top/src/Top/Types.o : ../../Top/src/Top/Types/Schemes.hi
../../Top/src/Top/Types.o : ../../Top/src/Top/Types/Classes.hi
../../Top/src/Top/Types.o : ../../Top/src/Top/Types/Unification.hi
../../Top/src/Top/Types.o : ../../Top/src/Top/Types/Synonyms.hi
../../Top/src/Top/Types.o : ../../Top/src/Top/Types/Qualification.hi
../../Top/src/Top/Types.o : ../../Top/src/Top/Types/Quantification.hi
../../Top/src/Top/Types.o : ../../Top/src/Top/Types/Substitution.hi
../../Top/src/Top/Types.o : ../../Top/src/Top/Types/Basics.hi
../../Top/src/Top/../Graph.o : ../../Top/src/Top/../Graph.hs
../../Top/src/Top/Constraints/Constraints.o : ../../Top/src/Top/Constraints/Constraints.hs
../../Top/src/Top/Constraints/Constraints.o : ../../Top/src/Top/Types.hi
../../Top/src/Top/Constraints/ExtraConstraints.o : ../../Top/src/Top/Constraints/ExtraConstraints.hs
../../Top/src/Top/Constraints/ExtraConstraints.o : ../../Top/src/Top/States/QualifierState.hi
../../Top/src/Top/Constraints/ExtraConstraints.o : ../../Top/src/Top/States/BasicState.hi
../../Top/src/Top/Constraints/ExtraConstraints.o : ../../Top/src/Top/States/SubstState.hi
../../Top/src/Top/Constraints/ExtraConstraints.o : ../../Top/src/Top/States/TIState.hi
../../Top/src/Top/Constraints/ExtraConstraints.o : ../../Top/src/Top/States/States.hi
../../Top/src/Top/Constraints/ExtraConstraints.o : ../../Top/src/Top/Constraints/Polymorphism.hi
../../Top/src/Top/Constraints/ExtraConstraints.o : ../../Top/src/Top/Qualifiers/Qualifiers.hi
../../Top/src/Top/Constraints/ExtraConstraints.o : ../../Top/src/Top/Constraints/TypeConstraintInfo.hi
../../Top/src/Top/Constraints/ExtraConstraints.o : ../../Top/src/Top/Constraints/Constraints.hi
../../Top/src/Top/Constraints/ExtraConstraints.o : ../../Top/src/Top/Types.hi
../../Top/src/Top/Constraints/TypeConstraintInfo.o : ../../Top/src/Top/Constraints/TypeConstraintInfo.hs
../../Top/src/Top/Constraints/TypeConstraintInfo.o : ../../Top/src/Top/Types.hi
../../Top/src/Top/Constraints/Equality.o : ../../Top/src/Top/Constraints/Equality.hs
../../Top/src/Top/Constraints/Equality.o : ../../Top/src/Top/States/TIState.hi
../../Top/src/Top/Constraints/Equality.o : ../../Top/src/Top/States/SubstState.hi
../../Top/src/Top/Constraints/Equality.o : ../../Top/src/Top/Constraints/TypeConstraintInfo.hi
../../Top/src/Top/Constraints/Equality.o : ../../Top/src/Top/Constraints/Constraints.hi
../../Top/src/Top/Constraints/Equality.o : ../../Top/src/Top/Types.hi
../../Top/src/Top/Constraints/Polymorphism.o : ../../Top/src/Top/Constraints/Polymorphism.hs
../../Top/src/Top/Constraints/Polymorphism.o : ../../Top/src/Top/States/QualifierState.hi
../../Top/src/Top/Constraints/Polymorphism.o : ../../Top/src/Top/States/SubstState.hi
../../Top/src/Top/Constraints/Polymorphism.o : ../../Top/src/Top/States/TIState.hi
../../Top/src/Top/Constraints/Polymorphism.o : ../../Top/src/Top/States/BasicState.hi
../../Top/src/Top/Constraints/Polymorphism.o : ../../Top/src/Top/Constraints/TypeConstraintInfo.hi
../../Top/src/Top/Constraints/Polymorphism.o : ../../Top/src/Top/Constraints/Equality.hi
../../Top/src/Top/Constraints/Polymorphism.o : ../../Top/src/Top/Qualifiers/Qualifiers.hi
../../Top/src/Top/Constraints/Polymorphism.o : ../../Top/src/Top/Constraints/Constraints.hi
../../Top/src/Top/Constraints/Polymorphism.o : ../../Top/src/Top/Types.hi
../../Top/src/Top/States/States.o : ../../Top/src/Top/States/States.hs
../../Top/src/Top/States/BasicState.o : ../../Top/src/Top/States/BasicState.hs
../../Top/src/Top/States/BasicState.o : ../../heliumNT/src/utils/Utils.hi
../../Top/src/Top/States/BasicState.o : ../../Top/src/Top/States/States.hi
../../Top/src/Top/States/BasicState.o : ../../Top/src/Top/Constraints/Constraints.hi
../../Top/src/Top/States/SubstState.o : ../../Top/src/Top/States/SubstState.hs
../../Top/src/Top/States/SubstState.o : ../../Top/src/Top/States/BasicState.hi
../../Top/src/Top/States/SubstState.o : ../../Top/src/Top/Types.hi
../../Top/src/Top/States/TIState.o : ../../Top/src/Top/States/TIState.hs
../../Top/src/Top/States/TIState.o : ../../Top/src/Top/Qualifiers/QualifierMap.hi
../../Top/src/Top/States/TIState.o : ../../Top/src/Top/States/SubstState.hi
../../Top/src/Top/States/TIState.o : ../../Top/src/Top/States/BasicState.hi
../../Top/src/Top/States/TIState.o : ../../Top/src/Top/States/States.hi
../../Top/src/Top/States/TIState.o : ../../Top/src/Top/Constraints/TypeConstraintInfo.hi
../../Top/src/Top/States/TIState.o : ../../Top/src/Top/Types.hi
../../Top/src/Top/States/QualifierState.o : ../../Top/src/Top/States/QualifierState.hs
../../Top/src/Top/States/QualifierState.o : ../../heliumNT/src/utils/Utils.hi
../../Top/src/Top/States/QualifierState.o : ../../Top/src/Top/States/States.hi
../../Top/src/Top/States/QualifierState.o : ../../Top/src/Top/Types.hi
../../Top/src/Top/Qualifiers/Qualifiers.o : ../../Top/src/Top/Qualifiers/Qualifiers.hs
../../Top/src/Top/Qualifiers/Qualifiers.o : ../../Top/src/Top/States/BasicState.hi
../../Top/src/Top/Qualifiers/Qualifiers.o : ../../Top/src/Top/States/SubstState.hi
../../Top/src/Top/Qualifiers/Qualifiers.o : ../../Top/src/Top/States/TIState.hi
../../Top/src/Top/Qualifiers/Qualifiers.o : ../../Top/src/Top/Qualifiers/QualifierMap.hi
../../Top/src/Top/Qualifiers/Qualifiers.o : ../../Top/src/Top/Constraints/TypeConstraintInfo.hi
../../Top/src/Top/Qualifiers/Qualifiers.o : ../../Top/src/Top/Types.hi
../../Top/src/Top/Qualifiers/Qualifiers.o : ../../Top/src/Top/States/QualifierState.hi
../../Top/src/Top/Qualifiers/Qualifiers.o : ../../Top/src/Top/States/States.hi
../../Top/src/Top/Qualifiers/QualifierMap.o : ../../Top/src/Top/Qualifiers/QualifierMap.hs
../../Top/src/Top/Qualifiers/QualifierMap.o : ../../Top/src/Top/Types.hi
../../Top/src/Top/Qualifiers/QualifierMap.o : ../../Top/src/Top/States/States.hi
../../Top/src/Top/Qualifiers/TypeClasses.o : ../../Top/src/Top/Qualifiers/TypeClasses.hs
../../Top/src/Top/Qualifiers/TypeClasses.o : ../../Top/src/Top/Constraints/Equality.hi
../../Top/src/Top/Qualifiers/TypeClasses.o : ../../Top/src/Top/Constraints/Constraints.hi
../../Top/src/Top/Qualifiers/TypeClasses.o : ../../Top/src/Top/Constraints/TypeConstraintInfo.hi
../../Top/src/Top/Qualifiers/TypeClasses.o : ../../Top/src/Top/Qualifiers/Qualifiers.hi
../../Top/src/Top/Qualifiers/TypeClasses.o : ../../Top/src/Top/States/SubstState.hi
../../Top/src/Top/Qualifiers/TypeClasses.o : ../../Top/src/Top/States/TIState.hi
../../Top/src/Top/Qualifiers/TypeClasses.o : ../../Top/src/Top/States/BasicState.hi
../../Top/src/Top/Qualifiers/TypeClasses.o : ../../Top/src/Top/Types.hi
../../Top/src/Top/Solvers/BasicMonad.o : ../../Top/src/Top/Solvers/BasicMonad.hs
../../Top/src/Top/Solvers/BasicMonad.o : ../../Top/src/Top/States/States.hi
../../Top/src/Top/Solvers/BasicMonad.o : ../../Top/src/Top/States/BasicState.hi
../../Top/src/Top/Solvers/BasicMonad.o : ../../Top/src/Top/Constraints/Constraints.hi
../../Top/src/Top/Solvers/GreedySolver.o : ../../Top/src/Top/Solvers/GreedySolver.hs
../../Top/src/Top/Solvers/GreedySolver.o : ../../Top/src/Top/Qualifiers/Qualifiers.hi
../../Top/src/Top/Solvers/GreedySolver.o : ../../Top/src/Top/Constraints/TypeConstraintInfo.hi
../../Top/src/Top/Solvers/GreedySolver.o : ../../Top/src/Top/Constraints/Constraints.hi
../../Top/src/Top/Solvers/GreedySolver.o : ../../Top/src/Top/Types.hi
../../Top/src/Top/Solvers/GreedySolver.o : ../../Top/src/Top/States/States.hi
../../Top/src/Top/Solvers/GreedySolver.o : ../../Top/src/Top/States/SubstState.hi
../../Top/src/Top/Solvers/GreedySolver.o : ../../Top/src/Top/States/TIState.hi
../../Top/src/Top/Solvers/GreedySolver.o : ../../Top/src/Top/Solvers/BasicMonad.hi
../../Top/src/Top/Solvers/GreedySolver.o : ../../Top/src/Top/Solvers/GreedySubst.hi
../../Top/src/Top/Solvers/GreedySolver.o : ../../Top/src/Top/Solvers/SolveConstraints.hi
../../Top/src/Top/Solvers/GreedySubst.o : ../../Top/src/Top/Solvers/GreedySubst.hs
../../Top/src/Top/Solvers/GreedySubst.o : ../../heliumNT/src/utils/Utils.hi
../../Top/src/Top/Solvers/GreedySubst.o : ../../Top/src/Top/Types.hi
../../Top/src/Top/Solvers/GreedySubst.o : ../../Top/src/Top/States/TIState.hi
../../Top/src/Top/Solvers/GreedySubst.o : ../../Top/src/Top/States/BasicState.hi
../../Top/src/Top/Solvers/GreedySubst.o : ../../Top/src/Top/States/SubstState.hi
../../Top/src/Top/Solvers/GreedySubst.o : ../../Top/src/Top/States/States.hi
../../Top/src/Top/Solvers/SimpleSolver.o : ../../Top/src/Top/Solvers/SimpleSolver.hs
../../Top/src/Top/Solvers/SimpleSolver.o : ../../Top/src/Top/Qualifiers/Qualifiers.hi
../../Top/src/Top/Solvers/SimpleSolver.o : ../../Top/src/Top/Constraints/TypeConstraintInfo.hi
../../Top/src/Top/Solvers/SimpleSolver.o : ../../Top/src/Top/Constraints/Constraints.hi
../../Top/src/Top/Solvers/SimpleSolver.o : ../../Top/src/Top/Types.hi
../../Top/src/Top/Solvers/SimpleSolver.o : ../../Top/src/Top/Solvers/SimpleSubst.hi
../../Top/src/Top/Solvers/SimpleSolver.o : ../../Top/src/Top/Solvers/SolveConstraints.hi
../../Top/src/Top/Solvers/SimpleSolver.o : ../../Top/src/Top/Solvers/BasicMonad.hi
../../Top/src/Top/Solvers/SimpleSolver.o : ../../Top/src/Top/States/States.hi
../../Top/src/Top/Solvers/SimpleSolver.o : ../../Top/src/Top/States/SubstState.hi
../../Top/src/Top/Solvers/SimpleSolver.o : ../../Top/src/Top/States/TIState.hi
../../Top/src/Top/Solvers/SimpleSubst.o : ../../Top/src/Top/Solvers/SimpleSubst.hs
../../Top/src/Top/Solvers/SimpleSubst.o : ../../Top/src/Top/States/TIState.hi
../../Top/src/Top/Solvers/SimpleSubst.o : ../../Top/src/Top/States/BasicState.hi
../../Top/src/Top/Solvers/SimpleSubst.o : ../../Top/src/Top/States/SubstState.hi
../../Top/src/Top/Solvers/SimpleSubst.o : ../../Top/src/Top/States/States.hi
../../Top/src/Top/Solvers/SimpleSubst.o : ../../Top/src/Top/Types.hi
../../Top/src/Top/Solvers/SolveConstraints.o : ../../Top/src/Top/Solvers/SolveConstraints.hs
../../Top/src/Top/Solvers/SolveConstraints.o : ../../Top/src/Top/Solvers/BasicMonad.hi
../../Top/src/Top/Solvers/SolveConstraints.o : ../../Top/src/Top/States/QualifierState.hi
../../Top/src/Top/Solvers/SolveConstraints.o : ../../Top/src/Top/States/TIState.hi
../../Top/src/Top/Solvers/SolveConstraints.o : ../../Top/src/Top/States/BasicState.hi
../../Top/src/Top/Solvers/SolveConstraints.o : ../../Top/src/Top/States/SubstState.hi
../../Top/src/Top/Solvers/SolveConstraints.o : ../../Top/src/Top/States/States.hi
../../Top/src/Top/Solvers/SolveConstraints.o : ../../Top/src/Top/Constraints/TypeConstraintInfo.hi
../../Top/src/Top/Solvers/SolveConstraints.o : ../../Top/src/Top/Qualifiers/Qualifiers.hi
../../Top/src/Top/Solvers/SolveConstraints.o : ../../Top/src/Top/Constraints/Constraints.hi
../../Top/src/Top/Solvers/SolveConstraints.o : ../../Top/src/Top/Types.hi
../../Top/src/Top/ComposedSolvers/CombinationSolver.o : ../../Top/src/Top/ComposedSolvers/CombinationSolver.hs
../../Top/src/Top/ComposedSolvers/CombinationSolver.o : ../../Top/src/Top/States/BasicState.hi
../../Top/src/Top/ComposedSolvers/CombinationSolver.o : ../../Top/src/Top/Solvers/SolveConstraints.hi
../../Top/src/Top/ComposedSolvers/ChunkySolver.o : ../../Top/src/Top/ComposedSolvers/ChunkySolver.hs
../../Top/src/Top/ComposedSolvers/ChunkySolver.o : ../../Top/src/Top/ComposedSolvers/Tree.hi
../../Top/src/Top/ComposedSolvers/ChunkySolver.o : ../../Top/src/Top/Solvers/SolveConstraints.hi
../../Top/src/Top/ComposedSolvers/ChunkySolver.o : ../../Top/src/Top/States/States.hi
../../Top/src/Top/ComposedSolvers/ChunkySolver.o : ../../Top/src/Top/Types.hi
../../Top/src/Top/ComposedSolvers/Tree.o : ../../Top/src/Top/ComposedSolvers/Tree.hs
../../Top/src/Top/ComposedSolvers/Tree.o : ../../Top/src/Top/ComposedSolvers/TreeWalk.hi
../../Top/src/Top/ComposedSolvers/TreeWalk.o : ../../Top/src/Top/ComposedSolvers/TreeWalk.hs
../../Top/src/Top/TypeGraph/ApplyHeuristics.o : ../../Top/src/Top/TypeGraph/ApplyHeuristics.hs
../../Top/src/Top/TypeGraph/ApplyHeuristics.o : ../../Top/src/Top/States/TIState.hi
../../Top/src/Top/TypeGraph/ApplyHeuristics.o : ../../Top/src/Top/States/BasicState.hi
../../Top/src/Top/TypeGraph/ApplyHeuristics.o : ../../Top/src/Top/../Graph.hi
../../Top/src/Top/TypeGraph/ApplyHeuristics.o : ../../Top/src/Top/Types.hi
../../Top/src/Top/TypeGraph/ApplyHeuristics.o : ../../heliumNT/src/utils/Utils.hi
../../Top/src/Top/TypeGraph/ApplyHeuristics.o : ../../Top/src/Top/TypeGraph/Paths.hi
../../Top/src/Top/TypeGraph/ApplyHeuristics.o : ../../Top/src/Top/TypeGraph/Basics.hi
../../Top/src/Top/TypeGraph/ApplyHeuristics.o : ../../Top/src/Top/TypeGraph/Heuristics.hi
../../Top/src/Top/TypeGraph/ApplyHeuristics.o : ../../Top/src/Top/TypeGraph/TypeGraphState.hi
../../Top/src/Top/TypeGraph/Basics.o : ../../Top/src/Top/TypeGraph/Basics.hs
../../Top/src/Top/TypeGraph/Basics.o : ../../heliumNT/src/utils/Utils.hi
../../Top/src/Top/TypeGraph/Basics.o : ../../Top/src/Top/Types.hi
../../Top/src/Top/TypeGraph/Basics.o : ../../Top/src/Top/TypeGraph/Paths.hi
../../Top/src/Top/TypeGraph/DefaultHeuristics.o : ../../Top/src/Top/TypeGraph/DefaultHeuristics.hs
../../Top/src/Top/TypeGraph/DefaultHeuristics.o : ../../Top/src/Top/TypeGraph/ApplyHeuristics.hi
../../Top/src/Top/TypeGraph/DefaultHeuristics.o : ../../Top/src/Top/States/BasicState.hi
../../Top/src/Top/TypeGraph/DefaultHeuristics.o : ../../Top/src/Top/TypeGraph/Paths.hi
../../Top/src/Top/TypeGraph/DefaultHeuristics.o : ../../Top/src/Top/TypeGraph/Heuristics.hi
../../Top/src/Top/TypeGraph/DefaultHeuristics.o : ../../Top/src/Top/TypeGraph/Basics.hi
../../Top/src/Top/TypeGraph/EquivalenceGroup.o : ../../Top/src/Top/TypeGraph/EquivalenceGroup.hs
../../Top/src/Top/TypeGraph/EquivalenceGroup.o : ../../heliumNT/src/utils/Utils.hi
../../Top/src/Top/TypeGraph/EquivalenceGroup.o : ../../Top/src/Top/Types.hi
../../Top/src/Top/TypeGraph/EquivalenceGroup.o : ../../Top/src/Top/TypeGraph/Basics.hi
../../Top/src/Top/TypeGraph/EquivalenceGroup.o : ../../Top/src/Top/TypeGraph/Paths.hi
../../Top/src/Top/TypeGraph/Heuristics.o : ../../Top/src/Top/TypeGraph/Heuristics.hs
../../Top/src/Top/TypeGraph/Heuristics.o : ../../heliumNT/src/utils/Utils.hi
../../Top/src/Top/TypeGraph/Heuristics.o : ../../Top/src/Top/Types.hi
../../Top/src/Top/TypeGraph/Heuristics.o : ../../Top/src/Top/TypeGraph/Basics.hi
../../Top/src/Top/TypeGraph/Heuristics.o : ../../Top/src/Top/TypeGraph/TypeGraphState.hi
../../Top/src/Top/TypeGraph/Paths.o : ../../Top/src/Top/TypeGraph/Paths.hs
../../Top/src/Top/TypeGraph/Paths.o : ../../heliumNT/src/utils/Utils.hi
../../Top/src/Top/TypeGraph/TypeGraphMonad.o : ../../Top/src/Top/TypeGraph/TypeGraphMonad.hs
../../Top/src/Top/TypeGraph/TypeGraphMonad.o : ../../heliumNT/src/utils/Utils.hi
../../Top/src/Top/TypeGraph/TypeGraphMonad.o : ../../Top/src/Top/TypeGraph/DefaultHeuristics.hi
../../Top/src/Top/TypeGraph/TypeGraphMonad.o : ../../Top/src/Top/TypeGraph/Heuristics.hi
../../Top/src/Top/TypeGraph/TypeGraphMonad.o : ../../Top/src/Top/TypeGraph/Paths.hi
../../Top/src/Top/TypeGraph/TypeGraphMonad.o : ../../Top/src/Top/States/States.hi
../../Top/src/Top/TypeGraph/TypeGraphMonad.o : ../../Top/src/Top/TypeGraph/Basics.hi
../../Top/src/Top/TypeGraph/TypeGraphMonad.o : ../../Top/src/Top/TypeGraph/EquivalenceGroup.hi
../../Top/src/Top/TypeGraph/TypeGraphSolver.o : ../../Top/src/Top/TypeGraph/TypeGraphSolver.hs
../../Top/src/Top/TypeGraph/TypeGraphSolver.o : ../../Top/src/Top/Types.hi
../../Top/src/Top/TypeGraph/TypeGraphSolver.o : ../../Top/src/Top/Qualifiers/Qualifiers.hi
../../Top/src/Top/TypeGraph/TypeGraphSolver.o : ../../Top/src/Top/Constraints/Constraints.hi
../../Top/src/Top/TypeGraph/TypeGraphSolver.o : ../../Top/src/Top/States/States.hi
../../Top/src/Top/TypeGraph/TypeGraphSolver.o : ../../Top/src/Top/States/SubstState.hi
../../Top/src/Top/TypeGraph/TypeGraphSolver.o : ../../Top/src/Top/States/TIState.hi
../../Top/src/Top/TypeGraph/TypeGraphSolver.o : ../../Top/src/Top/Solvers/BasicMonad.hi
../../Top/src/Top/TypeGraph/TypeGraphSolver.o : ../../Top/src/Top/Solvers/SolveConstraints.hi
../../Top/src/Top/TypeGraph/TypeGraphSolver.o : ../../Top/src/Top/TypeGraph/Implementation.hi
../../Top/src/Top/TypeGraph/TypeGraphSolver.o : ../../Top/src/Top/TypeGraph/TypeGraphState.hi
../../Top/src/Top/TypeGraph/TypeGraphSolver.o : ../../Top/src/Top/TypeGraph/TypeGraphSubst.hi
../../Top/src/Top/TypeGraph/TypeGraphSolver.o : ../../Top/src/Top/TypeGraph/TypeGraphMonad.hi
../../Top/src/Top/TypeGraph/TypeGraphState.o : ../../Top/src/Top/TypeGraph/TypeGraphState.hs
../../Top/src/Top/TypeGraph/TypeGraphState.o : ../../heliumNT/src/utils/Utils.hi
../../Top/src/Top/TypeGraph/TypeGraphState.o : ../../Top/src/Top/Types.hi
../../Top/src/Top/TypeGraph/TypeGraphState.o : ../../Top/src/Top/States/SubstState.hi
../../Top/src/Top/TypeGraph/TypeGraphState.o : ../../Top/src/Top/States/TIState.hi
../../Top/src/Top/TypeGraph/TypeGraphState.o : ../../Top/src/Top/States/BasicState.hi
../../Top/src/Top/TypeGraph/TypeGraphState.o : ../../Top/src/Top/TypeGraph/Basics.hi
../../Top/src/Top/TypeGraph/Implementation.o : ../../Top/src/Top/TypeGraph/Implementation.hs
../../Top/src/Top/TypeGraph/Implementation.o : ../../heliumNT/src/utils/Utils.hi
../../Top/src/Top/TypeGraph/Implementation.o : ../../Top/src/Top/Types.hi
../../Top/src/Top/TypeGraph/Implementation.o : ../../Top/src/Top/TypeGraph/ApplyHeuristics.hi
../../Top/src/Top/TypeGraph/Implementation.o : ../../Top/src/Top/TypeGraph/Basics.hi
../../Top/src/Top/TypeGraph/Implementation.o : ../../Top/src/Top/TypeGraph/EquivalenceGroup.hi
../../Top/src/Top/TypeGraph/Implementation.o : ../../Top/src/Top/TypeGraph/TypeGraphState.hi
../../Top/src/Top/TypeGraph/Implementation.o : ../../Top/src/Top/TypeGraph/TypeGraphMonad.hi
../../Top/src/Top/TypeGraph/Implementation.o : ../../Top/src/Top/States/SubstState.hi
../../Top/src/Top/TypeGraph/Implementation.o : ../../Top/src/Top/States/BasicState.hi
../../Top/src/Top/TypeGraph/Implementation.o : ../../Top/src/Top/States/TIState.hi
../../Top/src/Top/TypeGraph/TypeGraphSubst.o : ../../Top/src/Top/TypeGraph/TypeGraphSubst.hs
../../Top/src/Top/TypeGraph/TypeGraphSubst.o : ../../Top/src/Top/Types.hi
../../Top/src/Top/TypeGraph/TypeGraphSubst.o : ../../Top/src/Top/TypeGraph/Basics.hi
../../Top/src/Top/TypeGraph/TypeGraphSubst.o : ../../Top/src/Top/TypeGraph/TypeGraphMonad.hi
../../Top/src/Top/TypeGraph/TypeGraphSubst.o : ../../Top/src/Top/TypeGraph/TypeGraphState.hi
../../Top/src/Top/TypeGraph/TypeGraphSubst.o : ../../Top/src/Top/States/SubstState.hi
../../Top/src/Top/Repair/AExpr.o : ../../Top/src/Top/Repair/AExpr.hs
../../Top/src/Top/Repair/AExpr.o : ../../Top/src/Top/Types.hi
../../Top/src/Top/Repair/Transformations.o : ../../Top/src/Top/Repair/Transformations.hs
../../Top/src/Top/Repair/Transformations.o : ../../Top/src/Top/Repair/AExpr.hi
../../Top/src/Top/Repair/Transformations.o : ../../Top/src/Top/Types.hi
../../Top/src/Top/Repair/Repair.o : ../../Top/src/Top/Repair/Repair.hs
../../Top/src/Top/Repair/Repair.o : ../../Top/src/Top/Repair/AExpr.hi
../../Top/src/Top/Repair/Repair.o : ../../Top/src/Top/Constraints/Equality.hi
../../Top/src/Top/Repair/Repair.o : ../../Top/src/Top/Solvers/SolveConstraints.hi
../../Top/src/Top/Repair/Repair.o : ../../Top/src/Top/Solvers/GreedySolver.hi
../../Top/src/Top/Repair/Repair.o : ../../Top/src/Top/Types.hi
../../Top/src/Top/Repair/Repair.o : ../../Top/src/Top/Constraints/TypeConstraintInfo.hi
../../Top/src/Top/Repair/Repair.o : ../../Top/src/Top/Qualifiers/TypeClasses.hi
../../heliumNT/src/syntax/UHA_Syntax.o : ../../heliumNT/src/syntax/UHA_Syntax.hs
../../heliumNT/src/syntax/UHA_Utils.o : ../../heliumNT/src/syntax/UHA_Utils.hs
../../heliumNT/src/syntax/UHA_Utils.o : ../../heliumNT/src/utils/Utils.hi
../../heliumNT/src/syntax/UHA_Utils.o : ../../Top/src/Top/Types.hi
../../heliumNT/src/syntax/UHA_Utils.o : ../../lvm/src/lib/common/Id.hi
../../heliumNT/src/syntax/UHA_Utils.o : ../../heliumNT/src/syntax/UHA_Syntax.hi
../../heliumNT/src/syntax/UHA_Utils.o : ../../heliumNT/src/syntax/UHA_Range.hi
../../heliumNT/src/syntax/UHA_Range.o : ../../heliumNT/src/syntax/UHA_Range.hs
../../heliumNT/src/syntax/UHA_Range.o : ../../heliumNT/src/utils/Utils.hi
../../heliumNT/src/syntax/UHA_Range.o : ../../lvm/src/lib/common/Id.hi
../../heliumNT/src/syntax/UHA_Range.o : ../../heliumNT/src/syntax/UHA_Syntax.hi
../../heliumNT/src/syntax/UHA_Pretty.o : ../../heliumNT/src/syntax/UHA_Pretty.hs
../../heliumNT/src/syntax/UHA_Pretty.o : ../../heliumNT/src/utils/Utils.hi
../../heliumNT/src/syntax/UHA_Pretty.o : ../../heliumNT/src/syntax/UHA_Syntax.hi
../../heliumNT/src/syntax/UHA_Pretty.o : ../../Top/src/Top/Types.hi
../../heliumNT/src/syntax/UHA_Pretty.o : ../../lvm/src/lib/common/PPrint.hi
../../heliumNT/src/syntax/UHA_OneLine.o : ../../heliumNT/src/syntax/UHA_OneLine.hs
../../heliumNT/src/syntax/UHA_OneLine.o : ../../heliumNT/src/utils/Utils.hi
../../heliumNT/src/syntax/UHA_OneLine.o : ../../heliumNT/src/syntax/UHA_Syntax.hi
../../heliumNT/src/syntax/UHA_OneLine.o : ../../heliumNT/src/staticanalysis/miscellaneous/TypeConversion.hi
../../heliumNT/src/syntax/UHA_OneLine.o : ../../Top/src/Top/Types.hi
../../heliumNT/src/syntax/UHA_OneLine.o : ../../heliumNT/src/syntax/UHA_Utils.hi
../../heliumNT/src/syntax/UHA_OneLine.o : ../../heliumNT/src/utils/OneLiner.hi
../../heliumNT/src/codegeneration/CoreToLvm.o : ../../heliumNT/src/codegeneration/CoreToLvm.hs
../../heliumNT/src/codegeneration/CoreToLvm.o : ../../lvm/src/lib/lvm/LvmPretty.hi
../../heliumNT/src/codegeneration/CoreToLvm.o : ../../lvm/src/lib/common/PPrint.hi
../../heliumNT/src/codegeneration/CoreToLvm.o : ../../lvm/src/lib/lvm/LvmWrite.hi
../../heliumNT/src/codegeneration/CoreToLvm.o : ../../lvm/src/lib/asm/AsmOptimize.hi
../../heliumNT/src/codegeneration/CoreToLvm.o : ../../lvm/src/lib/asm/AsmToLvm.hi
../../heliumNT/src/codegeneration/CoreToLvm.o : ../../lvm/src/lib/core/CoreToAsm.hi
../../heliumNT/src/codegeneration/CoreToLvm.o : ../../lvm/src/lib/common/Id.hi
../../heliumNT/src/codegeneration/PatternMatch.o : ../../heliumNT/src/codegeneration/PatternMatch.hs
../../heliumNT/src/codegeneration/PatternMatch.o : ../../heliumNT/src/codegeneration/CoreUtils.hi
../../heliumNT/src/codegeneration/PatternMatch.o : ../../heliumNT/src/utils/Utils.hi
../../heliumNT/src/codegeneration/PatternMatch.o : ../../lvm/src/lib/common/Id.hi
../../heliumNT/src/codegeneration/PatternMatch.o : ../../heliumNT/src/syntax/UHA_Range.hi
../../heliumNT/src/codegeneration/PatternMatch.o : ../../heliumNT/src/syntax/UHA_Utils.hi
../../heliumNT/src/codegeneration/PatternMatch.o : ../../heliumNT/src/syntax/UHA_Syntax.hi
../../heliumNT/src/codegeneration/PatternMatch.o : ../../lvm/src/lib/core/Core.hi
../../heliumNT/src/codegeneration/DerivingShow.o : ../../heliumNT/src/codegeneration/DerivingShow.hs
../../heliumNT/src/codegeneration/DerivingShow.o : ../../Top/src/Top/Types.hi
../../heliumNT/src/codegeneration/DerivingShow.o : ../../heliumNT/src/utils/Utils.hi
../../heliumNT/src/codegeneration/DerivingShow.o : ../../lvm/src/lib/common/Id.hi
../../heliumNT/src/codegeneration/DerivingShow.o : ../../lvm/src/lib/core/Core.hi
../../heliumNT/src/codegeneration/DerivingShow.o : ../../heliumNT/src/codegeneration/CoreUtils.hi
../../heliumNT/src/codegeneration/DerivingShow.o : ../../heliumNT/src/syntax/UHA_Utils.hi
../../heliumNT/src/codegeneration/DerivingShow.o : ../../heliumNT/src/syntax/UHA_Syntax.hi
../../heliumNT/src/codegeneration/DerivingEq.o : ../../heliumNT/src/codegeneration/DerivingEq.hs
../../heliumNT/src/codegeneration/DerivingEq.o : ../../Top/src/Top/Types.hi
../../heliumNT/src/codegeneration/DerivingEq.o : ../../heliumNT/src/utils/Utils.hi
../../heliumNT/src/codegeneration/DerivingEq.o : ../../lvm/src/lib/common/Id.hi
../../heliumNT/src/codegeneration/DerivingEq.o : ../../lvm/src/lib/core/Core.hi
../../heliumNT/src/codegeneration/DerivingEq.o : ../../heliumNT/src/codegeneration/CoreUtils.hi
../../heliumNT/src/codegeneration/DerivingEq.o : ../../heliumNT/src/syntax/UHA_Utils.hi
../../heliumNT/src/codegeneration/DerivingEq.o : ../../heliumNT/src/syntax/UHA_Syntax.hi
../../heliumNT/src/codegeneration/CoreUtils.o : ../../heliumNT/src/codegeneration/CoreUtils.hs
../../heliumNT/src/codegeneration/CoreUtils.o : ../../lvm/src/lib/common/Byte.hi
../../heliumNT/src/codegeneration/CoreUtils.o : ../../lvm/src/lib/common/Id.hi
../../heliumNT/src/codegeneration/CoreUtils.o : ../../lvm/src/lib/core/Core.hi
../../heliumNT/src/codegeneration/CodeGeneration.o : ../../heliumNT/src/codegeneration/CodeGeneration.hs
../../heliumNT/src/codegeneration/CodeGeneration.o : ../../lvm/src/lib/common/Byte.hi
../../heliumNT/src/codegeneration/CodeGeneration.o : ../../lvm/src/lib/core/CoreParse.hi
../../heliumNT/src/codegeneration/CodeGeneration.o : ../../lvm/src/lib/lvm/Module.hi
../../heliumNT/src/codegeneration/CodeGeneration.o : ../../lvm/src/lib/core/Core.hi
../../heliumNT/src/codegeneration/CodeGeneration.o : ../../heliumNT/src/codegeneration/CoreUtils.hi
../../heliumNT/src/codegeneration/CodeGeneration.o : ../../heliumNT/src/codegeneration/DerivingEq.hi
../../heliumNT/src/codegeneration/CodeGeneration.o : ../../heliumNT/src/codegeneration/DerivingShow.hi
../../heliumNT/src/codegeneration/CodeGeneration.o : ../../heliumNT/src/codegeneration/PatternMatch.hi
../../heliumNT/src/codegeneration/CodeGeneration.o : ../../Top/src/Top/Types.hi
../../heliumNT/src/codegeneration/CodeGeneration.o : ../../heliumNT/src/utils/Utils.hi
../../heliumNT/src/codegeneration/CodeGeneration.o : ../../lvm/src/lib/common/IdSet.hi
../../heliumNT/src/codegeneration/CodeGeneration.o : ../../lvm/src/lib/common/Id.hi
../../heliumNT/src/codegeneration/CodeGeneration.o : ../../heliumNT/src/staticanalysis/miscellaneous/TypeConversion.hi
../../heliumNT/src/codegeneration/CodeGeneration.o : ../../heliumNT/src/modulesystem/DictionaryEnvironment.hi
../../heliumNT/src/codegeneration/CodeGeneration.o : ../../heliumNT/src/modulesystem/ImportEnvironment.hi
../../heliumNT/src/codegeneration/CodeGeneration.o : ../../heliumNT/src/syntax/UHA_Range.hi
../../heliumNT/src/codegeneration/CodeGeneration.o : ../../heliumNT/src/syntax/UHA_Utils.hi
../../heliumNT/src/codegeneration/CodeGeneration.o : ../../heliumNT/src/syntax/UHA_Syntax.hi
../../heliumNT/src/utils/Utils.o : ../../heliumNT/src/utils/Utils.hs
../../heliumNT/src/utils/Utils.o : ../../heliumNT/src/utils/Logger.hi
../../heliumNT/src/utils/Logger.o : ../../heliumNT/src/utils/Logger.hs
../../heliumNT/src/utils/OSSpecific.o : ../../heliumNT/src/utils/OSSpecific.hs
../../heliumNT/src/utils/OneLiner.o : ../../heliumNT/src/utils/OneLiner.hs
../../heliumNT/src/utils/Similarity.o : ../../heliumNT/src/utils/Similarity.hs
../../heliumNT/src/utils/Similarity.o : ../../heliumNT/src/utils/Utils.hi
../../heliumNT/src/utils/Texts.o : ../../heliumNT/src/utils/Texts.hs
../../lvm/src/lib/common/Id.o : ../../lvm/src/lib/common/Id.hs
../../lvm/src/lib/common/Id.o : ../../lvm/src/lib/common/IntMap.hi
../../lvm/src/lib/common/Id.o : ../../lvm/src/lib/common/Standard.hi
../../lvm/src/lib/common/IdMap.o : ../../lvm/src/lib/common/IdMap.hs
../../lvm/src/lib/common/IdMap.o : ../../lvm/src/lib/common/Id.hi
../../lvm/src/lib/common/IdMap.o : ../../lvm/src/lib/common/IntMap.hi
../../lvm/src/lib/common/IdSet.o : ../../lvm/src/lib/common/IdSet.hs
../../lvm/src/lib/common/IdSet.o : ../../lvm/src/lib/common/IdMap.hi
../../lvm/src/lib/common/IdSet.o : ../../lvm/src/lib/common/Id.hi
../../lvm/src/lib/common/IntMap.o : ../../lvm/src/lib/common/IntMap.hs
../../lvm/src/lib/common/Byte.o : ../../lvm/src/lib/common/Byte.hs
../../lvm/src/lib/common/Byte.o : ../../lvm/src/lib/common/Standard.hi
../../lvm/src/lib/common/Byte.o : ../../lvm/src/lib/common/ghc/Special.hi
../../lvm/src/lib/common/Standard.o : ../../lvm/src/lib/common/Standard.hs
../../lvm/src/lib/common/Standard.o : ../../lvm/src/lib/common/ghc/Special.hi
../../lvm/src/lib/common/TopSort.o : ../../lvm/src/lib/common/TopSort.hs
../../lvm/src/lib/common/TopSort.o : ../../lvm/src/lib/common/ghc/Special.hi
../../lvm/src/lib/common/Set.o : ../../lvm/src/lib/common/Set.hs
../../lvm/src/lib/common/ghc/Special.o : ../../lvm/src/lib/common/ghc/Special.hs
../../lvm/src/lib/common/PPrint.o : ../../lvm/src/lib/common/PPrint.hs
../../lvm/src/lib/lvm/Lvm.o : ../../lvm/src/lib/lvm/Lvm.hs
../../lvm/src/lib/lvm/Lvm.o : ../../lvm/src/lib/lvm/Module.hi
../../lvm/src/lib/lvm/Lvm.o : ../../lvm/src/lib/lvm/Instr.hi
../../lvm/src/lib/lvm/Lvm.o : ../../lvm/src/lib/common/Id.hi
../../lvm/src/lib/lvm/Lvm.o : ../../lvm/src/lib/common/Byte.hi
../../lvm/src/lib/lvm/LvmPretty.o : ../../lvm/src/lib/lvm/LvmPretty.hs
../../lvm/src/lib/lvm/LvmPretty.o : ../../lvm/src/lib/lvm/ModulePretty.hi
../../lvm/src/lib/lvm/LvmPretty.o : ../../lvm/src/lib/lvm/InstrPretty.hi
../../lvm/src/lib/lvm/LvmPretty.o : ../../lvm/src/lib/lvm/Lvm.hi
../../lvm/src/lib/lvm/LvmPretty.o : ../../lvm/src/lib/common/PPrint.hi
../../lvm/src/lib/lvm/LvmWrite.o : ../../lvm/src/lib/lvm/LvmWrite.hs
../../lvm/src/lib/lvm/LvmWrite.o : ../../lvm/src/lib/lvm/Lvm.hi
../../lvm/src/lib/lvm/LvmWrite.o : ../../lvm/src/lib/lvm/Instr.hi
../../lvm/src/lib/lvm/LvmWrite.o : ../../lvm/src/lib/common/Byte.hi
../../lvm/src/lib/lvm/LvmWrite.o : ../../lvm/src/lib/common/IdMap.hi
../../lvm/src/lib/lvm/LvmWrite.o : ../../lvm/src/lib/common/Id.hi
../../lvm/src/lib/lvm/LvmWrite.o : ../../lvm/src/lib/common/Standard.hi
../../lvm/src/lib/lvm/LvmRead.o : ../../lvm/src/lib/lvm/LvmRead.hs
../../lvm/src/lib/lvm/LvmRead.o : ../../lvm/src/lib/lvm/ModulePretty.hi
../../lvm/src/lib/lvm/LvmRead.o : ../../lvm/src/lib/lvm/InstrPretty.hi
../../lvm/src/lib/lvm/LvmRead.o : ../../lvm/src/lib/lvm/Instr.hi
../../lvm/src/lib/lvm/LvmRead.o : ../../lvm/src/lib/lvm/Lvm.hi
../../lvm/src/lib/lvm/LvmRead.o : ../../lvm/src/lib/common/Byte.hi
../../lvm/src/lib/lvm/LvmRead.o : ../../lvm/src/lib/common/IdMap.hi
../../lvm/src/lib/lvm/LvmRead.o : ../../lvm/src/lib/common/Id.hi
../../lvm/src/lib/lvm/LvmRead.o : ../../lvm/src/lib/common/Standard.hi
../../lvm/src/lib/lvm/LvmRead.o : ../../lvm/src/lib/common/PPrint.hi
../../lvm/src/lib/lvm/LvmImport.o : ../../lvm/src/lib/lvm/LvmImport.hs
../../lvm/src/lib/lvm/LvmImport.o : ../../lvm/src/lib/common/Id.hi
../../lvm/src/lib/lvm/LvmImport.o : ../../lvm/src/lib/common/Byte.hi
../../lvm/src/lib/lvm/LvmImport.o : ../../lvm/src/lib/common/PPrint.hi
../../lvm/src/lib/lvm/LvmImport.o : ../../lvm/src/lib/lvm/InstrPretty.hi
../../lvm/src/lib/lvm/LvmImport.o : ../../lvm/src/lib/lvm/ModulePretty.hi
../../lvm/src/lib/lvm/LvmImport.o : ../../lvm/src/lib/lvm/LvmRead.hi
../../lvm/src/lib/lvm/LvmImport.o : ../../lvm/src/lib/lvm/Lvm.hi
../../lvm/src/lib/lvm/LvmImport.o : ../../lvm/src/lib/lvm/Module.hi
../../lvm/src/lib/lvm/LvmImport.o : ../../lvm/src/lib/common/IdMap.hi
../../lvm/src/lib/lvm/LvmImport.o : ../../lvm/src/lib/common/Standard.hi
../../lvm/src/lib/lvm/Module.o : ../../lvm/src/lib/lvm/Module.hs
../../lvm/src/lib/lvm/Module.o : ../../lvm/src/lib/lvm/Instr.hi
../../lvm/src/lib/lvm/Module.o : ../../lvm/src/lib/common/IdSet.hi
../../lvm/src/lib/lvm/Module.o : ../../lvm/src/lib/common/Id.hi
../../lvm/src/lib/lvm/Module.o : ../../lvm/src/lib/common/Byte.hi
../../lvm/src/lib/lvm/Module.o : ../../lvm/src/lib/common/Standard.hi
../../lvm/src/lib/lvm/ModulePretty.o : ../../lvm/src/lib/lvm/ModulePretty.hs
../../lvm/src/lib/lvm/ModulePretty.o : ../../lvm/src/lib/lvm/Module.hi
../../lvm/src/lib/lvm/ModulePretty.o : ../../lvm/src/lib/common/IdSet.hi
../../lvm/src/lib/lvm/ModulePretty.o : ../../lvm/src/lib/common/IdMap.hi
../../lvm/src/lib/lvm/ModulePretty.o : ../../lvm/src/lib/common/Id.hi
../../lvm/src/lib/lvm/ModulePretty.o : ../../lvm/src/lib/common/Byte.hi
../../lvm/src/lib/lvm/ModulePretty.o : ../../lvm/src/lib/common/PPrint.hi
../../lvm/src/lib/lvm/Instr.o : ../../lvm/src/lib/lvm/Instr.hs
../../lvm/src/lib/lvm/Instr.o : ../../lvm/src/lib/common/Byte.hi
../../lvm/src/lib/lvm/Instr.o : ../../lvm/src/lib/common/Id.hi
../../lvm/src/lib/lvm/Instr.o : ../../lvm/src/lib/common/Standard.hi
../../lvm/src/lib/lvm/InstrPretty.o : ../../lvm/src/lib/lvm/InstrPretty.hs
../../lvm/src/lib/lvm/InstrPretty.o : ../../lvm/src/lib/lvm/Instr.hi
../../lvm/src/lib/lvm/InstrPretty.o : ../../lvm/src/lib/common/Id.hi
../../lvm/src/lib/lvm/InstrPretty.o : ../../lvm/src/lib/common/Byte.hi
../../lvm/src/lib/lvm/InstrPretty.o : ../../lvm/src/lib/common/PPrint.hi
../../lvm/src/lib/lvm/InstrResolve.o : ../../lvm/src/lib/lvm/InstrResolve.hs
../../lvm/src/lib/lvm/InstrResolve.o : ../../lvm/src/lib/lvm/Instr.hi
../../lvm/src/lib/lvm/InstrResolve.o : ../../lvm/src/lib/common/IdMap.hi
../../lvm/src/lib/lvm/InstrResolve.o : ../../lvm/src/lib/common/Id.hi
../../lvm/src/lib/lvm/InstrResolve.o : ../../lvm/src/lib/common/Standard.hi
../../lvm/src/lib/lvm/InstrRewrite.o : ../../lvm/src/lib/lvm/InstrRewrite.hs
../../lvm/src/lib/lvm/InstrRewrite.o : ../../lvm/src/lib/lvm/InstrPretty.hi
../../lvm/src/lib/lvm/InstrRewrite.o : ../../lvm/src/lib/lvm/Instr.hi
../../lvm/src/lib/lvm/InstrRewrite.o : ../../lvm/src/lib/common/Id.hi
../../lvm/src/lib/lvm/InstrRewrite.o : ../../lvm/src/lib/common/Standard.hi
../../lvm/src/lib/asm/AsmOptimize.o : ../../lvm/src/lib/asm/AsmOptimize.hs
../../lvm/src/lib/asm/AsmOptimize.o : ../../lvm/src/lib/asm/AsmInline.hi
../../lvm/src/lib/asm/AsmOptimize.o : ../../lvm/src/lib/asm/Asm.hi
../../lvm/src/lib/asm/AsmInline.o : ../../lvm/src/lib/asm/AsmInline.hs
../../lvm/src/lib/asm/AsmInline.o : ../../lvm/src/lib/asm/AsmOccur.hi
../../lvm/src/lib/asm/AsmInline.o : ../../lvm/src/lib/asm/Asm.hi
../../lvm/src/lib/asm/AsmInline.o : ../../lvm/src/lib/lvm/Module.hi
../../lvm/src/lib/asm/AsmInline.o : ../../lvm/src/lib/common/IdMap.hi
../../lvm/src/lib/asm/AsmInline.o : ../../lvm/src/lib/common/Id.hi
../../lvm/src/lib/asm/AsmInline.o : ../../lvm/src/lib/common/Standard.hi
../../lvm/src/lib/asm/AsmOccur.o : ../../lvm/src/lib/asm/AsmOccur.hs
../../lvm/src/lib/asm/AsmOccur.o : ../../lvm/src/lib/asm/Asm.hi
../../lvm/src/lib/asm/AsmOccur.o : ../../lvm/src/lib/lvm/Module.hi
../../lvm/src/lib/asm/AsmOccur.o : ../../lvm/src/lib/common/IdMap.hi
../../lvm/src/lib/asm/AsmOccur.o : ../../lvm/src/lib/common/Id.hi
../../lvm/src/lib/asm/Asm.o : ../../lvm/src/lib/asm/Asm.hs
../../lvm/src/lib/asm/Asm.o : ../../lvm/src/lib/lvm/Module.hi
../../lvm/src/lib/asm/Asm.o : ../../lvm/src/lib/common/Id.hi
../../lvm/src/lib/asm/Asm.o : ../../lvm/src/lib/common/Byte.hi
../../lvm/src/lib/asm/AsmPretty.o : ../../lvm/src/lib/asm/AsmPretty.hs
../../lvm/src/lib/asm/AsmPretty.o : ../../lvm/src/lib/lvm/ModulePretty.hi
../../lvm/src/lib/asm/AsmPretty.o : ../../lvm/src/lib/asm/Asm.hi
../../lvm/src/lib/asm/AsmPretty.o : ../../lvm/src/lib/common/Id.hi
../../lvm/src/lib/asm/AsmPretty.o : ../../lvm/src/lib/common/Byte.hi
../../lvm/src/lib/asm/AsmPretty.o : ../../lvm/src/lib/common/PPrint.hi
../../lvm/src/lib/asm/AsmToLvm.o : ../../lvm/src/lib/asm/AsmToLvm.hs
../../lvm/src/lib/asm/AsmToLvm.o : ../../lvm/src/lib/asm/AsmOccur.hi
../../lvm/src/lib/asm/AsmToLvm.o : ../../lvm/src/lib/lvm/InstrRewrite.hi
../../lvm/src/lib/asm/AsmToLvm.o : ../../lvm/src/lib/lvm/InstrResolve.hi
../../lvm/src/lib/asm/AsmToLvm.o : ../../lvm/src/lib/lvm/Instr.hi
../../lvm/src/lib/asm/AsmToLvm.o : ../../lvm/src/lib/lvm/Lvm.hi
../../lvm/src/lib/asm/AsmToLvm.o : ../../lvm/src/lib/asm/Asm.hi
../../lvm/src/lib/asm/AsmToLvm.o : ../../lvm/src/lib/common/IdMap.hi
../../lvm/src/lib/asm/AsmToLvm.o : ../../lvm/src/lib/common/Id.hi
../../lvm/src/lib/asm/AsmToLvm.o : ../../lvm/src/lib/common/Standard.hi
../../lvm/src/lib/core/Core.o : ../../lvm/src/lib/core/Core.hs
../../lvm/src/lib/core/Core.o : ../../lvm/src/lib/common/IdSet.hi
../../lvm/src/lib/core/Core.o : ../../lvm/src/lib/common/IdMap.hi
../../lvm/src/lib/core/Core.o : ../../lvm/src/lib/lvm/Module.hi
../../lvm/src/lib/core/Core.o : ../../lvm/src/lib/common/Id.hi
../../lvm/src/lib/core/Core.o : ../../lvm/src/lib/common/Byte.hi
../../lvm/src/lib/core/CorePretty.o : ../../lvm/src/lib/core/CorePretty.hs
../../lvm/src/lib/core/CorePretty.o : ../../lvm/src/lib/lvm/ModulePretty.hi
../../lvm/src/lib/core/CorePretty.o : ../../lvm/src/lib/core/Core.hi
../../lvm/src/lib/core/CorePretty.o : ../../lvm/src/lib/common/IdSet.hi
../../lvm/src/lib/core/CorePretty.o : ../../lvm/src/lib/common/Id.hi
../../lvm/src/lib/core/CorePretty.o : ../../lvm/src/lib/common/Byte.hi
../../lvm/src/lib/core/CorePretty.o : ../../lvm/src/lib/common/PPrint.hi
../../lvm/src/lib/core/CoreToAsm.o : ../../lvm/src/lib/core/CoreToAsm.hs
../../lvm/src/lib/core/CoreToAsm.o : ../../lvm/src/lib/core/CoreLift.hi
../../lvm/src/lib/core/CoreToAsm.o : ../../lvm/src/lib/core/CoreLetSort.hi
../../lvm/src/lib/core/CoreToAsm.o : ../../lvm/src/lib/core/CoreFreeVar.hi
../../lvm/src/lib/core/CoreToAsm.o : ../../lvm/src/lib/core/CoreNormalize.hi
../../lvm/src/lib/core/CoreToAsm.o : ../../lvm/src/lib/core/CoreSaturate.hi
../../lvm/src/lib/core/CoreToAsm.o : ../../lvm/src/lib/core/CoreNoShadow.hi
../../lvm/src/lib/core/CoreToAsm.o : ../../lvm/src/lib/asm/Asm.hi
../../lvm/src/lib/core/CoreToAsm.o : ../../lvm/src/lib/core/Core.hi
../../lvm/src/lib/core/CoreToAsm.o : ../../lvm/src/lib/common/IdSet.hi
../../lvm/src/lib/core/CoreToAsm.o : ../../lvm/src/lib/common/IdMap.hi
../../lvm/src/lib/core/CoreToAsm.o : ../../lvm/src/lib/common/Id.hi
../../lvm/src/lib/core/CoreToAsm.o : ../../lvm/src/lib/common/Standard.hi
../../lvm/src/lib/core/CoreParse.o : ../../lvm/src/lib/core/CoreParse.hs
../../lvm/src/lib/core/CoreParse.o : ../../lvm/src/lib/core/CorePretty.hi
../../lvm/src/lib/core/CoreParse.o : ../../lvm/src/lib/core/CoreLexer.hi
../../lvm/src/lib/core/CoreParse.o : ../../lvm/src/lib/core/Core.hi
../../lvm/src/lib/core/CoreParse.o : ../../lvm/src/lib/common/IdSet.hi
../../lvm/src/lib/core/CoreParse.o : ../../lvm/src/lib/common/IdMap.hi
../../lvm/src/lib/core/CoreParse.o : ../../lvm/src/lib/common/Id.hi
../../lvm/src/lib/core/CoreParse.o : ../../lvm/src/lib/common/Byte.hi
../../lvm/src/lib/core/CoreParse.o : ../../lvm/src/lib/common/Standard.hi
../../lvm/src/lib/core/CoreParse.o : ../../lvm/src/lib/common/PPrint.hi
../../lvm/src/lib/core/CoreLexer.o : ../../lvm/src/lib/core/CoreLexer.hs
../../lvm/src/lib/core/CoreLexer.o : ../../lvm/src/lib/common/Id.hi
../../lvm/src/lib/core/CoreLexer.o : ../../lvm/src/lib/common/Standard.hi
../../lvm/src/lib/core/CoreNormalize.o : ../../lvm/src/lib/core/CoreNormalize.hs
../../lvm/src/lib/core/CoreNormalize.o : ../../lvm/src/lib/core/Core.hi
../../lvm/src/lib/core/CoreNormalize.o : ../../lvm/src/lib/common/IdSet.hi
../../lvm/src/lib/core/CoreNormalize.o : ../../lvm/src/lib/common/Id.hi
../../lvm/src/lib/core/CoreRemoveDead.o : ../../lvm/src/lib/core/CoreRemoveDead.hs
../../lvm/src/lib/core/CoreRemoveDead.o : ../../lvm/src/lib/core/Core.hi
../../lvm/src/lib/core/CoreRemoveDead.o : ../../lvm/src/lib/common/IdSet.hi
../../lvm/src/lib/core/CoreRemoveDead.o : ../../lvm/src/lib/common/Id.hi
../../lvm/src/lib/core/CoreRemoveDead.o : ../../lvm/src/lib/common/Standard.hi
../../lvm/src/lib/core/CoreRemoveDead.o : ../../lvm/src/lib/common/Set.hi
../../lvm/src/lib/core/CoreNoShadow.o : ../../lvm/src/lib/core/CoreNoShadow.hs
../../lvm/src/lib/core/CoreNoShadow.o : ../../lvm/src/lib/core/Core.hi
../../lvm/src/lib/core/CoreNoShadow.o : ../../lvm/src/lib/common/IdSet.hi
../../lvm/src/lib/core/CoreNoShadow.o : ../../lvm/src/lib/common/IdMap.hi
../../lvm/src/lib/core/CoreNoShadow.o : ../../lvm/src/lib/common/Id.hi
../../lvm/src/lib/core/CoreFreeVar.o : ../../lvm/src/lib/core/CoreFreeVar.hs
../../lvm/src/lib/core/CoreFreeVar.o : ../../lvm/src/lib/core/Core.hi
../../lvm/src/lib/core/CoreFreeVar.o : ../../lvm/src/lib/common/IdSet.hi
../../lvm/src/lib/core/CoreFreeVar.o : ../../lvm/src/lib/common/Id.hi
../../lvm/src/lib/core/CoreFreeVar.o : ../../lvm/src/lib/common/Standard.hi
../../lvm/src/lib/core/CoreLetSort.o : ../../lvm/src/lib/core/CoreLetSort.hs
../../lvm/src/lib/core/CoreLetSort.o : ../../lvm/src/lib/core/Core.hi
../../lvm/src/lib/core/CoreLetSort.o : ../../lvm/src/lib/common/IdSet.hi
../../lvm/src/lib/core/CoreLetSort.o : ../../lvm/src/lib/common/Id.hi
../../lvm/src/lib/core/CoreLetSort.o : ../../lvm/src/lib/common/TopSort.hi
../../lvm/src/lib/core/CoreLift.o : ../../lvm/src/lib/core/CoreLift.hs
../../lvm/src/lib/core/CoreLift.o : ../../lvm/src/lib/core/Core.hi
../../lvm/src/lib/core/CoreLift.o : ../../lvm/src/lib/common/IdSet.hi
../../lvm/src/lib/core/CoreLift.o : ../../lvm/src/lib/common/IdMap.hi
../../lvm/src/lib/core/CoreLift.o : ../../lvm/src/lib/common/Id.hi
../../lvm/src/lib/core/CoreLift.o : ../../lvm/src/lib/common/Standard.hi
../../lvm/src/lib/core/CoreSaturate.o : ../../lvm/src/lib/core/CoreSaturate.hs
../../lvm/src/lib/core/CoreSaturate.o : ../../lvm/src/lib/core/Core.hi
../../lvm/src/lib/core/CoreSaturate.o : ../../lvm/src/lib/common/IdMap.hi
../../lvm/src/lib/core/CoreSaturate.o : ../../lvm/src/lib/common/Id.hi
../../heliumNT/src/main/Version.o : ../../heliumNT/src/main/Version.hs
../../heliumNT/src/main/Args.o : ../../heliumNT/src/main/Args.hs
../../heliumNT/src/main/Args.o : ../../heliumNT/src/utils/Utils.hi
../../heliumNT/src/main/Args.o : ../../heliumNT/src/main/Version.hi
../../heliumNT/src/main/Compile.o : ../../heliumNT/src/main/Compile.hs
../../heliumNT/src/main/Compile.o : ../../heliumNT/src/staticanalysis/messages/StaticErrors.hi
../../heliumNT/src/main/Compile.o : ../../heliumNT/src/utils/Utils.hi
../../heliumNT/src/main/Compile.o : ../../heliumNT/src/main/CompileUtils.hi
../../heliumNT/src/main/Compile.o : ../../heliumNT/src/main/PhaseCodeGenerator.hi
../../heliumNT/src/main/Compile.o : ../../heliumNT/src/main/PhaseDesugarer.hi
../../heliumNT/src/main/Compile.o : ../../heliumNT/src/main/PhaseTypeInferencer.hi
../../heliumNT/src/main/Compile.o : ../../heliumNT/src/main/PhaseTypingStrategies.hi
../../heliumNT/src/main/Compile.o : ../../heliumNT/src/main/PhaseKindInferencer.hi
../../heliumNT/src/main/Compile.o : ../../heliumNT/src/main/PhaseStaticChecks.hi
../../heliumNT/src/main/Compile.o : ../../heliumNT/src/main/PhaseResolveOperators.hi
../../heliumNT/src/main/Compile.o : ../../heliumNT/src/main/PhaseImport.hi
../../heliumNT/src/main/Compile.o : ../../heliumNT/src/main/PhaseParser.hi
../../heliumNT/src/main/Compile.o : ../../heliumNT/src/main/PhaseLexer.hi
../../heliumNT/src/main/PhaseLexer.o : ../../heliumNT/src/main/PhaseLexer.hs
../../heliumNT/src/main/PhaseLexer.o : ../../heliumNT/src/parser/LayoutRule.hi
../../heliumNT/src/main/PhaseLexer.o : ../../heliumNT/src/parser/Lexer.hi
../../heliumNT/src/main/PhaseLexer.o : ../../heliumNT/src/parser/LexerToken.hi
../../heliumNT/src/main/PhaseLexer.o : ../../heliumNT/src/main/CompileUtils.hi
../../heliumNT/src/main/PhaseParser.o : ../../heliumNT/src/main/PhaseParser.hs
../../heliumNT/src/main/PhaseParser.o : ../../parsec/Text/ParserCombinators/ParsecSpecial/Error.hi
../../heliumNT/src/main/PhaseParser.o : ../../heliumNT/src/parser/ParseLibrary.hi
../../heliumNT/src/main/PhaseParser.o : ../../heliumNT/src/parser/Parser.hi
../../heliumNT/src/main/PhaseParser.o : ../../heliumNT/src/parser/LexerToken.hi
../../heliumNT/src/main/PhaseParser.o : ../../heliumNT/src/main/CompileUtils.hi
../../heliumNT/src/main/PhaseImport.o : ../../heliumNT/src/main/PhaseImport.hs
../../heliumNT/src/main/PhaseImport.o : ../../lvm/src/lib/core/CorePretty.hi
../../heliumNT/src/main/PhaseImport.o : ../../heliumNT/src/modulesystem/ExtractImportDecls.hi
../../heliumNT/src/main/PhaseImport.o : ../../heliumNT/src/modulesystem/CoreToImportEnv.hi
../../heliumNT/src/main/PhaseImport.o : ../../lvm/src/lib/common/Id.hi
../../heliumNT/src/main/PhaseImport.o : ../../lvm/src/lib/lvm/LvmImport.hi
../../heliumNT/src/main/PhaseImport.o : ../../lvm/src/lib/common/Standard.hi
../../heliumNT/src/main/PhaseImport.o : ../../heliumNT/src/syntax/UHA_Range.hi
../../heliumNT/src/main/PhaseImport.o : ../../heliumNT/src/syntax/UHA_Utils.hi
../../heliumNT/src/main/PhaseImport.o : ../../heliumNT/src/syntax/UHA_Syntax.hi
../../heliumNT/src/main/PhaseImport.o : ../../lvm/src/lib/core/Core.hi
../../heliumNT/src/main/PhaseImport.o : ../../heliumNT/src/main/CompileUtils.hi
../../heliumNT/src/main/PhaseResolveOperators.o : ../../heliumNT/src/main/PhaseResolveOperators.hs
../../heliumNT/src/main/PhaseResolveOperators.o : ../../heliumNT/src/syntax/UHA_Pretty.hi
../../heliumNT/src/main/PhaseResolveOperators.o : ../../heliumNT/src/parser/ResolveOperators.hi
../../heliumNT/src/main/PhaseResolveOperators.o : ../../heliumNT/src/main/CompileUtils.hi
../../heliumNT/src/main/PhaseStaticChecks.o : ../../heliumNT/src/main/PhaseStaticChecks.hs
../../heliumNT/src/main/PhaseStaticChecks.o : ../../heliumNT/src/staticanalysis/messages/StaticErrors.hi
../../heliumNT/src/main/PhaseStaticChecks.o : ../../Top/src/Top/Types.hi
../../heliumNT/src/main/PhaseStaticChecks.o : ../../heliumNT/src/syntax/UHA_Syntax.hi
../../heliumNT/src/main/PhaseStaticChecks.o : ../../heliumNT/src/staticanalysis/staticchecks/StaticChecks.hi
../../heliumNT/src/main/PhaseStaticChecks.o : ../../heliumNT/src/staticanalysis/messages/Warnings.hi
../../heliumNT/src/main/PhaseStaticChecks.o : ../../heliumNT/src/main/CompileUtils.hi
../../heliumNT/src/main/PhaseTypingStrategies.o : ../../heliumNT/src/main/PhaseTypingStrategies.hs
../../heliumNT/src/main/PhaseTypingStrategies.o : ../../Top/src/Top/Types.hi
../../heliumNT/src/main/PhaseTypingStrategies.o : ../../heliumNT/src/syntax/UHA_Syntax.hi
../../heliumNT/src/main/PhaseTypingStrategies.o : ../../heliumNT/src/staticanalysis/directives/TS_Compile.hi
../../heliumNT/src/main/PhaseTypingStrategies.o : ../../lvm/src/lib/core/Core.hi
../../heliumNT/src/main/PhaseTypingStrategies.o : ../../heliumNT/src/main/CompileUtils.hi
../../heliumNT/src/main/PhaseTypeInferencer.o : ../../heliumNT/src/main/PhaseTypeInferencer.hs
../../heliumNT/src/main/PhaseTypeInferencer.o : ../../heliumNT/src/staticanalysis/messages/TypeErrors.hi
../../heliumNT/src/main/PhaseTypeInferencer.o : ../../heliumNT/src/syntax/UHA_Syntax.hi
../../heliumNT/src/main/PhaseTypeInferencer.o : ../../heliumNT/src/modulesystem/DictionaryEnvironment.hi
../../heliumNT/src/main/PhaseTypeInferencer.o : ../../heliumNT/src/staticanalysis/inferencers/TypeInferencing.hi
../../heliumNT/src/main/PhaseTypeInferencer.o : ../../heliumNT/src/staticanalysis/messages/Warnings.hi
../../heliumNT/src/main/PhaseTypeInferencer.o : ../../heliumNT/src/main/CompileUtils.hi
../../heliumNT/src/main/PhaseDesugarer.o : ../../heliumNT/src/main/PhaseDesugarer.hs
../../heliumNT/src/main/PhaseDesugarer.o : ../../heliumNT/src/syntax/UHA_Utils.hi
../../heliumNT/src/main/PhaseDesugarer.o : ../../Top/src/Top/Types.hi
../../heliumNT/src/main/PhaseDesugarer.o : ../../heliumNT/src/codegeneration/CodeGeneration.hi
../../heliumNT/src/main/PhaseDesugarer.o : ../../heliumNT/src/modulesystem/DictionaryEnvironment.hi
../../heliumNT/src/main/PhaseDesugarer.o : ../../heliumNT/src/modulesystem/ImportEnvironment.hi
../../heliumNT/src/main/PhaseDesugarer.o : ../../heliumNT/src/syntax/UHA_Range.hi
../../heliumNT/src/main/PhaseDesugarer.o : ../../heliumNT/src/syntax/UHA_Syntax.hi
../../heliumNT/src/main/PhaseDesugarer.o : ../../lvm/src/lib/core/CoreRemoveDead.hi
../../heliumNT/src/main/PhaseDesugarer.o : ../../lvm/src/lib/core/Core.hi
../../heliumNT/src/main/PhaseDesugarer.o : ../../lvm/src/lib/core/CorePretty.hi
../../heliumNT/src/main/PhaseDesugarer.o : ../../heliumNT/src/main/CompileUtils.hi
../../heliumNT/src/main/PhaseCodeGenerator.o : ../../heliumNT/src/main/PhaseCodeGenerator.hs
../../heliumNT/src/main/PhaseCodeGenerator.o : ../../heliumNT/src/codegeneration/CoreToLvm.hi
../../heliumNT/src/main/PhaseCodeGenerator.o : ../../heliumNT/src/main/CompileUtils.hi
../../heliumNT/src/main/PhaseCodeGenerator.o : ../../lvm/src/lib/core/Core.hi
../../heliumNT/src/main/CompileUtils.o : ../../heliumNT/src/main/CompileUtils.hs
../../heliumNT/src/main/CompileUtils.o : ../../lvm/src/lib/common/Standard.hi
../../heliumNT/src/main/CompileUtils.o : ../../heliumNT/src/syntax/UHA_Syntax.hi
../../heliumNT/src/main/CompileUtils.o : ../../heliumNT/src/modulesystem/ImportEnvironment.hi
../../heliumNT/src/main/CompileUtils.o : ../../heliumNT/src/utils/Logger.hi
../../heliumNT/src/main/CompileUtils.o : ../../heliumNT/src/utils/Utils.hi
../../heliumNT/src/main/CompileUtils.o : ../../heliumNT/src/staticanalysis/messages/HeliumMessages.hi
../../heliumNT/src/main/CompileUtils.o : ../../heliumNT/src/staticanalysis/messages/Messages.hi
../../heliumNT/src/main/CompileUtils.o : ../../heliumNT/src/main/Args.hi
../../heliumNT/src/main/PhaseKindInferencer.o : ../../heliumNT/src/main/PhaseKindInferencer.hs
../../heliumNT/src/main/PhaseKindInferencer.o : ../../heliumNT/src/staticanalysis/messages/KindErrors.hi
../../heliumNT/src/main/PhaseKindInferencer.o : ../../Top/src/Top/Types.hi
../../heliumNT/src/main/PhaseKindInferencer.o : ../../heliumNT/src/modulesystem/ImportEnvironment.hi
../../heliumNT/src/main/PhaseKindInferencer.o : ../../heliumNT/src/staticanalysis/inferencers/KindInferencing.hi
../../heliumNT/src/main/PhaseKindInferencer.o : ../../heliumNT/src/main/CompileUtils.hi
../../heliumNT/src/main/PhaseKindInferencer.o : ../../heliumNT/src/main/Args.hi
../../parsec/Text/ParserCombinators/ParsecSpecial.o : ../../parsec/Text/ParserCombinators/ParsecSpecial.hs
../../parsec/Text/ParserCombinators/ParsecSpecial.o : ../../parsec/Text/ParserCombinators/ParsecSpecial/Char.hi
../../parsec/Text/ParserCombinators/ParsecSpecial.o : ../../parsec/Text/ParserCombinators/ParsecSpecial/Combinator.hi
../../parsec/Text/ParserCombinators/ParsecSpecial.o : ../../parsec/Text/ParserCombinators/ParsecSpecial/Prim.hi
../../parsec/Text/ParserCombinators/ParsecSpecial.o : ../../parsec/Text/ParserCombinators/ParsecSpecial/Error.hi
../../parsec/Text/ParserCombinators/ParsecSpecial.o : ../../parsec/Text/ParserCombinators/ParsecSpecial/Pos.hi
../../parsec/Text/ParserCombinators/ParsecSpecial/Char.o : ../../parsec/Text/ParserCombinators/ParsecSpecial/Char.hs
../../parsec/Text/ParserCombinators/ParsecSpecial/Char.o : ../../parsec/Text/ParserCombinators/ParsecSpecial/Prim.hi
../../parsec/Text/ParserCombinators/ParsecSpecial/Char.o : ../../parsec/Text/ParserCombinators/ParsecSpecial/Pos.hi
../../parsec/Text/ParserCombinators/ParsecSpecial/Combinator.o : ../../parsec/Text/ParserCombinators/ParsecSpecial/Combinator.hs
../../parsec/Text/ParserCombinators/ParsecSpecial/Combinator.o : ../../parsec/Text/ParserCombinators/ParsecSpecial/Prim.hi
../../parsec/Text/ParserCombinators/ParsecSpecial/Error.o : ../../parsec/Text/ParserCombinators/ParsecSpecial/Error.hs
../../parsec/Text/ParserCombinators/ParsecSpecial/Error.o : ../../parsec/Text/ParserCombinators/ParsecSpecial/Pos.hi
../../parsec/Text/ParserCombinators/ParsecSpecial/Expr.o : ../../parsec/Text/ParserCombinators/ParsecSpecial/Expr.hs
../../parsec/Text/ParserCombinators/ParsecSpecial/Expr.o : ../../parsec/Text/ParserCombinators/ParsecSpecial/Combinator.hi
../../parsec/Text/ParserCombinators/ParsecSpecial/Expr.o : ../../parsec/Text/ParserCombinators/ParsecSpecial/Prim.hi
../../parsec/Text/ParserCombinators/ParsecSpecial/Language.o : ../../parsec/Text/ParserCombinators/ParsecSpecial/Language.hs
../../parsec/Text/ParserCombinators/ParsecSpecial/Language.o : ../../parsec/Text/ParserCombinators/ParsecSpecial/Token.hi
../../parsec/Text/ParserCombinators/ParsecSpecial/Language.o : ../../parsec/Text/ParserCombinators/ParsecSpecial.hi
../../parsec/Text/ParserCombinators/ParsecSpecial/Perm.o : ../../parsec/Text/ParserCombinators/ParsecSpecial/Perm.hs
../../parsec/Text/ParserCombinators/ParsecSpecial/Perm.o : ../../parsec/Text/ParserCombinators/ParsecSpecial.hi
../../parsec/Text/ParserCombinators/ParsecSpecial/Pos.o : ../../parsec/Text/ParserCombinators/ParsecSpecial/Pos.hs
../../parsec/Text/ParserCombinators/ParsecSpecial/Prim.o : ../../parsec/Text/ParserCombinators/ParsecSpecial/Prim.hs
../../parsec/Text/ParserCombinators/ParsecSpecial/Prim.o : ../../parsec/Text/ParserCombinators/ParsecSpecial/Error.hi
../../parsec/Text/ParserCombinators/ParsecSpecial/Prim.o : ../../parsec/Text/ParserCombinators/ParsecSpecial/Pos.hi
../../parsec/Text/ParserCombinators/ParsecSpecial/Token.o : ../../parsec/Text/ParserCombinators/ParsecSpecial/Token.hs
../../parsec/Text/ParserCombinators/ParsecSpecial/Token.o : ../../parsec/Text/ParserCombinators/ParsecSpecial.hi
../../heliumNT/src/modulesystem/ImportEnvironment.o : ../../heliumNT/src/modulesystem/ImportEnvironment.hs
../../heliumNT/src/modulesystem/ImportEnvironment.o : ../../heliumNT/src/staticanalysis/directives/TS_CoreSyntax.hi
../../heliumNT/src/modulesystem/ImportEnvironment.o : ../../heliumNT/src/staticanalysis/heuristics/RepairHeuristics.hi
../../heliumNT/src/modulesystem/ImportEnvironment.o : ../../heliumNT/src/staticanalysis/messages/Messages.hi
../../heliumNT/src/modulesystem/ImportEnvironment.o : ../../heliumNT/src/parser/OperatorTable.hi
../../heliumNT/src/modulesystem/ImportEnvironment.o : ../../Top/src/Top/Types.hi
../../heliumNT/src/modulesystem/ImportEnvironment.o : ../../heliumNT/src/syntax/UHA_Utils.hi
../../heliumNT/src/modulesystem/ImportEnvironment.o : ../../heliumNT/src/syntax/UHA_Syntax.hi
../../heliumNT/src/modulesystem/ImportEnvironment.o : ../../heliumNT/src/utils/Utils.hi
../../heliumNT/src/modulesystem/DictionaryEnvironment.o : ../../heliumNT/src/modulesystem/DictionaryEnvironment.hs
../../heliumNT/src/modulesystem/DictionaryEnvironment.o : ../../Top/src/Top/Types.hi
../../heliumNT/src/modulesystem/DictionaryEnvironment.o : ../../heliumNT/src/utils/Utils.hi
../../heliumNT/src/modulesystem/DictionaryEnvironment.o : ../../heliumNT/src/syntax/UHA_Utils.hi
../../heliumNT/src/modulesystem/DictionaryEnvironment.o : ../../heliumNT/src/syntax/UHA_Syntax.hi
../../heliumNT/src/modulesystem/CoreToImportEnv.o : ../../heliumNT/src/modulesystem/CoreToImportEnv.hs
../../heliumNT/src/modulesystem/CoreToImportEnv.o : ../../heliumNT/src/syntax/UHA_Range.hi
../../heliumNT/src/modulesystem/CoreToImportEnv.o : ../../lvm/src/lib/common/Byte.hi
../../heliumNT/src/modulesystem/CoreToImportEnv.o : ../../Top/src/Top/Types.hi
../../heliumNT/src/modulesystem/CoreToImportEnv.o : ../../heliumNT/src/parser/OperatorTable.hi
../../heliumNT/src/modulesystem/CoreToImportEnv.o : ../../heliumNT/src/syntax/UHA_Syntax.hi
../../heliumNT/src/modulesystem/CoreToImportEnv.o : ../../lvm/src/lib/common/Id.hi
../../heliumNT/src/modulesystem/CoreToImportEnv.o : ../../heliumNT/src/syntax/UHA_Utils.hi
../../heliumNT/src/modulesystem/CoreToImportEnv.o : ../../heliumNT/src/modulesystem/ImportEnvironment.hi
../../heliumNT/src/modulesystem/CoreToImportEnv.o : ../../heliumNT/src/parser/Parser.hi
../../heliumNT/src/modulesystem/CoreToImportEnv.o : ../../heliumNT/src/parser/Lexer.hi
../../heliumNT/src/modulesystem/CoreToImportEnv.o : ../../heliumNT/src/parser/ParseLibrary.hi
../../heliumNT/src/modulesystem/CoreToImportEnv.o : ../../heliumNT/src/staticanalysis/miscellaneous/TypeConversion.hi
../../heliumNT/src/modulesystem/CoreToImportEnv.o : ../../heliumNT/src/utils/Utils.hi
../../heliumNT/src/modulesystem/CoreToImportEnv.o : ../../lvm/src/lib/core/Core.hi
../../heliumNT/src/modulesystem/ExtractImportDecls.o : ../../heliumNT/src/modulesystem/ExtractImportDecls.hs
../../heliumNT/src/modulesystem/ExtractImportDecls.o : ../../heliumNT/src/utils/Utils.hi
../../heliumNT/src/modulesystem/ExtractImportDecls.o : ../../lvm/src/lib/core/Core.hi
../../heliumNT/src/modulesystem/ExtractImportDecls.o : ../../lvm/src/lib/common/Id.hi
../../heliumNT/src/modulesystem/ExtractImportDecls.o : ../../heliumNT/src/syntax/UHA_Utils.hi
../../heliumNT/src/modulesystem/ExtractImportDecls.o : ../../heliumNT/src/syntax/UHA_Syntax.hi
../../uust/lib/util/UU/Util/BinaryTrees.o : ../../uust/lib/util/UU/Util/BinaryTrees.hs
../../uust/lib/scanner/UU/Scanner/Position.o : ../../uust/lib/scanner/UU/Scanner/Position.hs
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
../../uust/lib/parsing/UU/Parsing/CharParser.o : ../../uust/lib/parsing/UU/Parsing/CharParser.hs
../../uust/lib/parsing/UU/Parsing/CharParser.o : ../../uust/lib/scanner/UU/Scanner/Position.hi
../../uust/lib/parsing/UU/Parsing/CharParser.o : ../../uust/lib/parsing/UU/Parsing/Interface.hi
../../proxima/src/generator/GenCommon.o : ../../proxima/src/generator/GenCommon.hs
../../proxima/src/generator/GenParser.o : ../../proxima/src/generator/GenParser.hs
../../proxima/src/generator/GenParser.o : ../../uust/lib/parsing/UU/Parsing/CharParser.hi
../../proxima/src/generator/GenParser.o : ../../uust/lib/parsing/UU/Parsing.hi
../../proxima/src/generator/GenParser.o : ../../proxima/src/generator/GenCommon.hi
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
../../proxima/src/generator/Generator.o : ../../proxima/src/generator/Generator.hs
../../proxima/src/generator/Generator.o : ../../proxima/src/generator/GenProxParser.hi
../../proxima/src/generator/Generator.o : ../../proxima/src/generator/GenDocUtils.hi
../../proxima/src/generator/Generator.o : ../../proxima/src/generator/GenEditable.hi
../../proxima/src/generator/Generator.o : ../../proxima/src/generator/GenTypes.hi
../../proxima/src/generator/Generator.o : ../../proxima/src/generator/GenAG.hi
../../proxima/src/generator/Generator.o : ../../proxima/src/generator/GenParser.hi
../../proxima/src/generator/Generator.o : ../../proxima/src/generator/GenCommon.hi
# DO NOT DELETE: End of Haskell dependencies

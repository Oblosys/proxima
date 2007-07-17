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

# it would be nice if the document specific makefile could always execute this makefile 
# first. Is this possible?

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
EXE = .exe

SHELL = /bin/sh

default: all
all: agc generator arranger

.PHONY : default all always

PROXIMADIR = ../../proxima

PROXIMABINDIR = $(PROXIMADIR)/bin
PROXIMASRCDIR = $(PROXIMADIR)/src
UUAGCDIR = $(PROXIMADIR)/../uuagc/src
GENERATORSRCDIR = $(PROXIMADIR)/../generator/src

AGC             = $(PROXIMABINDIR)/uuagc$(EXE)
GEN	        = $(PROXIMABINDIR)/generate$(EXE)


HC_OPTS = -static -fglasgow-exts -fallow-undecidable-instances -fno-monomorphism-restriction\
		-package wx -package parsec\
		 -i$(IFACES) $(EXTRA_OPTS)


# Proxima directories

PRESENTATIONDIR = $(PROXIMADIR)/src/presentation
ARRANGEMENTDIR  = $(PROXIMADIR)/src/arrangement
COMMONDIR       = $(PROXIMADIR)/src/common
EVALUATIONDIR   = $(PROXIMADIR)/src/evaluation
LAYOUTDIR       = $(PROXIMADIR)/src/layout
MAINDIR         = $(PROXIMADIR)/src/main
PRESENTATIONDIR = $(PROXIMADIR)/src/presentation
RENDERINGDIR    = $(PROXIMADIR)/src/rendering
PROXIMADIRS	= $(ARRANGEMENTDIR):$(COMMONDIR):$(EVALUATIONDIR):$(LAYOUTDIR):$(MAINDIR):$(PRESENTATIONDIR):$(RENDERINGDIR)


GENERATORDIRS = $(GENERATORSRCDIR):$(UUAGCDIR)


# The Generator
generator : $(GEN)

$(GEN): always
	@echo ghc: compiling Generator
	@$(HC) --make -o $(GEN) $(GENERATORSRCDIR)/Main.hs -i$(GENERATORDIRS) -fglasgow-exts

# The AG compiler

agc: $(AGC)

$(AGC): always
	@echo ghc: compiling uuagc compiler with -O2
	@$(HC) --make -o $(AGC) $(UUAGCDIR)/Ag.hs -i$(UUAGCDIR) -fglasgow-exts -O2

arranger: $(ARRANGEMENTDIR)/ArrangerAG.hs

# The arranger AG

$(ARRANGEMENTDIR)/ArrangerAG.hs : $(AGC) $(ARRANGEMENTDIR)/ArrangerAG.ag \
	
	@echo uuagc: compiling arrangerAG.ag
	@$(AGC) -o $(ARRANGEMENTDIR)/ArrangerAG.hs \
	-cfpm --Werrors --module=ArrangerAG \
	$(ARRANGEMENTDIR)/ArrangerAG.ag -P $(ARRANGEMENTDIR)

#	-cfpm --Werrors --module=ArrangerAG \
#	-cfpm --visit --case --strictdata --strictwrap --unbox --Werrors --module=ArrangerAG \

# -module is because otherwise uuagc uses ../../presentation/PresentationAG as module name
	

clean:
	$(FIND) $(PROXIMASRCDIR) -name "*.hi" -delete
	$(FIND) $(PROXIMASRCDIR) -name "*.o" -delete
	

clean-agc:
	$(FIND) $(UUAGCDIR) -name "*.hi" -delete
	$(FIND) $(UUAGCDIR) -name "*.o" -delete
	$(FIND) $(PROXIMABINDIR) -name "uuagc.*" -delete

clean-all: clean clean-agc

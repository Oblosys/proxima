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


.PHONY: default all agc generator arranger scanner always

PROXIMADIR = ../../proxima

PROXIMABINDIR = $(PROXIMADIR)/bin
PROXIMASRCDIR = $(PROXIMADIR)/src
UUAGCDIR = $(PROXIMADIR)/../uuagc/src
UUAGCDERIVEDDIR = $(PROXIMADIR)/../uuagc/src-derived
GENERATORSRCDIR = $(PROXIMADIR)/../generator/src

AGC             = $(PROXIMABINDIR)/uuagc$(EXE)
GEN	        = $(PROXIMABINDIR)/generate$(EXE)

# Proxima directories

ARRANGEMENTDIR  = $(PROXIMADIR)/src/Arrangement
LAYOUTDIR       = $(PROXIMADIR)/src/Layout

#PRESENTATIONDIR = $(PROXIMADIR)/src/presentation
#COMMONDIR       = $(PROXIMADIR)/src/common
#EVALUATIONDIR   = $(PROXIMADIR)/src/evaluation
#MAINDIR         = $(PROXIMADIR)/src/main
#RENDERINGDIR    = $(PROXIMADIR)/src/rendering
#PROXIMADIRS	= $(ARRANGEMENTDIR):$(COMMONDIR):$(EVALUATIONDIR):$(LAYOUTDIR):$(MAINDIR):$(PRESENTATIONDIR):$(RENDERINGDIR)

GENERATORDIRS = $(GENERATORSRCDIR):$(UUAGCDIR)


# Rules:

default: all

all: agc generator arranger scanner

# The Generator
generator : $(GEN)

# the always phony target is because we don't want to put all the generator sources here
$(GEN): always
	@echo ghc: compiling Generator
	@$(HC) --make -o $(GEN) $(GENERATORSRCDIR)/Main.hs -i$(GENERATORDIRS) -fglasgow-exts

# The AG compiler

agc: $(AGC)

# the always phony target is because we don't want to put all the ag compiler sources here
$(AGC): always
	@echo ghc: compiling uuagc compiler with -O2
	$(HC) --make -o $(AGC) $(UUAGCDIR)/Ag.hs -i$(UUAGCDIR) -i$(UUAGCDERIVEDDIR) -fglasgow-exts -O2 -package mtl


# The arranger AG


arranger: $(ARRANGEMENTDIR)/ArrangerAG.hs

$(ARRANGEMENTDIR)/ArrangerAG.hs : $(AGC) $(ARRANGEMENTDIR)/ArrangerAG.ag \
	
	@echo uuagc: compiling arrangerAG.ag
	@$(AGC) -o $(ARRANGEMENTDIR)/ArrangerAG.hs \
	-cfp --Wmax=12 --genlinepragmas --self --Werrors --module=Arrangement.ArrangerAG \
	$(ARRANGEMENTDIR)/ArrangerAG.ag -P $(ARRANGEMENTDIR):$(PROXIMADIR)/src
#	--visit --case --strictdata --strictwrap --unbox\

# no type sigs, because parameterized types are not supported by AG


# The scanner AG

scanner: $(LAYOUTDIR)/ScannerAG.hs

$(LAYOUTDIR)/ScannerAG.hs : $(AGC) $(LAYOUTDIR)/ScannerAG.ag \
	
	@echo uuagc: compiling ScannerAG.ag
	@$(AGC) -o $(LAYOUTDIR)/ScannerAG.hs \
	-cfp --Wmax=12 --genlinepragmas --self --Werrors --module=Layout.ScannerAG \
	$(LAYOUTDIR)/ScannerAG.ag -P $(LAYOUTDIR)

# no type sigs, because parameterized types are not supported by AG


# Cleaning

clean:
	$(FIND) $(PROXIMASRCDIR) -name "*.hi" -delete
	$(FIND) $(PROXIMASRCDIR) -name "*.o" -delete
	

clean-agc:
	$(FIND) $(UUAGCDIR) -name "*.hi" -delete
	$(FIND) $(UUAGCDIR) -name "*.o" -delete
	$(FIND) $(UUAGCDERIVEDDIR) -name "*.hi" -delete
	$(FIND) $(UUAGCDERIVEDDIR) -name "*.o" -delete
	$(FIND) $(PROXIMABINDIR) -name "uuagc.*" -delete

clean-all: clean clean-agc

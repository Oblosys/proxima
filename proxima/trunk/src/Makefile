#-----------------------------------------------------------------------
#  The Proxima generic editor
#-----------------------------------------------------------------------

# make		 - build everything that is needed
# make depend	 - build dependencies for hs files, ag dependencies are	written	by hand
# make clean	 - remove .hi and .o files


# TODO:
# add platform dependent stuff (configure?)
# add strip

HC    = ghc

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
	@$(AGC) -cfp --Wmax=12 --genlinepragmas --self --Werrors \
	$(ARRANGEMENTDIR)/ArrangerAG.ag --module=Arrangement.ArrangerAG -P $(ARRANGEMENTDIR)
#	--visit --case --strictdata --strictwrap --unbox\

# no type sigs, because parameterized types are not supported by AG


# The scanner AG

scanner: $(LAYOUTDIR)/ScannerAG.hs

$(LAYOUTDIR)/ScannerAG.hs : $(AGC) $(LAYOUTDIR)/ScannerAG.ag \
	
	@echo uuagc: compiling ScannerAG.ag
	@$(AGC) -scfp --Wmax=12 --genlinepragmas --self --Werrors \
	$(LAYOUTDIR)/ScannerAG.ag --module=Layout.ScannerAG -P $(LAYOUTDIR)

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

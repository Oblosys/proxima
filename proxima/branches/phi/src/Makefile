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

.PHONY: default all generator arranger scanner always

PROXIMADIR = ../../proxima

PROXIMABINDIR = $(PROXIMADIR)/bin
PROXIMASRCDIR = $(PROXIMADIR)/src
GENERATORSRCDIR = $(PROXIMADIR)/../generator/src

AGC             = uuagc
GEN	        = $(PROXIMABINDIR)/generate$(EXE)

# Proxima directories

ARRANGEMENTDIR  = $(PROXIMADIR)/src/Arrangement
LAYOUTDIR       = $(PROXIMADIR)/src/Layout


GENERATORDIRS = $(GENERATORSRCDIR)


# Rules:

default: all

all:  generator arranger scanner

# The Generator
generator : $(GEN)

# the always phony target is because we don't want to put all the generator sources here
$(GEN): always
	@echo ghc: compiling Generator
	@$(HC) --make -o $(GEN) $(GENERATORSRCDIR)/Main.hs -i$(GENERATORDIRS) -fglasgow-exts



# The arranger AG
arranger: $(ARRANGEMENTDIR)/ArrangerAG.hs

$(ARRANGEMENTDIR)/ArrangerAG.hs : $(ARRANGEMENTDIR)/ArrangerAG.ag \
	
	@echo uuagc: compiling arrangerAG.ag
	@$(AGC) -scfp --Wmax=12 --genlinepragmas --self --Werrors \
	$(ARRANGEMENTDIR)/ArrangerAG.ag --module=Arrangement.ArrangerAG -P $(ARRANGEMENTDIR)


# The scanner AG
scanner: $(LAYOUTDIR)/ScannerAG.hs

$(LAYOUTDIR)/ScannerAG.hs :  $(LAYOUTDIR)/ScannerAG.ag \
	
	@echo uuagc: compiling ScannerAG.ag
	@$(AGC) -scfp --Wmax=12 --genlinepragmas --self --Werrors \
	--cycle --visit --case --strictdata --strictwrap --strictsem --seq --strictercase --optimize --unbox \
	$(LAYOUTDIR)/ScannerAG.ag --module=Layout.ScannerAG -P $(LAYOUTDIR)
       
# --bangpats only in GHC > 6.8.1

# Cleaning

clean:
	$(FIND) $(PROXIMASRCDIR) -name "*.hi" -delete
	$(FIND) $(PROXIMASRCDIR) -name "*.o" -delete
	

clean-all: clean

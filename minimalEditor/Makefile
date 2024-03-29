#HC = ghc
#PROXIMADIR = ../proxima

HC   = /c/Progra~1/Visual~1/bin/ghc
PROXIMADIR = src/proxima

# use the latter one for a Visual Studio configuration (which cannot access dirs outside project dir)

FIND = /usr/bin/find

PROXIMASRCDIR = $(PROXIMADIR)/src
PROXIMABINDIR = $(PROXIMADIR)/bin

UUSRCDIR = ../uuagc/src

AGC  = $(PROXIMABINDIR)/uuagc$(EXE)
GEN  = $(PROXIMABINDIR)/generate$(EXE)

EXE            = .exe
MAIN           = proxima$(EXE)

SHELL = /bin/sh

.PHONY: default proxima generate generator presenter lexer

default: proxima generate presenter lexer
	@echo "Make completed"


LAYOUTDIR       = $(PROXIMADIR)/src/Layout

PROXIMADIRS = $(PROXIMASRCDIR)

# Generation 

generator:
	cd $(PROXIMASRCDIR); make generator

generate: src/DocTypes_Generated.hs
	
src/DocTypes_Generated.hs : src/DocumentType.prx $(GEN)
	cd $(PROXIMASRCDIR); make generator
	$(GEN) src src/DocumentType.prx


# The presentation AG

# target for only compiling presentationAG, useful when working with GHCi
presenter: src/PresentationAG.hs
	
$(AGC):
	cd $(PROXIMASRCDIR); make agc

src/PresentationAG.hs: $(AGC) \
	src/PresentationAG.ag \
	src/PresentationAG_Generated.ag \
	
	@echo "Compiling presentationAG.ag"
	@$(AGC) -o src/PresentationAG.hs -cfmspw --self --Wmax=12 --genlinepragmas --module=PresentationAG src/PresentationAG.ag -P src


lexer: src/ScannerSheet.hs

# the -t causes Alex to look for a template file. 
src/ScannerSheet.hs: src/ScannerSheet.x $(LAYOUTDIR)/AlexTemplate-ghc
	alex -t $(LAYOUTDIR) --ghc src/ScannerSheet.x


clean:
	$(FIND) src -maxdepth 1 -name "*.hi" -delete
	$(FIND) src -maxdepth 1 -name "*.o" -delete
	$(FIND) src -maxdepth 1 -name "presentationAG.hs" -delete
	touch src/DocumentType.prx
	$(FIND) . -maxdepth 1 -name "proxima.*" -delete

# cannot delete *_Generated.hs sources, so we touch src/DocumentType.prx, causing them to be
# recompiled on the next make
	

# also clean Proxima
clean-proxima: clean
	cd $(PROXIMASRCDIR); make clean

# also clean uust
clean-proxima-all: clean
	cd $(PROXIMASRCDIR); make clean-all
	
#directories


ALLSRCDIRS = src:$(PROXIMASRCDIR):$(UUSRCDIR)

HC_OPTS = -static -fglasgow-exts -fallow-overlapping-instances -fallow-undecidable-instances \
		-package parsec -package mtl \
		-fno-monomorphism-restriction  $(EXTRA_OPTS)

proxima:
	@echo "Making proxima"
	cd $(PROXIMASRCDIR); make


# For generating Haddock documentation

GHC_LIB = /usr/local/lib/ghc-6.8.2/
TARGET = src/Main.hs
TITLE = "Proxima minimalEditor"

IMPORTS = --optghc=-i$(PROXIMASRCDIR):src
GHCOPTS = $(IMPORTS) --optghc=-fglasgow-exts \
  --optghc=-fallow-undecidable-instances \
  --optghc=-fno-monomorphism-restriction \
  --optghc=-w

haddock: 
	haddock -o doc -h -B $(GHC_LIB) -t $(TITLE) $(GHCOPTS) $(TARGET) 


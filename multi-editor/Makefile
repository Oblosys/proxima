HC = ghc
PROXIMADIR = src/proxima

FIND = /usr/bin/find

PROXIMASRCDIR = $(PROXIMADIR)/src
PROXIMABINDIR = $(PROXIMADIR)/bin

UUSRCDIR = src/uuagc/src

AGC  = uuagc
GEN  = proxima-generator

EXE            = .exe
MAIN           = proxima$(EXE)

SHELL = /bin/sh

.PHONY: default proxima generate generator presenter lexer

default: proxima generate presenter lexer
	@echo "Make completed"


LAYOUTDIR       = $(PROXIMADIR)/src/Layout

PROXIMADIRS = $(PROXIMASRCDIR)

# Generation 

generate: src/DocTypes_Generated.hs
	
src/DocTypes_Generated.hs : src/DocumentType.prx
	cd $(PROXIMASRCDIR)
	$(GEN) src src/DocumentType.prx


# The presentation AG

# target for only compiling presentationAG, useful when working with GHCi
presenter: src/PresentationAG.hs
	
src/PresentationAG.hs: src/PresentationAG.ag \
	src/PresentationAG_Generated.ag \
	
	@echo "Compiling presentationAG.ag"
	@$(AGC) -o src/PresentationAG.hs -cfmspw --self --Wmax=12 --genlinepragmas --module=PresentationAG src/PresentationAG.ag -P src


lexer: src/ScannerSheetHS.hs

src/ScannerSheetHS.hs: src/ScannerSheet.x \
  $(LAYOUTDIR)/AlexWrappers.hs $(LAYOUTDIR)/AlexTemplate.hs
	alex --template=$(LAYOUTDIR) --o src/ScannerSheetHS.hs --ghc src/ScannerSheet.x 

# the generated file is ScannerSheetHS, because if ScannerSheet is listed as a source in the
# cabal file, setup automatically runs Alex on it, and in cabal it's not possible to specify
# the extra Alex options we need.


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
TITLE = "Proxima DazzleEditor"

IMPORTS = --optghc=-i$(PROXIMASRCDIR):src
GHCOPTS = $(IMPORTS) --optghc=-fglasgow-exts \
  --optghc=-fallow-undecidable-instances \
  --optghc=-fno-monomorphism-restriction \
  --optghc=-w

haddock: 
	haddock -o doc -h -B $(GHC_LIB) -t $(TITLE) $(GHCOPTS) $(TARGET) 

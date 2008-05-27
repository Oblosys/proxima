GHC_LIB = /usr/local/lib/ghc-6.8.2/

PROXIMASRC = ../src
EDITORSRC = ../../dazzleEditor/src/

TARGET = $(PROXIMASRC)/Proxima/Proxima.hs

TITLE = Proxima

IMPORTS = --optghc=-i$(PROXIMASRC):$(EDITORSRC)
OPTS = $(IMPORTS) --optghc=-fglasgow-exts \
  --optghc=-fallow-undecidable-instances \
  --optghc=-fno-monomorphism-restriction

all: 
	haddock -o haddock -h -B $(GHC_LIB) -t $(TITLE) $(OPTS) $(TARGET) 

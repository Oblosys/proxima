HC   = /c/Progra~1/Visual~1/bin/ghc
FIND = /usr/bin/find

PROXIMADIR = src/proxima
PROXIMASRCDIR = $(PROXIMADIR)/src
PROXIMABINDIR = $(PROXIMADIR)/bin

AGC  = $(PROXIMABINDIR)/uuagc$(EXE)
GEN  = $(PROXIMABINDIR)/generate$(EXE)

EXE            = .exe
MAIN           = proxima$(EXE)

SHELL = /bin/sh

.PHONY : default proxima generate generator presenter agc always

default: $(MAIN)


PRESENTATIONDIR = $(PROXIMADIR)/src/presentation
ARRANGEMENTDIR  = $(PROXIMADIR)/src/arrangement
COMMONDIR       = $(PROXIMADIR)/src/common
EVALUATIONDIR   = $(PROXIMADIR)/src/evaluation
LAYOUTDIR       = $(PROXIMADIR)/src/layout
MAINDIR         = $(PROXIMADIR)/src/main
PRESENTATIONDIR = $(PROXIMADIR)/src/presentation
RENDERINGDIR    = $(PROXIMADIR)/src/rendering

PROXIMADIRS	= $(ARRANGEMENTDIR):$(COMMONDIR):$(EVALUATIONDIR):$(LAYOUTDIR):$(MAINDIR):$(PRESENTATIONDIR):$(RENDERINGDIR)


# Generation 

generator :
	cd $(PROXIMASRCDIR); make generator

generate : src/DocTypes_Generated.hs
	
$(GEN):
	cd $(PROXIMASRCDIR); make generator

src/DocTypes_Generated.hs : src/DocumentType.hs $(GEN)
	cd $(PROXIMASRCDIR); make generator
	$(GEN) src src/DocumentType.hs


# The presentation AG

# target for only compiling presentationAG, useful when working with GHCi
presenter: src/PresentationAG.hs
	
$(AGC):
	cd $(PROXIMASRCDIR); make agc

src/PresentationAG.hs : $(AGC) \
	src/PresentationAG.ag \
	src/PresentationAG_Generated.ag \
	
	@echo "Compiling presentationAG.ag"
	@$(AGC) -o src/PresentationAG.hs -cfmspw --self --module=PresentationAG src/PresentationAG.ag -P src


clean:
	$(FIND) src -name "*.hi" -delete
	$(FIND) src -name "*.o" -delete
	$(FIND) src -name "presentationAG.hs" -delete
	touch src/DocumentType.hs
	$(FIND) . -name "proxima.*" -delete

# cannot delete .._Generated sources, so we touch src/DocumentType.hs, causing them to be
# recompiled on the next make
	

# also clean Proxima
clean-proxima: clean
	cd $(PROXIMASRCDIR); make clean

# also clean uust
clean-proxima-all: clean
	cd $(PROXIMASRCDIR); make clean-all
	
#directories


ALLSRCDIRS = src:$(PROXIMADIRS)

HC_OPTS = -static -fglasgow-exts -fallow-overlapping-instances -fallow-undecidable-instances \
		-package wx -package parsec -package mtl\
		-fno-monomorphism-restriction $(EXTRA_OPTS)

# The main target

proxima:
	cd $(PROXIMASRCDIR); make
	
$(MAIN): proxima generate presenter
	$(HC) --make -o $(MAIN) src/Main.hs -i$(ALLSRCDIRS) $(HC_OPTS)
# make trekt aan helium objecten als ze in $(OBJS) staan en evaluateTypes/compileHelium 
# in EDITORSRCS zitten. Dit kan ervoor zorgen dat deze makefile helium sources gaat
# compileren. Zodra er geen cpp dingen meer in lvm zitten kan Helium waarschijnlijk 
# gecompileerd worden vanuit de Proxima makefile. Tot die tijd misschien even make 
# op Helium aanroepen wanneer de executable niet bestaat (Helium wordt dan geacht niet te 
# veranderen.)

# Dit zou ook de oorzaak kunnen zijn van de problemen bij het compileren van Set, nl. dat
# het vanuit de Proxima makefile gebeurde.

HC   = ghc
FIND = /usr/bin/find

#PROXIMADIR = ../proxima
PROXIMADIR = src/proxima
# use the latter one for a Visual Studio configuration (which cannot access dirs outside project dir)

PROXIMASRCDIR = $(PROXIMADIR)/src
PROXIMABINDIR = $(PROXIMADIR)/bin

AGC  = $(PROXIMABINDIR)/uuagc$(EXE)
GEN  = $(PROXIMABINDIR)/generate$(EXE)

EXE            = .exe
MAIN           = proxima$(EXE)

SHELL = /bin/sh




default: $(MAIN)
.PHONY : default proxima generate presenter lexer


LAYOUTDIR       = $(PROXIMADIR)/src/layout

# Generation 

generator:
	cd $(PROXIMASRCDIR); make generator
	
$(GEN): 
	cd $(PROXIMASRCDIR); make generator

generate: src/DocTypes_Generated.hs

src/DocTypes_Generated.hs : src/DocumentType.prx $(GEN)
	$(GEN) src src/DocumentType.prx


# The presentation AG

# target for only compiling presentationAG, useful when working with GHCi
presenter: src/PresentationAG.hs
	
$(AGC):
	cd $(PROXIMASRCDIR); make agc

src/PresentationAG.hs: $(AGC) \
	src/PresentationAG.ag \
	src/PresentationAG_Generated.ag \
	src/LambdaReduce.ag \
	
	@echo "Compiling presentationAG.ag"
	@$(AGC) -o src/PresentationAG.hs -cfmspw --self --Wmax=10 --genlinepragmas --module=PresentationAG src/PresentationAG.ag -P src

lexer: src/ScannerSheet.hs

# the -t causes Alex to look for a template file. 
src/ScannerSheet.hs: src/ScannerSheet.x $(LAYOUTDIR)/AlexTemplate-ghc
	alex -t $(LAYOUTDIR) --ghc src/ScannerSheet.x


clean:
	$(FIND) src -name "*.hi" -delete
	$(FIND) src -name "*.o" -delete
	$(FIND) src -name "presentationAG.hs" -delete
	touch src/DocumentType.prx
	$(FIND) . -name "proxima.*" -delete

# cannot delete .._Generated sources, so we touch src/DocumentType.prx, causing them to be
# recompiled on the next make
	

# also clean Proxima
clean-proxima: clean
	cd $(PROXIMASRCDIR); make clean

# also clean uust
clean-proxima-all: clean
	cd $(PROXIMASRCDIR); make clean-all

# also clean Helium       Make sure that a bash with the right PATH is used, otherwise 
#                         Helium does not clean well (use bash from start menu)
clean-all: clean-proxima-all
	cd $(HELIUMSRCDIR); make clean
	



	
HELIUMSRCDIR = $(PROXIMADIR)/../helium/src
#directories
PARSECDIR     = ../parsec
LVMLIBDIR     = ../lvm/src/lib
TOPDIR        = ../Top/src/Top

HELIUMDIRS	= $(LVMLIBDIR)/common:$(LVMLIBDIR)/common/ghc:$(LVMLIBDIR)/lvm:$(LVMLIBDIR)/asm:$(LVMLIBDIR)/core:$(HELIUMSRCDIR)/utils:$(PARSECDIR):$(HELIUMSRCDIR)/parser:$(HELIUMSRCDIR)/staticanalysis/miscellaneous:$(HELIUMSRCDIR)/staticanalysis/inferencers:$(HELIUMSRCDIR)/staticanalysis/directives:$(HELIUMSRCDIR)/staticanalysis/staticchecks:$(HELIUMSRCDIR)/staticanalysis/heuristics:$(HELIUMSRCDIR)/staticanalysis/messages:$(HELIUMSRCDIR)/syntax:$(HELIUMSRCDIR)/codegeneration:$(HELIUMSRCDIR)/main:$(HELIUMSRCDIR)/modulesystem:$(TOPDIR):$(TOPDIR)/..

ALLSRCDIRS = src:$(PROXIMASRCDIR):$(HELIUMDIRS)

HC_OPTS = -static -fglasgow-exts -fallow-overlapping-instances -fallow-undecidable-instances \
		-package gtk -package parsec -package mtl -package time\
		-fno-monomorphism-restriction $(EXTRA_OPTS)


proxima:
	cd $(PROXIMASRCDIR); make

# The main target	
$(MAIN): proxima generate presenter lexer
	$(HC) --make -o $(MAIN) src/Main.hs -i$(ALLSRCDIRS) $(HC_OPTS)

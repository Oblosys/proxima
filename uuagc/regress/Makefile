# Define $(GHC) and $(AG) before calling this Makefile
# i.e. GHC=ghc AG=uuagc make test

agflags := -mdcfspr --strictdata --strictwrap --dumpgrammar --dumpcgrammar --genlinepragmas --genvisage --genattrlist
ghcflags := --make -O2

ags := $(wildcard *.ag)
hss := $(ags:.ag=.hs)
runs := $(ags:.ag=.run)
prgs := $(ags:.ag=.exec)
exps := $(ags:.ag=.exp)
drvs := $(hss) $(prgs) $(ags:.ag=.o) $(ags:.ag=.hi) $(ags:.ag=.hs.visage) $(ags:.ag=.out) $(ags:.ag=.hs.attrs)

test : $(runs)
	rm -f $(drvs)
	echo "Finished running tests."

%.run : %.msg %.out
	diff $*.out $*.exp

%.msg :
	echo "Testing $*"

%.out : %.hs
	$(GHC) $(ghcflags) $< -o $*.exec
	./$*.exec | tr -d "[:space:]" > $@

%.hs : %.ag
	$(AG) $(agflags) $< -o $@


expect : $(exps)
	echo "Finished generating expectation files."

%.exp : %.out
	mv $*.out $*.exp


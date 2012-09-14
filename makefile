
GHC_OPTIONS="-outputdir tmp -threaded"
GHC=ghc ${GHC_OPTIONS}

all: deployapp

deployapp: tmp log *.hs
	${GHC} --make DeployApp.hs -o deployapp

log:
	mkdir -p log

tmp:
	mkdir -p tmp

clean: 
	rm -rf *.hi *.o deployapp log tmp

.PHONY: clean

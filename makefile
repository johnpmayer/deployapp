
GHC_OPTIONS="-outputdir tmp"
GHC=ghc ${GHC_OPTIONS}

all: deployapp

deployapp: tmp log DeployApp.hs Queries.hs Types.hs Utils.hs
	${GHC} --make DeployApp.hs -o deployapp

log:
	mkdir -p log

tmp:
	mkdir -p tmp

clean: 
	rm -rf *.hi *.o deployapp log tmp

.PHONY: clean

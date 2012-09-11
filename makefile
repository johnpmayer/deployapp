

all: log *.hs
	ghc --make Main.hs -o deployapp

log:
	mkdir -p log

clean: 
	rm -rf *.hi *.o deployapp log

.PHONY: clean

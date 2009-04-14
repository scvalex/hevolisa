SRC=/home/scvalex/proj/hevolisa

all:
	cd $(SRC) &&\
	cabal configure &&\
	cabal build &&\
	./dist/build/hevolisa/hevolisa --write-interval 50 --sample-size 0.2 --resize 1.5 --generation-size 2 alex.png +RTS -sstderr -N2

clean:
	cd $(SRC) &&\
	cabal clean


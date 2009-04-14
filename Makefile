SRC=/home/scvalex/proj/hevolisa

all:
	cd $(SRC) &&\
	cabal configure &&\
	cabal build &&\
	./dist/build/hevolisa/hevolisa --write-interval 200 --sample-size 0.2 --resize 1.5 --generation-size 2 alex.png

clean:
	cd $(SRC) &&\
	cabal clean


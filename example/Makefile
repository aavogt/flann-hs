

Rplot001.png: test.hs Flann.hs flann_adapter.o
	ghc -O2 -lflann test.hs flann_adapter.o
	./test

interactive: Flann.hs  flann_adapter.o
	haddock -h Flann.hs -o /tmp/
	ghci -lflann flann_adapter.o Flann.hs

flann_adapter.o: flann_adapter.c
	ghc -lflann flann_adapter.c Flann.hs

Flann.hs: Flann.chs flann_adapter.h C2HS.hs
	c2hs /usr/include/flann/flann.h flann_adapter.h Flann.chs

SHELL := bash


Flann.chs: ../src/Flann.chs
	cp ../src/Flann.chs .
C2HS.hs: ../src/C2HS.hs
	cp ../src/C2HS.hs .
flann_adapter.c: ../cbits/flann_adapter.c
	cp ../cbits/flann_adapter.c .
flann_adapter.h: ../cbits/flann_adapter.h
	cp ../cbits/flann_adapter.h .

clean:
	rm -f C2HS.{hi,o,hs} flann_adapter.{c,h,o} Flann.{chs,chs.h,chi,o,hi,hs} test.{o,hi} test Rplot001.png points_conns.txt points_conn.txt points.csv


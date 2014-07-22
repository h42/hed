--HSFLAGS =  -O -Wall -fno-warn-missing-signatures -fno-warn-unused-do-bind
HSFLAGS =  -O -Wall -fno-warn-unused-do-bind
CLG = $(HSFLAGS) --make  # -dynamic -threaded -rtsopts #-static

CFLAGS=-Wall

HEDFILES=hed.hs Glob.hs Ffi.hs Global.hs Func2.hs Vi.hs Func0.hs Func1.hs\
 Find.hs HTerm.hs HTermDefs.hs Display.hs Getfn.hs File.hs

%.o : %.hs
	ghc -c $(HSFLAGS) -o $@ $<

% : %.hs
	ghc $(CLG) -o $@ $<

PROGS=hed #kb # kb htest chktabs pretty #Tglob

.PHONY:all install

hed:hed.hs $(HEDFILES) ffilib.o
	ghc $(CLG)  -o hed hed.hs ffilib.o


htest:htest.hs Glob.hs

install:
	install -m755 -ojerry -gjerry hed /usr/local/bin/hed
	install -m755 -ojerry -gjerry hed /usr/local/bin/e
	#install -m755 -ojerry -gjerry chktabs /usr/local/bin/chktabs
	#install -m755 -ojerry -gjerry pretty /usr/local/bin/pretty

clean:
	-rm *.hi *.o $(PROGS)

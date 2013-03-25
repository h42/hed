HSFLAGS =  -fwarn-name-shadowing -XBangPatterns -XOverloadedStrings
CLG = $(HSFLAGS) --make  # -threaded -rtsopts #-static

CFLAGS=-Wall

OBJS=Glob.o Ffi.o Global.o Func0.o Hterm.o GetKB.o  Display.o Func2.o Func1.o\
 Getfn.o File.o

PROGS=hed kb htest #Tglob


%.o : %.hs
	ghc -c $(HSFLAGS) -o $@ $<

% : %.hs
	ghc $(CLG) -o $@ $<

.PHONY:all install

all:$(PROGS)

htest:htest.hs Glob.hs

hed:hed.hs ffilib.o $(OBJS)
	ghc $(CLG) -o $@ $< ffilib.o

install:
	install -m755 -ojerry -gjerry hed /usr/local/bin/e

clean:
	-rm *.hi *.o $(PROGS)

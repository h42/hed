HSFLAGS =  -fwarn-name-shadowing -XBangPatterns -XOverloadedStrings
CLG = $(HSFLAGS) --make  # -threaded -rtsopts #-static

CFLAGS=-Wall

OBJS=GetKB.o Glob.o Ffi.o Global.o Func0.o Hterm.o Display.o Func2.o Func1.o\
 Getfn.o File.o

PROGS=bad hed kb htest chktabs pretty #Tglob


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
	install -m755 -ojerry -gjerry hed /usr/local/bin/hed
	install -m755 -ojerry -gjerry hed /usr/local/bin/e
	#install -m755 -ojerry -gjerry chktabs /usr/local/bin/chktabs
	install -m755 -ojerry -gjerry pretty /usr/local/bin/pretty

clean:
	-rm *.hi *.o $(PROGS)

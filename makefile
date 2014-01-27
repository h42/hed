HSFLAGS =  -fwarn-name-shadowing -XBangPatterns -XOverloadedStrings
CLG = $(HSFLAGS) --make  # -threaded -rtsopts #-static

CFLAGS=-Wall

PROGS=hed kb # kb htest chktabs pretty #Tglob

ALL:hed kb

hed:hed.hs # $(OBJS)

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
	#install -m755 -ojerry -gjerry pretty /usr/local/bin/pretty

clean:
	-rm *.hi *.o $(PROGS)

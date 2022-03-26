
OBJS	= asteroid.o astrelem.o astro.o cometel.o csvelements.o \
	  gifout.o graphics.o htmlutil.o jplelements.o packargs.o planetp.o \
	  pluto.o project.o psrtext.o strlcpy.o sunmoon.o uncgi.o vsop87.o 

all: astro.a

astro.a: $(OBJS)
	rm -f astro.a
	ar rs astro.a $(OBJS)

clean:
	rm -f *.o *.bak core.* *.out

asteroid.o: asteroid.c vplanet.h

astrelem.o: astrelem.c vplanet.h

astro.o:    astro.c vplanet.h

cometel.o:  cometel.c vplanet.h

csvelements.o:	csvelements.c vplanet.h

gifout.o:   gifout.c vplanet.h

graphics.o: graphics.c vplanet.h

htmlutil.o: htmlutil.c vplanet.h

jplelements.o: jplelements.c vplanet.h

packargs.o: packargs.c vplanet.h hostkey.h

planetp.o:  planetp.c vplanet.h

pluto.o:    pluto.c vplanet.h

project.o:  project.c vplanet.h project.h

psrtext.o:  psrtext.c vplanet.h

strlcpy.o:  strlcpy.c

sunmoon.o:  sunmoon.c vplanet.h

uncgi.o:    uncgi.c vplanet.h

vsop87.o:   vsop87.c vplanet.h

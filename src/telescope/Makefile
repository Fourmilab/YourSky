
FTPDIR =    /server/pub/www.fourmilab.ch/web/yourtel/
CGIDIR =    /server/bin/httpd/cgi-bin

SAOCAT =   ../starmaps/sao/sao.o

YALECAT =   ../starmaps/yale/yale.o

OBJS	= vplanet.o sky.o terrain.o

ICONS = yoursky-icons.bmp yoursky-icons-b.bmp yoursky-icons-w.bmp yoursky-icons-r.bmp

all:	yourtel yourizon

yourtel: $(OBJS) $(LIBS)
	$(CC) $(CFLAGS) -o yourtel $(OBJS) $(SAOCAT) $(LIBS)

yourizon: $(OBJS) $(LIBS)
	$(CC) $(CFLAGS)  -o yourizon $(OBJS) $(YALECAT) $(LIBS)

sky.o:	    sky.c ../astro/vplanet.h ../astro/project.h

terrain.o:  terrain.c ../astro/vplanet.h

vplanet.o:  vplanet.c ../astro/vplanet.h

clean:
	rm -f *.o yourtel yourizon *.bak core.* *.out *.p?m *.gif

#   Collect pieces from various directories as installed and archive
#   in the build directory.  Think of this as "reverse install"

collect:
	rm -rf ftp-dir cgi-dir
	cp -pr $(FTPDIR) ftp-dir
	mkdir cgi-dir
	cp $(CGIDIR)/yourtel cgi-dir
	cp $(CGIDIR)/yoursky-post.html cgi-dir
	cp $(CGIDIR)/$(ICONS) cgi-dir

#   Make source release archive

archive:
	make clean
	rm -f /tmp/yourtel.tar /tmp/yourtel.tar.gz
	tar cfv /tmp/yourtel.tar .
	gzip /tmp/yourtel.tar

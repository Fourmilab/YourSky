
FTPDIR =    /server/pub/www.fourmilab.ch/web/yoursky/
CGIDIR =    /server/bin/httpd/cgi-bin

OBJS	= vplanet.o sky.o ../starmaps/yale/yale.o

ICONS = yoursky-icons.bmp yoursky-icons-b.bmp yoursky-icons-w.bmp yoursky-icons-r.bmp

all:	yoursky

yoursky: $(OBJS) $(LIBS)
	$(CC) $(CFLAGS) -o yoursky $(OBJS) $(LIBS)

hang:	    yoursky
	./yoursky -e22451831.90972 -h -o/tmp/ys.gif >/tmp/ys.html '42d21d24' '71d3d25'

sky.o:	    sky.c ../astro/vplanet.h ../astro/project.h

vplanet.o: vplanet.c ../astro/vplanet.h

clean:
	rm -f *.o yoursky *.bak core *.out *.p?m *.gif

#   Collect pieces from various directories as installed and archive
#   in the build directory.  Think of this as "reverse install"

collect:
	rm -rf ftp-dir cgi-dir
	cp -pr $(FTPDIR) ftp-dir
	mkdir cgi-dir
	cp $(CGIDIR)/Yoursky cgi-dir
	cp $(CGIDIR)/yoursky-post.html cgi-dir
	cp $(CGIDIR)/$(ICONS) cgi-dir

#   Make source release archive

archive:
	make clean
	rm -f /tmp/yoursky.tar /tmp/yoursky.tar.gz
	tar cfv /tmp/yoursky.tar .
	gzip /tmp/yoursky.tar

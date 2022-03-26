
.SUFFIXES: .bin

STARCAT =   yale

all: $(STARCAT).o

$(STARCAT).o:	$(STARCAT).c

$(STARCAT).c:	$(STARCAT).bin

clean:
	rm -f *.o $(STARCAT).c core *.bak

#   Embed a binary file as an array in a C program

.bin.c:
	xd -dstarcat $*.bin >$*.c

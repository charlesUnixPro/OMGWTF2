all : omgwtf2 omgwtf2_32 omgwtf2_32.exe

omgwtf2 : omgwtf2.c atbas.tab.c omgwtf2.h
	cc omgwtf2.c atbas.tab.c -o omgwtf2 -lm -Wall -lncurses 

omgwtf2_32 : omgwtf2.c atbas.tab.c omgwtf2.h
	cc -m32 omgwtf2.c atbas.tab.c -o omgwtf2_32 -lm -Wall -lncurses 

CROSS=i686-w64-mingw32-
omgwtf2_32.exe : omgwtf2.c atbas.tab.c omgwtf2.h
	$(CROSS)gcc omgwtf2.c atbas.tab.c -mno-ms-bitfields -static -static-libgcc -o omgwtf2_32.exe pdcurses.a

atbas.tab.c : atbas.y omgwtf2.h
	bison atbas.y

.PHONY: clean

clean :
	rm omgwtf2 omgwtf2_32 atbas.tab.c


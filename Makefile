all : omgwtf2 omgwtf2_32

omgwtf2 : omgwtf2.c atbas.tab.c omgwtf2.h
	cc omgwtf2.c atbas.tab.c -o omgwtf2 -O0 -g -lm -Wall -lncurses 

omgwtf2_32 : omgwtf2.c atbas.tab.c omgwtf2.h
	cc -m32 omgwtf2.c atbas.tab.c -o omgwtf2_32 -O0 -g -lm -Wall -lncurses 

atbas.tab.c : atbas.y omgwtf2.h
	bison atbas.y

.PHONY: clean

clean :
	rm omgwtf2 omgwtf2_32 atbas.tab.c


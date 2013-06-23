OMGWTF2
-------

OMGWTF2 contest entry.


MANIFEST
--------

    README.md       This file   
    omgwtf2.c       Main source code   
    omgwtf2_annotated.c   
                    The same as omgwtf2.c, but with WTF annotation comments   
    omgwtf2.h       Main include file   
    atbas.y         Bison file for interpreting Atari BASIC tokenized code   
    atbas_annotated.y   
                    The same as atbas.y, but with WTF annotation comments.   
    "Executive Decision Maker.atr"   
                    Actual decision making program (*)   
    gpl-2.0.txt     GPL License text   
    Makefile   
    omgwtf2         The contest entry, 64 bit Linux executable   
    omgwtf2_32      The contest entry, 32 bit Lunux executable   
    omgwtf2_32.exe  The contest entry, 32 Windows executable   
    
    README.wtf      The WTF notes for the entry   

SPECIAL NOTE ABOUT "Executive Decision Maker.atr"
-------------------------------------------------

This is a disk image of an apparently abandonware product.
It contains an internal copyright:

    COPYRIGHT (C) 1987 FIRST BYTE   
    P.O. BOX 130822   
    TYLER, TX 75713-0822   

A brief search of the web turned up no trace of First Byte.

The image was downloaded from www.atarimania.com. 

The image is used in an unaltered form in the contest.


COPYRIGHT
---------

Copyright 2013 by Charles Anthony.   
Licensed under GPL 2.0   


INSTRUCTIONS
------------

Fetch the executable (omgwtf2, omgwtf2_32 or omgwtf2_32.exe) and the data file 
(Executive Decision Maker.atr)

On a Linux box, run the executable inside a terminal window:

    ./omgwtf2

  or

    ./omgwtf2_32

Or, on a Windows box, run the executeable inside a console window:

  ./omgwtf2_32.exe

You will see a splash screen; press any key to continue.

You will then see an instruction screen; press any key to continue.

You will then be prompted for a business decision question. Type in your
question, and it will be answered. Press any key to ask another question,
or ESC to exit the program.

Much understanding will be gained by reading the README.wtf file.

The Windows executable was generated using MinGW32, and has only been tested
under wine. I have no idea how it will behave on a Windows machine.



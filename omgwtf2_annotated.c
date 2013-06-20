/* OMGWTF2 contest entry */

/* Copyright 2013 by Charles Anthony */

/*
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
//

// WTF: The "Executive Decision Maker.atr" file is a floppy disk image of an
// apparently abandonware produt. Basing a product on undocumented and
// unsupported abandonware is questionable.


// "Executive Decision Maker.atr" from:
//   http://www.atarimania.com/game-atari-400-800-xl-xe-executive-decision-maker_20061.html
//


#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <string.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <ctype.h>
#include <math.h>
#include <curses.h>
#include <setjmp.h>
#include <time.h>

#include "omgwtf2.h"

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
//
// Module definitions
//

typedef uint8_t byte;
typedef uint16_t word;
typedef uint32_t dword;

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
//
// Common utility routines
//

// WTF: Undocumented preprocessor symbol logfile
#ifdef logfile
static FILE * logFile = NULL;
#endif

// WTF: No usage comments
static void fatalErrno (char * desc, int errsav)
  {
#ifdef logfile
    fprintf (logFile, "Fatal error: %s\n%s\n", desc, errsav ? strerror (errsav) : "");
#endif

// WTF: Bug: endwin() called without checking gInit
    endwin ();
    printf ("Fatal error: %s\n%s\n", desc, errsav ? strerror (errsav) : "");
    exit (1);
  }

// WTF: No usage comments
static void fatal (const char * desc)
  {
#ifdef logfile
    fprintf (logFile, "Fatal error: %s\n", desc);
#endif
// WTF: Bug: endwin() called without checking gInit
    endwin ();
    printf ("Fatal error: %s\n", desc);
    exit (1);
  }

// WTF: No usage comments
static void fatalN (char * desc, int n)
  {
#ifdef logfile
    fprintf (logFile, "Fatal error: %s %d (0x%02x)\n", desc, n, n);
#endif
// WTF: Bug: endwin() called without checking gInit
    endwin ();
    printf ("Fatal error: %s %d (0x%02x)\n", desc, n, n);
    exit (1);
  }

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
//
// Extract a file from an Atari disk image file
//

// WTF: Why is this procedure done every time the application is run? The
// file should be extracted from the atr and saved as a normal file.

// WTF: This code assumed the host machine is little endian.

// WTF: This code frequently converts unsigned to signed without
// checking for overflow.

//
// Format of ATR files:
// 
// 
// http://www.atarimax.com/jindroush.atari.org/afmtatr.html
// 
// DWORD - 32bit unsigned long (little endian) 
// WORD - 16bit unsigned short (little endian) 
// BYTE - 8bit unsigned char
// Header
// 
// 16 bytes long. 
//  
// Type	Name	Description
// WORD	 wMagic	 $0296 (sum of 'NICKATARI')
// WORD	 wPars	 size of this disk image, in paragraphs (size/$10)
// WORD	 wSecSize	 sector size. ($80 or $100) bytes/sector
// BYTE	 btParsHigh	 high part of size, in paragraphs (added by REV 3.00)
// DWORD	 dwCRC	 32bit CRC of file (added by APE?)
// DWORD	 dwUnused	 unused
// BYTE	 btFlags	 bit 0 (ReadOnly) (added by APE?)
// 
// Body
// 
// Then there are continuous sectors. Some ATR files are incorrect - if 
// sector size is > $80 first three sectors should be $80 long. But, few 
// files have these sectors $100 long.

static unsigned int sectorSize;
static void * diskImage;
static byte * programFile;

static void readAtrFile (const char * name)
  {
    int fd;
    ssize_t sz;

    struct header
      {
        word wMagic, wPars, wSecSize;
        byte btParsHigh;
        dword dwCRC, dwUnused;
        byte btFlags;
      } __attribute__((packed));

    struct header hdr;

// WTF: Undocumented magic number 16
    if (sizeof (struct header) != 16)
      fatalN ("struct header wrong size", sizeof (struct header));

    fd = open (name, O_RDONLY);
    if (fd == -1)
      fatalErrno ("opening ATR file", errno);

    sz = read (fd, & hdr, sizeof (hdr));
    if (sz != sizeof (hdr))
      fatalErrno ("reading ATR header", errno);
    
// WTF: Undocumented magic number 0x0296
    if (hdr . wMagic != 0x0296)
      fatalErrno ("ATR header magic value wrong", errno);

// WTF: Undocumented preprocessor symbol info
#ifdef info
    fprintf (logFile, "Information: size of ATR disk image, in paragraphs: %hu\n", hdr . wPars);
    fprintf (logFile, "Information: sector size of ATR disk image: %hu (0x%hx)\n", hdr . wSecSize, hdr . wSecSize);
    fprintf (logFile, "Information: high part of size of ATR disk image: %hu\n", hdr . btParsHigh);
#endif

// WTF: Undocumented magic number 16
// WTF: Undocumented magic number 16
    uint32_t diskSize = ((hdr . btParsHigh << 16) + hdr . wPars) * 16;

#ifdef info
    fprintf (logFile, "Information: calculated disk size: %u\n", diskSize);
#endif

    sectorSize = hdr . wSecSize;
    diskImage = malloc (diskSize);
    if (! diskImage)
      fatal ("unable to allocate diskImage");

    sz = read (fd, diskImage, diskSize);
    if (sz != diskSize)
      fatalErrno ("reading ATR body", errno);

  }

//
// disk organization:
//   http://www.atariarchives.org/iad/chapter2.php

// convert one base addressing to zero base addressing 
#define oneBase 1

// convert one based sector number to byte pointer in diskImage
#define mapSectorPtr(n) ((byte *) diskImage +((n) - oneBase) * sectorSize )

static void readBasicFile (char * fname, char * ext)
  {
    // Number of sectors of directory entries
#   define numDirSectors 8

    // First directory sector (the above web page is 1-based sector #s)
#   define startDirSector 0x169

    // Number of directory entries in a directory sector
#   define numDirEntries 8

    // primary file name length
#   define pnameLen 8

    // file name extension length
#   define extLen 3

    // entry deleted flag bit
#   define flagDeleted 0x80

    // entry in use flag bit
#   define flagInUse 0x40

    // number of byte bytes in a data sector
#   define numDataBytes 125

    // mask out file number from next sector number field
#   define nextMask 0x3

    struct dirEntry
      {
        byte flag;
        word count;
        word ssn;
        char pname [pnameLen];
        char ext [extLen];
      } __attribute__((packed));

    struct dirSector
      {
        struct dirEntry entries [numDirEntries];
      };

    struct dataSector
      {
        byte data [numDataBytes];
// Figure 2-3 in the documentation is wrong; the text is correct
        byte nextSectorNumHi; // File number encoded in high six bits
        byte nextSectorNumLo;
        byte numBytesUsed;
      } __attribute__((packed));
 
    struct dirEntry * entry;
    int found = 0;

    if (sizeof (struct dirEntry) != 16)
      fatalN ("struct dirEntry wrong size", sizeof (struct dirEntry));
    if (sizeof (struct dirSector) != 128)
      fatalN ("struct dirSector wrong size", sizeof (struct dirSector));
    if (sizeof (struct dataSector) != 128)
      fatalN ("struct dataSector wrong size", sizeof (struct dataSector));

    unsigned int i;
    for (i = 0; i < numDirSectors; i ++)
      {
        struct dirSector * sectorPtr = (struct dirSector *) mapSectorPtr (startDirSector + i);
        int entryNum;
        for (entryNum = 0; entryNum < numDirEntries; entryNum ++)
          {
            entry = & sectorPtr -> entries [entryNum];
#ifdef info
            fprintf (logFile, "Information: directory entry %0x %.8s.%.3s\n", entry -> flag, entry -> pname, entry -> ext);
#endif
            if ((entry -> flag & flagDeleted) == 0 &&
                (entry -> flag & flagInUse) != 0 &&
                strncmp (entry -> pname, fname, pnameLen) == 0 &&
                strncmp (entry -> ext, ext, extLen) == 0)
              {
                found = 1;
                break;
              }
          }
        if (found)
          break;
      }

    if (! found)
      fatal ("program file not found");

    unsigned int fileSize = entry -> count * numDataBytes;
#ifdef info
    fprintf (logFile, "Information: program file size %u\n", fileSize);
#endif

    programFile = (byte *) malloc (fileSize);
    if (! programFile)
      fatal ("failed to allocate programFile");

    word count = entry -> count;
    word ssn = entry -> ssn;
    unsigned int programFileUsed = 0;

    byte * pfp = programFile;

#ifdef info
    fprintf (logFile, "Information: count %hd, ssn %hd\n", count, ssn);
#endif

    word sn = ssn;
    for (i = 0; i < count; i ++)
      {
        struct dataSector * data = (struct dataSector *) mapSectorPtr (sn);
#if 0
        int j;
        for (j = 0; j < data -> numBytesUsed; j ++)
          if (isprint (data -> data [j]))
            fprintf (logFile, " %c", data -> data [j]);
          else
            fprintf (logFile, " %02x", data -> data [j]);
        fprintf (logFile, "\n");
#endif
        if (programFileUsed + data -> numBytesUsed > fileSize)
          fatal ("overran programFile");

        memmove (pfp, data -> data, data -> numBytesUsed);
        pfp += data -> numBytesUsed;
        programFileUsed += data -> numBytesUsed;
        
// WTF: Undocumented magic number 8
        sn = (((data -> nextSectorNumHi) & nextMask) << 8) | data -> nextSectorNumLo;
      }

#if 0
    {
      int fd = open ("dec.bas", O_WRONLY);
      write (fd, programFile, programFileUsed);
      close (fd);
    }
#endif
  }

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
//
// Extract symbol table and line number information from an Atari BASIC save 
// file
//


////////////////////////////////////////////////////////////////////////////////
//
// Define constants used in Atari BASIC save files
//

// Operands with the sign bit set are variable references

#define varFlagBit 0x80
#define removeVarFlagBit 0x7f

// Some strings mark the end-of-string with the sign bit

#define eosFlagBit 0x80
#define removeEosFlagBit 0x7f

// REM and SCONST mark the end-of-strings with 0x9b

#define EOS 0x9b

// Variable table types

#define vtTypeScalar 0x00
#define vtTypeString 0x80

// WTF Inconsistent capitalization style

// Operands

// WTF: These constants are common to this file and atbas.y, but are
// inconsistently spelled. Also the values of the constants have 0x100 
// added to them in atbay.y. Thus:
//    here:
//      #define opNCONST 0x0e
//    atbas.y
//      %token NCONST 0x10e

#define opNCONST 0x0e
#define opSCONST 0x0f
#define opComma1 0x12
#define opColon  0x14
#define opSemi   0x15
#define opEOL    0x16
#define opTo     0x19
#define opStep   0x1a
#define opThen   0x1b
#define opSharp  0x1c
#define opNE     0x1e
#define opEQ1    0x22
#define opTimes  0x24
#define opMinus1 0x26
#define opClose  0x2c
#define opPlus   0x25
#define opDiv    0x27
#define opOr     0x29
#define opOpen1  0x2b
#define opEQ2    0x2d
#define opEQ3    0x2e
#define opEQ4    0x34
#define opMinus2 0x36
#define opOpen2  0x37
#define opOpen3  0x3a
#define opOpen4  0x3b
#define opComma2 0x3c
#define opLen    0x42
#define opPeek   0x46
#define opRnd    0x48
#define opInt    0x50

// tokens

// WTF: These constants are common to this file and atbas.y, but are
// inconsistently spelled. (e.g. tokINPUT here is INPUT in atbas.y)
#define tokREM      0x00
#define tokINPUT    0x02
#define tokIF       0x07
#define tokFOR      0x08
#define tokNEXT     0x09
#define tokGOTO     0x0a
#define tokGO_TO    0x0b
#define tokGOSUB    0x0c
#define tokTRAP     0x0d
#define tokDIM      0x14
#define tokEND      0x15
#define tokPOKE     0x1f
#define tokPRINT    0x20
#define tokRETURN   0x24
#define tokPOP      0x27
#define tokQMARK    0x28
#define tokGRAPHICS 0x2b
#define tokPOSITION 0x2d
#define tokSETCOLOR 0x30
#define tokLET      0x36 // silent let

// Special tokens for our parser

#define tokEOT      0xf1  // end of token
#define tokEOF      0x00  // end of file
#define tokREM2     0xf3  // alias for tokREM that is non-zero
#define opVAR       0x80
#define opSVAR      0x81

// WTF: This auxilary table that the interpreter creates consumes four times
// the addressable memory of the chip the code was originally written for.

static byte * lineNumberTable [0x10000];
static word firstLineNumber;
static word lastLineNumber;

// WTF: What are this variables?

static word vvt;
static word vnt;
static word nv;
static word stmtab;
static word codeLen;

struct variableTable
  {
    char * name;
    int type; // 0: scalar, 1: string
    int dim;
    double val; // scalar
    char * sval; // string
  };

static struct variableTable * variableTable = NULL;

struct vvtEntry
  {
    byte btType;
    byte btNumber;
    byte data [6];
  };

// WTF:  analyzeBasicFile() is 160 lines long, and has one comment.

static void analyzeBasicFile (void)
  {

// Documentation at:
//   http://www.atarimax.com/jindroush.atari.org/afmtbas.html
// Code borrowed from:
//   http://www.atarimax.com/jindroush.atari.org/data/asoft/chkbas_src.zip

    struct header
      {
        word lomem;
        word vnt;
        word vnte;
        word vvt;
        word stmtab;
        word stmcur;
        word starp;
      } __attribute__((packed));

    struct header * hdr = (struct header *) programFile;
#ifdef info
    fprintf (logFile, "Information: lomem %x\n", hdr -> lomem);
    fprintf (logFile, "Information: vnt %x\n", hdr -> vnt);
    fprintf (logFile, "Information: vnte %x\n", hdr -> vnte);
    fprintf (logFile, "Information: vvt %x\n", hdr -> vvt);
    fprintf (logFile, "Information: stmtab %x\n", hdr -> stmtab);
    fprintf (logFile, "Information: stmcur %x\n", hdr -> stmcur);
    fprintf (logFile, "Information: starp %x\n", hdr -> starp);

    fprintf (logFile, "Information: applying corrections\n");
#endif

    word wcor = hdr -> vnt - hdr -> lomem - 0x0e;

    vnt = hdr -> vnt - wcor;
    // word vnte = hdr -> vnte - wcor;
    vvt = hdr -> vvt - wcor;
    stmtab = hdr -> stmtab - wcor;
    word stmcur = hdr -> stmcur - wcor;
    // word starp = hdr -> starp - wcor;

    // word vntl = vnte - vnt + 1;
    word vvte = stmtab - 1;
    word vvtl = vvte - vvt + 1;
    nv = vvtl / 8;

    codeLen = stmcur - stmtab;
    // word codeLenCur = starp - stmcur;

#ifdef info
    fprintf (logFile, "Constants & pointers:\n" );
    fprintf (logFile, "Start of Name Table      (VNT)   : %04X\n", vnt );
    fprintf (logFile, "End of Name Table        (VNTE)  : %04X\n", vnte );
    fprintf (logFile, "Length of Name Table     (VNTL)  : %04X\n", vntl );

    fprintf (logFile, "Start of Variable Table  (VVT)   : %04X\n", vvt );
    fprintf (logFile, "End of Variable Table    (VVTE)  : %04X\n", vvte );
    fprintf (logFile, "Length of Variable Table (VVTL)  : %04X\n", vvtl );

    fprintf (logFile, "Number of Variables      (NV)    : %04X\n", nv );

    fprintf (logFile, "Start of Code            (STMTAB): %04X\n", stmtab );
    fprintf (logFile, "Length of Code                   : %04X\n", codeLen );
    fprintf (logFile, "Current command          (STMCUR): %04X\n", stmcur );
    fprintf (logFile, "Length of current command        : %04X\n", codeLenCur );
    fprintf (logFile, "First byte after program (STARP) : %04X\n", starp );
#endif

    byte * linePtr = programFile + stmtab;

#define scan(t,d,p) d = * (t *) (p); (p) += sizeof (t)
#define skip(t,p) (p) += sizeof (t)

    variableTable = malloc (nv * sizeof (struct variableTable));
    byte * name = (byte *) (programFile + vnt);
    int i;
    for (i = 0; i < nv; i ++)
      {
        int len = 0;
        while ((name [len] & eosFlagBit) == 0)
          len ++;
        len ++; // the flagged character is to be included

        char * str = malloc (len + 1);
        int j;
        for (j = 0; j < len; j ++)
          str [j] = name [j] & removeEosFlagBit;
        str [j] = '\0';
        variableTable [i] . name = str;

        // fprintf (logFile, "%s\n", variableTable [i] . name);
        name += len;
      }

#if 0
    fprintf (logFile, "Name table dump\n");
    byte * name = (byte *) (programFile + vnt);
    int i;
    for (i = 0; i < vvtl; i ++)
      {
        fprintf (logFile, "%c", (* name) & removeEosFlagBit);
        if ((* name) & varFlagBit)
          fprintf (logFile, "\n");;
        name ++;
      }
    fprintf (logFile, "\n");;
#endif

    for (i = 0; i < nv; i ++)
      {
        struct vvtEntry * entry = ((struct vvtEntry *) (programFile + vvt)) + i;
        if (entry -> btType != vtTypeScalar && entry -> btType != vtTypeString)
          fatalN ("unrecognized variable type", entry -> btType);
        variableTable [i] . type = entry -> btType;
        variableTable [i] . dim = 0;
        variableTable [i] . val = 0.0;
        variableTable [i] . sval = NULL;
      }

#if 0
    fprintf (logFile, "Variable table dump\n");
    for (i = 0; i < nv; i ++)
      {
        struct vvtEntry * entry = ((struct vvtEntry *) (programFile + vvt)) + i;
        fprintf (logFile, "type %02x number %d data %02x %02x %02x %02x %02x %02x\n",
          entry -> btType, entry -> btNumber,
          entry -> data [0], entry -> data [1], entry -> data [2],
          entry -> data [3], entry -> data [4], entry -> data [5]);
      }
    fprintf (logFile, "\n");;
#endif

    word lineNumber;
    byte lineLength;

    firstLineNumber = 0;
    lastLineNumber = 0;

    // Build line number table

    memset (lineNumberTable, 0, sizeof (lineNumberTable));

    while (linePtr < programFile + stmtab + codeLen)
      {
        byte * lineStart = linePtr;
        scan (word, lineNumber, linePtr);
        scan (byte, lineLength, linePtr);
        byte * lineNext = lineStart + lineLength;
        if (! firstLineNumber)
          firstLineNumber = lineNumber;
        lastLineNumber = lineNumber;
        //fprintf (logFile, "%u %u\n", lineNumber, lineLength);    
        lineNumberTable [lineNumber] = lineStart;
        linePtr = lineNext;
      }

#ifdef info
    fprintf (logFile, "Information: first line number %hu\n", firstLineNumber);
    fprintf (logFile, "Information: last line number %hu\n", lastLineNumber);
#endif

  }

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
//
// Support routines for executing Atari BASIC program
//

// WTF: Comments?

static double parseNCONST (byte * * linePtr)
  {
    byte exp;
    int iexp;
    double res = 0.0;

    scan (byte, exp, (* linePtr));
    if (! exp)
      {
        iexp = 0;
        skip (byte, (* linePtr));
        skip (byte, (* linePtr));
        skip (byte, (* linePtr));
        skip (byte, (* linePtr));
        skip (byte, (* linePtr));
      }
    else
      {
        iexp = (((int) exp) - 68) * 2;
        int i;
        for (i = 0; i < 5; i ++)
          {
            byte pom;
            scan (byte, pom, (* linePtr));
            byte num = (pom >> 4) * 10 + (pom & 0xf);
            res *= 100;
            res += num;
          }
      }
    res *= pow (10, iexp);
    return res;
  }

#if 0
static void pString (byte * * linePtr)
  {
    byte ch;
    for (;;)
      {
        scan (byte, ch, (* linePtr));
        if (ch == EOS)
          {
            fprintf (logFile, "\n");
            break;
          }
        else
          if (isprint (ch))
            fprintf (logFile, "%c", ch);
          else
            fprintf (logFile, "\\0%u", ch);
      }
  }
#endif

#ifdef logfile
static void pNString (char * ptr)
  {
    byte len = ptr [0];
    fprintf (logFile, "[%d] ", len);
    byte ch;
    int i;
    for (i = 0; i < len; i ++)
      {
        ch = ptr [i + 1];
        if (ch == EOS)
          {
            break;
          }
        else
          if (isprint (ch))
            fprintf (logFile, "%c", ch);
          else
            fprintf (logFile, "\\0%u", ch);
      }
  }
#endif


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
//
// Execute an Atari BASIC program
//

////////////////////////////////////////////////////////////////////////////////
//
// Parser state variables
//

#define startOfLine 0
//#define startOfToken 1
#define inToken 2
#define inOperands 3
#define atEnd 4

// WTF: These "Parser state variables" are very fragile and are undocumented.

static int lexState = startOfLine;
static byte * lexLinePtr;

static byte * lineStart;
static byte * lineNext;
static byte * tokenEnd;

// For debugging
static int currentLineNumber;

// WTF: Undocumented longjmp()
static jmp_buf trapJmp;
static void fireTrap (void); // forward

////////////////////////////////////////////////////////////////////////////////
//
// Graphics state variables
//

#if 0
struct setColor
  {
    int hue;
    int luminance;
  };

static struct setColor setColor [5];
#endif

// WTF: The "Graphics state variables" are undocumented.

static int posCol, posRow;
static int gMode;
static WINDOW * gWin;
static WINDOW * tWin;
static int gInit = 0;

static byte lastKBChar;
static byte leftMargin;

////////////////////////////////////////////////////////////////////////////////
//
// Graphics/text emulation
//

static void clearWindows (void)
  {
    erase ();
    werase (tWin);
    werase (gWin);
    refresh ();
    wrefresh (tWin);
    wrefresh (gWin);
    posCol = 0;
    posRow = 0;
    //leftMargin = 2;
  }

// WTF:  The "Graphics/text emulation" is undocumented; is neither
//    clear what is being emulated or how it is being emulated.

void setGraphicsMode (int mode)
  {
    if (!gInit)
      {
        initscr ();
        //cbreak ();
        halfdelay (1);
        echo ();
        nonl ();
        intrflush (stdscr, FALSE);
        keypad (stdscr, TRUE);
        gWin = newwin (10, 20, 0, 10);
        //nodelay (gWin, TRUE);
        tWin = newwin (4, 40, 10, 0); 
        //nodelay (tWin, TRUE);
        //nodelay (stdscr, TRUE);
        wresize (stdscr, 24, 40);
        gInit = 1;
      }
    gMode = mode;
    clearWindows ();
  }

void setPosition (double col, double row)
  {
    posCol = lrint (col);
    posRow = lrint (row);
    //wmove (gWin, posCol, posRow);
    move (posRow, posCol + leftMargin);
    wmove (gWin, posRow, posCol + leftMargin);
    //wmove (tWin, posCol + leftMargin, posRow);
  }

static int printChannel = 0;

void setChannel (double channel)
  {
    printChannel = lrint (channel);
  }

static void printChar (char ch)
  {
    unsigned char c = ch;
    unsigned char c0 = ch;
    //fprintf (logFile, "printChar %02x %c %c %d %d\n", c, isprint (c) ? c : '?', isprint ((c & 0x7f)) ? c & 0x7f : '?', gMode, printChannel);
// WTF: Magic numbers 0x80, 0x7f
    int invert = (c & 0x80);
    c &= 0x7f;

// WTF: Magic number 0x12
    if (c == 0x12)
      c = '-';

    if (gInit)
      {
        if (c0 == 0xfd) // ATASCII bell
          {
            beep ();
            return;
          }
        if (c0 == 0x7d) // clear screen
          {
            clearWindows ();
            return;
          }
        WINDOW * w;
        if (gMode == 0)
          w = stdscr;
        else if (printChannel == 6)
          w = gWin;
        else
          w = tWin;

#if 0
        if (c == 0x0a) // new line
          {
            int cury, curx;
            getyx (w, cury, curx);
            setPosition (0, cury + 1);
            return;
          }
#endif
        if (invert)
          attrset (A_REVERSE);
        if (w == gWin || islower (c))
          c = toupper (c);
        if (isprint (c) || c == '\n')
          wechochar (w, c);
        else
          wechochar (w, '?');
        if (invert)
          attrset (0);
      }

    else
      {
        if (c == 0xfd) // ATASCII bell
          return;
        if (isprint (c) || c == '\n')
          printf ("%c", c);
        else
         printf ("<%02x>", (unsigned char) c);
      }
  }

void printNL (void)
  {
    //printChar ('\r');
    printChar ('\n');
    // printNL is called by the parser at the end of the PRINT statement;
    // set the channel back to the default
    printChannel = 0;
  }

void printExp (double exp)
  {
    char sbuf[256];
    sprintf (sbuf + 1, "%g", exp);
    sbuf [0] = strlen (sbuf + 1);
    printSExp (sbuf);
  }

void printSExp (char * exp)
  {
    int l = exp [0];
    int i;
    for (i = 0; i < l; i ++)
      printChar (exp [i + 1]);
  }

void doInput (int varNum)
  {
    if (varNum < 0 || varNum >= nv)
      fatalN ("varNum out of range", varNum);
    if (variableTable [varNum] . type != vtTypeString)
      fatal ("getVarVal expected a string variable");
    if (! variableTable [varNum] . sval)
      fatal ("unallocated string");
    int maxlen = variableTable [varNum] . dim;
    int i;
    nocbreak ();
    nodelay (stdscr, FALSE);
    echo ();
    printChar ('?');
    for (i = 0; ; i++)
      {
        int ch;
        ch = getch ();
        if (ch == -1)
          break;
        if (ch == 0xa)
          break;
        if (i < maxlen)
          variableTable [varNum] . sval [i + 1] = ch;
      }
    variableTable [varNum] . sval [0] = i;
    //fprintf (logFile, "input %s < ", variableTable [varNum] . name);
    //pNString (variableTable [varNum] . sval);
    //fprintf (logFile, "\n");
  }

////////////////////////////////////////////////////////////////////////////////
//
// Variable access
//

double getVarValue (int varNum)
  {
    //fprintf (logFile, "get var val %d\n", varNum);
    if (varNum < 0 || varNum >= nv)
      fatalN ("varNum out of range", varNum);
    if (variableTable [varNum] . type != vtTypeScalar)
      fatal ("getVarVal expected a scalar variable");
    return variableTable [varNum] . val;
  }

char * getSVarValue (int varNum)
  {
    //fprintf (logFile, "get var val %d\n", varNum);
    if (varNum < 0 || varNum >= nv)
      fatalN ("varNum out of range", varNum);
    if (variableTable [varNum] . type != vtTypeString)
      fatal ("getVarVal expected a string variable");
    if (! variableTable [varNum] . sval)
      fatal ("unallocated string");
    return variableTable [varNum] . sval;
  }

char * getSVarValue1 (int varNum, double sub)
  {
    //fprintf (logFile, "get var val %d %g\n", varNum, sub);
    fatal ("getSVarValue1 failed");
    return "svar1";
  }

// WTF:  getSVarValue2() is used to extract sub-strings while
//    evaluating expressions, but relies on a single, statically allocated
//    buffer. If an expression had multiple sub-strings, the evaluation
//    would fail. The routine needs to allocate a buffer, and interpret()
//    needs to garbage collect the buffers each time through the yyparse()
//    loop.

static char substrBuffer [257];

char * getSVarValue2 (int varNum, double sub1, double sub2)
  {
    //fprintf (logFile, "get var val %d %g %g\n", varNum, sub1, sub2);
    if (varNum < 0 || varNum >= nv)
      fatalN ("varNum out of range", varNum);
    if (variableTable [varNum] . type != vtTypeString)
      fatal ("getVarVal expected a string variable");
    if (! variableTable [varNum] . sval)
      fatal ("unallocated string");

    int isub1 = lrint (sub1);
    int isub2 = lrint (sub2);
    int len = variableTable [varNum] . sval [0];

    if (isub1 < 1 || isub1 > len)
      {
        fireTrap ();
        fatalN ("string subscript 1 out of range", isub1);
      }
    if (isub2 < 1 || isub2 > len)
      {
        fireTrap ();
        fatalN ("string subscript 2 out of range", isub2);
      }
    if (isub1 > isub2)
      {
        fireTrap ();
        fatal ("string subscript 2 > subscript1");
      }

    int slen = isub2 - isub1 + 1;
    if (slen > 256)
      {
        fireTrap ();
        fatalN ("improbable... substring too long", slen);
      }

    substrBuffer [0] = slen;
    int i;
    for (i = 0; i < slen; i ++)
      substrBuffer [i + 1] = variableTable [varNum] . sval [isub1 + i];
 
    //fprintf (logFile, "sub2 <");
    //pNString (variableTable [varNum] . sval);
    //fprintf (logFile, "> (%d, %d) --> <", isub1, isub2);
    //pNString (substrBuffer);
    //fprintf (logFile, ">\n");
    return substrBuffer;
  }

////////////////////////////////////////////////////////////////////////////////
//
// Math
//

double evalOp (double exp1, int op, double exp2)
  {
    //fprintf (logFile, "eval op %g %d %g\n", exp1, op, exp2);
    if (op == opPlus)
      return exp1 + exp2;
    if (op == opMinus1)
      return exp1 - exp2;
    if (op == opNE)
      return exp1 != exp2;
    if (op == opEQ1)
      return exp1 == exp2;
    if (op == opMinus2)
      return -exp2;
    if (op == opTimes)
      return exp1 * exp2;
    if (op == opDiv)
      {
        if (exp2 == 0.0)
          fatal ("div by 0");
        return exp1 / exp2;
      }
    if (op == opOr)
      return ((exp1 != 0) || (exp2 != 0));
// WTF: Incomplete implementation
    fatalN ("evalOp failed", op);
    return 0.0;
  }

static void updateLastKBChar (void)
  {
    //cbreak ();
    halfdelay (1);
    //nodelay (stdscr, TRUE);
    noecho ();
    int ch = getch ();
    if (ch != ERR)
      {
        lastKBChar = ch;
        // For some reason, ATASCII escape is 28
        if (lastKBChar == 27)
          lastKBChar = 28;
      }
  }

// WTF: randu(): a notoriously bad random number generator.
// WTF: The OMGWTF2 entry form specifies that this
//    application (may) rely on srand48(); it actually (may) rely
//    on randu().
// WTF: No citation given for this code.
// WTF: Magic numbers

static int iseed;

static float randu (int * iseed)
  {
    int IMAX = 2147483647;
    float XMAX_INV = 1. / (float) IMAX;
    * iseed = * iseed * 65539;
    if (* iseed < 0)
      * iseed = * iseed + IMAX + 1;
    return * iseed * XMAX_INV;
  }

double evalFunc (int func, double arg)
  {
    //fprintf (logFile, "eval func %x %g\n", func, arg);
    if (func == opPeek)
      {
        int addr = lrint (arg);
        if (addr == 0xd01f) // Console switches
          {
            // Since we don't have a start switch, wait for any key
            updateLastKBChar ();
            if (lastKBChar != 255)
              return 6.0; // indicate that start is pressed XXX
            return 7.0; // no keys pressed
          }
        else if (addr == 764) // keypress
          {
            updateLastKBChar ();
            return lastKBChar;
          }
        else
          fatalN ("peek failed", addr);
      }
    else if (func == opInt)
      {
        return lrint (arg);
      }
    else if (func == opRnd)
      {
        double v = randu (& iseed);
        return v;
      }
    else
// WTF: Incomplete implementation
      fatal ("evalFunc failed");
    return 0.0;
  }

////////////////////////////////////////////////////////////////////////////////
//
// String manipulation
//

// WTF: This char* is a counted string, not zero-terminated
double evalSOp (int exp1, int op, char * exp2)
  {
    //fprintf (logFile, "eval op %d %d %s\n", exp1, op, exp2);
// WTF: Incomplete implementation
    fatal ("evalSOp failed");
    return 0.0;
  }

// WTF: This char* is a counted string, not zero-terminated
double evalSFunc (int func, char * arg)
  {
    //fprintf (logFile, "eval func %d %s\n", func, arg);
    if (func == opLen)
      {
        //fprintf (logFile, "eval len ");
        //pNString (arg);
        //fprintf (logFile, " > %d\n", arg [0]);
        return arg [0];
      }
    else
// WTF: Incomplete implementation
      fatal ("evalSFunc failed");
    return 0.0;
  }

// WTF: These char*'s are counted strings, not zero-terminated
//double stringEq (int varNum, char * str)
double stringEq (char * str1, char * str2)
  {
    //fprintf (logFile, "str eq %d %s\n", varNum, str);
    //char * varp = getSVarValue (varNum);

    //fprintf (logFile, "cmp ");
    //pNString (str1);
    //fprintf (logFile, "<>");
    //pNString (str2);
    //fprintf (logFile, "\n");

      if (str1 [0] != str2 [0])
        return 0.0;
// WTF:  stringEq() uses strncasecmp() instead of strncmp()
//    because the Atari keyboard is uppercase, and the legacy application
//    assumes the the typed input will be uppercase. A better solution
//    would be to have doInput() do the appeasing.

      return strncasecmp (str1 + 1, str2 + 1, str1 [0]) ? 0 : 1;
    return 0.0;
  }


////////////////////////////////////////////////////////////////////////////////
//
// Array allocation
//

void dimSVar (int varNum, double dim)
  {
    if (varNum < 0 || varNum >= nv)
      fatalN ("varNum out of range", varNum);
    if (variableTable [varNum] . type != vtTypeString)
      fatal ("dimSVar expected a string variable");
    int idim = lrint (dim);
    if (dim < 1 || dim > 256)
      fatalN ("dim out of range", idim);
    variableTable [varNum] . dim = idim;
    if (variableTable [varNum] . sval)
      free (variableTable [varNum] . sval);
    variableTable [varNum] . sval = malloc (idim + 1); // counted strings
    //fprintf (logFile, "\ndimSvar %s (%d)\n", variableTable [varNum] . name, idim);
  }


////////////////////////////////////////////////////////////////////////////////
//
// Memory functions
//

void doPoke (double addr, double val)
  {
    // 752, 1 suppress cursor
    int iaddr = lrint (addr);
    int ival = lrint (val);

    if (iaddr == 752) // suppress cursor
      return; // ignore XXX

    if (iaddr == 764) // keyboard charater last pressed
      {
        lastKBChar = ival;
        return;
      }
    if (iaddr == 82) // left margin
      {
        //leftMargin = lrint (val); 
        return;
      }
    //fprintf (logFile, "poke %d, %d\n", iaddr, ival); // XXX
  }

////////////////////////////////////////////////////////////////////////////////
//
// Flow contol
//
//   IF/THEN
//   IF/THEN/END
//   GOSUB
//   TRAP
//   END


int doIfLine (double exp1, double exp2)
  {
    //fprintf (logFile, "doIfLine %g %g\n", exp1, exp2);
    if (exp1 != 0.0)
      {
        int lineNum = lrint (exp2);
        if (lineNum < 1 || lineNum >= 0x10000)
          fatalN ("line number out of range", lineNum);
        if (! lineNumberTable [lineNum])
          fatalN ("no such line number", lineNum);
        lexLinePtr = lineNumberTable [lineNum];
        lexState = startOfLine;
// WTF: The role of YYACCEPT in control flow emulation in not documented.
        return 1; // Forces YYACCEPT, causing parser restart
      }
    return 0;
  }

int doIfThen (double exp)
  {
    if (exp == 0.0)
      {
        // consume tokens until end
        while (1)
          {
            int tok = yylex ();
            //fprintf (logFile, "\nconsuming %02x\n", tok);
            if (tok == tokEND)
              break;
            if (tok == tokEOF)
              fatal ("ran off end looking for END");
          }
        // consume the end statement
        while (1)
          {
            int tok = yylex ();
            //fprintf (logFile, "\nconsuming %02x\n", tok);
            if (tok == tokEOT)
              break;
            if (tok == tokEOF)
              fatal ("ran off end looking for END");
          }
// WTF: The role of YYACCEPT in control flow emulation in not documented.
        return 1; // Forces YYACCEPT, causing parser restart
      }
    return 0;
  }

struct returnStack
  {
    byte * tokenEnd;
    byte * lineNext;
    byte * lineStart;
  };
// WTF: Undocumented magic number 16. The value of returnStackSize is 
// arbitrary; I have no idea what the correct value should be, due to 
// utter failure to research the issue.
#define returnStackSize 16

struct returnStack returnStack [returnStackSize];
static int rsp = 0;

void doGosub (double exp)
  {
    if (rsp == returnStackSize)
      fatal ("return stack overflow");

    int lineNum = lrint (exp);
    if (lineNum < 1 || lineNum >= 0x10000)
      fatalN ("line number out of range", lineNum);
    if (! lineNumberTable [lineNum])
      fatalN ("no such line number", lineNum);

    returnStack [rsp] . tokenEnd = tokenEnd;
    returnStack [rsp] . lineNext = lineNext;
    returnStack [rsp] . lineStart = lineStart;
    rsp ++;

    lexLinePtr = lineNumberTable [lineNum];
    lexState = startOfLine;
    //fprintf (logFile, "gosub %d\n", lineNum);
  }

void doReturn (void)
  {
    if (rsp <= 0)
      fatal ("return stack underflow");
    rsp --;
    lexState = inOperands;
    lexLinePtr = tokenEnd = returnStack [rsp] . tokenEnd;
    lineNext = returnStack [rsp] . lineNext;
    lineStart = returnStack [rsp] . lineStart;
// WTF: The role of YYACCEPT in control flow emulation in not documented.
    return; // Forces YYACCEPT, causing parser restart
  }

void doPop (void)
  {
    if (rsp <= 0)
      fatal ("return stack underflow");
    rsp --;
    return;
  }

void doGoto (double exp)
  {
    int lineNum = lrint (exp);
    if (lineNum < 1 || lineNum >= 0x10000)
      fatalN ("line number out of range", lineNum);
    if (! lineNumberTable [lineNum])
      fatalN ("no such line number", lineNum);

    lexLinePtr = lineNumberTable [lineNum];
    lexState = startOfLine;
    //fprintf (logFile, "goto %d\n", lineNum);
  }

static int trapLine = 0;

void doTrap (double exp)
  {
    int lineNum = lrint (exp);
    if (lineNum == 0)
      {
        trapLine = 0;
        return;
      }

    if (lineNum < 1 || lineNum >= 0x10000)
      fatalN ("line number out of range", lineNum);
    if (! lineNumberTable [lineNum])
      fatalN ("no such line number", lineNum);
    trapLine = lineNum;
    //fprintf (logFile, "trap %d\n", lineNum);
  }

static void fireTrap (void)
  {
    if (trapLine)
      {
        if (trapLine < 1 || trapLine >= 0x10000)
          fatalN ("line number out of range", trapLine);
        if (! lineNumberTable [trapLine])
          fatalN ("no such line number", trapLine);

        //fprintf (logFile, "trap fire %d\n", trapLine);
        lexLinePtr = lineNumberTable [trapLine];
        lexState = startOfLine;
        longjmp (trapJmp, 1); // XXX should be the atari errno
      }
    return;
  }
   
void doEnd (void)
  {
    erase ();
    refresh ();
    endwin ();
#ifdef logfile
    fclose (logFile);
#endif
    exit (1);
  }

////////////////////////////////////////////////////////////////////////////////
//
// FOR/NEXT
//

struct forData
  {
    int indexVarNum;
    double limit;
    double step;
    byte * tokenEnd;
    byte * lineNext;
    byte * lineStart;
  };

struct forData forData;

// forward
static byte * tokenEnd;

// WTF:  The routine doFor() fails to either detect or deal with nested FOR 
// loops.
void doFor (int varNum, double initial, double final, double step)
  {
    if (varNum < 0 || varNum >= nv)
      fatalN ("varNum out of range", varNum);
    if (variableTable [varNum] . type != vtTypeScalar)
      fatal ("dimSVar expected a scalar variable");

    //fprintf (logFile, "for %s = %g to %g step %g\n", variableTable [varNum] . name, initial, final, step);

    variableTable [varNum] . val = initial;
    forData . indexVarNum = varNum;
    forData . limit = final;
    forData . step = step;
    forData . tokenEnd = tokenEnd;
    forData . lineNext = lineNext;
    forData . lineStart = lineStart;
  }
    
int doNext (int varNum)
  {
    if (varNum != forData . indexVarNum)
      fatal ("next wrong variable");
    variableTable [varNum] . val += forData . step;
    int done;
    if (forData . step < 0.0)
      done = variableTable [varNum] . val < forData . limit;
    else
      done = variableTable [varNum] . val > forData . limit;
    //fprintf (logFile, "next %s %g %d\n", variableTable [varNum] . name, variableTable [varNum] . val, done);
    if (! done)
      {
        //lexState = inToken;
        lexState = inOperands;
        lexLinePtr = tokenEnd = forData . tokenEnd;
        lineNext = forData . lineNext;
        lineStart = forData . lineStart;
// WTF: The role of YYACCEPT in control flow emulation in not documented.
        return 1; // Forces YYACCEPT, causing parser restart
      }
    return 0;
  }

////////////////////////////////////////////////////////////////////////////////
//
// LET
//

void doLet (int varNum, double val)
  {
    if (varNum < 0 || varNum >= nv)
      fatalN ("varNum out of range", varNum);
    if (variableTable [varNum] . type != vtTypeScalar)
      fatal ("doLet expected a scalar variable");
    variableTable [varNum] . val = val;
    //fprintf (logFile, "let %s = %g\n", variableTable [varNum] . name, val);
  }

void doSLet (int varNum, char * val)
  {
    if (varNum < 0 || varNum >= nv)
      fatalN ("varNum out of range", varNum);
    if (variableTable [varNum] . type != vtTypeString)
      fatal ("doSLet expected a string variable");
    int l = (unsigned char) (val [0]);
    if (l > variableTable [varNum] . dim)
      l = variableTable [varNum] . dim;
// WTF: Undocumented magic number 1
    memmove (variableTable [varNum] . sval, val, l + 1);
    variableTable [varNum] . sval [0] = l;
    
    //fprintf (logFile, "let %s = ", variableTable [varNum] . name);
    //pNString (variableTable [varNum] . sval);
    //fprintf (logFile, "\n");

  }


////////////////////////////////////////////////////////////////////////////////
//
// Parser/interpreter
//

// WTF: YYSTYPE is defined independently in omgwtf2.c and atbas.y.
//    This arises because yylex() properly belongs in atbas.y where it can
//    see the natural definition. Yylex() is in omgwtf2.c because is was
//    first written, under a different name, before bison was in play.

union YYSTYPE
  {
    double val;
    int ival;
    char * sval;
  };

extern union YYSTYPE yylval;
int yyparse (void);

// WTF: The routine yylex() is very fragile; it is a state machine
//    whose state is manipulated by the control flow code, without a line of
//    documentation.

int yylex (void)
  {

    if (lexState == atEnd)
      return tokEOF;

    if (lexLinePtr >= programFile + stmtab + codeLen)
      {
        lexState = atEnd;
        return tokEOT;
      }

    switch (lexState)
      {
        case startOfLine:
          {
            lineStart = lexLinePtr;
            word lineNumber;
            scan (word, lineNumber, lexLinePtr);
            //fprintf (logFile, "line %d\n", lineNumber);
            currentLineNumber = lineNumber;
            byte lineLength;
            scan (byte, lineLength, lexLinePtr);
            lineNext = lineStart + lineLength;
            yylval . ival = lineNumber;
            lexState = inToken;
            goto intoken;
          }

        case inToken:
          {
intoken:
            if (lexLinePtr > lineNext)
              {
                lexState = startOfLine;
                //fprintf (logFile, "yylex eot %02x\n", 3);
                return tokEOT;
              }
            //byte * tokenPtr = lexLinePtr;
            byte tokenLen;
            scan (byte, tokenLen, lexLinePtr);
            byte token;
            scan (byte, token, lexLinePtr);
            tokenEnd = lineStart + tokenLen;
            lexState = inOperands;

            // Special cases
            // Discard every thing after REM
            if (token == tokREM)
              lexLinePtr = tokenEnd;

            if (token == tokREM)
              token = tokREM2;

            //fprintf (logFile, "yylex token %02x\n", token);
            return token;
          }

        case inOperands:
          {
            if (lexLinePtr >= lineNext)
              {
                lexState = startOfLine;
                //fprintf (logFile, "yylex eot %02x\n", 2);
                return tokEOT;
              }

            if (lexLinePtr >= tokenEnd)
              {
                lexState = inToken;
                //fprintf (logFile, "yylex eot %02x\n", 1);
                return tokEOT;
              }
            byte op;
            scan (byte, op, lexLinePtr);

            // Special cases
            if (op == opNCONST)
              {
                double val = parseNCONST (& lexLinePtr);
                yylval . val = val;
              }

            else if (op == opSCONST)
              {
                yylval . sval = (char *) lexLinePtr;
                byte len;
                scan (byte, len, (lexLinePtr));
                lexLinePtr += len;
              }
            else if (op & varFlagBit)
              {
                yylval . ival = op & removeVarFlagBit;
                if (variableTable [yylval . ival] . type == 0)
                  op = opVAR;
                else
                  op = opSVAR;
              }
            //fprintf (logFile, "yylex op 0x1%02x\n", op);
            return op | 0x100;
          }
      }
    fatal ("yylex fell through");
    return tokEOF;
  }

void yyerror (const char * msg)
  {
    fatal (msg);
  }

static void interpret (void)
  {
    lexLinePtr = programFile + stmtab;
    lexState = startOfLine;

    setjmp (trapJmp);
// WTF: The routine interpret() is a marvel of simplicity whose
//    behavior is controlled entirely be outside code using longjmp and
//    YYACCEPT without a shred of documentation.

    while (1)
      {
        //fprintf (logFile, "parse...\n");
        yyparse ();
      }
  }  
    

int main (int argc, char * argv [])
  {

#ifdef logfile
    logFile = fopen ("omgwtf2.log", "w");
    setvbuf (logFile, NULL, _IONBF, 0);
#endif

// WTF:  The pseudo-random number generator is seeded with
//    a constant value, causing the application to return the the
//    sequence of responses for the same input data on different runs.

    iseed = 12345;

    lastKBChar = 255;

    readAtrFile ("Executive Decision Maker.atr");
    readBasicFile ("DECISION", "BAS");
    analyzeBasicFile ();
    interpret ();
    return 0;
  }

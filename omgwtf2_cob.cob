       identification division.
       program-id.  omgwtf2.

       environment division.

       input-output section.

       file-control.
           select atr-fd assign "Executive Decision Maker.atr"
              organization is record sequential.

       data division.

       file section.
       fd atr-fd.

       01 atr-file.
           05 atr-header.
               10 atr-header-wMagic usage binary-short unsigned.
               10 atr-header-wPars usage binary-short unsigned.
               10 atr-header-wSecSize usage binary-short unsigned.
               10 atr-header-btParsHigh usage binary-char unsigned.
               10 atr-header-dwCRC usage binary-long unsigned.
               10 atr-header-dwUnused usage binary-long unsigned.
               10 atr-header-btFlags usage binary-char unsigned.
           05 atr-sectors occurs 720 times.
               10 atr-sectors-bytes occurs 128 times.
                   15 atr-sectors-byte usage binary-char unsigned.

       working-storage section.

      * readAtrFile()
       
       01 diskSize usage binary-long unsigned.
       01 sectorSize usage binary-long unsigned.

      * readBasicFile()

      * Number of sectors of directory entries
       01 numDirSectors constant as 8.

      * First directory sector (the above web page is 1-based sector #s)
       01 startDirSector constant as H"169".

      * Number of directory entries in a directory sector
       01 numDirEntries constant as 8.

      * primary file name length
       01 pnameLen constant as 8.

      * file name extension length
       01 extLen constant as 3.

      * entry deleted flag bit
       01 flagDeleted constant as H"80".

      * entry in use flag bit
       01 flagInUse constant as H"40".

      * number of byte bytes in a data sector
       01 numDataBytes constant as 125.

      * mask out file number from next sector number field
       01 nextMask constant as H"3".

      * struct dirEntry
      *   {
      *     byte flag;
      *     word count;
      *     word ssn;
      *     char pname [pnameLen];
      *     char ext [extLen];
      *   } __attribute__((packed));

      * 01 dirEntry based.
      *     02 dirEntry-flag usage binary-char unsigned.
      *     02 dirEntry-count usage binary-short unsigned.
      *     02 dirEntry-ssn usage binary-short unsigned.
      *     02 dirEntry-pname pic x(pnameLen).
      *     02 dirEntry-ext pic x(extLen).

      * struct dirSector
      *   {
      *     struct dirEntry entries [numDirEntries];
      *   };

        01 dirSector based.
            05 aDirSector occurs numDirEntries times.
                10  dirEntry-flag usage binary-char signed.
                10  dirEntry-count usage binary-short unsigned.
                10  dirEntry-ssn usage binary-short unsigned.
                10  dirEntry-pname pic x(pnameLen).
                10  dirEntry-ext pic x(extLen).

      * struct dataSector
      *   {
      *     byte data [numDataBytes];
      *     byte nextSectorNumHi; // File number encoded in high six bits
      *     byte nextSectorNumLo;
      *     byte numBytesUsed;
      *   } __attribute__((packed));
 
       01 dataSector based.
           02 dataSector-data occurs numDataBytes times.
               04 dataSector-bytes usage binary-char unsigned.
           02 dataSector-nextSectorNumHi usage binary-char unsigned.
           02 dataSector-nextSectorNumLo usage binary-char unsigned.
           02 dataSector-numBytesUsed usage binary-char unsigned.

       01 pfp-data based.
           02 pfpDataSector-data occurs numDataBytes times.
               04 pfpDataSector-bytes usage binary-char unsigned.

       01 found usage binary-long.
       01 i usage binary-long.
       01 j usage binary-long.
       01 entryNum usage binary-long.
       01 isDeleted usage binary-long.
       01 fileSize usage binary-long.
       01 programFile usage pointer.
       01 pfp usage pointer.
       01 cnt usage binary-short unsigned.
       01 ssn usage binary-short unsigned.
       01 sn usage binary-short unsigned.
       01 programFileUsed usage binary-long unsigned.



      * struct header
      *   {
      *     word lomem;
      *     word vnt;
      *     word vnte;
      *     word vvt;
      *     word stmtab;
      *     word stmcur;
      *     word starp;
      *   } __attribute__((packed));

       01 header based.
           05 hdr-lomem usage binary-short unsigned.
           05 hdr-vnt usage binary-short unsigned.
           05 hdr-vnte usage binary-short unsigned.
           05 hdr-vvt usage binary-short unsigned.
           05 hdr-stmtab usage binary-short unsigned.
           05 hdr-stmcur usage binary-short unsigned.
           05 hdr-starp usage binary-short unsigned.

       01 wcor usage binary-short unsigned.
       01 vnt usage binary-short unsigned.
       01 vvt usage binary-short unsigned.
       01 stmtab usage binary-short unsigned.
       01 stmcur usage binary-short unsigned.
       01 vvte usage binary-short unsigned.
       01 vvtl usage binary-short unsigned.
       01 codeLen usage binary-short unsigned.

       01 linePtr usage pointer.

       01 len usage binary-long signed.
       01 name usage pointer.
       01 name-array based.
           05 name-array-chars occurs 256 times.
               10 name-array-char usage binary-char unsigned.
       01 str usage pointer.
       01 str-array based.
           05 str-array-chars occurs 256 times.
               10 str-array-char usage binary-char unsigned.

       01 char usage binary-char unsigned.

      * struct vvtEntry
      *   {
      *     byte btType;
      *     byte btNumber;
      *     byte data [6];
      *   };

       01 vvtEntryStruct based.
           05 vvtTable occurs 256 times.
               10 btType usage binary-char unsigned.
               10 btNumber usage binary-char unsigned.
               10 btDataArray occurs 6 times.
                   20 btData usage binary-char unsigned.
       01 vvtEntry usage pointer.

       01 lineNumber usage binary-short unsigned.
       01 lineLength usage binary-char unsigned.
       01 firstLineNumber usage binary-short unsigned.
       01 lastLineNumber usage binary-short unsigned.

       01 wordCast based.
           05 wordCasted usage binary-short unsigned.

       01 byteCast based.
           05 byteCasted usage binary-char unsigned.

       01 startOfLine constant as 0.
       01 inToken constant as 2.
       01 inOperands constant as 3.
       01 atEnd constant as 4.

       01 vtTypeScalar constant as 0.
       01 vtTypeString constant as H"80".

      *  Operands

       01 opNCONST constant as H"0e".
       01 opSCONST constant as H"0f".
       01 opComma1 constant as H"12".
       01 opColon  constant as H"14".
       01 opSemi   constant as H"15".
       01 opEOL    constant as H"16".
       01 opTo     constant as H"19".
       01 opStep   constant as H"1a".
       01 opThen   constant as H"1b".
       01 opSharp  constant as H"1c".
       01 opNE     constant as H"1e".
       01 opEQ1    constant as H"22".
       01 opTimes  constant as H"24".
       01 opMinus1 constant as H"26".
       01 opClose  constant as H"2c".
       01 opPlus   constant as H"25".
       01 opDiv    constant as H"27".
       01 opOr     constant as H"29".
       01 opOpen1  constant as H"2b".
       01 opEQ2    constant as H"2d".
       01 opEQ3    constant as H"2e".
       01 opEQ4    constant as H"34".
       01 opMinus2 constant as H"36".
       01 opOpen2  constant as H"37".
       01 opOpen3  constant as H"3a".
       01 opOpen4  constant as H"3b".
       01 opComma2 constant as H"3c".
       01 opLen    constant as H"42".
       01 opPeek   constant as H"46".
       01 opRnd    constant as H"48".
       01 opInt    constant as H"50".

      *  tokens

       01 tokREM      constant as H"00".
       01 tokINPUT    constant as H"02".
       01 tokIF       constant as H"07".
       01 tokFOR      constant as H"08".
       01 tokNEXT     constant as H"09".
       01 tokGOTO     constant as H"0a".
       01 tokGO_TO    constant as H"0b".
       01 tokGOSUB    constant as H"0c".
       01 tokTRAP     constant as H"0d".
       01 tokDIM      constant as H"14".
       01 tokEND      constant as H"15".
       01 tokPOKE     constant as H"1f".
       01 tokPRINT    constant as H"20".
       01 tokRETURN   constant as H"24".
       01 tokPOP      constant as H"27".
       01 tokQMARK    constant as H"28".
       01 tokGRAPHICS constant as H"2b".
       01 tokPOSITION constant as H"2d".
       01 tokSETCOLOR constant as H"30".
      *  silent let
       01 tokLET      constant as H"36".

      *  Special tokens for our parser

      *  end of token
       01 tokEOT      constant as H"f1".
      *  end of file
       01 tokEOF      constant as H"00".
      *  alias for tokREM that is non-zero
       01 tokREM2     constant as H"f3".
       01 opVAR       constant as H"80".
       01 opSVAR      constant as H"81".

       01 frozen usage is binary-long signed value 0.
       01 hell usage is binary-long signed value 1.
       copy "globals.cob".

       procedure division.

      * readAtrFile()

           if function length (atr-header) not equal to 16 then
               display "struct header wrong size"
               stop run
           end-if.

           open input atr-fd.
           read atr-fd record into atr-file.
           close atr-fd.

           if atr-header-wMagic not equal to H"0296" then
               display "ATR header magic value wrong"
               stop run
           end-if.

      * uint32_t diskSize = ((hdr . btParsHigh << 16) + hdr . wPars) * 16;

           compute diskSize equal ((atr-header-btParsHigh * H"10000") +
                                   atr-header-wPars) * 16.

           if diskSize + 16 not equal to 92176 then
               display "diskSize value wrong"
               stop run
           end-if.

      * sectorSize = hdr . wSecSize;

           compute sectorSize = atr-header-wSecSize.



      * readBasicFile()

           if function length (dirSector) not equal to 128 then
               display "struct dirSector wrong size"
               stop run
           end-if.

           if function length (dataSector) not equal to 128 then
               display "struct datairSector wrong size"
               stop run
           end-if.

           perform with test before
             varying i from 0 by 1 until i 
             is greater than or equal to numDirSectors

      * #define mapSectorPtr(n) ((byte *) diskImage +((n) - oneBase) *
      *     sectorSize )

               set address of dirSector to address of
                 atr-sectors(startDirSector + i)

               perform with test before 
                 varying entryNum from 1 by 1 until entryNum 
                 is greater than numDirEntries

                   if dirEntry-flag(entryNum) equal to 66 and
                      dirEntry-pname(entryNum) equal to "DECISION" and
                      dirEntry-ext(entryNum) equal to "BAS" then
                       move 1 to found
                       exit perform
                   end-if
               end-perform
               if found not equal zero then
                 exit perform
               end-if
           end-perform.

           if found equal zero then
             display "program file not found"
             stop run.
          
      * unsigned int fileSize = entry -> count * numDataBytes;

           compute fileSize equal dirEntry-count(entryNum) * 
             numDataBytes.

           allocate (fileSize) characters returning programFile.
                           display "programFile " programFile
           move dirEntry-count(entryNum) to cnt
           move dirEntry-ssn(entryNum) to ssn
           move zero to programFileUsed
           move programFile to pfp

           move ssn to sn

           perform varying i from 0 by 1 until i 
             is greater than or equal to cnt

      * struct dataSector * data = (struct dataSector *) mapSectorPtr (sn);

               set address of dataSector to address of
                 atr-sectors(sn)

               set address of pfp-data to pfp

               perform varying j from 1 by 1 until 
                 j greater than numDataBytes
                   move dataSector-data(j) to pfpDataSector-data(j)
               end-perform

               set pfp up by dataSector-numBytesUsed
               add dataSector-numBytesUsed to programFileUsed
               compute sn equal 
                 function mod (dataSector-nextSectorNumHi, 4) *
                 256 + dataSector-nextSectorNumLo
           end-perform

      * analyzeBasicFile()

           set address of header to programFile

      *    display "lomem " hdr-lomem
      *    display "vnt " hdr-vnt
      *    display "vnte " hdr-vnte
      *    display "vvt " hdr-vvt
      *    display "stmtab " hdr-stmtab
      *    display "stmcur " hdr-stmcur
      *    display "starp " hdr-starp

           compute wcor equal hdr-vnt - hdr-lomem - H"0e"
           compute vnt equal hdr-vnt - wcor
           compute vvt equal hdr-vvt - wcor
           compute stmtab equal hdr-stmtab - wcor
           compute stmcur equal hdr-stmcur - wcor
           compute vvte equal stmtab - 1
           compute vvtl equal vvte - vvt + 1
           compute nv equal vvtl / 8
           compute codeLen equal stmcur - stmtab

      *    display "Constants & pointers:"
      *    display "Start of Name Table      (VNT)   : " vnt
      *    display "Start of Variable Table  (VVT)   : " vvt
      *    display "End of Variable Table    (VVTE)  : " vvte
      *    display "Length of Variable Table (VVTL)  : " vvtl

      *    display "Number of Variables      (NV)    : " nv

      *    display "Start of Code            (STMTAB): " stmtab
      *    display "Length of Code                   : " codeLen
      *    display "Current command          (STMCUR): " stmcur


      *    allocate (nv * length of variableTableStruct) characters
      *      returning variableTable
      *    set address of variableTableStruct to variableTable

           move programFile to name
           set name up by vnt
           set address of name-array to name
      *    call "puts" using name-array

           perform varying i from 1 by 1 until i 
             is greater than nv

               move 1 to len
               perform with test before until
                 name-array-char(len) greater than 127
                   add 1 to len
               end-perform

               allocate (len + 1) characters returning str
               set address of str-array to str

               perform varying j from 1 by 1 until 
                 j greater than len
                   move name-array-chars(j) to char
                   if char greater than 127 then
                     subtract 128 from char
                   end-if
                   move char to str-array-char(j)
               end-perform
               move H"00" to str-array-char(len + 1)
      *    call "puts" using str-array
               move str to vts-name(i)
               set name up by len
               set address of name-array to name

           end-perform

           move programFile to vvtEntry
           set vvtEntry up by vvt              
           set address of vvtEntryStruct to vvtEntry

           perform varying i from 1 by 1 until i 
             is greater than nv

               move btType(i) to vts-type(i)
               move 0 to vts-dim(i)
               move 0 to vts-val(i)
               move NULL to vts-sval(i)
           end-perform

           move zero to firstLineNumber
           move zero to lastLineNumber

           move programFile to linePtr
           set linePtr up by stmtab

           move programFile to programEnd
           set programEnd up by stmtab
           set programEnd up by codeLen

           perform until linePtr greater than or equal to
             programEnd

               move linePtr to lineStart

               set address of wordCast to linePtr
               move wordCasted to lineNumber
               set linePtr up by length of wordCast

               set address of byteCast to linePtr
               move byteCasted to lineLength
               set linePtr up by length of byteCast

               move lineStart to lineNext
               set lineNext up by lineLength

               if firstLineNumber equal to zero then
                 move lineNumber to firstLineNumber 
               end-if
               move lineNumber to lastLineNumber

      *        display "lineNumber " lineNumber 
      *                " lineLength " lineLength
               set lineNumberRow(lineNumber) to lineStart
               move lineNext to linePtr

           end-perform

      * interpret()

           move zero to gInit
           move programFile to lexLinePtr
           set lexLinePtr up by stmtab
                              display "lexLinePtr init " lexLinePtr

           move startOfLine to lexState              

           call "setjmp" using trapJmp
           perform until hell equal frozen
             display "calling yyparse"
             call "yyparse"
             display "back from  yyparse"
           end-perform

           exit program.

       identification division.
       program-id. yylex.
       environment division.
       data division.
       working-storage section.
       copy "globals.cob".
       01 lineNumber usage binary-short unsigned.
       01 lineLength usage binary-char unsigned.
       01 tokenLen usage binary-char unsigned.
       01 token usage binary-char unsigned.
       01 len usage binary-char unsigned.
       01 op usage binary-char unsigned.
       01 iop usage binary-long unsigned.
       01 val usage computational-2.

       01 wordCast based.
           05 wordCasted usage binary-short unsigned.

       01 byteCast based.
           05 byteCasted usage binary-char unsigned.

       01 intCast based.
           05 intCasted usage binary-long signed.

       01 doubleCast based.
           05 doubleCasted usage computational-2.

       01 ptrCast based.
           05 ptrCasted usage pointer.

       01 dbg usage is pointer.

       linkage section.

      *procedure division returning rc.
       procedure division.

       display "in yylex; lexState " lexState

       if lexState equal atEnd then
         move 0 to return-code
         goback
       end-if

       if lexState equal atEnd
         move tokEOF to return-code
         goback
       end-if
      
       if lexLinePtr greater than programEnd then
         move atEnd to lexState
         move tokEOT to return-code
         goback
       end-if
      
      *  case startOfLine:

       if lexState equal startOfLine then
                                     display "yylex state startOfLine"
      *     lineStart = lexLinePtr;
         move lexLinePtr to lineStart
      *     scan (word, lineNumber, lexLinePtr);
         set address of wordCast to lexLinePtr
         move wordCasted to lineNumber
         set lexLinePtr up by length of wordCast
      *     currentLineNumber = lineNumber;
                                     display "line# " lineNumber
         move lineNumber to currentLineNumber
      *     scan (byte, lineLength, lexLinePtr);
         set address of byteCast to lexLinePtr
         move byteCasted to lineLength
         set lexLinePtr up by length of byteCast
                                     display "lineLength " lineLength
      *     lineNext = lineStart + lineLength;
         move lineStart to lineNext
         set lineNext up by lineLength
      *     yylval . ival = lineNumber;
         set address of intCast to address of yylval
         move lineNumber to intCasted
      *     lexState = inToken;
         move inToken to lexState
      *     goto intoken;
       end-if

       if lexState equal inToken then
                                     display "yylex state inToken"
      *        case inToken:
      *          {
      *intoken:
      *            if (lexLinePtr > lineNext)
         if lexLinePtr greater than lineNext then
      *              {
                                     display "lexLinePtr > lineNext "
                                       lexLinePtr "  " lineNext
      *                lexState = startOfLine;
           move startOfLine to lexState
      *                //fprintf (logFile, "yylex eot %02x\n", 3);
      *                return tokEOT;
           move tokEOT to return-code
           goback
      *              }
         end-if
      *            //byte * tokenPtr = lexLinePtr;
      *            byte tokenLen;
      *            scan (byte, tokenLen, lexLinePtr);
         set address of byteCast to lexLinePtr
         move byteCasted to tokenLen
         set lexLinePtr up by length of byteCast
                                     display "tokenLen " tokenLen
      *            byte token;
      *            scan (byte, token, lexLinePtr);
         set address of byteCast to lexLinePtr
         move byteCasted to token
         set lexLinePtr up by length of byteCast
                                     display "token " token
      *            tokenEnd = lineStart + tokenLen;
         move lineStart to tokenEnd
         set tokenEnd up by tokenLen
      *            lexState = inOperands;
         move inOperands to lexState
      *
      *            // Special cases
      *            // Discard every thing after REM
      *            if (token == tokREM)
      *              lexLinePtr = tokenEnd;
         if token equal tokREM then
                                     display "is REM"
           move tokenEnd to lexLinePtr
         end-if
      *
      *            if (token == tokREM)
      *              token = tokREM2;
         if token equal tokREM then
           move tokREM2 to token
         end-if
      *
      *            //fprintf (logFile, "yylex token %02x\n", token);
      *            return token;
         move token to return-code
                                   display "yylex returns " token
                                   display "yylex returns " return-code
         goback
      *          }
       end-if
      *
      *        case inOperands:
       if lexState equal inOperands then
      *          {
                                     display "yylex state inOperands"
      *            if (lexLinePtr >= lineNext)
         if lexLinePtr greater than or equal lineNext then
      *              {
                                     display "lexLinePtr >= lineNext"
      *                lexState = startOfLine;
           move startOfLine to lexState
      *                //fprintf (logFile, "yylex eot %02x\n", 2);
      *                return tokEOT;
           move tokEOT to return-code
           goback
      *              }
         end-if
      *
      *            if (lexLinePtr >= tokenEnd)
         if lexLinePtr greater than or equal tokenEnd then
      *              {
                                     display "lexLinePtr >= tokenEnd"
      *                lexState = inToken;
           move inToken to lexState
      *                //fprintf (logFile, "yylex eot %02x\n", 1);
      *                return tokEOT;
           move tokEOT to return-code
           goback
      *              }
         end-if
      *            byte op;
      *            scan (byte, op, lexLinePtr);
         set address of byteCast to lexLinePtr
         move byteCasted to op
         set lexLinePtr up by length of byteCast
                                     display "op " op
      *
      *            // Special cases
      *            if (op == opNCONST)
         if op equal opNCONST then
      *              {
      *                double val = parseNCONST (& lexLinePtr);
           call "parseNCONST" using by reference lexLinePtr, 
                                    by reference val
      *                yylval . val = val;
           set address of doubleCast to address of yylval
           move val to doubleCasted
      *              }
         end-if
      *
      *            else if (op == opSCONST)
         if op equal opSCONST then
      *              {
      *                yylval . sval = (char *) lexLinePtr;
           set address of ptrCast to address of yylval
           move lexLinePtr to ptrCasted
      *                byte len;
      *                scan (byte, len, (lexLinePtr));
           set address of byteCast to lexLinePtr
           move byteCasted to len
           set lexLinePtr up by length of byteCast
      *                lexLinePtr += len;
           set lexLinePtr up by len
      *              }
         end-if
      *            else if (op & varFlagBit)
         if op greater than 127 then
      *              {
      *                yylval . ival = op & removeVarFlagBit;
                                     display "op > 127"
           subtract 128 from op
                                     display "op now " op
           set address of intCast to address of yylval
           move op to intCasted
      *                if (variableTable [yylval . ival] . type == 0)
      * vts is 1-based
           compute iop equal op + 1
                                     display "iop now " iop
                                     display "vts-type(iop) now "
                                           vts-type(iop)
           if vts-type(iop) equal 0 then
      *                  op = opVAR;
             move opVAR to op
      *                else
           else
      *                  op = opSVAR;
             move opSVAR to op
           end-if
      *              }
                                     display "op now " op
         end-if
      *            //fprintf (logFile, "yylex op 0x1%02x\n", op);
      *            return op | 0x100;
         move op to iop
         add H"100" to iop
         move iop to return-code
         goback
      *          }
      *      }
      *    fatal ("yylex fell through");
        call "fatal" using "yylex fell through"
      *    return tokEOF;
       move tokEOF to return-code
       goback.

       end program yylex.

       identification division.
       program-id. yyerror.
       environment division.
       data division.
       working-storage section.
       linkage section.
       01 msg usage is pointer.

       procedure division using msg.
       call "fatal" using msg
       stop run.

       end program yyerror.

       identification division.
       program-id. fatal.
       environment division.
       data division.
       working-storage section.
       linkage section.
       01 desc usage is pointer.

       procedure division using desc.
       call "printf" using "Fatal error: %s" & x"0a" desc
       stop run.

       end program fatal.

       identification division.
       program-id. doPop.
       environment division.
       data division.
       working-storage section.
      *01 rsp is external usage binary-long signed.
       copy "globals.cob".

       procedure division.
      
       if rsp less than or equal to 1 then
         call "fatal" using "return stack underflow"
       end-if
       subtract 1 from rsp
       goback.

       end program doPop.

       identification division.
       program-id. doReturn.
       environment division.
       data division.
       working-storage section.
       copy "globals.cob".

       procedure division.
      
       if rsp less than or equal to 1 then
         call "fatal" using "return stack underflow"
       end-if

       subtract 1 from rsp
       move inOperands to lexState
       move rs-tokenEnd(rsp) to tokenEnd
       move tokenEnd to lexLinePtr
       move rs-lineNext(rsp) to lexLinePtr
       move rs-lineStart(rsp) to lineStart
       goback.

       end program doReturn.

       identification division.
       program-id. doTrap.
       environment division.
       data division.
       working-storage section.
       copy "globals.cob".
       01 lineNum usage is binary-long signed.

       linkage section.
       01 exp usage is computational-2.

       procedure division using exp.

       move exp to lineNum
       if lineNum equal zero then
         move zero to trapLine 
         goback
       end-if

       if lineNum less than 1 or lineNum greater than H"10000" then
         call "fatal" using "line number out of range"
       end-if

       if lineNumberRow (lineNum) equal 0 then
         call "fatal" using "no such line number"
       end-if

       move lineNum to trapLine.
       goback.

       end program doTrap.

       identification division.
       program-id. fireTrap.
       environment division.
       data division.
       working-storage section.
       copy "globals.cob".
       01 lineNum usage is binary-long signed.

       procedure division.

       if trapLine not equal zero then
           if trapLine less than 1 or
             trapLine greater than H"10000" then
               call "fatal" using "line number out of range"
           end-if

           if lineNumberRow (trapLine) equal 0 then
               call "fatal" using "no such line number"
           end-if

           move lineNumberRow(trapLine) to lexLinePtr
           move startOfLine to lexState
           call "longjmp" using trapJmp, 1
       end-if
       goback.

       end program fireTrap.

       identification division.
       program-id. doGoto.
       environment division.
       data division.
       working-storage section.
       copy "globals.cob".
       01 lineNum usage is binary-long signed.

       linkage section.
       01 exp usage is computational-2.

       procedure division using exp.

       move exp to lineNum

       if lineNum less than 1 or lineNum greater than H"10000" then
         call "fatal" using "line number out of range"
       end-if

       if lineNumberRow (lineNum) equal NULL then
         call "fatal" using "no such line number"
       end-if

       move lineNumberRow(lineNum) to lexLinePtr
       move startOfLine to lexState.
       goback.

       end program doGoto.

       identification division.
       program-id. doGosub.
       environment division.
       data division.
       working-storage section.
       copy "globals.cob".
       01 lineNum usage is binary-long signed.

       linkage section.
       01 exp usage is computational-2.

       procedure division using exp.
      
       if rsp greater than 16 then
         call "fatal" using "return stack overflow"
       end-if

       move exp to lineNum

       if lineNum less than 1 or lineNum greater than H"10000" then
         call "fatal" using "line number out of range"
       end-if

       if lineNumberRow (lineNum) equal NULL then
         call "fatal" using "no such line number"
       end-if

       move tokenEnd to rs-tokenEnd(rsp)
       move lineNext to rs-lineNext(rsp)
       move lineStart to rs-lineStart(rsp)
       add 1 to rsp

       move lineNumberRow(lineNum) to lexLinePtr
       move startOfLine to lexState
       goback.

       end program doGosub.


       identification division.
       program-id. doInput.
       environment division.
       data division.
       working-storage section.
       copy "globals.cob".
       01 maxlen usage is binary-long signed.
       01 i usage is binary-long signed.
       01 buf pic x(256) usage is display.
       01 cbuf pic x usage is display.
       01 zbuf usage is binary-char unsigned value zero.
       01 byteCast based.
           05 byteCasted usage binary-char unsigned.
     
       01 sval usage is pointer.
       01 svalCast based.
           05 svalCasted usage binary-char unsigned.

       linkage section.
       01 varNum usage is binary-long signed.

       procedure division using varNum.
      
       if varNum less than zero or greater than or equal to nv then
         call "fatal" using "varNum out of range"
       end-if

       if vts-type(varNum) not equal to vtTypeString then
         call "fatal" using "doInput expected a string variable"
       end-if

       if vts-sval(varNum) equal NULL then
         call "fatal" using "unallocated string"
       end-if

       move vts-dim(varNum) to maxLen
       display "? "

       accept buf

       set address of byteCast to address of cbuf
       set sval to address of vts-sval(varNum)

       perform varying i from 1 by 1 until i greater than 256
           move buf(i:1) to cbuf
           if i is less than or equal to maxLen then
             move byteCasted to svalCasted
             set sval up by 1
           end-if
       end-perform
       set address of byteCast to address of zbuf
       move byteCasted to svalCasted
       goback.

       end program doInput.

       identification division.
       program-id. doEnd.
       environment division.
       data division.
       working-storage section.

       procedure division.
      
       stop run.

       end program doEnd.

       identification division.
       program-id. doIfThen.
       environment division.
       data division.
       working-storage section.
       copy "globals.cob".
       01 tok usage is binary-long signed.
       01 frozen usage is binary-long signed value 0.
       01 hell usage is binary-long signed value 1.

       linkage section.
       01 exp usage is computational-2.
      *01 rc usage is binary-long signed.

      *procedure division using exp returning rc.
       procedure division using exp.
      
       if exp equal to 0.0 then
      * consume tokens until end
           perform until hell equal frozen
               call "yylex" returning tok
               if tok equal tokEOF then
                 call "fatal" using "ran off end looking for END"
               end-if            
               if tok equal tokEnd then
                 exit perform
               end-if
           end-perform
      * consume the end statement
           perform until hell equal frozen
               call "yylex" returning tok
               if tok equal tokEOF then
                 call "fatal" using "ran off end looking for END"
               end-if            
               if tok equal tokEOT then
                 exit perform
               end-if
           end-perform
      * Forces YYACCEPT, causing parser restart
      *    move 1 to rc
           move 1 to return-code
           goback
       end-if
      *move 0 to rc
       move 0 to return-code
       goback.

       end program doIfThen.

       identification division.
       program-id. doIfLine.
       environment division.
       data division.
       working-storage section.
       copy "globals.cob".
       01 lineNum usage is binary-long signed.

       linkage section.
       01 exp1 usage is computational-2.
       01 exp2 usage is computational-2.
      *01 rc usage is binary-long signed.

      *procedure division using exp1 exp2 returning rc.
       procedure division using exp1 exp2.
      
       if exp1 not equal to 0.0 then
      * consume tokens until end
           move exp2 to lineNum

           if lineNum less than 1 or lineNum greater than H"10000" then
             call "fatal" using "line number out of range"
           end-if

           if lineNumberRow (lineNum) equal NULL then
             call "fatal" using "no such line number"
           end-if

           move lineNumberRow(lineNum) to lexLinePtr
           move startOfLine to lexState

      * Forces YYACCEPT, causing parser restart
      *    move 1 to rc
           move 1 to return-code
           goback
       end-if
      *move 0 to rc
       move 0 to return-code
       goback.

       end program doIfLine.

       identification division.
       program-id. doSLet.
       environment division.
       data division.
       working-storage section.
       copy "globals.cob".
       01 i usage is binary-char unsigned.
       01 l usage is binary-char unsigned.
     
       01 sval usage is pointer.
       01 svalCast based.
           05 svalCasted usage binary-char unsigned.
       01 byteCast based.
           05 byteCasted usage binary-char unsigned.

       linkage section.
       01 varNum usage is binary-long signed.
       01 val usage is pointer.

       procedure division using varNum, val.
      
       if varNum less than zero or greater than or equal to nv then
         call "fatal" using "varNum out of range"
       end-if

       if vts-type(varNum) not equal to vtTypeString then
         call "fatal" using "doSLet expected a string variable"
       end-if

       move vts-sval(varNum) to sval
       move svalCasted to l
       if l greater than vts-dim(varNum) then
         move vts-dim(varNum) to l
       end-if

       perform varying i from 1 by 1 until i greater than l + 1
         set address of byteCast to val
         move byteCasted to svalCasted
         set sval up by 1
         set val up by 1
       end-perform
       move vts-sval(varNum) to sval
       move l to svalCasted
       goback.

       end program doSLet.

       identification division.
       program-id. doLet.
       environment division.
       data division.
       working-storage section.
       copy "globals.cob".

       linkage section.
       01 varNum usage is binary-long signed.
       01 val usage is computational-2.

       procedure division using varNum, val.
      
       if varNum less than zero or greater than or equal to nv then
         call "fatal" using "varNum out of range"
       end-if

       if vts-type(varNum) not equal to vtTypeScalar then
         call "fatal" using "doSLet expected a scalar variable"
       end-if

       move val to vts-val(varNum)
       goback.
       end program doLet.

       identification division.
       program-id. doFor.
       environment division.
       data division.
       working-storage section.
       copy "globals.cob".

       linkage section.
       01 varNum usage is binary-long signed.
       01 vinitial usage is computational-2.
       01 vfinal usage is computational-2.
       01 vstep usage is computational-2.

       procedure division using varNum vinitial vfinal vstep.
      
       if varNum less than zero or greater than or equal to nv then
         call "fatal" using "varNum out of range"
       end-if

       if vts-type(varNum) not equal to vtTypeScalar then
         call "fatal" using "doSLet expected a scalar variable"
       end-if

       move vinitial to vts-val(varNum)
       move varNum to fd-indexVarNum
       move vfinal to fd-limit
       move vstep to fd-step
       move tokenEnd to fd-tokenEnd
       move lineNext to fd-lineNext
       move lineStart to fd-lineStart
       goback.
       end program doFor.

       identification division.
       program-id. doPoke.
       environment division.
       data division.
       working-storage section.
       copy "globals.cob".
       01 iaddr usage is binary-long signed.
       01 ival usage is binary-long signed.

       linkage section.
       01 addr usage is computational-2.
       01 val usage is computational-2.

       procedure division using addr val.
      
       move addr to iaddr
       move val to ival

       if iaddr equal 752 then
         goback
       end-if

       if iaddr equal 764 then
         move ival to lastKBChar
         goback
       end-if

       if iaddr equal 82 then
         goback
       end-if

       goback.
       end program doPoke.

       identification division.
       program-id. doNext.
       environment division.
       data division.
       working-storage section.
       copy "globals.cob".
       01 done usage is binary-long signed.

       linkage section.
       01 varNum usage is binary-long signed.
      *01 rc usage is binary-long signed.

      *procedure division using varNum returning rc.
       procedure division using varNum.
      
       if varNum not equal fd-indexVarNum then
         call "fatal" using "next wrong variable"
       end-if
       add fd-step to vts-val(varNum)
       if fd-step less than zero then
         if vts-val(varNum) less than fd-limit then
           move 1 to done
         else
           move 0 to done
         end-if
       else
         if vts-val(varNum) greater than fd-limit then
           move 1 to done
         else
           move 0 to done
       end-if
       if done equal 0 then
         move inOperands to lexState
         move fd-tokenEnd to tokenEnd
         move fd-tokenEnd to lineNext
         move fd-lineStart to lineStart
      *  move 1 to rc
         move 1 to return-code
         goback
       end-if
      *move 0 to rc
       move 0 to return-code
       goback.

       end program doNext.

       identification division.
       program-id. clearWindows.
       environment division.
       data division.
       working-storage section.
       copy "globals.cob".

       procedure division.
      * XXX
      * erase ();
      * werase (tWin);
      * werase (gWin);
      * refresh ();
      * wrefresh (tWin);
      * wrefresh (gWin);
       move 0 to posCol
       move 0 to posRow
       goback.
       end program clearWindows.

       identification division.
       program-id. setGraphicsMode.
       environment division.
       data division.
       working-storage section.
       copy "globals.cob".

       linkage section.
       01 vmode usage is computational-2.

       procedure division using vmode.
                           display "in setGM"
       if gInit equal zero then
      * XXX
      * initscr ()
      * halfdelay (1)
      * echo ()
      * nonl ()
      * intrflush (stdscr, FALSE)
      * keypad (stdscr, TRUE)
      * gWin = newwin (10, 20, 0, 10);
      * tWin = newwin (4, 40, 10, 0);
      * wresize (stdscr, 24, 40);
         move 1 to gInit
       end-if

                           display "in setGM move " vmode
       move vmode to gMode
                           display "in setGM call"
       call "clearWindows"
                           display "in setGM goback"
       goback.
       end program setGraphicsMode.

       identification division.
       program-id. setPosition.
       environment division.
       data division.
       working-storage section.
       copy "globals.cob".

       linkage section.
       01 vcol usage is computational-2.
       01 vrow usage is computational-2.

       procedure division using vcol vrow.
      
       move vcol to posCol
       move vrow to posRow
      * XXX
      * move (posRow, posCol + leftMargin);
      * wmove (gWin, posRow, posCol + leftMargin);

       goback.
       end program setPosition.

       identification division.
       program-id. setChannel.
       environment division.
       data division.
       working-storage section.
       copy "globals.cob".

       linkage section.
       01 channel usage is computational-2.

       procedure division using channel.
       move channel to printChannel
       goback.
       end program setChannel.

       identification division.
       program-id. printExp.
       environment division.
       data division.
       working-storage section.
       copy "globals.cob".

       linkage section.
       01 exp usage is computational-2.

       procedure division using exp.
       display exp
       goback.
       end program printExp.

       identification division.
       program-id. printSExp.
       environment division.
       data division.
       working-storage section.
       copy "globals.cob".
       01 l usage binary-long signed.
       01 byteCast based.
           05 byteCasted usage binary-char unsigned.
       01 dispCast based.
           05 dispCasted pic x(256) usage is display.

       linkage section.
       01 exp usage is pointer.

       procedure division using exp.
       set address of byteCast to exp
       move byteCasted to l
       set address of dispCast to exp.
       display dispCasted(2:l)
       goback.
       end program printSExp.

       identification division.
       program-id. printNL.
       environment division.
       data division.
       working-storage section.
       copy "globals.cob".

       procedure division.
       display " "
       move zero to printChannel
       goback.

       end program printNL.

       identification division.
       program-id. evalOp.
       environment division.
       data division.
       working-storage section.
       copy "globals.cob".
       01 l usage binary-long signed.
       01 byteCast based.
           05 byteCasted usage binary-char unsigned.
       01 dispCast based.
           05 dispCasted pic x(256) usage is display.

       linkage section.
       01 exp1 usage is computational-2.
       01 op usage is binary-long signed.
       01 exp2 usage is computational-2.
       01 res usage is computational-2.

       procedure division using exp1 op exp2 by reference res.

       if op equal opPlus then
         compute res equal exp1 + exp2
         goback
       end-if

       if op equal opMinus1 then
         compute res equal exp1 - exp2
         goback
       end-if

       if op equal opNE then
          if exp1 not equal exp2 then
            move 1 to res
          else
            move 0 to res
          end-if
         goback
       end-if

       if op equal opEQ1 then
          if exp1 equal exp2 then
            move 1 to res
          else
            move 0 to res
          end-if
         goback
       end-if

       if op equal opMinus2 then
         compute res equal 0 - exp2
         goback
       end-if

       if op equal opTimes then
         compute res equal exp1 * exp2
         goback
       end-if

       if op equal opDiv then
         if exp2 equal 0 then
           call "fatal" using "div by 0"
         end-if
         compute res equal exp1 / exp2
         goback
       end-if

       if op equal opOr then
          if exp1 not equal 0 or exp2 not equal 0 then
            move 1 to res
          else
            move 0 to res
          end-if
         goback
       end-if

       call "fatal" using "evalOp failed".

       end program evalOp.

       identification division.
       program-id. stringEq.
       environment division.
       data division.
       working-storage section.
       copy "globals.cob".
       01 i usage binary-char signed.
       01 l1 usage binary-char signed.
       01 l2 usage binary-char signed.
       01 byteCast1 based.
           05 byteCasted1 usage binary-char unsigned.
       01 byteCast2 based.
           05 byteCasted2 usage binary-char unsigned.

       linkage section.
       01 str1 usage is pointer.
       01 str2 usage is pointer.
       01 res usage is computational-2.

       procedure division using str1 str2 by reference res.

       set address of byteCast1 to str1
       set address of byteCast2 to str2
       move byteCasted1 to l1
       move byteCasted2 to l2
       if l1 not equal l2 then
         move 0 to res
         goback
       end-if

       set str1 up by 1
       set str2 up by 1
       call "strncasecmp" using str1 str2 l1 returning i
       if i not equal 0 then
         move 0 to res
       else
         move 1 to res
       goback.

       end program stringEq.

       identification division.
       program-id. evalSFunc.
       environment division.
       data division.
       working-storage section.
       copy "globals.cob".
       01 byteCast based.
           05 byteCasted usage binary-char unsigned.

       linkage section.
       01 func usage is binary-long signed.
       01 arg usage is pointer.
       01 res usage is computational-2.

       procedure division using func arg by reference res.
       if func equal opLen then
         set address of byteCast to arg
         move byteCasted to res
         goback
       end-if

       call "fatal" using "evalSFunc failed".

       end program evalSFunc.

       identification division.
       program-id. evalFunc.
       environment division.
       data division.
       working-storage section.
       copy "globals.cob".
       01 byteCast based.
           05 byteCasted usage binary-char unsigned.
       01 addr usage is binary-long signed.
       01 i usage is binary-long signed.

       linkage section.
       01 func usage is binary-long signed.
       01 arg usage is computational-2.
       01 res usage is computational-2.

       procedure division using func arg by reference res.
       if func equal opPeek then
         move arg to addr
         if addr equal H"d01f" then
      * XXX
      *   updateLastKBChar ();
      *   if (lastKBChar != 255)
      *     return 6.0; // indicate that start is pressed XXX
      *   return 7.0; // no keys pressed
           move 6.0 to res
           goback
         end-if
         if addr equal 764 then
      * XXX
      *     updateLastKBChar ();
      *     return lastKBChar;
           move 32 to res
           goback
         end-if
         call "fatal" using "peek failed"
       end-if

       if func equal opInt then
         move arg to i
         move i to res
         goback
       end-if

       if func equal opRnd
         compute res equal function random ()
         goback
       end-if

       call "fatal" using "evalFunc failed".

       end program evalFunc.

       identification division.
       program-id. dimSVar.
       environment division.
       data division.
       working-storage section.
       copy "globals.cob".
       01 byteCast based.
           05 byteCasted usage binary-char unsigned.
       01 addr usage is binary-long signed.
       01 idim usage is binary-long signed.

       linkage section.
       01 varNum usage is binary-long signed.
       01 dim usage is computational-2.

       procedure division using varNum dim.
                           display "dimSVar"
                           display "dimSVar varNum " varNum
                           display "dimSVar dim " dim
       if varNum less than zero or greater than or equal to nv then
         call "fatal" using "varNum out of range"
       end-if

       if vts-type(varNum) not equal to vtTypeString then
         call "fatal" using "dimSVar expected a string variable"
       end-if

       move dim to idim
       if idim less than 1 or dim greater than 256 then
         call "fatal" using "dim out of range"
       end-if
                        display "idim " idim
       move idim to vts-dim(varNum)
       if vts-sval(varNum) not equal null then
         free vts-sval(varNum)
       add 1 to dim
       allocate (dim) characters returning vts-sval(varNum)
       goback.
       end program dimSVar.

       identification division.
       program-id. getVarValue.
       environment division.
       data division.
       working-storage section.
       copy "globals.cob".
       01 byteCast based.
           05 byteCasted usage binary-char unsigned.
       01 addr usage is binary-long signed.
       01 i usage is binary-long signed.

       linkage section.
       01 varNum usage is binary-long signed.
       01 res usage is computational-2.

       procedure division using varNum by reference res.

       if varNum less than zero or greater than or equal to nv then
         call "fatal" using "varNum out of range"
       end-if

       if vts-type(varNum) not equal to vtTypeScalar then
         call "fatal" using "getVarValue expected a scalar variable"
       end-if

       move vts-val(varNum) to res
       goback.

       end program getVarValue.

       identification division.
       program-id. getSVarValue.
       environment division.
       data division.
       working-storage section.
       copy "globals.cob".
       01 byteCast based.
           05 byteCasted usage binary-char unsigned.
       01 addr usage is binary-long signed.
       01 i usage is binary-long signed.

       linkage section.
       01 varNum usage is binary-long signed.
       01 res usage is pointer.

       procedure division using varNum by reference res.

       if varNum less than zero or greater than or equal to nv then
         call "fatal" using "varNum out of range"
       end-if

       if vts-type(varNum) not equal to vtTypeScalar then
         call "fatal" using "getSVarValue expected a string variable"
       end-if

      * XXX I wonder if OpenCOBOL will grol the indirection here
       move vts-sval(varNum) to res
       goback.

       end program getSVarValue.

       identification division.
       program-id. getSVarValue1.
       environment division.
       data division.
       working-storage section.
       copy "globals.cob".
       01 byteCast based.
           05 byteCasted usage binary-char unsigned.
       01 addr usage is binary-long signed.
       01 i usage is binary-long signed.

       linkage section.
       01 varNum usage is binary-long signed.
       01 sub usage is computational-2.
       01 res usage is pointer.

       procedure division using varNum sub by reference res.

       call "fatal" using "getSVarValue1 failed".

       end program getSVarValue1.

       identification division.
       program-id. getSVarValue2.
       environment division.
       data division.
       working-storage section.
       copy "globals.cob".
       01 byteCast based.
           05 byteCasted usage binary-char unsigned.
       01 addr usage is binary-long signed.
       01 isub1 usage is binary-long signed.
       01 isub2 usage is binary-long signed.
       01 len usage is binary-long signed.
       01 slen usage is binary-long signed.
       01 i usage is binary-long signed.
       01 j usage is binary-long signed.
       01 sval usage is pointer.
       01 svalCast based.
           05 svalCasted usage binary-char unsigned.

       linkage section.
       01 varNum usage is binary-long signed.
       01 sub1 usage is computational-2.
       01 sub2 usage is computational-2.
       01 res usage is pointer.

       procedure division using varNum sub1 sub2 by reference res.

       if varNum less than zero or greater than or equal to nv then
         call "fatal" using "varNum out of range"
       end-if

       if vts-type(varNum) not equal to vtTypeScalar then
         call "fatal" using "getSVarValue expected a string variable"
       end-if

       if vts-sval(varNum) equal to NULL then
         call "fatal" using "unallocated string"
       end-if

       move sub1 to isub1
       move sub2 to isub2

       move vts-sval(varNum) to sval
       move svalCasted to len

       if isub1 less than 1 or isub1 greater than len then
         call "fireTrap"
         call "fatal" using "string subscript 1 out of range"
       end-if

       if isub2 less than 1 or isub2 greater than len then
         call "fireTrap"
         call "fatal" using "string subscript 2 out of range"
       end-if

       if isub1 greater than isub2 then
         call "fireTrap"
         call "fatal" using "string subscript 2 > subscript1"
       end-if

       compute slen equal isub2 - isub1 + 1
       if slen > 256 then
         call "fireTrap"
         call "fatal" using "improbable... substring too long"
       end-if

       move vts-sval(varNum) to sval
       set sval up by isub1
       move slen to substrBufferChar(1)
       perform varying i from 1 by 1 until i equal slen
         set address of svalCast to sval
         compute j equal i + 1
         move svalCasted to substrBufferChar(j)
       end-perform
       set res to address of substrBuffer
       goback.
       end program getSVarValue2.

       identification division.
       program-id. parseNCONST.
       environment division.
       data division.
       working-storage section.
       copy "globals.cob".
       01 exp usage is binary-char unsigned.
       01 pom usage is binary-char unsigned.
       01 poml usage is binary-char unsigned.
       01 pomh usage is binary-char unsigned.
       01 num usage is binary-char unsigned.
       01 iexp usage is binary-long signed.
       01 i usage is binary-long signed.
       01 ptr usage is pointer.

       01 ptrCast based.
           05 ptrCasted usage is pointer.

       01 byteCast based.
           05 byteCasted usage binary-char unsigned.

       linkage section.
       01 linePtr usage is pointer.
       01 res usage is computational-2.

       procedure division using by reference linePtr, by reference res.

       move zero to res

       set address of byteCast to linePtr
       move byteCasted to exp
       set linePtr up by length of byteCast

       if exp equal zero then
           move zero to iexp
           set linePtr up by length of byteCast
           set linePtr up by length of byteCast
           set linePtr up by length of byteCast
           set linePtr up by length of byteCast
           set linePtr up by length of byteCast
       else
           compute iexp equal (function integer(exp) - 68) * 2
           perform varying i from 1 by 1 until i greater than 5
               
               set address of byteCast to linePtr
               move byteCasted to pom
               set linePtr up by length of byteCast

               compute pomh equal pom / 16
               compute poml equal function mod (pom, 16)

               compute num equal pomh * 10 + poml
               compute res equal res * 100
               add num to res

           end-perform
       end-if
       compute res equal res * function exp10 (iexp)
                 display "parseNCONST returns " res
       goback.
       end program parseNCONST.

       end program omgwtf2.

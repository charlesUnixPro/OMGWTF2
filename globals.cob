       01 nv is external usage binary-short unsigned.

      * struct variableTable
      *   {
      *     char * name;
      *     int type; // 0: scalar, 1: string
      *     int dim;
      *     double val; // scalar
      *     char * sval; // string
      *   };

      *01 variableTableStruct based.
       01 variableTableStruct is external.
           05 vts occurs 256 times.
               10 vts-name usage pointer.
               10 vts-type usage binary-long signed.
               10 vts-dim usage binary-long signed.
               10 vts-val usage computational-2.
               10 vts-sval usage pointer.

      *01 variableTable is external usage pointer.

       01 lineNumberTable is external value zero.
           05 lineNumberRows occurs H"10000" times.
               10 lineNumberRow usage pointer.

       01 lineStart is external usage pointer.
       01 lineNext is external usage pointer.
       01 tokenEnd is external usage pointer.

       01 trapJmp is external pic x(512).
       01 lexLinePtr is external usage pointer.

       01 lexState is external usage binary-long.

       01 returnStack is external.
           05 returnStackRows occurs 16 times.
               10 rs-tokenEnd usage pointer.
               10 rs-lineNext usage pointer.
               10 rs-lineStart usage pointer.

       01 rsp is external usage binary-long signed.

       01 trapLine is external usage binary-long signed.
       01 printChannel is external usage binary-long signed.
       01 lastKBChar is external usage binary-char unsigned.
       01 posCol is external usage binary-long signed.
       01 posRow is external usage binary-long signed.
       01 gInit is external usage binary-long signed.
       01 gMode is external usage binary-long signed.

       01 forData is external.
           05 fd-indexVarNum usage binary-long signed.
           05 fd-limit usage computational-2.
           05 fd-step usage computational-2.
           05 fd-tokenEnd usage is pointer.
           05 fd-lineNext usage is pointer.
           05 fd-lineStart usage is pointer.

       01 substrBuffer is external.
           05 substrBufferChars occurs 257 times.
               10 substrBufferChar usage is binary-char unsigned.

       01 programEnd is external usage pointer.

       01 yylval is external usage binary-double unsigned.
       01 currentLineNumber is external usage binary-long signed.


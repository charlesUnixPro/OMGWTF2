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

%{
#include <stdio.h>
#include "omgwtf2.h"
%}

%debug
%union {
  double val;
  int ival;
  char * sval;
}

/* statements */
%token INPUT    0x02
%token IF       0x07
%token FOR      0x08
%token NEXT     0x09
%token GOTO     0x0a
%token GOSUB    0x0c
%token TRAP     0x0d
%token DIM      0x14
%token END      0x15
%token POKE     0x1f
%token PRINT    0x20
%token RETURN   0x24
%token POP      0x27
%token QMARK    0x28
%token GRAPHICS 0x2b
%token POSITION 0x2d
%token SETCOLOR 0x30
%token SLET     0x36

%token EOT      0xf1
%token REM      0xf3

/* operands */
%token NCONST 0x10e
%token SCONST 0x10f
%token COMMA1 0x112
%token COLON  0x114
%token SEMI   0x115
%token EOL    0x116
%token TO     0x119
%token STEP   0x11a
%token THEN   0x11b
%token SHARP  0x11c
%token NE     0x11e
%token EQ1    0x122
%token TIMES  0x124
%token PLUS   0x125
%token MINUS1 0x126
%token DIV    0x127
%token OR     0x129
%token OPEN1  0x12b
%token CLOSE  0x12c
%token EQ2    0x12d
%token EQ3    0x12e
%token EQ4    0x134
%token MINUS2 0x136
%token OPEN2  0x137
%token OPEN3  0x13a
%token OPEN4  0x13b
%token COMMA2 0x13c
%token LEN    0x142
%token PEEK   0x146
%token RND    0x148
%token INT    0x150

%token VAR    0x180
%token SVAR   0x181

%type <val> exp NCONST var
%type <ival> func OR rel PLUS MINUS1 TIMES DIV MINUS2 PEEK LEN INT RND VAR SVAR
%type <ival> EQ1 EQ4 NE sfunc
%type <sval> svar sexp SCONST

/* WTF: seven shift/reduce conflicts. */

%left OR
%left EQ1 EQ4 NE
%left PLUS MINUS1
%left TIMES DIV
%left MINUS2

%start program

%%

program :
	  stmt EOT
		{ YYACCEPT; }
	| EOT
		{ YYACCEPT; }
	;

dims :
	  dim
	| dims COMMA1 dim
	;

dim :
	  VAR OPEN4 NCONST CLOSE
	| SVAR OPEN4 NCONST CLOSE
		{ dimSVar ($1, $3); }
	;

func :
	  PEEK
		{ $$ = PEEK; }
	| INT
		{ $$ = INT; }
	| RND
		{ $$ = RND; }
	;

sfunc :
	  LEN
		{ $$ = LEN; }
	;

var :
	  VAR
		{ $$ = getVarValue ($1); }
	;

svar :
	  SVAR
		{ $$ = getSVarValue ($1); }
	| SVAR OPEN2 exp CLOSE
		{ $$ = getSVarValue1 ($1, $3); }
	| SVAR OPEN2 exp COMMA2 exp CLOSE
		{ $$ = getSVarValue2 ($1, $3, $5); }
	;

exp :
	  NCONST
		{ $$ = $1; }
	| var
		{ $$ = $1; }
	| func OPEN3 exp CLOSE
		{ $$ = evalFunc ($1 & 0x7f, $3); }
	| sfunc OPEN3 sexp CLOSE
		{ $$ = evalSFunc ($1 & 0x7f, $3); }
	| exp OR exp
		{ $$ = evalOp ($1, OR & 0x7f, $3); }
	| exp rel exp
		{ $$ = evalOp ($1, $2 & 0x7f, $3); }
	| svar EQ4 SCONST
		{ $$ = stringEq ($1, $3); } 
	| exp PLUS exp
		{ $$ = evalOp ($1, PLUS & 0x7f, $3); }
	| exp MINUS1 exp
		{ $$ = evalOp ($1, MINUS1 & 0x7f, $3); }
	| exp TIMES exp
		{ $$ = evalOp ($1, TIMES & 0x7f, $3); }
	| exp DIV exp
		{ $$ = evalOp ($1, DIV & 0x7f, $3); }
	| MINUS2 exp
		{ $$ = evalOp (0.0, MINUS2 & 0x7f, $2); }
	| OPEN1 exp CLOSE
		{ $$ = $2; }
	;

sexp :
	  svar
		{ $$ = $1; }
	| SCONST
		{ $$ = $1; }
	;

rel :
	  EQ1
		{ $$ = EQ1; }
	| NE
		{ $$ = NE; }
	;

eol :
	  EOL
	| COLON
	;

plist :
	  exp
		{ printExp ($1); }
	| sexp
		{ printSExp ($1); }
	| plist SEMI exp
		{ printExp ($3); }
	| plist SEMI sexp
		{ printSExp ($3); }
	;

channel :
	  SHARP NCONST SEMI
		{ setChannel ($2); }
	;

/* WTF: Atbas.y only defines enough of Atari BASIC to run the */
/* legacy application. The missing bits are simply elided, usually without */
/* comment, but sometimes a yyerror(). */

stmt :
	  REM
	| GRAPHICS exp eol
		 { setGraphicsMode ($2); }
	| SETCOLOR exp COMMA1 exp COMMA1 exp eol
	| DIM dims eol
        | POSITION NCONST COMMA1 NCONST eol
		{ setPosition ($2, $4); }
	| QMARK eol
		{ printNL (); }
	| QMARK plist eol
		{ printNL (); }
	| QMARK channel plist eol
		{ printNL (); }
	| POKE NCONST COMMA1 NCONST eol
		{ doPoke ($2, $4); }
	| FOR VAR EQ2 exp TO exp eol
		{ doFor ($2, $4, $6, 1.0); }
	| FOR VAR EQ2 exp TO exp STEP exp eol
		{ doFor ($2, $4, $6, $8); }
	| NEXT VAR eol
		{ if (doNext ($2)) YYACCEPT; }
	| PRINT eol
		{ printNL (); }
	| PRINT plist eol
		{ printNL (); }
	| SLET VAR EQ2 exp eol
		{ doLet ($2, $4); }
	| SLET SVAR EQ3 sexp eol
		{ doSLet ($2, $4); }
	| IF exp THEN NCONST eol
		{ if (doIfLine ($2, $4)) YYACCEPT; }
	| IF exp THEN 
		{ if (doIfThen ($2)) YYACCEPT; }
	| IF sexp THEN NCONST eol
		{ yyerror ("IF?"); }
	| IF sexp THEN 
		{ yyerror ("IF?"); }
	| END eol
		{ doEnd (); }
	| INPUT SVAR eol
		{ doInput ($2); }
	| GOSUB exp eol
		{ doGosub ($2); YYACCEPT; }
	| GOTO exp eol
		{ doGoto ($2); YYACCEPT; }
	| TRAP exp eol
		{ doTrap ($2); }
	| RETURN eol
		{ doReturn (); YYACCEPT; }
	| POP eol
		{ doPop (); }
	;


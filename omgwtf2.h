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

int yylex (void);
void yyerror (const char * msg);
void setGraphicsMode (int mode);
double getVarValue (int varNum);
char * getSVarValue (int varNum);
char * getSVarValue1 (int varNum, double sub);
char * getSVarValue2 (int varNum, double sub1, double sub2);
double evalOp (double exp1, int op, double exp2);
double evalSOp (int exp1, int op, char * exp2);
double evalFunc (int func, double arg);
double evalSFunc (int func, char * arg);
double stringEq (char * str1, char * str2);
void setPosition (double col, double row);
void printExp (double exp);
void printSExp (char * exp);
void printNL (void);
void dimSVar (int varNum, double dim);
void doPoke (double addr, double val);
void doFor (int varNum, double initial, double final, double step);
int doNext (int varNum);
void doLet (int varNum, double val);
void doSLet (int varNum, char * val);
int doIfLine (double exp1, double exp2);
int doIfThen (double exp);
void doInput (int varNum);
void doGosub (double exp);
void doTrap (double exp);
void doReturn (void);
void doPop (void);
void doGoto (double exp);
void doEnd (void);
void setChannel (double channel);

/* 
**********************************************
 *  341 Programming Languages                *
 *  Fall 2019                                *
 *  Author: Gonca Ezgi Çakır                 *
 *		     151044054                       *
 *           HW2 PART1                       *
 ********************************************* 
 */

/*
COMPILATION 

*******LEXER FOR FILE IMPLEMENTED CODE******
		step1-> flex gpp_lexer.l
		step2-> gcc lex.yy.c -lfl -o gppLexer
		step3->cat file.g++ | ./gppLexer 

********LEXER FOR TERMINAL CODE*********
		step1-> flex gpp_lexer.l
		step2-> gcc lex.yy.c -lfl -o gppLexer
		step3-> ./gppLexer 
*/

/*library implementation*/ 
%{

#include "stdio.h"

%}


/*rule definitions of G++ lexer*/
%%

"and"		printf("KW_AND\n");		/*and keyword*/
"or"		printf("KW_OR\n"); 		/*or keyword*/
"not"		printf("KW_NOT\n"); 	/*nto keyword*/
"equal"		printf("KW_EQUAL\n"); 	/*equal keyword*/
"less"		printf("KW_LESS\n"); 	/*less keyword*/
"nil"		printf("KW_NIL\n"); 	/*nil keyword*/
"list"		printf("KW_LIST\n"); 	/*list keyword*/
"append"	printf("KW_APPEND\n"); 	/*append keyword*/
"concat"	printf("KW_CONCAT\n"); 	/*concat keyword*/
"set"		printf("KW_SET\n"); 	/*set keyword*/
"deffun"	printf("KW_DEFFUN\n"); 	/*deffun keyword*/
"for"		printf("KW_FOR\n");		/*for keyword*/
"if"		printf("KW_IF\n"); 		/*if keyword*/
"exit"		printf("KW_EXIT\n"); 	/*exit keyword*/
"load"		printf("KW_LOAD\n"); 	/*load keyword*/
"disp"		printf("KW_DISP\n"); 	/*disp keyword*/
"true"		printf("KW_TRUE\n"); 	/*true keyword*/
"false"		printf("KW_FALSE\n");	/*false keyword*/
"+"			printf("OP_PLUS\n"); 	/*plus keyword*/
"-"			printf("OP_MINUS\n");	/*minus keyword*/
"/"			printf("OP_DIV\n"); 	/*division keyword*/
"*"			printf("OP_MULT\n"); 	/*multiplier keyword*/
"**"		printf("OP_DBLMULT\n"); /*double multiplier keyword*/
","			printf("OP_COMMA\n"); 	/*comma keyword*/
"‘"			printf("OP_OC\n"); 		/*open quote keyword*/
"’"			printf("OP_CC\n"); 		/*close quote  keyword*/
"("			printf("OP_OP\n"); 		/*open paranthesis keyword*/
")"			printf("OP_CP\n"); 		/*close paranthesis keyword*/


[;;]+([. + - / *]|[A-Za-z]|[0-9]|[ \t])* 	printf("COMMENT\n");		/*regular expression for comment token*/  /*after ;; symbol anything else that come count as a comment*/

[0]					printf("VALUE\n");								/*regular expression for 0 digit */  

[1-9][0-9]* 			printf("VALUE\n");							/*regular expression for invalid integer value */  

[0-9][0-9]* 			printf("INVALID VALUE (zero leading)\n");					/*regular expression for integer value */  /*integer value no leading zeros*/

[a-zA-Z][_a-zA-Z0-9]* 	printf("IDENTIFIER\n");						/*regular expression for identifier*/  /*digit or alphabetic character could fallow each other*/

[ \t\n]+				;											/*regular exprssion for ignored characters*/

.						printf("unexpected input\n");

%%




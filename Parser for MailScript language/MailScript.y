%{
#include <stdio.h>
#include <stdlib.h>

extern int yylex();
void yyerror(const char *s);
%}

%token tMAIL tENDMAIL tSCHEDULE tENDSCHEDULE tSEND tSET tTO tFROM tAT tCOMMA tCOLON tLPR tRPR tLBR tRBR tIDENT tSTRING tADDRESS tDATE tTIME

%start program

%%

program
    :
    | program component
    ;

component
    : mail_block
    | set_statement
    ;

mail_block
    : tMAIL tFROM tADDRESS tCOLON statement_list tENDMAIL
    ;

statement_list
    :
    | statement_list statement
    ;

statement
    : set_statement
    | send_statement
    | schedule_block
    ;

set_statement
    : tSET tIDENT tLPR tSTRING tRPR
    ;

send_statement
    : tSEND tLBR content tRBR tTO recipient_list
    ;

content
    : tSTRING
    | tIDENT
    ;

recipient_list
    : tLBR recipients tRBR
    ;

recipients
    : recipient
    | recipients tCOMMA recipient
    ;

recipient
    : tLPR tADDRESS tRPR
    | tLPR content tCOMMA tADDRESS tRPR
    ;



schedule_block
    : tSCHEDULE tAT tLBR tDATE tCOMMA tTIME tRBR tCOLON statement_list tENDSCHEDULE
    ;

%%

void yyerror(const char *s) {}

int main ()
{
    if (yyparse())
    {
        printf("ERROR\n");
        return 1;
    }
    else
    {
        printf("OK\n");
        return 0;
    }
}

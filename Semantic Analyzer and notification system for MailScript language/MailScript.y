%{
#ifdef YYDEBUG
  yydebug = 1;
#endif
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "MailScript.tab.h"

int yylex(void);
extern char* yytext;
extern int yylineno;
int insideMailBlock = 0;
int errorFlag = 0;
int currentDepth = 0;

typedef struct SymbolTableEntry {
    char *name;
    char *value;
    int depth;
    struct SymbolTableEntry *next;
} SymbolTableEntry;

typedef struct Recipient {
    char *name;
    char *email;
    struct Recipient *next;
} Recipient;

typedef struct {
    char *email;
} Sender;

Recipient *recipientsHead = NULL;
Sender currentSender;
SymbolTableEntry *symbolTable = NULL;


void enterScope();
void exitScope();
void addSymbol(char *name, char *value);
char *getVariableValue(const char *name);
int symbolExists(const char *name);
void checkVariableAndAddError(const char* name, int line);
void yyerror (const char *msg);
int isLeapYear(int year);
int isValidDate(const char *date);
int isValidTime(const char *time);
void addRecipient(char *name, char *email);
void clearRecipientsList();
void printSendNotification(const char *from, const char *message, Recipient *recipient);

%}

%union {
    char* sval;
}

%token <sval> tIDENT tDATE tTIME tSTRING tADDRESS
%type <sval> messageOption
%token tMAIL tENDMAIL tSCHEDULE tENDSCHEDULE tSEND tTO tFROM tSET tCOMMA tCOLON tLPR tRPR tLBR tRBR tAT tNEWLINE
%start program

%%

program : statements
;

statements : 
            | statement statements
;

statement : setStatement
           | mailBlock
;

mailStatementList : 
                | mailStatement mailStatementList
;

mailStatement : setStatement
               | sendStatement
               | scheduleStatement
;

sendStatements : sendStatement
                | sendStatement sendStatements 
;

sendStatement : tSEND tLBR messageOption tRBR tTO tLBR recipientList tRBR {
    if (errorFlag == 0) {
        char* message = $3;
        if (errorFlag == 0) {
            Recipient *current = recipientsHead;
            while (current != NULL) {
                printSendNotification(currentSender.email, message, current);
                current = current->next;
            }
            clearRecipientsList();
        }
    }
};


messageOption : tSTRING { $$ = strdup($1); }
              | tIDENT  { 
                char *value = getVariableValue($1);
                if (value) {
                    $$ = strdup(value);
                } else {
                    checkVariableAndAddError($1, yylineno);
                    $$ = strdup($1);
                }
                };


recipientList : recipient
               | recipient tCOMMA recipientList
;

recipient : tLPR recipientDetail tRPR
;

recipientDetail : tADDRESS {
                    addRecipient(NULL, $1);
                 }
                 | tSTRING tCOMMA tADDRESS {
                    addRecipient($1, $3);
                 }
                 | tIDENT tCOMMA tADDRESS {
                    checkVariableAndAddError($1, yylineno);
                    addRecipient($1, $3);
                 };


scheduleStatement : tSCHEDULE tAT tLBR tDATE tCOMMA tTIME tRBR tCOLON {
    if (!isValidDate($4)) {
        fprintf(stderr, "ERROR at line %d: date object is not correct (%s)\n", yylineno, $4);
        errorFlag = 1;
    }
    if (!isValidTime($6)) {
        fprintf(stderr, "ERROR at line %d: time object is not correct (%s)\n", yylineno, $6);
        errorFlag = 1;
    }
    enterScope();
} sendStatements tENDSCHEDULE { exitScope(); }
;


mailBlock : tMAIL tFROM tADDRESS tCOLON {
    enterScope(); 
    insideMailBlock = 1;  
    currentSender.email = strdup($3);
} mailStatementList tENDMAIL { 
    exitScope(); 
    insideMailBlock = 0; 
    free(currentSender.email);
}
;

setStatement : tSET tIDENT tLPR tSTRING tRPR {
    if (!insideMailBlock || !symbolExists($2)) {
        addSymbol($2, $4);
    }
}

%%


void enterScope() { currentDepth++; }

void exitScope() {
    SymbolTableEntry *current = symbolTable;
    while (current != NULL && current->depth >= currentDepth) {
        SymbolTableEntry *toDelete = current;
        symbolTable = symbolTable->next;
        current = current->next;
        free(toDelete->name);
        free(toDelete->value);
        free(toDelete);
    }
    symbolTable = current;
    currentDepth--;
}

void addSymbol(char *name, char *value) {
    SymbolTableEntry *current = symbolTable;

    while (current != NULL && current->depth == currentDepth) {
        if (strcmp(current->name, name) == 0) {
            free(current->value);
            current->value = strdup(value);
            return;
        }
        current = current->next;
    }

    SymbolTableEntry *newEntry = (SymbolTableEntry *)malloc(sizeof(SymbolTableEntry));
    if (!newEntry) {
        fprintf(stderr, "Memory allocation failed for symbol table entry\n");
        exit(1);
    }
    newEntry->name = strdup(name);
    newEntry->value = strdup(value);
    newEntry->depth = currentDepth;
    newEntry->next = symbolTable;
    symbolTable = newEntry;
}


char *getVariableValue(const char *name) {
    SymbolTableEntry *current = symbolTable;

    while (current != NULL) {
        if (strcmp(current->name, name) == 0) {
            return current->value;
        }
        current = current->next;
    }

    return NULL;
}




int symbolExists(const char *name) {
    SymbolTableEntry *current = symbolTable;
    while (current != NULL && current->depth == currentDepth) {
        if (strcmp(current->name, name) == 0) {
            return 1;
        }
        current = current->next;
    }
    return 0;
}

void checkVariableAndAddError(const char* name, int line) {
    SymbolTableEntry *current = symbolTable;
    int found = 0;
    while (current != NULL) {
        if (strcmp(current->name, name) == 0 && (current->depth <= currentDepth || !insideMailBlock)) {
            found = 1;
            break;
        }
        current = current->next;
    }
    if (!found) {
        fprintf(stderr, "ERROR at line %d: %s is undefined\n", line, name);
        errorFlag = 1;
    }
}

int isLeapYear(int year) {  return (year % 4 == 0) && (year % 100 != 0 || year % 400 == 0); }

int isValidDate(const char *date) {
    int day, month, year;
    if (sscanf(date, "%d/%d/%d", &day, &month, &year) != 3) return 0;

    if (month < 1 || month > 12) return 0;

    int daysInMonth[] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};

    if (month == 2 && isLeapYear(year)) daysInMonth[1] = 29;

    if (day < 1 || day > daysInMonth[month - 1]) return 0;

    return 1;
}


int isValidTime(const char *time) {
    int hour, minute;
    if (sscanf(time, "%d:%d", &hour, &minute) != 2) return 0;
    if (hour < 0 || hour > 23 || minute < 0 || minute > 59) return 0;
    return 1;
}

void clearRecipientsList() {
    while (recipientsHead != NULL) {
        Recipient *toDelete = recipientsHead;
        recipientsHead = recipientsHead->next;

        free(toDelete->name);
        free(toDelete->email);
        free(toDelete);
    }
}

void addRecipient(char *name, char *email) {
    for (Recipient *current = recipientsHead; current != NULL; current = current->next) {
        if (strcmp(current->email, email) == 0) {
            return;
        }
    }

    Recipient *newRecipient = (Recipient *)malloc(sizeof(Recipient));
    if (newRecipient == NULL) {
        fprintf(stderr, "Failed to allocate memory for recipient\n");
        return;
    }
    newRecipient->name = name ? strdup(name) : NULL;
    newRecipient->email = strdup(email);
    newRecipient->next = NULL;

    if (recipientsHead == NULL) {
        recipientsHead = newRecipient;
    } else {
        Recipient *current = recipientsHead;
        while (current->next != NULL) {
            current = current->next;
        }
        current->next = newRecipient;
    }
}

void printSendNotification(const char *from, const char *message, Recipient *recipient) {
    const char *recipientDetail = recipient->name ? recipient->name : recipient->email;
    printf("E-mail sent from %s to %s: \"%s\"\n", from, recipientDetail, message);
}



void yyerror (const char *msg) {
    fprintf(stderr, "ERROR\n");
    exit(1);
}


int main() {
    errorFlag = 0;
    
    if (yyparse()) {
        return 1;
    } else {
        return 0;
    }

}
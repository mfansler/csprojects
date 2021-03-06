/*
  Filename   : CMinParser.cc
  Author     : Merv Fansler
  Course     : CSCI 435
  Assignment : Assignment 5, C- Parser_1
  Description: Implements predictive recursive descent parsing of C-
                 tokens according to grammar specified in Louden;
*/

/******************************************************************************/
// System Includes

#include <iostream>
#include <map>
#include <string>
#include <functional>

/******************************************************************************/
// Local Includes

#include "CMinTokens.h"

/******************************************************************************/
// External references

extern "C"
int
yylex ();

extern FILE* yyin;

extern int lineCount;
extern int colCount;

/******************************************************************************/
// Function declarations

int
getToken ();

void
error (std::string msg);

std::function<void(int)>
createMatch (std::string callee);

// Non-terminals

void
program ();

void
declaration ();

void
functionOrVariableDeclaration ();

void
functionDeclaration ();

void
parameterList ();

void
parameter ();

void
compoundStatement ();

void
localVariableList ();

void
statement ();

void
ifStatement ();

void
whileStatement ();

void
returnStatement ();

void
expressionStatement ();

void
expression ();

void
functionCallOrVariable ();

void
assignmentOrMathExpression ();

void
mathematicalExpression ();

void
argumentList ();

void
additiveExpression ();

void
term ();

void
factor ();

void
factorFunctionCallOrVariable ();

/******************************************************************************/
// Global vars

int g_token;

std::map<int, std::string> tokenNames = 
  {
    { INCREMENT, "INCREMENT" },
    { DECREMENT, "DECREMENT" },
    { DIVIDE, "DIVIDE" },
    { RETURN, "RETURN" },
    { ASSIGN, "ASSIGN" },
    { LPAREN, "LPAREN" },
    { RPAREN, "RPAREN" },
    { LBRACK, "LBRACK" },
    { RBRACK, "RBRACK" },
    { LBRACE, "LBRACE" },
    { RBRACE, "RBRACE" },
    { ERROR, "ERROR" },
    { MINUS, "MINUS" },
    { TIMES, "TIMES" },
    { WHILE, "WHILE" },
    { COMMA, "COMMA" },
    { ELSE, "ELSE" },
    { VOID, "VOID" },
    { PLUS, "PLUS" },
    { SEMI, "SEMI" },
    { INT, "INT" },
    { FOR, "FOR" },
    { LTE, "LTE" },
    { GTE, "GTE" },
    { NEQ, "NEQ" },
    { NUM, "NUM" },
    { IF, "IF" },
    { LT, "LT" },
    { GT, "GT" },
    { EQ, "EQ" },
    { ID, "ID" }
  };

/******************************************************************************/

int
main (int argc, char* argv[])
{
  ++argv, --argc;
  if (argc > 0)
  {
    yyin = fopen (argv[0], "r");
  }
  else
  {
    yyin = stdin;
  }
  
  g_token = getToken ();
  program ();
  if (g_token == 0)
    std::cout << "Valid C- program.\n";
  else
    error ("main");
  
  return EXIT_SUCCESS;
}

/******************************************************************************/

int
getToken ()
{
  return yylex ();
}

/******************************************************************************/

void
error (std::string msg)
{
  std::cout << "Error in " + msg << std::endl
	    << "Line " << lineCount << ", Column " << colCount << std::endl;
  exit (1);
}

/******************************************************************************/

std::function<void(int)>
createMatch (std::string callee)
{
  return [callee] (int expectedToken)
  {
    if (g_token == expectedToken)
      g_token = getToken ();
    else
    {
      std::cout << "Expected " << tokenNames[expectedToken]
		<< ", found " << tokenNames[g_token] << std::endl;
      error (callee);
    }
  };
}

/******************************************************************************/
// program --> { declaration }

void
program ()
{
  // assuming entire file/string must be parsed to be considered valid
  while (g_token != 0)
    declaration ();
}

/******************************************************************************/
// declaration --> VOID ID functionDeclaration |
//                 INT ID functionOrVariableDeclaration

void
declaration ()
{
  auto match = createMatch ("declaration");
  
  if (g_token == VOID)
  { // must be function declaration
    match (VOID);
    match (ID);
    functionDeclaration ();
  }
  else if (g_token == INT)
  { // could still be function or variable
    match (INT);
    match (ID);
    functionOrVariableDeclaration ();
  }
  else
    error ("declaration: invalid declaration");
}

/******************************************************************************/
// functionOrVariableDeclaration --> functionDeclaration |
//                                   [ LBRACK NUM RBRACK ] SEMI

void
functionOrVariableDeclaration ()
{
  auto match = createMatch ("functionOrVariable");
 
  if (g_token == LPAREN)
  { // must be function declaration
    functionDeclaration ();
  }
  else if (g_token == LBRACK)
  { // array declaration
    match (LBRACK);
    match (NUM);
    match (RBRACK);
    match (SEMI);
  }
  else if (g_token == SEMI)
  { // basic var declaration
    match (SEMI);
  }
  else
    error ("functionOrVariableDeclaration: invalid declaration");
}

/******************************************************************************/
// functionDeclaration --> LPAREN parameterList RPAREN compoundStatement

void
functionDeclaration ()
{
  auto match = createMatch ("functionDeclaration");
 
  match (LPAREN);
  parameterList ();
  match (RPAREN);
  compoundStatement ();
}

/******************************************************************************/
// parameterList --> VOID | parameter { COMMA parameter }

void
parameterList ()
{
  auto match = createMatch ("parameterList");
 
  if (g_token == VOID) // no parameters
    match (VOID);
  else if (g_token == INT)
  { // at least one parameter
    parameter ();
    while (g_token == COMMA)
    { // more parametes
      match (COMMA);
      parameter ();
    }
  }
  else
    error ("parameterList: invalid parameter syntax");
}

/******************************************************************************/
// parameter --> INT ID [ LBRACK RBRACK ]

void
parameter ()
{
  auto match = createMatch ("parameter");

  match (INT);
  match (ID);
  if (g_token == LBRACK)
  { // array
    match (LBRACK);
    match (RBRACK);
  }
}

/******************************************************************************/
// compoundStatement --> LBRACE localVariableList { statement } RBRACE

void
compoundStatement ()
{
  auto match = createMatch ("compoundStatement");
 
  match (LBRACE);
  localVariableList ();

  // AD HOC: Rather than positively test for FIRST(statement), it is much more
  //           simple to assume 'statement' if we don't find the termiating
  //           RBRACE. This has the consequence of pushing compoundStatement
  //           syntax errors to be 'statement' syntax errors.
  while (g_token != RBRACE)
    statement ();
  match (RBRACE);
}

/******************************************************************************/
// localVariableList --> { INT ID [ LBRACK NUM RBRACK ] SEMI }

void
localVariableList ()
{
  auto match = createMatch ("localVariableList");
 
  while (g_token == INT)
  {
    match (INT);
    match (ID);
    if (g_token == LBRACK)
    {
      match (LBRACK);
      match (NUM);
      match (RBRACK);
    }
    match (SEMI);
  }
}

/******************************************************************************/
// statement --> compoundStatement | ifStatement | whileStatement |
//               returnStatement | expressionStatement

void
statement ()
{
  if (g_token == LBRACE)
    compoundStatement ();
  else if (g_token == IF)
    ifStatement ();
  else if (g_token == WHILE)
    whileStatement ();
  else if (g_token == RETURN)
    returnStatement ();
  else
    expressionStatement ();
}

/******************************************************************************/
// ifStatement --> IF LPAREN expression RPAREN statement [ ELSE statement ]

void
ifStatement ()
{
  auto match = createMatch ("ifStatement");
 
  match (IF);
  match (LPAREN);
  expression ();
  match (RPAREN);
  statement ();
  if (g_token == ELSE)
  { // optional else
    match (ELSE);
    statement ();
  }
}

/******************************************************************************/
// whileStatement --> WHILE LPAREN expression RPAREN statement

void
whileStatement ()
{
  auto match = createMatch ("whileStatement");
 
  match (WHILE);
  match (LPAREN);
  expression ();
  match (RPAREN);
  statement ();
}

/******************************************************************************/
// returnStatement --> RETURN expressionStatement

void
returnStatement ()
{
  auto match = createMatch ("returnStatement");
 
  match (RETURN);
  expressionStatement ();
}

/******************************************************************************/
// expressionStatement --> [ expression ] SEMI

void
expressionStatement ()
{
  auto match = createMatch ("expressionStatement");

  // AD HOC: Rather than positively test for FIRST(expression), it is much
  //           more simple to assume expression if we don't find the termiating
  //           SEMI. This has the consequence of pushing expressionStatement
  //           syntax errors to be expression syntax errors.
  if (g_token != SEMI)
    expression ();
  match (SEMI);
}

/******************************************************************************/
// expression --> NUM mathematicalExpression |
//                LPAREN expression RPAREN mathematicalExpression |
//                ID functionCallOrVariable

void
expression ()
{
  auto match = createMatch ("expression");
 
  if (g_token == NUM)
  { // numerical expression
    match (NUM);
    mathematicalExpression ();
  }
  else if (g_token == LPAREN)
  { // associative expression
    match (LPAREN);
    expression ();
    match (RPAREN);
    mathematicalExpression ();
  }
  else if (g_token == ID)
  { // either function call or variable reference
    match (ID);
    functionCallOrVariable ();
  }
  else
    error ("expression, invalid expression");
}

/******************************************************************************/
// functionCallOrVariable -->
//   LPAREN argumentList RPAREN mathematicalExpression |
//   [ LBRACK expression RBRACK ] assignmentOrMathExpression

void
functionCallOrVariable ()
{
  auto match = createMatch ("functionCallOrVariable");
 
  if (g_token == LPAREN)
  { // function call
    match (LPAREN);
    argumentList ();
    match (RPAREN);
    mathematicalExpression ();
  }
  else
  { // assignment or mathematical expression
    if (g_token == LBRACK)
    { // array
      match (LBRACK);
      expression ();
      match (RBRACK);
    }
    assignmentOrMathExpression ();
  }
}

/******************************************************************************/
// assignmentOrMathExpression --> ASSIGN expression | mathematicalExpression

void
assignmentOrMathExpression ()
{
  auto match = createMatch ("assignmentOrMathExpression");
 
  if (g_token == ASSIGN)
  { // must be assignment 
    match (ASSIGN);
    expression ();
  }
  else // must be math (or epsilon)
    mathematicalExpression ();
}

/******************************************************************************/
// mathematicalExpression -->
//   [ ( TIMES | DIVIDE ) term ]
//   [ ( PLUS | MINUS ) additiveExpression ]
//   { ( LT | LTE | GT | GTE | EQ | NEQ ) additiveExpression }

void
mathematicalExpression ()
{
  auto match = createMatch ("mathematicalExpression");
 
  if (g_token == TIMES || g_token == DIVIDE)
  { // found multiplicative expression
    match (g_token);
    term ();
  }
  if (g_token == PLUS || g_token == MINUS)
  { // found additive expression
    match (g_token);
    additiveExpression ();
  }
  while (g_token == LT || g_token == LTE || g_token == EQ ||
	 g_token == GT || g_token == GTE || g_token == NEQ)
  { // found relational expression
    match (g_token);
    additiveExpression ();
  }
}

/******************************************************************************/
// argumentList --> [ expression { COMMA expression } ]

void
argumentList ()
{
  auto match = createMatch ("argumentList");
 
  if (g_token == NUM || g_token == LPAREN || g_token == ID)
  { // found an argument
    expression ();
    while (g_token == COMMA)
    { // and another
      match (COMMA);
      expression ();
    }
  }
  else if (g_token == RPAREN)
  {
    // epsilon
  }
  else
    error ("argumentList, argument syntax invalid");
}

/******************************************************************************/
// additiveExpression --> term { ( PLUS | MINUS ) additiveExpression }

void
additiveExpression ()
{
  auto match = createMatch ("additiveExpression");
 
  term ();
  while (g_token == PLUS || g_token == MINUS)
  { // more expressions
    match (g_token);
    additiveExpression ();
  }
}

/******************************************************************************/
// term --> factor { ( TIMES | DIVIDE ) term }

void
term ()
{
  auto match = createMatch ("term");
  
  factor ();
  while (g_token == TIMES || g_token == DIVIDE)
  { // more terms
    match (g_token);
    term ();
  }
}

/******************************************************************************/
// factor -> LPAREN expression RPAREN | NUM | ID factorFunctionCallOrVariable

void
factor ()
{
  auto match = createMatch ("factor");
 
  if (g_token == LPAREN)
  { // associative expression
    match (LPAREN);
    expression ();
    match (RPAREN);
  }
  else if (g_token == NUM) // number
    match (NUM);
  else if (g_token == ID)
  { // function or variable
    match (ID);
    factorFunctionCallOrVariable ();
  }
  else
    error ("factor, expected factor");
}

/******************************************************************************/
// factorFunctionCallOrVariable --> LPAREN args RPAREN |
//                                  [ LBRACK expression RBRACK ]

void
factorFunctionCallOrVariable ()
{
  auto match = createMatch ("factorFunctionCallOrVariable");
 
  if (g_token == LPAREN)
  { // function call
    match (LPAREN);
    argumentList ();
    match (RPAREN);
  }
  else if (g_token == LBRACK)
  { // array
    match (LBRACK);
    expression ();
    match (RBRACK);
  }
  // else => epsilon
}

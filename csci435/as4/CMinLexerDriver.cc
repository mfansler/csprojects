/*
  Filename   : CMinLexerDriver.cc
  Author     : Merv Fansler
  Course     : CSCI 435
  Assignment : Assignment 4, Lex Luthor vs C-
  Description: Driver for C- lexer that accepts a C- source input and outputs
                 a list of the resulting tokens
*/

/******************************************************************************/
// System Includes

#include <iostream>
#include <iomanip>
#include <map>
#include <string>

/******************************************************************************/
// Local Includes

#include "CMinTokens.h"

/******************************************************************************/
// Namespace declarations

using std::cout;
using std::endl;
using std::setw;

/******************************************************************************/
// External references

extern "C"
int
yylex ();

extern FILE* yyin;

extern char* yytext;

extern int lineCount;
extern int colCount;

/******************************************************************************/
// Global vars

std::map<int, std::string> tokenNames = 
  {
    {
      ERROR, "ERROR"
    },
    {
      IF, "IF"
    },
    {
      ELSE, "ELSE"
    },
    {
      INT, "INT"
    },
    {
      VOID, "VOID"
    },
    {
      RETURN, "RETURN"
    },
    {
      WHILE, "WHILE"
    },
    {
      FOR, "FOR"
    },
    {
      PLUS, "PLUS"
    },
    {
      MINUS, "MINUS"
    },
    {
      TIMES, "TIMES"
    },
    {
      DIVIDE, "DIVIDE"
    },
    {
      LT, "LT"
    },
    {
      LTE, "LTE"
    },
    {
      GT, "GT"
    },
    {
      GTE, "GTE"
    },
    {
      EQ, "EQ"
    },
    {
      NEQ, "NEQ"
    },
    {
      INCREMENT, "INCREMENT"
    },
    {
      DECREMENT, "DECREMENT"
    },
    {
      ASSIGN, "ASSIGN"
    },
    {
      SEMI, "SEMI"
    },
    {
      COMMA, "COMMA"
    },
    {
      LPAREN, "LPAREN"
    },
    {
      RPAREN, "RPAREN"
    },
    {
      LBRACK, "LBRACK"
    },
    {
      RBRACK, "RBRACK"
    },
    {
      LBRACE, "LBRACE"
    },
    {
      RBRACE, "RBRACE"
    },
    {
      ID, "ID"
    },
    {
      NUM, "NUM"
    } 
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
  
  int result;
  
  cout << std::left
       << setw (20) << "TOKEN" << setw (22) << "LEXEME" << "VALUE" << endl
       << setw (20) << "=====" << setw (22) << "======" << "=====" << endl;
  
  do
  {
    result = yylex ();
    
    if (result != 0)
    {
      cout << setw (20) << tokenNames[result] 
           << setw (22) << '"' + std::string (yytext) + '"'; 
    }

    if (result == NUM)
    {
      cout << std::stoi (yytext);
    }
    else if (result == ID)
    {
      cout << '"' <<  yytext << '"';
    }
    else if (result == ERROR)
    {
      cout << "Line: " << lineCount << "; Column: " << colCount;
    }

    cout << endl;

  } while (result != 0);

  return EXIT_SUCCESS;
}

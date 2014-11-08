/********************************************************************/
// C- AST Class Definitions
// Header file by Daniel J. Rabiega & Mervin Fansler
// Design by CSCI 435: Compilers class
// Fall 2014

/********************************************************************/
// System Includes

#include <string>
#include <vector>


#include "CMinusAst.h"

/********************************************************************/
// Using Declarations

using std::string;
using std::vector;

/********************************************************************/
// Empty Virtual Destructors

Node::~Node () {}
DeclarationNode::~DeclarationNode () {}
StatementNode::~StatementNode () {}
ExpressionNode::~ExpressionNode () {}
VariableDeclarationNode::~VariableDeclarationNode () {}
VariableExpressionNode::~VariableExpressionNode () {}

/********************************************************************/
// ProgramNode Methods

ProgramNode::ProgramNode (vector<DeclarationNode*> children)
{
  this->children = children;
}

ProgramNode::~ProgramNode ()
{
  for (DeclarationNode* d : children)
    delete d;

  children.clear ();
}

void ProgramNode::accept (IVisitor* visitor)
{
  visitor->visit (this);
}

/********************************************************************/
// DeclarationNode Methods

void DeclarationNode::accept (IVisitor* visitor)
{
  visitor->visit (this);
}

/********************************************************************/
// FunctionDeclarationNode methods

FunctionDeclarationNode::FunctionDeclarationNode (ValueType t,
						  string id,
						  vector <ParameterNode*> params,
						  CompoundStatementNode* csNode)
{
  valueType = t;
  identifier = id;
  parameters = params;
  functionBody = csNode;
}

FunctionDeclarationNode::~FunctionDeclarationNode ()
{
  delete functionBody;
  for (ParameterNode* p : parameters)
    delete p;
  parameters.clear ();
}

/********************************************************************/
// VariableDeclarationNode methods

VariableDeclarationNode::VariableDeclarationNode (string id)
{
  valueType = ValueType::INT;
  identifier = id;
}

/********************************************************************/
// ArrayDeclarationNode methods

ArrayDeclarationNode::ArrayDeclarationNode (string id, size_t size)
  : VariableDeclarationNode (id)
{
  this->size = size;
}

ArrayDeclarationNode::~ArrayDeclarationNode () {}

/********************************************************************/
// StatementNode Methods

void StatementNode::accept (IVisitor* visitor)
{
  visitor->visit (this);
}

/********************************************************************/
// CompoundStatementNode Methods

CompoundStatementNode::CompoundStatementNode (vector<VariableDeclarationNode*> decls,
					      vector<StatementNode*> stmts)
{
  localDeclarations = decls;
  statements = stmts;
}

CompoundStatementNode::~CompoundStatementNode ()
{
  for (VariableDeclarationNode* v : localDeclarations)
    delete v;
  localDeclarations.clear ();
  
  for (StatementNode* s : statements)
    delete s;
  statements.clear ();
}

/********************************************************************/
// IfStatementNode Methods

IfStatementNode::IfStatementNode (ExpressionNode* expr,
				  StatementNode* thenStmt,
				  StatementNode* elseStmt)
{
  conditionalExpression = expr;
  thenStatement = thenStmt;
  elseStatement = elseStmt;
}

IfStatementNode::~IfStatementNode ()
{
  delete conditionalExpression;
  delete thenStatement;
  delete elseStatement;
}

/********************************************************************/
// WhileStatementNode Methods

WhileStatementNode::WhileStatementNode (ExpressionNode* expr,
					StatementNode* stmt)
{
  conditionalExpression = expr;
  body = stmt;
}

WhileStatementNode::~WhileStatementNode ()
{
  delete conditionalExpression;
  delete body;
}

/********************************************************************/
// ForStatementNode Methods


ForStatementNode::ForStatementNode (ExpressionNode* e1,
				    ExpressionNode* e2,
				    ExpressionNode* e3,
				    StatementNode* s)
{
  initializer = e1;
  condition = e2;
  updater = e3;
  body = s;
}

ForStatementNode::~ForStatementNode ()
{
  delete initializer;
  delete condition;
  delete updater;
  delete body;
}

/********************************************************************/
// ReturnStatementNode Methods

ReturnStatementNode::ReturnStatementNode (ExpressionNode* expr)
{
  expression = expr;
}

ReturnStatementNode::~ReturnStatementNode ()
{
  delete expression;
}

/********************************************************************/
// ExpressionStatementNode Methods

ExpressionStatementNode::ExpressionStatementNode (ExpressionNode* expr)
{
  expression = expr;
}

ExpressionStatementNode::~ExpressionStatementNode ()
{
  delete expression;
}

/********************************************************************/
// ExpressionNode Methods

void ExpressionNode::accept (IVisitor* visitor)
{
  visitor->visit (this);
}

/********************************************************************/
// AssignmentExpressionNode Methods

AssignmentExpressionNode::AssignmentExpressionNode (VariableExpressionNode* v,
						    ExpressionNode* e)
{
  variable = v;
  expression = e;
}

AssignmentExpressionNode::~AssignmentExpressionNode ()
{
  delete variable;
  delete expression;
}

/********************************************************************/
// VariableExpressionNode Methods

VariableExpressionNode::VariableExpressionNode (string id)
{
  identifier = id;
}

/********************************************************************/
// SubscriptExpressionNode Methods

SubscriptExpressionNode::SubscriptExpressionNode (string id,
						  ExpressionNode* index)
  : VariableExpressionNode (id)
{
  this->index = index;
}

SubscriptExpressionNode::~SubscriptExpressionNode ()
{
  delete index;
}

/********************************************************************/
// CallExpressionNode Methods

CallExpressionNode::CallExpressionNode (string id, vector<ExpressionNode*> args)
{
  identifier = id;
  arguments = args;
}

CallExpressionNode::~CallExpressionNode ()
{
  for (ExpressionNode* e : arguments)
    delete e;
  arguments.clear ();
}

/********************************************************************/
// AdditiveExpressionNode Methods

AdditiveExpressionNode::AdditiveExpressionNode (AdditiveOperatorType addop,
						ExpressionNode* l,
						ExpressionNode* r)
{
  addOperator = addop;
  left = l;
  right = r;
}

AdditiveExpressionNode::~AdditiveExpressionNode ()
{
  delete left;
  delete right;
}

/********************************************************************/
// MultiplicativeExpressionNode Methods

MultiplicativeExpressionNode::MultiplicativeExpressionNode (MultiplicativeOperatorType multop,
							    ExpressionNode* l,
							    ExpressionNode* r)
{
  multOperator = multop;
  left = l;
  right = r;
}

MultiplicativeExpressionNode::~MultiplicativeExpressionNode ()
{
  delete left;
  delete right;
}

/********************************************************************/
// RelationExpressionNode Methods

RelationalExpressionNode::RelationalExpressionNode (RelationalOperatorType relop,
						    ExpressionNode* l,
						    ExpressionNode* r)
{
  relationalOperator = relop;
  left = l;
  right = r;
}

RelationalExpressionNode::~RelationalExpressionNode ()
{
  delete left;
  delete right;
}

/********************************************************************/
// UnaryExpressionNode Methods

UnaryExpressionNode::UnaryExpressionNode (UnaryOperatorType unop,
					  VariableExpressionNode* v)
{
  unaryOperator = unop;
  variable = v;
}

UnaryExpressionNode::~UnaryExpressionNode ()
{
  delete variable;
}

/********************************************************************/
// IntegerLiteralExpressionNode Methods

IntegerLiteralExpressionNode::IntegerLiteralExpressionNode (int v)
{
  value = v;
}

IntegerLiteralExpressionNode::~IntegerLiteralExpressionNode () {}

/********************************************************************/
// ParameterNode Methods

ParameterNode::ParameterNode (string id, bool array) : DeclarationNode ()
{
  identifier = id;
  isArray = array;
  valueType = ValueType::INT;
}

ParameterNode::~ParameterNode () {}
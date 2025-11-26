#ifndef PARSER__H
#define PARSER__H

#include <array>
#include <functional>

#include "ast_node.h"
#include "errors.h"
#include "memory.h"
#include "scanner.h"
#include "tokens.h"

class Parser {
public:
  Parser(Scanner* scanner, MemoryAllocator* allocator);

  AstProgram* Parse();

protected:
  enum Precedence {
    PREC_NONE,
    PREC_ASSIGNMENT,  // =
    PREC_FUNCTION,    // function
    PREC_OR,          // or
    PREC_XOR,         // xor
    PREC_AND,         // and  
    PREC_EQUALITY,    // == !=
    PREC_BITWISE_OR,  // |
    PREC_BITWISE_XOR, // ^
    PREC_BITWISE_AND, // &
    PREC_SHIFT,       // << >>
    PREC_COMPARISON,  // < > <= >=
    PREC_TERM,        // + -
    PREC_FACTOR,      // * / // %
    PREC_UNARY,       // * & not - ~
    PREC_CALL,        // . () []
    PREC_PRIMARY,     // 'nil', 'true', 'false', number, string, 
  };

  struct ParseRule {
    std::function<AstExpr*()> prefix;
    std::function<AstExpr*()> infix;
    Parser::Precedence precedence;
  };


  // Parsing Functions.
  AstBlock* Block();
  AstBlock* BlockStatement();
  AstPrimaryExp* Number();
  AstPrimaryExp* Integer();
  AstPrimaryExp* Nil();
  AstPrimaryExp* True();
  AstPrimaryExp* False();
  AstPrimaryExp* String();
  AstPrimaryExp* Type();
  AstArrayValueExpr* ArrayValue();

  AstVarDeclStmt* ValDecl();
  AstVarDeclStmt* VarDecl();
  AstPrintStmt* Print();
  AstIndexOrGenericArgs* IndexOrGenericArgs();
  AstClassConstructor* ClassConstructor();
  AstRangeExpr* RangeExpr();
  

  AstExpr* Group();

  AstStmt* Statement();
  AstExpressionStmt* ExpressionStmt();
  AstBlock* DoBlock();

  AstNode* UnaryOp();
  AstNode* BinaryOp();
  AstNode* InPlaceBinaryOp();
  AstNode* grouping();
  AstExpr* Expr();
  AstNode* Id();
  AstNode* FunctionDef();
  AstNode* ReturnStatement();
  AstNode* ClassDef();
  AstNode* FunctionCallArgs();
  AstNode* FunctionCallArg();
  AstNode* TypeDef();

  AstExpr* ParsePrecedence(Precedence precedence);
  bool ParseValue(Value* ret);
  bool ParseValueExpr(Value* val);
  Token ParseVariable(Token token);

  // Scanning.
  void Advance();
  void Consume(TokenType type, const char* err_msg);
  void Match(TokenType type);
  bool Check(TokenType);
  void Synchronize();

  // Returns true if the current token is in the follow set of a block.
  // 'until' closes syntactical blocks, but do not close scope,
  // so it is handled in separate. This is used, for example, when determining if
  // a function ends with a simple `return` or with a `return expr`.
  bool BlockFollow(bool withuntil);
  void ReduceToVarExpr(AstExpr* expr);

  // Errors
  void ErrorAt(Token* token, const char* const message, va_list argp);
  void ErrorAtCurrent(const char* message, ...);
  void Error(const char* message, ...);

  void MakeParseRules();
  const ParseRule& ExprParseRule(TokenType tk);

  template <class Ty_>
  Ty_* MakeNode(int line=-1) {
    if (line == -1) line = current_.line;

    Ty_* ret = allocator_->make<Ty_>();
    static_cast<AstNode*>(ret)->line = line;

    return ret;
  }

  Token current_;
  Token previous_;

  Scanner* scanner_;
  MemoryAllocator* allocator_;
  ErrorsContainer* errors_;
  std::array<ParseRule, __TK_COUNT__> parse_rules_;
};

#endif  // PARSER__H
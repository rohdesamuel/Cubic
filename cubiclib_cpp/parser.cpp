#include "parser.h"

#include "memory.h"
#include "scanner.h"
#include "memory.h"

Parser::Parser(Scanner* scanner, MemoryAllocator* allocator) : scanner_(scanner), allocator_(allocator) {
  MakeParseRules();
}

AstProgram* Parser::Parse() {
  Advance();
  AstProgram* root = allocator_->alloc<AstProgram>();

  root->block = Block();

  Consume(TK_EOF, "Expected end of file.");
  return root;
}

AstBlock* Parser::Block() {
  AstBlock* block = MakeNode<AstBlock>();

  // TODO: how to handle the 'pass' keyword? Should it stop parsing statement lists?
  while (!BlockFollow(true)) {

    AstStmt* statement = Statement();
    block->statements.push_back(statement);

    if (errors_->panic_mode()) {
      Synchronize();
    }

    if (previous_.type == TK_RETURN) {
      return nullptr;
    }
  }

  return block;
}

bool Parser::BlockFollow(bool withuntil) {
  switch (current_.type) {
    case TK_ELSE:
    case TK_ELIF:
    case TK_END:
    case TK_EOF:
    case TK_CASE:
      return true;

    case TK_UNTIL:
      return withuntil;

    default:
      return false;
  }
}

// Escape edge of the parser state machine from the error state to the initial state.
void Parser::Synchronize() {
  errors_->ClearPanic();

  // These cases are all the tokens that can start a statement.
  while (current_.type != TK_EOF) {
    switch (current_.type) {
      case TK_DO:
      case TK_VAL:
      case TK_VAR:
      case TK_END:
      case TK_WHILE:
      case TK_ASSERT:
      case TK_RETURN:
      case TK_IF:
      case TK_PRINT:
      case TK_PASS:
      case TK_SEMICOLON:
      case TK_EOF:
      case TK_FUNCTION:
      case TK_TYPE:
        return;
      default:
        ;
    }

    Advance();
  }
}

AstBlock* Parser::BlockStatement() {
  AstBlock* block = Block();
  Consume(TK_END, "Expected 'end' at the end of a block.");
  return block;
}

AstBlock* Parser::DoBlock() {
  Advance();
  return BlockStatement();
}

AstStmt* Parser::Statement() {
  TokenType tk = current_.type;
  AstNode* ret = nullptr;

  switch (tk) {
    case TK_DO: return DoBlock();
    case TK_VAL: return ValDecl();
    case TK_VAR: return VarDecl();

    case TK_END:
    {
      errors_->Err(current_.line, "Unmatched 'do'");
      return MakeNode<AstNoopStmt>();
    }

#if 0

    case TK_FUNCTION:
    {
      advance(parser, scanner);
      return FunctionDef(parser, scanner);
    }

    case TK_WHILE:
    {
      advance(parser, scanner);
      AstWhileStmt_* stmt = MakeNode<AstWhileStmt>(current_.line);
      stmt->condition_expr = Expr(parser, scanner);

      consume(parser, scanner, TK_DO, "Expected 'do' at end of while statement.");

      stmt->block_stmt = Block(parser, scanner);

      consume(parser, scanner, TK_END, "Expected 'end' at end of while definition.");

      return (AstNode*)stmt;
    }

    case TK_FOR:
    {
      // ForStmt ::= 'for' [VarDecl | ExpressionStmt] ';' [Expr] ';' [Expr] 'do' Block 'end'
      AstForStmt_* stmt = MakeNode<AstForStmt>(current_.line);
      advance(parser, scanner);
      if (!match(parser, scanner, TK_SEMICOLON)) {
        if (match(parser, scanner, TK_VAL) || match(parser, scanner, TK_VAR)) {
          stmt->opt_var_decl = VarDecl(parser, scanner, previous_.type);
        } else {
          stmt->opt_var_decl = ExpressionStmt(parser, scanner);
        }
        consume(parser, scanner, TK_SEMICOLON, "Expected ';' after variable declaration in for loop.");
      }

      if (!match(parser, scanner, TK_SEMICOLON)) {
        stmt->opt_condition_expr = Expr(parser, scanner);
        consume(parser, scanner, TK_SEMICOLON, "Expected ';' after condition expression in for loop.");
      }

      if (!match(parser, scanner, TK_DO)) {
        stmt->opt_step_expr = Expr(parser, scanner);
        consume(parser, scanner, TK_DO, "Expected 'do' at end of for loop statement.");
      }

      stmt->block_stmt = Block(parser, scanner);
      consume(parser, scanner, TK_END, "Expected 'end' at end of for loop definition.");

      return (AstNode*)stmt;
    }

    case TK_ASSERT:
    {
      advance(parser, scanner);
      AstAssertStmt_* stmt = MakeNode<AstAssertStmt>(current_.line);
      stmt->expr = Expr(parser, scanner);
      return (AstNode*)stmt;
    }

    // TODO: add parsing multiple return values
    case TK_RETURN:
    {
      advance(parser, scanner);
      return ReturnStatement(parser, scanner);
    }

    case TK_IF:
    {
      advance(parser, scanner);
      AstIfStmt_* stmt = MakeNode<AstIfStmt>(current_.line);

      stmt->condition_expr = Expr(parser, scanner);
      consume(parser, scanner, TK_THEN, "Expected 'then' after condition.");

      stmt->if_stmt = (AstNode*)Block(parser, scanner);
      cstlist_init(&stmt->elif_stmts, parser->allocator);
      cstlist_init(&stmt->elif_exprs, parser->allocator);

      for (;;) {
        if (!match(parser, scanner, TK_ELIF)) {
          break;
        }
        cstlist_append(&stmt->elif_exprs, Expr(parser, scanner));
        consume(parser, scanner, TK_THEN, "Expected 'then' after condition.");
        cstlist_append(&stmt->elif_stmts, Block(parser, scanner));
      }

      if (match(parser, scanner, TK_ELSE)) {
        stmt->else_stmt = (AstNode*)Block(parser, scanner);
      } else {
        stmt->else_stmt = nullptr;
      }

      consume(parser, scanner, TK_END, "Expected 'end' after if-statement.");

      return (AstNode*)stmt;
    }

    case TK_PRINT:
    {
      advance(parser, scanner);
      AstPrintStmt_* stmt = MakeNode<AstPrintStmt>(current_.line);
      stmt->expr = Expr(parser, scanner);
      return (AstNode*)stmt;
    }

    case TK_CLASS:
    {
      advance(parser, scanner);
      return ClassDef(parser, scanner);
    }

    case TK_TYPE:
    {
      advance(parser, scanner);
      return TypeDef(parser, scanner);
    }

    case TK_PASS:
    case TK_SEMICOLON:
    case TK_EOF:
      advance(parser, scanner);
      return MAKE_CST_NOOP(allocator);
#endif
    default:
      return ExpressionStmt();
  }

  return nullptr;
}

AstExpressionStmt* Parser::ExpressionStmt() {
  AstExpressionStmt* stmt = MakeNode<AstExpressionStmt>();
  stmt->expr = Expr();
  return stmt;
}

AstExpr* Parser::Expr() {
  AstExpr* ret = ParsePrecedence(PREC_NONE);
  if (!ret) {
    return nullptr;
  }
  return ret;
}

bool Parser::ParseValue(Value* ret) {
  Token tk = previous_;
  switch (tk.type) {
    case TK_NIL:
      *ret = NIL_VAL;
      break;

    case TK_TRUE:
      *ret = TRUE_VAL;
      break;

    case TK_FALSE:
      *ret = FALSE_VAL;
      break;

    case TK_STRING:
      *ret = CSTR_VAL(tk.tk.data() + 1, tk.tk.length() - 2);
      break;

    case TK_NUMBER:
      if (tk.tk[tk.tk.length() - 1] == 'f') {
        *ret = FLOAT_VAL(strtof(tk.tk.data(), nullptr));
      } else {
        *ret = DOUBLE_VAL(strtod(tk.tk.data(), nullptr));
      }
      break;

    case TK_INTEGER:
      *ret = INT_VAL(strtoll(tk.tk.data(), nullptr, 10));
      break;

    default:
      return false;
  }

  return true;
}

AstPrintStmt* Parser::Print() {
  AstPrintStmt* stmt = MakeNode<AstPrintStmt>();
  stmt->expr = Expr();

  return stmt;
}

bool Parser::ParseValueExpr(Value* val) {
  bool ret = ParseValue(val);
  if (!ret) {
    errors_->Err(previous_.line, "Trying to parse unknown token as value: %d", previous_.type);
  }
  return ret;
}

AstPrimaryExp* Parser::Number() {
  Token token = previous_;
  AstPrimaryExp* expr = MakeNode<AstPrimaryExp>(previous_.line);

  if (token.tk[token.tk.length() - 1] == 'f') {
    float value = strtof(previous_.tk.data(), nullptr);
    expr->type = TK_FLOAT;
    ParseValueExpr(&expr->value);
  } else {
    double value = strtod(previous_.tk.data(), nullptr);
    expr->type = TK_DOUBLE;
    ParseValueExpr(&expr->value);
  }

  return expr;
}

AstPrimaryExp* Parser::Integer() {
  int64_t value = strtoll(previous_.tk.data(), nullptr, 10);
  AstPrimaryExp* expr = MakeNode<AstPrimaryExp>(previous_.line);
  expr->type = TK_INT;
  ParseValueExpr(&expr->value);
  return expr;
}

AstPrimaryExp* Parser::Nil() {
  AstPrimaryExp* expr = MakeNode<AstPrimaryExp>(previous_.line);
  expr->type = TK_NIL;
  expr->value = NIL_VAL;
  return expr;
}

AstPrimaryExp* Parser::True() {
  AstPrimaryExp* expr = MakeNode<AstPrimaryExp>(previous_.line);
  expr->type = TK_BOOL;
  expr->value = TRUE_VAL;
  return expr;
}

AstPrimaryExp* Parser::False() {
  AstPrimaryExp* expr = MakeNode<AstPrimaryExp>(previous_.line);
  expr->type = TK_BOOL;
  expr->value = FALSE_VAL;
  return expr;
}

AstPrimaryExp* Parser::String() {
  AstPrimaryExp* expr = MakeNode<AstPrimaryExp>(previous_.line);
  expr->type = TK_STRING_TYPE;
  ParseValueExpr(&expr->value);
  return expr;
}

template<class To_, class From_>
To_* ast_cast(From_ from) {
  assert(typeid(From_) == typeid(To_));
  return (To_*)(from);
}

AstExpr* Parser::ParsePrecedence(Parser::Precedence precedence) {
  Advance();
  const auto& prefix_rule = ExprParseRule(previous_.type).prefix;
  if (!prefix_rule) {
    errors_->Err(previous_.line, "Expected expression");
    return nullptr;
  }

  AstExpr* exp = prefix_rule();
  if (typeid(*exp) == typeid(AstType) && current_.type != TK_LBRACE && current_.type != TK_RBRACKET) {
    errors_->Err(current_.line, "Bad type constructor.");
  }

  while (precedence <= ExprParseRule(current_.type).precedence) {
    const auto& infix_rule = ExprParseRule(current_.type).infix;
    if (!infix_rule) {
      break;
    }
    Advance();

    if (previous_.type == TK_LPAREN) {
      AstFunctionCall* new_exp = MakeNode<AstFunctionCall>(previous_.line);
      new_exp->prefix = exp;
      new_exp->args = infix_rule();

      exp = new_exp;
    } else if (previous_.type == TK_EQUAL) {
      AstAssignmentExpr* new_exp = MakeNode<AstAssignmentExpr>(previous_.line);
      new_exp->left = exp;
      new_exp->right = infix_rule();
      if (typeid(new_exp->right) == typeid(AstClassConstructor)) {
        AstVarExpr* var_expr = ast_cast<AstVarExpr>(new_exp->left);
        AstClassConstructor* c_expr = ast_cast<AstClassConstructor>(new_exp->right);
        c_expr->name = ast_cast<AstIdExpr>(var_expr->expr)->name;
      }
      exp = new_exp;
    } else if (previous_.type == TK_DOT) {
      AstVarExpr* var_exp = MakeNode<AstVarExpr>(previous_.line);
      AstDotExpr* new_exp = MakeNode<AstDotExpr>(previous_.line);
      var_exp->expr = new_exp;
      new_exp->prefix = exp;
      new_exp->id = ParseVariable(current_);
      Advance();
      exp = var_exp;
    } else if (previous_.type == TK_LBRACE) {
      AstClassConstructor* new_exp = ast_cast<AstClassConstructor>(infix_rule());
      new_exp->prefix = exp;
      if (typeid(*exp) == typeid(AstVarExpr)) {
        AstVarExpr* var_expr = ast_cast<AstVarExpr>(exp);
        AstIdExpr* id_expr = ast_cast<AstIdExpr>(var_expr->expr);
        new_exp->name = id_expr->name;
      }
      allocator_->dealloc(exp);
      exp = new_exp;
    } else if (previous_.type == TK_LBRACKET) {
      AstVarOrTypeExpr* new_exp = MakeNode<AstVarOrTypeExpr>(previous_.line);
      new_exp->prefix = exp;
      new_exp->index_args = ast_cast<AstIndexOrGenericArgs>(infix_rule());
      exp = new_exp;
    } else if (previous_.type >= TK_PLUS_EQUAL && previous_.type <= TK_SLASH_SLASH_EQUAL) {
      AstInPlaceBinaryExpr* bin_exp =
        MakeNode<AstInPlaceBinaryExpr>(previous_.line);

      bin_exp->op = previous_.type;
      bin_exp->left = exp;
      bin_exp->right = infix_rule();

      exp = bin_exp;
    } else if (previous_.type == TK_DOUBLE_DOT) {
      AstRangeExpr* expr = MakeNode<AstRangeExpr>(previous_.line);

      expr->left = exp;
      expr->right = Expr();

      exp = expr;
    } else {
      AstBinaryExp* bin_exp = MakeNode<AstBinaryExp>(previous_.line);
      bin_exp->op = previous_.type;
      bin_exp->left = exp;
      bin_exp->right = infix_rule();

      exp = bin_exp;
    }
  }

  return exp;
}

Token Parser::ParseVariable(Token token) {
  return token;
}

void Parser::MakeParseRules() {
#define WRAP_THIS(FN) ([this](){return static_cast<AstExpr*>((FN)());})

  parse_rules_[TK_EOF]               = { nullptr,                     nullptr,                       PREC_NONE };
  parse_rules_[TK_ERR]               = { nullptr,                     nullptr,                       PREC_NONE };
  parse_rules_[TK_ID]                = { WRAP_THIS(Id),               nullptr,                       PREC_NONE };
  parse_rules_[TK_LPAREN]            = { WRAP_THIS(Group),            WRAP_THIS(FunctionCallArgs),   PREC_CALL };
  parse_rules_[TK_RPAREN]            = { nullptr,                     nullptr,                       PREC_NONE };
  parse_rules_[TK_LBRACKET]          = { WRAP_THIS(ArrayValue),       WRAP_THIS(IndexOrGenericArgs), PREC_CALL };
  parse_rules_[TK_RBRACKET]          = { nullptr,                     nullptr,                       PREC_NONE };
  parse_rules_[TK_LBRACE]            = { WRAP_THIS(ClassConstructor), WRAP_THIS(ClassConstructor),   PREC_CALL };
  parse_rules_[TK_RBRACE]            = { nullptr,                     nullptr,                       PREC_NONE };
  parse_rules_[TK_SEMICOLON]         = { nullptr,                     nullptr,                       PREC_NONE };
  parse_rules_[TK_COLON]             = { nullptr,                     nullptr,                       PREC_NONE };
  parse_rules_[TK_DOT]               = { nullptr,                     WRAP_THIS(BinaryOp),           PREC_CALL };
  parse_rules_[TK_COMMA]             = { nullptr,                     nullptr,                       PREC_NONE };
  parse_rules_[TK_PLUS]              = { nullptr,                     WRAP_THIS(BinaryOp),           PREC_TERM };
  parse_rules_[TK_MINUS]             = { WRAP_THIS(UnaryOp),          WRAP_THIS(BinaryOp),           PREC_TERM };
  parse_rules_[TK_AMPERSAND]         = { nullptr,                     WRAP_THIS(BinaryOp),           PREC_BITWISE_AND };
  parse_rules_[TK_PIPE]              = { nullptr,                     WRAP_THIS(BinaryOp),           PREC_BITWISE_OR };
  parse_rules_[TK_HAT]               = { nullptr,                     WRAP_THIS(BinaryOp),           PREC_BITWISE_XOR };
  parse_rules_[TK_TILDE]             = { WRAP_THIS(UnaryOp),          nullptr,                       PREC_UNARY };
  parse_rules_[TK_STAR]              = { nullptr,                     WRAP_THIS(BinaryOp),           PREC_FACTOR };
  parse_rules_[TK_PERCENT]           = { nullptr,                     WRAP_THIS(BinaryOp),           PREC_FACTOR };
  parse_rules_[TK_QUESTION]          = { nullptr,                     nullptr,                       PREC_NONE };
  parse_rules_[TK_BANG]              = { nullptr,                     nullptr,                       PREC_NONE };
  parse_rules_[TK_BANG_EQUAL]        = { nullptr,                     WRAP_THIS(BinaryOp),           PREC_EQUALITY };
  parse_rules_[TK_EQUAL]             = { nullptr,                     WRAP_THIS(BinaryOp),           PREC_ASSIGNMENT };
  parse_rules_[TK_EQUAL_EQUAL]       = { nullptr,                     WRAP_THIS(BinaryOp),           PREC_EQUALITY };
  parse_rules_[TK_GT]                = { nullptr,                     WRAP_THIS(BinaryOp),           PREC_COMPARISON };
  parse_rules_[TK_GTE]               = { nullptr,                     WRAP_THIS(BinaryOp),           PREC_COMPARISON };
  parse_rules_[TK_RSHIFT]            = { nullptr,                     WRAP_THIS(BinaryOp),           PREC_SHIFT };
  parse_rules_[TK_LT]                = { nullptr,                     WRAP_THIS(BinaryOp),           PREC_COMPARISON };
  parse_rules_[TK_LTE]               = { nullptr,                     WRAP_THIS(BinaryOp),           PREC_COMPARISON };
  parse_rules_[TK_LSHIFT]            = { nullptr,                     WRAP_THIS(BinaryOp),           PREC_SHIFT };
  parse_rules_[TK_ARROW]             = { nullptr,                     nullptr,                       PREC_NONE };
  parse_rules_[TK_FAT_ARROW]         = { nullptr,                     nullptr,                       PREC_NONE };
  parse_rules_[TK_SLASH]             = { nullptr,                     WRAP_THIS(BinaryOp),           PREC_FACTOR };
  parse_rules_[TK_DOUBLE_SLASH]      = { nullptr,                     WRAP_THIS(BinaryOp),           PREC_FACTOR };
  parse_rules_[TK_DOUBLE_DOT]        = { nullptr,                     WRAP_THIS(RangeExpr),          PREC_NONE };
  parse_rules_[TK_TRIPLE_DOT]        = { nullptr,                     nullptr,                       PREC_NONE };
  parse_rules_[TK_QQ]                = { nullptr,                     nullptr,                       PREC_NONE };
  parse_rules_[TK_QQE]               = { nullptr,                     nullptr,                       PREC_NONE };
  parse_rules_[TK_PLUS_EQUAL]        = { nullptr,                     WRAP_THIS(InPlaceBinaryOp),    PREC_ASSIGNMENT };
  parse_rules_[TK_MINUS_EQUAL]       = { nullptr,                     WRAP_THIS(InPlaceBinaryOp),    PREC_ASSIGNMENT };
  parse_rules_[TK_AMPERSAND_EQUAL]   = { nullptr,                     WRAP_THIS(InPlaceBinaryOp),    PREC_ASSIGNMENT };
  parse_rules_[TK_PIPE_EQUAL]        = { nullptr,                     WRAP_THIS(InPlaceBinaryOp),    PREC_ASSIGNMENT };
  parse_rules_[TK_HAT_EQUAL]         = { nullptr,                     WRAP_THIS(InPlaceBinaryOp),    PREC_ASSIGNMENT };
  parse_rules_[TK_TILDE_EQUAL]       = { nullptr,                     WRAP_THIS(InPlaceBinaryOp),    PREC_ASSIGNMENT };
  parse_rules_[TK_STAR_EQUAL]        = { nullptr,                     WRAP_THIS(InPlaceBinaryOp),    PREC_ASSIGNMENT };
  parse_rules_[TK_PERCENT_EQUAL]     = { nullptr,                     WRAP_THIS(InPlaceBinaryOp),    PREC_ASSIGNMENT };
  parse_rules_[TK_SLASH_EQUAL]       = { nullptr,                     WRAP_THIS(InPlaceBinaryOp),    PREC_ASSIGNMENT };
  parse_rules_[TK_SLASH_SLASH_EQUAL] = { nullptr,                     WRAP_THIS(InPlaceBinaryOp),    PREC_ASSIGNMENT };
  parse_rules_[TK_STRING]            = { WRAP_THIS(String),           nullptr,                       PREC_NONE };
  parse_rules_[TK_INTEGER]           = { WRAP_THIS(Integer),          nullptr,                       PREC_NONE };
  parse_rules_[TK_NUMBER]            = { WRAP_THIS(Number),           nullptr,                       PREC_NONE };
  parse_rules_[TK_DO]                = { nullptr,                     nullptr,                       PREC_NONE };
  parse_rules_[TK_END]               = { nullptr,                     nullptr,                       PREC_NONE };
  parse_rules_[TK_IF]                = { nullptr,                     nullptr,                       PREC_NONE };
  parse_rules_[TK_THEN]              = { nullptr,                     nullptr,                       PREC_NONE };
  parse_rules_[TK_ELIF]              = { nullptr,                     nullptr,                       PREC_NONE };
  parse_rules_[TK_ELSE]              = { nullptr,                     nullptr,                       PREC_NONE };
  parse_rules_[TK_FOR]               = { nullptr,                     nullptr,                       PREC_NONE };
  parse_rules_[TK_IN]                = { nullptr,                     nullptr,                       PREC_NONE };
  parse_rules_[TK_STEP]              = { nullptr,                     nullptr,                       PREC_NONE };
  parse_rules_[TK_WHILE]             = { nullptr,                     nullptr,                       PREC_NONE };
  parse_rules_[TK_REPEAT]            = { nullptr,                     nullptr,                       PREC_NONE };
  parse_rules_[TK_UNTIL]             = { nullptr,                     nullptr,                       PREC_NONE };
  parse_rules_[TK_CLASS]             = { nullptr,                     nullptr,                       PREC_NONE };
  parse_rules_[TK_IS]                = { nullptr,                     nullptr,                       PREC_NONE };
  parse_rules_[TK_MATCH]             = { nullptr,                     nullptr,                       PREC_NONE };
  parse_rules_[TK_CASE]              = { nullptr,                     nullptr,                       PREC_NONE };
  parse_rules_[TK_FUNCTION]          = { WRAP_THIS(FunctionDef),      nullptr,                       PREC_FUNCTION };
  parse_rules_[TK_RETURN]            = { nullptr,                     nullptr,                       PREC_NONE };
  parse_rules_[TK_PRINT]             = { nullptr,                     nullptr,                       PREC_NONE };
  parse_rules_[TK_AND]               = { nullptr,                     WRAP_THIS(BinaryOp),           PREC_AND };
  parse_rules_[TK_OR]                = { nullptr,                     WRAP_THIS(BinaryOp),           PREC_OR };
  parse_rules_[TK_XOR]               = { nullptr,                     WRAP_THIS(BinaryOp),           PREC_XOR };
  parse_rules_[TK_NOT]               = { WRAP_THIS(UnaryOp),          nullptr,                       PREC_UNARY };
  parse_rules_[TK_TRUE]              = { WRAP_THIS(True),             nullptr,                       PREC_NONE };
  parse_rules_[TK_FALSE]             = { WRAP_THIS(False),            nullptr,                       PREC_NONE };
  parse_rules_[TK_PASS]              = { nullptr,                     nullptr,                       PREC_NONE };
  parse_rules_[TK_VAL]               = { nullptr,                     nullptr,                       PREC_NONE };
  parse_rules_[TK_VAR]               = { nullptr,                     nullptr,                       PREC_NONE };
  parse_rules_[TK_REF]               = { nullptr,                     nullptr,                       PREC_NONE };
  parse_rules_[TK_NIL]               = { WRAP_THIS(Nil),              nullptr,                       PREC_NONE };
  parse_rules_[TK_NEW]               = { WRAP_THIS(UnaryOp),          nullptr,                       PREC_UNARY };
  parse_rules_[TK_DEL]               = { WRAP_THIS(UnaryOp),          nullptr,                       PREC_UNARY };
  parse_rules_[TK_ASSERT]            = { nullptr,                     nullptr,                       PREC_NONE };
  parse_rules_[TK_TYPE]              = { nullptr,                     nullptr,                       PREC_NONE };
  parse_rules_[TK_BOOL]              = { nullptr,                     nullptr,                       PREC_NONE };
  parse_rules_[TK_INT]               = { WRAP_THIS(Type),             nullptr,                       PREC_NONE };
  parse_rules_[TK_UINT]              = { WRAP_THIS(Type),             nullptr,                       PREC_NONE };
  parse_rules_[TK_FLOAT]             = { WRAP_THIS(Type),             nullptr,                       PREC_NONE };
  parse_rules_[TK_DOUBLE]            = { WRAP_THIS(Type),             nullptr,                       PREC_NONE };
  parse_rules_[TK_STRING_TYPE]       = { WRAP_THIS(Type),             nullptr,                       PREC_NONE };
  parse_rules_[TK_LIST]              = { WRAP_THIS(Type),             nullptr,                       PREC_NONE };
  parse_rules_[TK_MAP]               = { WRAP_THIS(Type),             nullptr,                       PREC_NONE };
  parse_rules_[TK_SET]               = { WRAP_THIS(Type),             nullptr,                       PREC_NONE };
  parse_rules_[TK_ASYNC]             = { nullptr,                     nullptr,                       PREC_NONE };
  parse_rules_[TK_AWAIT]             = { nullptr,                     nullptr,                       PREC_NONE };
  parse_rules_[TK_YIELD]             = { nullptr,                     nullptr,                       PREC_NONE };
#undef WRAP_THIS
}

#if 0
void Consume(TokenType type, const char* err_msg);
void Match(TokenType type);
bool Check(TokenType);

#include "symbol_table.h"
#include "object.h"
#include "type_expr.h"
#include "cst.h"

#include <memory.h>
#include <stdarg.h>
#include <string.h>

#pragma warning(3 : 4062)

// TODO: Reenable these
// #define ENABLE_GENERIC_ARRAYS


typedef AstNode* (*ParseFn)(Parser_*, Scanner_*);

typedef struct ParseRule_ {
  ParseFn prefix;
  ParseFn infix;
  Precedence precedence;
} ParseRule_;

static AstNode* Number(Parser_* parser, Scanner_* scanner);
static AstNode* UnaryOp(Parser_* parser, Scanner_* scanner);
static AstNode* BinaryOp(Parser_* parser, Scanner_* scanner);
static AstNode* InPlaceBinaryOp(Parser_* parser, Scanner_* scanner);
static AstNode* Group(Parser_* parser, Scanner_* scanner);
static AstNode* Expr(Parser_* parser, Scanner_* scanner);
static AstNode* parse_precedence(Parser_* parser, Scanner_* scanner, Precedence precedence);
static AstNode* Statement(Parser_* parser, Scanner_* scanner);
static AstNode* Id(Parser_* parser, Scanner_* scanner);
static AstNode* FunctionDef(Parser_* parser, Scanner_* scanner);
static AstNode* ReturnStatement(Parser_* parser, Scanner_* scanner);
static AstNode* ClassDef(Parser_* parser, Scanner_* scanner);
static AstNode* FunctionCallArgs(Parser_* parser, Scanner_* scanner);
static AstNode* FunctionCallArg(Parser_* parser, Scanner_* scanner);
static AstNode* TypeDef(Parser_* parser, Scanner_* scanner);

static bool parse_value(Parser_* parser, Value_* ret);
static bool parse_value_expr(Parser_* parser, Value_* ret);

static void advance(Parser_* parser, Scanner_* scanner);
static void consume(Parser_* parser, Scanner_* scanner, TokenType_ type, const char* message);
static bool match(Parser_* parser, Scanner_* scanner, TokenType_ type);
static bool check(Parser_* parser, TokenType_ type);

static ParseRule_* ExprParseRule(TokenType_ type);

static void error_at_current(Parser_* parser, const char* message, ...);
static void error(Parser_* parser, const char* message, ...);
static bool block_follow(Parser_* parser, Scanner_* scanner, bool withuntil);
static void statement_list(Parser_* parser, Scanner_* scanner, AstList_* statements);

// Returns true if the current token is in the follow set of a block.
// 'until' closes syntactical blocks, but do not close scope,
// so it is handled in separate. This is used, for example, when determining if
// a function ends with a simple `return` or with a `return expr`.
static bool block_follow(Parser_* parser, Scanner_* scanner, bool withuntil) {
  switch (current_.type) {
    case TK_ELSE:
    case TK_ELIF:
    case TK_END:
    case TK_EOF:
    case TK_CASE:
      return true;

    case TK_UNTIL:
      return withuntil;

    default:
      return false;
  }
}

AstNode* parse_cst(Parser_* parser, Scanner_* scanner, struct Scope_* root_scope, const char* source) {
  advance(parser, scanner);  
  AstProgram_* root = MakeNode<AstProgram>(current_.line);

  root->block = (AstBlock_*)Block(parser, scanner);

  consume(parser, scanner, TK_EOF, "Expected end of file.");
  return (AstNode*)root;
}

AstNode* make_primitive_node(TokenType_ tk, MemoryAllocator_* allocator, int line) {
  AstPrimitiveType_* ret = MakeNode<AstPrimitiveType>(line);
  ret->type = tk;

  return (AstNode*)ret;
}

AstNode* make_id_node(Token_ id, MemoryAllocator_* allocator, int line) {
  AstIdType_* ret = MakeNode<AstIdType>(line);
  ret->id = id;

  return (AstNode*)ret;
}

static AstNode* parse_unary_type(Parser_* parser, Scanner_* scanner) {
  Token_ token = previous_;
  AstNode* ret = nullptr;
  switch (token.type) {
    case TK_BOOL:
    case TK_INT:
    case TK_UINT:
    case TK_FLOAT:
    case TK_DOUBLE:
    case TK_STRING_TYPE:
    case TK_NIL:
      ret = make_primitive_node(token.type, parser->allocator, current_.line);
      break;
    case TK_FUNCTION:
    {
      error_at_current(parser, "Function typed values are unimplemented.");
      return ret;
    }
    case TK_ID:
      ret = make_id_node(parse_variable(parser, &token), parser->allocator, current_.line);
      break;
    default:
      errors_->Err(previous_.line, "encountered unknown token type when parsing type.");
      return ret;
  }

  if (match(parser, scanner, TK_LBRACKET)) {
    do {
      AstGenericOrArrayType_* generic_type = MakeNode<AstGenericOrArrayType>(token.line);
      generic_type->prefix = ret;
      cstlist_init(&generic_type->args, parser->allocator);
      if (!match(parser, scanner, TK_RBRACKET)) {
        do {
          advance(parser, scanner);

          AstNode* arg = nullptr;
          Value_ maybe_val = { 0 };
          if (parse_value(parser, &maybe_val)) {
            AstPrimaryExp_* exp = MakeNode<AstPrimaryExp>(previous_.line);
            exp->value = maybe_val;
            exp->type = previous_.type;
            arg = (AstNode*)exp;
          } else {
            arg = parse_unary_type(parser, scanner);
          }
          cstlist_append(&generic_type->args, arg);

        } while (match(parser, scanner, TK_COMMA));
        ret = (AstNode*)generic_type;

        if (!match(parser, scanner, TK_RBRACKET)) {
          errors_->Err(previous_.line, "type malformed, expected ']' at end of type.");
          return ret;
        }
      } else {
        ret = (AstNode*)generic_type;
      }
    } while (match(parser, scanner, TK_LBRACKET));
  }

  if (!ret) {
    errors_->Err(previous_.line, "Encountered unknown token type when trying to parse type expression: %d", token.type);
  }

  return ret;
}

static AstNode* parse_singleton_or_union_type(Parser_* parser, Scanner_* scanner) {
  AstNode* single_type = nullptr;
  AstUnionType_* union_type = nullptr;
  AstList_ union_types = { 0 };
  
  bool try_parsing_union = false;
  do {
    if (try_parsing_union) {
      if (union_types.count == 0) {
        cstlist_init(&union_types, parser->allocator);
        cstlist_append(&union_types, single_type);
      }

      advance(parser, scanner);
    }

    single_type = parse_unary_type(parser, scanner);

    if (try_parsing_union) {
      cstlist_append(&union_types, single_type);
    }

    try_parsing_union = true;
  } while (match(parser, scanner, TK_PIPE));

  if (union_type) {
    union_type = MakeNode<AstUnionType>(current_.line);
    union_type->types = union_types;
    return (AstNode*)union_type;
  } else {
    return single_type;
  }
}

static AstNode* parse_singleton_or_tuple_type(Parser_* parser, Scanner_* scanner) {
  AstNode* single_type = nullptr;
  AstTupleType_* tuple_type = nullptr;
  AstList_ tuple_types = { 0 };

  bool try_parsing_tuple = false;
  do {
    if (try_parsing_tuple) {
      if (tuple_types.count == 0) {
        cstlist_init(&tuple_types, parser->allocator);
        cstlist_append(&tuple_types, single_type);
      }
      advance(parser, scanner);
    }

    single_type = parse_singleton_or_union_type(parser, scanner);

    if (try_parsing_tuple) {
      cstlist_append(&tuple_types, (void*)single_type);
    }

    try_parsing_tuple = true;
  } while (match(parser, scanner, TK_COMMA));

  if (tuple_type) {
    tuple_type = MakeNode<AstTupleType>(current_.line);
    tuple_type->types = tuple_types;
    return (AstNode*)tuple_type;
  } else {
    return single_type;
  }
}

static AstNode* parse_type_expr(Parser_* parser, Scanner_* scanner) {
  return parse_singleton_or_union_type(parser, scanner);
}

static AstNode* parse_type(Parser_* parser, Scanner_* scanner) {
  advance(parser, scanner);
  return parse_type_expr(parser, scanner);
}

static void statement_list(Parser_* parser, Scanner_* scanner, AstList_* statements) {
  // TODO: how to handle the 'pass' keyword? Should it stop parsing statement lists?
  while (!block_follow(parser, scanner, true)) {
    
    AstNode* statement = Statement(parser, scanner);
    cstlist_append(statements, statement);

    if (parser->panic_mode) {
      synchronize(parser, scanner);
    }

    if (previous_.type == TK_RETURN) {
      return;
    }
  }
}

static void ExprList(Parser_* parser, Scanner_* scanner, AstList_* exprs) {
  TokenType_ tk = current_.type;
  do {
    cstlist_append(exprs, Expr(parser, scanner));
  } while (match(parser, scanner, TK_COMMA));
}

static AstNode* GenericParam(Parser_* parser, Scanner_* scanner) {
  if (!match(parser, scanner, TK_ID)) {
    errors_->Err(previous_.line, "");
    return nullptr;
  }

  AstGenericParam_* param = MakeNode<AstGenericParam>(current_.line);
  param->name = parse_variable(parser, &previous_);
  cstlist_init(&param->constraints, parser->allocator);

  if (match(parser, scanner, TK_COLON)) {
    do {
      AstNode* type = parse_type(parser, scanner);
      cstlist_append(&param->constraints, type);
    } while (match(parser, scanner, TK_AMPERSAND));
  }
  
  return (AstNode*)param;
}

void GenericParams(Parser_* parser, Scanner_* scanner, AstList_* params) {
  if (!match(parser, scanner, TK_LBRACKET)) {
    return;
  }

  do {
    AstNode* param = GenericParam(parser, scanner);
    if (param) {
      cstlist_append(params, param);
    }
  } while (match(parser, scanner, TK_COMMA));

  consume(parser, scanner, TK_RBRACKET, "expected ']' at end of generic parameters");
}

static AstNode* FunctionParam(Parser_* parser, Scanner_* scanner) {
  AstFunctionParam_* param = MakeNode<AstFunctionParam>(current_.line);

  if (match(parser, scanner, TK_IN)) {
    param->kind = TK_IN;
  } else if (match(parser, scanner, TK_OUT)) {
    param->kind = TK_OUT;
  } else {
    param->kind = 0;
  }

  consume(parser, scanner, TK_ID, "Expecting parameter name");
  param->name = parse_variable(parser, &previous_);

  consume(parser, scanner, TK_COLON, "Expecting colon after parameter name.");
  param->type = parse_type(parser, scanner);

  return (AstNode*)param;
}

static AstNode* FunctionDef(Parser_* parser, Scanner_* scanner) {
  MemoryAllocator_* allocator = parser->allocator;
  AstFunctionDef_* def = MakeNode<AstFunctionDef>(current_.line);
  AstGenericFunctionDef_* generic_def = nullptr;

  if (match(parser, scanner, TK_ID)) {
    def->name = parse_variable(parser, &previous_);
  } else {
    def->name = (Token_) { 0 };
  }

  if (check(parser, TK_LBRACKET)) {
    generic_def = MakeNode<AstGenericFunctionDef>(current_.line);
    generic_def->function_def = def;
    cstlist_init(&generic_def->generic_params, parser->allocator);
    GenericParams(parser, scanner, &generic_def->generic_params);
  }

  cstlist_init(&def->function_params, allocator);
  consume(parser, scanner, TK_LPAREN, "Expected a '(' for the start of a function's parameter list.");
  if (!match(parser, scanner, TK_RPAREN)) {
    do {
      if (match(parser, scanner, TK_TRIPLE_DOT)) {
        // TODO: implement variadic args
        errors_->Err(previous_.line, "Variadic args are unimplemented.");
        break;
      }

      AstFunctionParam_* param = (AstFunctionParam_*)FunctionParam(parser, scanner);
      cstlist_append(&def->function_params, (AstNode*)param);
    } while (match(parser, scanner, TK_COMMA));
    consume(parser, scanner, TK_RPAREN, "Expected ')' after arguments.");
  }

  if (match(parser, scanner, TK_ARROW)) {
    def->return_type = parse_type(parser, scanner);
  } else {
    def->return_type = make_primitive_node(TK_NIL, allocator, current_.line);
  }

  if (match(parser, scanner, TK_END)) {
    def->body = MAKE_CST_NOOP(allocator);
  } else {
    def->body = BlockStatement(parser, scanner);
  }

  return generic_def ? (AstNode*)generic_def : (AstNode*)def;
}

static AstNode* ReturnStatement(Parser_* parser, Scanner_* scanner) {
  AstReturnStmt_* stmt = MakeNode<AstReturnStmt>(current_.line);
  if (block_follow(parser, scanner, true) || current_.type == ';') {
    stmt->expr = (AstNode*)MakeNode<AstNoopExpr>(0);
  } else {
    stmt->expr = Expr(parser, scanner);
  }

  return (AstNode*)stmt;
}

static AstNode* ClassMemberDecl(Parser_* parser, Scanner_* scanner) {
  AstClassMemberDecl_* decl = MakeNode<AstClassMemberDecl>(current_.line);

  TokenType_ kind = 0;
  if (match(parser, scanner, TK_VAL)) {
    kind = TK_VAL;
  } else if (match(parser, scanner, TK_VAR)) {
    kind = TK_VAR;
  }

  consume(parser, scanner, TK_ID, "Class field must include a name.");
  decl->name = parse_variable(parser, &previous_);

  consume(parser, scanner, TK_COLON, "Expected a ':' after class member name.");
  AstNode* parsed_type = parse_type(parser, scanner);
  if (kind == TK_VAR) {
    AstReferenceType_* ref = MakeNode<AstReferenceType>(current_.line);
    ref->type = parsed_type;
    decl->field_type = (AstNode*)ref;
  } else {
    decl->field_type = parsed_type;
  }

  if (match(parser, scanner, TK_EQUAL)) {
    decl->opt_expr = Expr(parser, scanner);
  }

  return (AstNode*)decl;
}

static AstNode* ClassDef(Parser_* parser, Scanner_* scanner) {
  AstClassDef_* def = MakeNode<AstClassDef>(current_.line);
  cstlist_init(&def->members, parser->allocator); 

  consume(parser, scanner, TK_ID, "Class definition must include a name.");
  def->name = parse_variable(parser, &previous_);
  cstlist_init(&def->generic_params, parser->allocator);
  GenericParams(parser, scanner, &def->generic_params);

  while (check(parser, TK_ID) || check(parser, TK_VAL) || check(parser, TK_VAR)) {
    AstClassMemberDecl_* decl = (AstClassMemberDecl_*)ClassMemberDecl(parser, scanner);
    cstlist_append(&def->members, (AstNode*)decl);

    if (check(parser, TK_END)) {
      break;
    }

    match(parser, scanner, TK_COMMA);
  }

  consume(parser, scanner, TK_END, "Expected 'end' at the end of a class definition.");

  return (AstNode*)def;
}

static AstNode* VarDecl(Parser_* parser, Scanner_* scanner, TokenType_ decl_token) {  
  AstVarDeclStmt_* stmt = MakeNode<AstVarDeclStmt>(current_.line);
  consume(parser, scanner, TK_ID, "Expected variable name.");
  stmt->name = parse_variable(parser, &previous_);
  stmt->decl_token = decl_token;

  if (!match(parser, scanner, TK_COLON)) {
    errors_->Err(previous_.line, "Expected a ':' for a variable declaration.");
    return (AstNode*)stmt;
  }
  

  if (!check(parser, TK_EQUAL)) {
    stmt->opt_type = parse_type(parser, scanner);
  }

  if (match(parser, scanner, TK_EQUAL)) {
    stmt->expr = Expr(parser, scanner);
  } else if (!stmt->opt_type) {
    error_at_current(parser, "Expected an expression for a deduced type variable.");
  }

  return (AstNode*)stmt;
}

static void advance(Parser_* parser, Scanner_* scanner) {
  previous_ = current_;

  for (;;) {
    current_ = scanner_scan(scanner);
    if (current_.type != TK_ERR) break;

    error_at_current(parser, current_.start);
  }
}

static void consume(Parser_* parser, Scanner_* scanner, TokenType_ type, const char* message) {
  if (current_.type == type) {
    advance(parser, scanner);
    return;
  }

  error_at_current(parser, message);
}

static bool match(Parser_* parser, Scanner_* scanner, TokenType_ type) {
  if (!check(parser, type)) return false;
  advance(parser, scanner);
  return true;
}

static bool check(Parser_* parser, TokenType_ type) {
  return current_.type == type;
}

///////////////////////////////////////////////////////////////////////////////

static AstNode* Type(Parser_* parser, Scanner_* scanner) {
  AstType_* type = MakeNode<AstType>(previous_.line);
  type->impl = parse_type_expr(parser, scanner);
  return (AstNode*)type;
}

static AstNode* Group(Parser_* parser, Scanner_* scanner) {
  AstNode* expr = Expr(parser, scanner);
  consume(parser, scanner, TK_RPAREN, "Expect ')' after expression.");

  return expr;
}

static AstNode* ArrayValue(Parser_* parser, Scanner_* scanner) {
  AstArrayValueExpr_* array = MakeNode<AstArrayValueExpr>(previous_.line);
  cstlist_init(&array->values, parser->allocator);
  if (match(parser, scanner, TK_RBRACKET)) {
    return (AstNode*)array;
  }

  do {
    cstlist_append(&array->values, Expr(parser, scanner));
  } while (match(parser, scanner, TK_COMMA));

  consume(parser, scanner, TK_RBRACKET, "Expected ']' at end of array definition.");
  return (AstNode*)array;
}

static AstNode* IndexExpr(Parser_* parser, Scanner_* scanner) {
  AstIndexExpr_* array = MakeNode<AstIndexExpr>(previous_.line);
  array->index = Expr(parser, scanner);
  consume(parser, scanner, TK_RBRACKET, "Expected ']' at end of array definition.");
  return (AstNode*)array;
}

static AstNode* IndexOrGenericArgs(Parser_* parser, Scanner_* scanner) {
  AstIndexOrGenericArgs_* node = MakeNode<AstIndexOrGenericArgs>(previous_.line);
  cstlist_init(&node->args, parser->allocator);
  do {
    cstlist_append(&node->args, Expr(parser, scanner));
  } while (!match(parser, scanner, TK_RBRACKET) && match(parser, scanner, TK_COMMA));

  if (previous_.type != TK_RBRACKET) {
    error_at_current(parser, "Malformed type or array. Expected ']' at end.");
  }
  return (AstNode*)node;
}

static AstNode* UnaryOp(Parser_* parser, Scanner_* scanner) {
  TokenType_ operator_type = previous_.type;

  AstUnaryExp_* unary = MakeNode<AstUnaryExp>previous_.line);
  unary->op = operator_type;
  unary->expr = parse_precedence(parser, scanner, PREC_UNARY);

  return (AstNode*)unary;
}

static AstNode* BinaryOp(Parser_* parser, Scanner_* scanner) {
  TokenType_ operator_type = previous_.type;
  ParseRule_* rule = ExprParseRule(operator_type);  
  AstNode* expr = parse_precedence(parser, scanner, (Precedence)(rule->precedence + 1));
  return expr;
}

static AstNode* InPlaceBinaryOp(Parser_* parser, Scanner_* scanner) {
  TokenType_ operator_type = previous_.type;
  ParseRule_* rule = ExprParseRule(operator_type);
  AstNode* expr = parse_precedence(parser, scanner, (Precedence)(rule->precedence + 1));
  return expr;
}

static AstNode* Id(Parser_* parser, Scanner_* scanner) {
  AstVarExpr_* var_expr = MakeNode<AstVarExpr>(previous_.line);
  AstIdExpr_* expr = MakeNode<AstIdExpr>(previous_.line);
  var_expr->expr = (AstNode*)expr;
  expr->name = parse_variable(parser, &previous_);
  return (AstNode*)var_expr;
}

static AstNode* FunctionCallArgs(Parser_* parser, Scanner_* scanner) {
  AstFunctionCallArgs_* args = MakeNode<AstFunctionCallArgs>(current_.line);
  cstlist_init(&args->args, parser->allocator);

  if (match(parser, scanner, TK_RPAREN)) {
    return (AstNode*)args;
  }

  do {
    cstlist_append(&args->args, Expr(parser, scanner));
  } while (match(parser, scanner, TK_COMMA));

  consume(parser, scanner, TK_RPAREN, "Expected a ')' after a function call.");
  return (AstNode*)args;
}

static AstNode* ClassConstructorParam(Parser_* parser, Scanner_* scanner) {
  AstClassConstructorParam_* param = MakeNode<AstClassConstructorParam>(previous_.line);
  AstNode* field_expr = Expr(parser, scanner);

  if (field_expr->cls == typeid(AstAssignmentExpr_)) {
    AstAssignmentExpr_* assignment_exp = ast_cast<AstAssignmentExpr>(field_expr);
    AstVarExpr_* var_exp = ast_cast<AstVarExpr>(assignment_exp->left);
    AstIdExpr_* id_exp = ast_cast<AstIdExpr>(var_exp->expr);
    param->expr = assignment_exp->right;
    param->name = id_exp->name;
  } else {
    param->expr = field_expr;
  }

  return (AstNode*)param;
}

static AstNode* ClassConstructor(Parser_* parser, Scanner_* scanner) {
  AstClassConstructor_* constructor = MakeNode<AstClassConstructor>(previous_.line);  
  cstlist_init(&constructor->params, parser->allocator);
  
  if (match(parser, scanner, TK_RBRACE)) {
    return (AstNode*)constructor;
  }

  bool parsing_named_params = false;
  do {
    // Allow for trailing commas in field list.
    if (check(parser, TK_RBRACE)) {
      break;
    }

    AstClassConstructorParam_* param = ast_cast<AstClassConstructorParam_>(
      ClassConstructorParam(parser, scanner));

    if (param->name.start) {
      parsing_named_params = true;
    } else if (parsing_named_params) {
      errors_->Err(previous_.line, "Constructor parameters must have all named parameters at the end.");
    }
    cstlist_append(&constructor->params, (AstNode*)param);
  } while (match(parser, scanner, TK_COMMA));

  consume(parser, scanner, TK_RBRACE, "Expected a '}' to terminate a struct constructor.");
  return (AstNode*)constructor;
}

static AstTypeMemberDecl_* TypeMemberDecl(Parser_* parser, Scanner_* scanner) {
  AstTypeMemberDecl_* member = MakeNode<AstTypeMemberDecl>(previous_.line);
  
  if (match(parser, scanner, TK_ID)) {
    if (check(parser, TK_COLON)) {
      member->opt_name = parse_variable(parser, &previous_);
      advance(parser, scanner);
    }
  }
  member->type = parse_type(parser, scanner);
  return member;
}

static AstNode* TypeDef(Parser_* parser, Scanner_* scanner) {
  AstTypeDef_* type_def = MakeNode<AstTypeDef>(previous_.line);
  type_def->name = parse_variable(parser, &current_);
  advance(parser, scanner);

  cstlist_init(&type_def->generic_params, parser->allocator);
  GenericParams(parser, scanner, &type_def->generic_params);

  if (match(parser, scanner, TK_END)) {
    return (AstNode*)type_def;
  }
  advance(parser, scanner);
  type_def->type = parse_singleton_or_tuple_type(parser, scanner);
  consume(parser, scanner, TK_END, "Expected 'end' at the end of a type definition.");

  return (AstNode*)type_def;
}

static AstNode* RangeExpr(Parser_* parser, Scanner_* scanner) {
  return nullptr;
}

static AstNode* PrefixRangeExpr(Parser_* parser, Scanner_* scanner) {
  AstRangeExpr_* expr = MakeNode<AstRangeExpr>(previous_.line);
  
  expr->left = nullptr;
  expr->right = Expr(parser, scanner);

  return (AstNode*)expr;
}

// Recursively replaces all AstVarOrTypeExpr_ nodes with a AstGenericOrArrayType_.
// This is for parsing expressions like:
//   val a := foo[1][3][4]{1, 2, 3} OR
// Where the parse tree is ((((foo[int]) [1]) [3]) [4]) {1, 2, 3}
static void reduce_to_type_expr(AstNode* expr, MemoryAllocator_* allocator) {
  if (expr->cls != typeid(AstVarOrTypeExpr_)) return;

  AstVarOrTypeExpr_* index_or_expr = ast_cast<AstVarOrTypeExpr>(expr);
  index_or_expr->base.cls = typeid(AstType_);

  AstGenericOrArrayType_* generic_or_array = MakeNode<AstGenericOrArrayType>(expr->line);
  generic_or_array->prefix = index_or_expr->prefix;
  generic_or_array->args = index_or_expr->index_args->args;
  index_or_expr->prefix = nullptr;
  index_or_expr->index_args = nullptr;

  AstType_* cst_type = ast_cast<AstType>(expr);
  cst_type->impl = (AstNode*)generic_or_array;

  ReduceToVarExpr((AstNode*)generic_or_array->prefix, allocator);
}

///////////////////////////////////////////////////////////////////////////////

static void error_at(Parser_* parser, Token_* token, const char* const message, va_list argp) {
  if (parser->panic_mode) return;
  parser->panic_mode = true;
  fprintf(stderr, "[line %d] Error", token->line);

  if (token->type == TK_EOF) {
    fprintf(stderr, " at EOF: ");
  }
  else if (token->type == TK_ERR) {
    // Nothing.
  }
  else {
    fprintf(stderr, " at '%.*s': ", token->length, token->start);
  }
  vfprintf(stderr, message, argp);
  fprintf(stderr, "\n");
  parser->had_error = true;
}

static void error_at_current(Parser_* parser, const char* message, ...) {
  va_list argp;
  va_start(argp, message);
  error_at(parser, &current_, message, argp);
  va_end(argp);
}

static void error(Parser_* parser, const char* message, ...) {
  va_list argp;
  va_start(argp, message);
  error_at(parser, &previous_, message, argp);
  va_end(argp);
}
#endif
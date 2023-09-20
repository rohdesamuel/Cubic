#include "parser.h"
#include "memory.h"
#include "symbol_table.h"
#include "object.h"

#include <memory.h>
#include <stdarg.h>
#include <string.h>

#pragma warning(3 : 4062)

typedef enum {
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
} Precedence;

typedef AstNode_* (*ParseFn)(Parser_*, Scanner_*, Scope_*);

typedef struct ParseRule_ {
  ParseFn prefix;
  ParseFn infix;
  Precedence precedence;
} ParseRule_;

static AstNode_* Number(Parser_* parser, Scanner_* scanner, Scope_* scope);
static AstNode_* UnaryOp(Parser_* parser, Scanner_* scanner, Scope_* scope);
static AstNode_* BinaryOp(Parser_* parser, Scanner_* scanner, Scope_* scope);
static AstNode_* InPlaceBinaryOp(Parser_* parser, Scanner_* scanner, Scope_* scope);
static AstNode_* grouping(Parser_* parser, Scanner_* scanner, Scope_* scope);
static AstNode_* Expr(Parser_* parser, Scanner_* scanner, Scope_* scope);
static AstNode_* parse_precedence(Parser_* parser, Scanner_* scanner, Precedence precedence, Scope_* scope);
static AstNode_* Statement(Parser_* parser, Scanner_* scanner, Scope_* scope);
static AstNode_* Block(Parser_* parser, Scanner_* scanner, Scope_* scope);
static AstNode_* Id(Parser_* parser, Scanner_* scanner, Scope_* scope);
static AstNode_* FunctionDef(Parser_* parser, Scanner_* scanner, Scope_* scope);
static AstNode_* ReturnStatement(Parser_* parser, Scanner_* scanner, Scope_* scope);
static AstNode_* ClassDef(Parser_* parser, Scanner_* scanner, Scope_* scope);
static AstNode_* ClassMemberDecl(Parser_* parser, Scanner_* scanner, Scope_* scope);
static AstNode_* FunctionCallArgs(Parser_* parser, Scanner_* scanner, Scope_* scope);
static AstNode_* FunctionCallArg(Parser_* parser, Scanner_* scanner, Scope_* scope);

static void advance(Parser_* parser, Scanner_* scanner);
static void consume(Parser_* parser, Scanner_* scanner, TokenType_ type, const char* message);
static bool match(Parser_* parser, Scanner_* scanner, TokenType_ type);
static bool check(Parser_* parser, TokenType_ type);

static ParseRule_* get_rule(TokenType_ type);

static void error_at_current(Parser_* parser, const char* message, ...);
static void error(Parser_* parser, const char* message, ...);
static bool block_follow(Parser_* parser, Scanner_* scanner, bool withuntil);
static void statement_list(Parser_* parser, Scanner_* scanner, AstList_* statements, Scope_* scope);

void parser_init(Parser_* parser, MemoryAllocator_* allocator) {
  *parser = (Parser_){ 0 };
  parser->allocator = allocator;
}

void parser_clear(Parser_* parser) {
  *parser = (Parser_){ 0 };
}

static AstList_* astlist_create(struct MemoryAllocator_* allocator) {
  AstList_* ret = alloc(allocator, sizeof(AstList_));
  memset(ret, 0, sizeof(AstList_));
  ret->allocator = allocator;
  return ret;
}

static void astlist_init(AstList_* list, struct MemoryAllocator_* allocator) {
  memset(list, 0, sizeof(AstList_));
  list->allocator = allocator;
}

static void astlist_clear(AstList_* list) {
  struct MemoryAllocator_* allocator = list->allocator;

  AstListNode_* cur = list->head;
  while (cur) {
    AstListNode_* next = cur->next;
    dealloc(allocator, cur);
    cur = next;
  }

  list->count = 0;
}

static void astlist_destroy(AstList_** list) {
  struct MemoryAllocator_* allocator = (*list)->allocator;

  astlist_clear(*list);
  dealloc(allocator, *list);
  *list = NULL;
}

static void astlist_append(AstList_* list, AstNode_* node) {
  if (!node) return;

  struct MemoryAllocator_* allocator = list->allocator;
  AstListNode_* n = alloc(allocator, sizeof(AstListNode_));
  n->node = node;
  
  if (!list->head) {
    list->head = n;
  }

  if (list->tail) {
    list->tail->next = n;    
  }

  list->tail = n;
  ++list->count;
}

// Returns true if the current token is in the follow set of a block.
// 'until' closes syntactical blocks, but do not close scope,
// so it is handled in separate. This is used, for example, when determining if
// a function ends with a simple `return` or with a `return expr`.
static bool block_follow(Parser_* parser, Scanner_* scanner, bool withuntil) {
  switch (parser->current.type) {
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

AstNode_* parse(Parser_* parser, Scanner_* scanner, struct Scope_* root_scope, const char* source) {
  advance(parser, scanner);  
  AstProgram_* root = MAKE_AST_NODE(parser->allocator, AstProgram_, root_scope, parser->current.line);

  root->block = (AstBlock_*)Block(parser, scanner, root->base.scope);

  consume(parser, scanner, TK_EOF, "Expected end of file.");
  return (AstNode_*)root;
}

Token_ parse_variable(Parser_* parser, Scanner_* scanner, Token token) {
  Token_ ret = *token;
  ret.start = alloc(parser->allocator, ret.length);
  memcpy((char*)ret.start, token->start, token->length);

  return ret;
}

static Type_* parse_type_expr(Parser_* parser, Scanner_* scanner) {
  Token_ token = parser->previous;

  Type_* ret = NULL;

  // The following code, once implemented, allows functions to return references.
  // if (token.type == TK_VAR) {
  //   ret.kind = KIND_REF;
  //   advance(parser, scanner);
  //   token = parser->previous;
  // }

  switch (token.type) {
    case TK_BOOL:        ret = (Type_*)&Bool_Ty;   break;
    case TK_INT:         ret = (Type_*)&Int_Ty;    break;
    case TK_UINT:        ret = (Type_*)&Uint_Ty;   break;
    case TK_FLOAT:       ret = (Type_*)&Float_Ty;  break;
    case TK_DOUBLE:      ret = (Type_*)&Double_Ty; break;
    case TK_STRING_TYPE: ret = (Type_*)&String_Ty; break;
    case TK_FUNCTION:
    {
      error_at_current(parser, "Function typed values are unimplemented.");
      break;
    }
    case TK_ID:
      ret = make_placeholder_ty(parse_variable(parser, scanner, &token), parser->allocator);
      break;
    default: break;
  }

  if (match(parser, scanner, TK_LBRACKET)) {
    size_t count = 0;
    if (match(parser, scanner, TK_INTEGER)) {
      count = strtoll(parser->previous.start, NULL, 10);
    }

    if (!match(parser, scanner, TK_RBRACKET)) {
      error(parser, "array type malformed, expected ']' at end of type.");
      return ret;
    }

    ret = make_array_ty(ret, count, parser->allocator);
  }

  if (!ret) {
    error(parser, "Encountered unknown token type when trying to parse type expression: %d", token.type);
  }

  return ret;
}

static Type_* parse_type(Parser_* parser, Scanner_* scanner) {
  advance(parser, scanner);
  return parse_type_expr(parser, scanner);
}

// Escape edge of the parser state machine from the error state to the initial state.
void synchronize(Parser_* parser, Scanner_* scanner) {
  parser->panic_mode = false;

  // These cases are all the tokens that can start a statement.
  while (parser->current.type != TK_EOF) {
    switch (parser->current.type) {
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
        return;
      default:
        ;
    }

    advance(parser, scanner);
  }
}

static void statement_list(Parser_* parser, Scanner_* scanner, AstList_* statements, Scope_* scope) {
  // TODO: how to handle the 'pass' keyword? Should it stop parsing statement lists?
  while (!block_follow(parser, scanner, true)) {
    
    AstNode_* statement = Statement(parser, scanner, scope);
    astlist_append(statements, statement);

    if (parser->panic_mode) {
      synchronize(parser, scanner);
    }

    if (parser->previous.type == TK_RETURN) {
      return;
    }
  }
}

static void enter_scope(Scope_* scope) {
  Frame_* frame = scope->frame;
  frame_enterscope(frame, scope);
}

static void leave_scope(Scope_* scope) {
  Frame_* frame = scope->frame;
  frame_leavescope(frame, scope);
}

static AstNode_* Block(Parser_* parser, Scanner_* scanner, Scope_* scope) {
  AstBlock_* block = MAKE_AST_NODE(parser->allocator, AstBlock_, scope_createfrom(scope), parser->current.line);

  enter_scope(block->base.scope);
  astlist_init(&block->statements, parser->allocator);
  statement_list(parser, scanner, &block->statements, block->base.scope);
  leave_scope(block->base.scope);

  return (AstNode_*)block;
}

static AstNode_* BlockStatement(Parser_* parser, Scanner_* scanner, Scope_* scope) {
  AstNode_* block = Block(parser, scanner, scope);
  consume(parser, scanner, TK_END, "Expected 'end' at the end of a block.");
  return block;
}

static bool in_assign_first(TokenType_ type) {
  return
    type == TK_ID ||
    type == TK_LPAREN;
}

static void ExprList(Parser_* parser, Scanner_* scanner, AstList_* exprs, Scope_* scope) {
  TokenType_ tk = parser->current.type;
  do {
    astlist_append(exprs, Expr(parser, scanner, scope));
  } while (match(parser, scanner, TK_COMMA));
}

static AstNode_* FunctionParam(Parser_* parser, Scanner_* scanner, Scope_* scope) {
  AstFunctionParam_* param = MAKE_AST_NODE(parser->allocator, AstFunctionParam_, scope, parser->current.line);

  int ref_type = 0;
  if (match(parser, scanner, TK_IN)) {
    ref_type = TYPE_CLS(InType_);
  } else if (match(parser, scanner, TK_OUT)) {
    ref_type = TYPE_CLS(OutType_);
  }

  consume(parser, scanner, TK_ID, "Expecting parameter name");
  param->name = parse_variable(parser, scanner, &parser->previous);

  consume(parser, scanner, TK_COLON, "Expecting colon after parameter name.");
  Type_* type = parse_type(parser, scanner);

  if (ref_type) {
    if (ref_type == TYPE_CLS(InType_)) {
      type = make_in_ty(type, parser->allocator);
    } else if (ref_type == TYPE_CLS(OutType_)) {
      type = make_out_ty(type, parser->allocator);
    }    
  }

  param->type = type;
  frame_addparam(scope->frame, &param->name)->ty = type;

  return (AstNode_*)param;
}

static AstNode_* FunctionBody(Parser_* parser, Scanner_* scanner, Scope_* scope, Symbol_* fn_symbol) {
  AstFunctionBody_* body = MAKE_AST_NODE(parser->allocator, AstFunctionBody_, scope, parser->current.line);
  body->fn_symbol = fn_symbol;
  astlist_init(&body->function_params, parser->allocator);  
  FunctionType_* fn_type = type_as(FunctionType_, body->fn_symbol->ty);

  consume(parser, scanner, TK_LPAREN, "Expected a '(' for the start of a function's parameter list.");
  if (!match(parser, scanner, TK_RPAREN)) {
    do {
      if (match(parser, scanner, TK_TRIPLE_DOT)) {
        // TODO: implement variadic args
        error(parser, "Variadic args are unimplemented.");
        break;
      }

      AstFunctionParam_* param = (AstFunctionParam_*)FunctionParam(parser, scanner, scope);
      list_push(&fn_type->params, &param->type);
      astlist_append(&body->function_params, (AstNode_*)param);
    } while (match(parser, scanner, TK_COMMA));
    consume(parser, scanner, TK_RPAREN, "Expected ')' after arguments.");
  }

  if (match(parser, scanner, TK_ARROW)) {
    body->return_type = parse_type(parser, scanner);
  } else {
    body->return_type = (Type_*)&Nil_Ty;
  }
  fn_type->ret_ty = body->return_type;

  if (match(parser, scanner, TK_END)) {
    body->stmt = MAKE_AST_NOOP(parser->allocator);
  } else {
    body->stmt = BlockStatement(parser, scanner, scope);
  }
  return (AstNode_*)body;
}

static AstNode_* FunctionDef(Parser_* parser, Scanner_* scanner, Scope_* scope) {  
  MemoryAllocator_* allocator = parser->allocator;
  AstFunctionDef_* def = MAKE_AST_NODE(allocator, AstFunctionDef_, scope, parser->current.line);

  Token_ name;
  if (match(parser, scanner, TK_ID)) {
    name = parse_variable(parser, scanner, &parser->previous);
  } else {
    name = (Token_) { 0 };
  }

  Symbol_* fn_symbol = frame_addfn(scope->frame, &name, scope);
  Frame_* fn_frame = frame_createfrom(scope->frame, scope, fn_symbol);
  Scope_* fn_scope = fn_frame->scope;
  fn_symbol->ty = make_function_ty(name, parser->allocator);

  def->fn_symbol = fn_symbol;
  def->body = (AstFunctionBody_*)FunctionBody(parser, scanner, fn_scope, fn_symbol);

  return (AstNode_*)def;
}

static AstNode_* ExpressionStmt(Parser_* parser, Scanner_* scanner, Scope_* scope) {
  AstExpressionStmt_* stmt = MAKE_AST_STMT(parser->allocator, AstExpressionStmt_, scope, parser->current.line);
  stmt->expr = (AstExpr_*)Expr(parser, scanner, scope);
  return (AstNode_*)stmt;
}

static AstNode_* clean_up_temps(Parser_* parser, Scanner_* scanner, Scope_* scope) {
  Frame_* frame = scope->frame;
  AstCleanUpTemps_* ret = MAKE_AST_NODE(parser->allocator, AstCleanUpTemps_, scope, parser->current.line);
  list_of(&ret->tmps, Symbol_*, parser->allocator);

  frame_movetemps(scope->frame, &ret->tmps);

  return (AstNode_*)ret;
}

static AstNode_* ReturnStatement(Parser_* parser, Scanner_* scanner, Scope_* scope) {
  AstReturnStmt_* stmt = MAKE_AST_STMT(parser->allocator, AstReturnStmt_, scope, parser->current.line);
  if (block_follow(parser, scanner, true) || parser->current.type == ';') {
    stmt->expr = (AstExpr_*)MAKE_AST_NODE(parser->allocator, AstNoopExpr_, NULL, 0);
  } else {
    stmt->expr = (AstExpr_*)Expr(parser, scanner, scope);
  }

  return (AstNode_*)stmt;
}

static AstNode_* ClassDef(Parser_* parser, Scanner_* scanner, Scope_* scope) {
  AstClassDef_* def = MAKE_AST_NODE(parser->allocator, AstClassDef_, scope, parser->current.line);
  astlist_init(&def->members, parser->allocator); 

  consume(parser, scanner, TK_ID, "Class definition must include a name.");
  def->name = parse_variable(parser, scanner, &parser->previous);
  Symbol_* cls_sym = frame_addclass(scope->frame, &def->name, scope);

  // consume(parser, scanner, TK_IS, "Expected 'is' after class name.");
  if (!cls_sym) {
    error(parser, "class '%.*s already defined.'", def->name.length, def->name.start);
    return NULL;
  }

  ClassType_* cls_ty = type_as(ClassType_, cls_sym->ty);
  def->class_type = cls_sym->ty;

  static const char constructor_str[] = "__constructor";
  int name_size = sizeof(constructor_str) + def->name.length;
  char* constructor_name = alloc(parser->allocator, name_size);
  sprintf_s(constructor_name, name_size, "%.*s%.*s\0", def->name.length, def->name.start, name_size - 1, constructor_str);

  FunctionType_* constructor_ty = (FunctionType_*)make_function_ty(token_string("constructor"), parser->allocator);
  constructor_ty->ret_ty = cls_sym->ty;
  constructor_ty->self.opt_name = (Token_){ .start = constructor_name, .length = name_size };
  cls_ty->constructor = constructor_ty;

  Scope_* class_scope = scope_createfrom(scope);
  while (check(parser, TK_ID) || check(parser, TK_VAL) || check(parser, TK_VAR)) {
    AstClassMemberDecl_* decl = (AstClassMemberDecl_*)ClassMemberDecl(parser, scanner, class_scope);
    astlist_append(&def->members, (AstNode_*)decl);

    ClassTypeField_ class_field = {
      .type = decl->field_type,
      .name = decl->name,
      .opt_expr = decl->opt_expr,
    };
    list_push(&cls_ty->members, &class_field);

    if (check(parser, TK_END)) {
      break;
    }

    match(parser, scanner, TK_COMMA);
  }

  consume(parser, scanner, TK_END, "Expected 'end' at the end of a class definition.");

  return (AstNode_*)def;
}

static AstNode_* ClassMemberDecl(Parser_* parser, Scanner_* scanner, Scope_* scope) {
  AstClassMemberDecl_* decl = MAKE_AST_NODE(parser->allocator, AstClassMemberDecl_, scope, parser->current.line);

  TokenType_ kind = 0;
  if (match(parser, scanner, TK_VAL)) {
    kind = TK_VAL;
  } else if (match(parser, scanner, TK_VAR)) {
    kind = TK_VAR;
  }

  consume(parser, scanner, TK_ID, "Class field must include a name.");
  decl->name = parse_variable(parser, scanner, &parser->previous);

  consume(parser, scanner, TK_COLON, "Expected a ':' after class member name.");
  Type_* parsed_type = parse_type(parser, scanner);
  if (kind == TK_VAR) {
    decl->field_type = make_var_ty(parsed_type, parser->allocator);
  } else {
    decl->field_type = parsed_type;
  }

  if (match(parser, scanner, TK_EQUAL)) {
    decl->opt_expr = (AstExpr_*)Expr(parser, scanner, scope);
  }

  return (AstNode_*)decl;
}

static AstNode_* VarDecl(Parser_* parser, Scanner_* scanner, Scope_* scope, TokenType_ decl_token) {  
  AstVarDeclStmt_* stmt = MAKE_AST_STMT(parser->allocator, AstVarDeclStmt_, scope, parser->current.line);
  consume(parser, scanner, TK_ID, "Expected variable name.");
  stmt->name = parse_variable(parser, scanner, &parser->previous);

  if (!match(parser, scanner, TK_COLON)) {
    error(parser, "Expected a ':' for a variable declaration.");
    stmt->decl_type = make_unknown_ty(parser->allocator);
    return (AstNode_*)stmt;
  }
  
  if (check(parser, TK_EQUAL)) {
    stmt->decl_type = make_unknown_ty(parser->allocator);
  } else {
    stmt->decl_type = parse_type(parser, scanner);
  }

  if (decl_token == TK_VAR) {
    stmt->decl_type = make_var_ty(stmt->decl_type, parser->allocator);
  }  

  if (match(parser, scanner, TK_EQUAL)) {
    stmt->expr = (AstExpr_*)Expr(parser, scanner, scope);
  } else if (type_isunknown(stmt->decl_type)) {
    error_at_current(parser, "Expected an expression for a deduced type variable.");
  }

  if (!frame_addvar(scope->frame, &stmt->name, stmt->decl_type, scope)) {
    error(parser, "variable '%.*s' already declared.", stmt->name.length, stmt->name.start);
  }

  return (AstNode_*)stmt;
}

static AstNode_* Statement(Parser_* parser, Scanner_* scanner, Scope_* scope) {
  TokenType_ tk = parser->current.type;
  MemoryAllocator_* allocator = parser->allocator;
  AstStmt_* ret = MAKE_AST_NODE(allocator, AstStmt_, scope, parser->current.line);
  ret->cleanup = MAKE_AST_NOOP(allocator);

  switch (tk) {
    case TK_DO:
    {
      advance(parser, scanner);
      ret->stmt = BlockStatement(parser, scanner, scope);
      break;
    }

    case TK_VAL:
    {
      advance(parser, scanner);
      ret->stmt = (AstNode_*)VarDecl(parser, scanner, scope, TK_VAL);
      break;
    }

    case TK_VAR:
    {
      advance(parser, scanner);
      ret->stmt = (AstNode_*)VarDecl(parser, scanner, scope, TK_VAR);
      break;
    }

    case TK_END:
    {
      error_at_current(parser, "Unmatched 'do'");
      ret->stmt = MAKE_AST_NOOP(parser->allocator);
      break;
    }

    case TK_FUNCTION:
    {
      advance(parser, scanner);
      ret->stmt = FunctionDef(parser, scanner, scope);
      break;
    }

    case TK_WHILE:
    {
      advance(parser, scanner);
      AstWhileStmt_* stmt = MAKE_AST_STMT(parser->allocator, AstWhileStmt_, scope, parser->current.line);
      stmt->condition_expr = (AstExpr_*)Expr(parser, scanner, scope);

      consume(parser, scanner, TK_DO, "Expected 'do' at end of while expression.");

      stmt->block_stmt = Block(parser, scanner, scope);

      consume(parser, scanner, TK_END, "Expected 'end' at end of while expression.");

      ret->stmt = (AstNode_*)stmt;
      break;
    }

    case TK_ASSERT:
    {
      advance(parser, scanner);
      AstAssertStmt_* stmt = MAKE_AST_STMT(parser->allocator, AstAssertStmt_, scope, parser->current.line);
      stmt->base.line = parser->current.line;
      stmt->expr = (AstExpr_*)Expr(parser, scanner, scope);
      ret->stmt = (AstNode_*)stmt;
      break;
    }

    // TODO: add parsing multiple return values
    case TK_RETURN:
    {
      advance(parser, scanner);
      ret->stmt = ReturnStatement(parser, scanner, scope);
      break;
    }

    case TK_IF:
    {
      advance(parser, scanner);
      AstIfStmt_* stmt = MAKE_AST_STMT(parser->allocator, AstIfStmt_, scope, parser->current.line);

      stmt->condition_expr = (AstExpr_*)Expr(parser, scanner, scope);
      consume(parser, scanner, TK_THEN, "Expected 'then' after condition.");

      stmt->if_stmt = (AstNode_*)Block(parser, scanner, scope);
      astlist_init(&stmt->elif_stmts, parser->allocator);
      astlist_init(&stmt->elif_exprs, parser->allocator);

      for(;;) {
        if (!match(parser, scanner, TK_ELIF)) {
          break;
        }
        astlist_append(&stmt->elif_exprs, Expr(parser, scanner, scope));
        consume(parser, scanner, TK_THEN, "Expected 'then' after condition.");
        astlist_append(&stmt->elif_stmts, Block(parser, scanner, scope));
      }

      if (match(parser, scanner, TK_ELSE)) {
        stmt->else_stmt = (AstNode_*)Block(parser, scanner, scope);
      } else {
        stmt->else_stmt = NULL;
      }

      consume(parser, scanner, TK_END, "Expected 'end' after if-statement.");

      ret->stmt = (AstNode_*)stmt;
      break;
    }

    case TK_PRINT:
    {
      advance(parser, scanner);
      AstPrintStmt_* stmt = MAKE_AST_STMT(parser->allocator, AstPrintStmt_, scope, parser->current.line);
      stmt->expr = (AstExpr_*)Expr(parser, scanner, scope);
      ret->stmt = (AstNode_*)stmt;
      break;
    }

    case TK_CLASS:
    {
      advance(parser, scanner);
      ret->stmt = ClassDef(parser, scanner, scope);
      break;
    }

    case TK_PASS:
    case TK_SEMICOLON:
    case TK_EOF:
      advance(parser, scanner);
      ret->stmt = MAKE_AST_NOOP(allocator);
      break;

    default:
      ret->stmt = ExpressionStmt(parser, scanner, scope);
      break;
  }
  
  ret->cleanup = clean_up_temps(parser, scanner, scope);
  return (AstNode_*)ret;
}

static void advance(Parser_* parser, Scanner_* scanner) {
  parser->previous = parser->current;

  for (;;) {
    parser->current = scanner_scan(scanner);
    if (parser->current.type != TK_ERR) break;

    error_at_current(parser, parser->current.start);
  }
}

static void consume(Parser_* parser, Scanner_* scanner, TokenType_ type, const char* message) {
  if (parser->current.type == type) {
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
  return parser->current.type == type;
}

///////////////////////////////////////////////////////////////////////////////
static AstNode_* Expr(Parser_* parser, Scanner_* scanner, Scope_* scope) {
  //AstExpr_* ret = MAKE_AST_NODE(parser->allocator, AstExpr_, scope, parser->current.line);
  AstExpr_* ret = (AstExpr_*)parse_precedence(parser, scanner, PREC_NONE, scope);
  if (!ret) {
    return NULL;
  }
  ret->top_type = (Type_*)&Unknown_Ty;
  return (AstNode_*)ret;
}

static AstPrintStmt_* Print(Parser_* parser, Scanner_* scanner, Scope_* scope) {
  AstPrintStmt_* stmt = MAKE_AST_STMT(parser->allocator, AstPrintStmt_, scope, parser->current.line);
  stmt->expr = (AstExpr_*)Expr(parser, scanner, scope);

  return stmt;
}

static AstNode_* Number(Parser_* parser, Scanner_* scanner, Scope_* scope) {
  Token_ token = parser->previous;
  AstPrimaryExp_* expr = MAKE_AST_EXPR(parser->allocator, AstPrimaryExp_, scope, parser->previous.line);

  if (token.start[token.length - 1] == 'f') {
    float value = strtof(parser->previous.start, NULL);
    expr->base.type = (Type_*)&Float_Ty;
    expr->value = FLOAT_VAL(value);
  } else {
    double value = strtod(parser->previous.start, NULL);
    expr->base.type = (Type_*)&Double_Ty;
    expr->value = DOUBLE_VAL(value);
  }
  
  return (AstNode_*)expr;
}

static AstNode_* Integer(Parser_* parser, Scanner_* scanner, Scope_* scope) {
  int64_t value = strtoll(parser->previous.start, NULL, 10);
  AstPrimaryExp_* expr = MAKE_AST_EXPR(parser->allocator, AstPrimaryExp_, scope, parser->previous.line);
  expr->base.type = (Type_*)&Int_Ty;
  expr->value = INT_VAL(value);
  return (AstNode_*)expr;
}

static AstNode_* Nil(Parser_* parser, Scanner_* scanner, Scope_* scope) {
  AstPrimaryExp_* expr = MAKE_AST_EXPR(parser->allocator, AstPrimaryExp_, scope, parser->previous.line);
  expr->base.type = (Type_*)&Nil_Ty;
  expr->value = NIL_VAL;
  return (AstNode_*)expr;
}

static AstNode_* True(Parser_* parser, Scanner_* scanner, Scope_* scope) {
  AstPrimaryExp_* expr = MAKE_AST_EXPR(parser->allocator, AstPrimaryExp_, scope, parser->previous.line);
  expr->base.type = (Type_*)&Bool_Ty;
  expr->value = TRUE_VAL;
  return (AstNode_*)expr;
}

static AstNode_* False(Parser_* parser, Scanner_* scanner, Scope_* scope) {
  AstPrimaryExp_* expr = MAKE_AST_EXPR(parser->allocator, AstPrimaryExp_, scope, parser->previous.line);
  expr->base.type = (Type_*)&Bool_Ty;
  expr->value = FALSE_VAL;
  return (AstNode_*)expr;
}

static AstNode_* String(Parser_* parser, Scanner_* scanner, Scope_* scope) {
  AstPrimaryExp_* expr = MAKE_AST_EXPR(parser->allocator, AstPrimaryExp_, scope, parser->previous.line);
  expr->base.type = (Type_*)&String_Ty;
  expr->value = OBJ_VAL(objstring_from(parser->previous.start + 1, parser->previous.length - 2));
  return (AstNode_*)expr;
}

static AstNode_* Type(Parser_* parser, Scanner_* scanner, Scope_* scope) {
  AstTypeExpr_* type = MAKE_AST_EXPR(parser->allocator, AstTypeExpr_, scope, parser->previous.line);
  type->base.type = parse_type_expr(parser, scanner);
  return (AstNode_*)type;
}

static AstNode_* grouping(Parser_* parser, Scanner_* scanner, Scope_* scope) {
  AstNode_* expr = Expr(parser, scanner, scope);
  consume(parser, scanner, TK_RPAREN, "Expect ')' after expression.");

  return expr;
}

static AstNode_* ArrayValue(Parser_* parser, Scanner_* scanner, Scope_* scope) {
  AstArrayValueExpr_* array = MAKE_AST_EXPR(parser->allocator, AstArrayValueExpr_, scope, parser->previous.line);
  astlist_init(&array->values, parser->allocator);
  if (match(parser, scanner, TK_RBRACKET)) {
    return (AstNode_*)array;
  }

  do {
    astlist_append(&array->values, Expr(parser, scanner, scope));
  } while (match(parser, scanner, TK_COMMA));

  consume(parser, scanner, TK_RBRACKET, "Expected ']' at end of array definition.");
  return (AstNode_*)array;
}

static AstNode_* IndexExpr(Parser_* parser, Scanner_* scanner, Scope_* scope) {
  AstIndexExpr_* array = MAKE_AST_EXPR(parser->allocator, AstIndexExpr_, scope, parser->previous.line);
  array->base.type = make_unknown_ty(parser->allocator);
  array->index = (AstExpr_*)Expr(parser, scanner, scope);
  consume(parser, scanner, TK_RBRACKET, "Expected ']' at end of array definition.");
  return (AstNode_*)array;
}

static AstNode_* UnaryOp(Parser_* parser, Scanner_* scanner, Scope_* scope) {
  TokenType_ operator_type = parser->previous.type;

  AstUnaryExp_* unary = MAKE_AST_EXPR(parser->allocator, AstUnaryExp_, scope, parser->previous.line);
  unary->op = operator_type;
  unary->expr = (AstExpr_*)parse_precedence(parser, scanner, PREC_UNARY, scope);
  unary->base.type = make_unknown_ty(parser->allocator);

  return (AstNode_*)unary;
}

static AstNode_* BinaryOp(Parser_* parser, Scanner_* scanner, Scope_* scope) {
  TokenType_ operator_type = parser->previous.type;
  ParseRule_* rule = get_rule(operator_type);  
  AstNode_* expr = parse_precedence(parser, scanner, (Precedence)(rule->precedence + 1), scope);
  return expr;
}

static AstNode_* InPlaceBinaryOp(Parser_* parser, Scanner_* scanner, Scope_* scope) {
  TokenType_ operator_type = parser->previous.type;
  ParseRule_* rule = get_rule(operator_type);
  AstNode_* expr = parse_precedence(parser, scanner, (Precedence)(rule->precedence + 1), scope);
  return expr;
}

static AstNode_* Id(Parser_* parser, Scanner_* scanner, Scope_* scope) {
  AstVarExpr_* var_expr = MAKE_AST_EXPR(parser->allocator, AstVarExpr_, scope, parser->previous.line);
  AstIdExpr_* expr = MAKE_AST_EXPR(parser->allocator, AstIdExpr_, scope, parser->previous.line);
  var_expr->expr = (AstExpr_*)expr;
  expr->name = parse_variable(parser, scanner, &parser->previous);
  return (AstNode_*)var_expr;
}

static AstNode_* FunctionCallArgs(Parser_* parser, Scanner_* scanner, Scope_* scope) {
  AstFunctionCallArgs_* args = MAKE_AST_NODE(parser->allocator, AstFunctionCallArgs_, scope, parser->current.line);
  astlist_init(&args->args, parser->allocator);

  if (match(parser, scanner, TK_RPAREN)) {
    return (AstNode_*)args;
  }

  do {
    astlist_append(&args->args, FunctionCallArg(parser, scanner, scope));
  } while (match(parser, scanner, TK_COMMA));

  consume(parser, scanner, TK_RPAREN, "Expected a ')' after a function call.");
  return (AstNode_*)args;
}

static AstNode_* FunctionCallArg(Parser_* parser, Scanner_* scanner, Scope_* scope) {
  AstFunctionCallArg_* arg = MAKE_AST_NODE(parser->allocator, AstFunctionCallArg_, scope, parser->current.line);

  arg->expr = (AstExpr_*)Expr(parser, scanner, scope);

  return (AstNode_*)arg;
}

static AstNode_* ClassConstructorParam(Parser_* parser, Scanner_* scanner, Scope_* scope) {
  AstClassConstructorParam_* param = MAKE_AST_EXPR(parser->allocator, AstClassConstructorParam_, scope, parser->previous.line);
  AstExpr_* field_expr = (AstExpr_*)Expr(parser, scanner, scope);

  if (field_expr->base.cls == AST_CLS(AstAssignmentExpr_)) {
    AstAssignmentExpr_* assignment_exp = AST_CAST(AstAssignmentExpr_, field_expr);
    AstVarExpr_* var_exp = AST_CAST(AstVarExpr_, assignment_exp->left);
    AstIdExpr_* id_exp = AST_CAST(AstIdExpr_, var_exp->expr);
    param->expr = assignment_exp->right;
    param->name = id_exp->name;
  } else {
    param->expr = field_expr;
  }

  return (AstNode_*)param;
}

static AstNode_* ClassConstructor(Parser_* parser, Scanner_* scanner, Scope_* scope) {
  AstClassConstructor_* constructor = MAKE_AST_EXPR(parser->allocator, AstClassConstructor_, scope, parser->previous.line);
  astlist_init(&constructor->params, parser->allocator);
  
  if (match(parser, scanner, TK_RBRACE)) {
    return (AstNode_*)constructor;
  }

  bool parsing_named_params = false;
  do {
    // Allow for trailing commas in field list.
    if (check(parser, TK_RBRACE)) {
      break;
    }

    AstClassConstructorParam_* param = AST_CAST(
      AstClassConstructorParam_, ClassConstructorParam(parser, scanner, scope));

    if (param->name.start) {
      parsing_named_params = true;
    } else if (parsing_named_params) {
      error(parser, "Constructor parameters must have all named parameters at the end.");
    }
    astlist_append(&constructor->params, (AstNode_*)param);
  } while (match(parser, scanner, TK_COMMA));

  consume(parser, scanner, TK_RBRACE, "Expected a '}' to terminate a struct constructor.");
  return (AstNode_*)constructor;
}

static AstNode_* parse_precedence(Parser_* parser, Scanner_* scanner, Precedence precedence, Scope_* scope) {
  advance(parser, scanner);
  ParseFn prefix_rule = get_rule(parser->previous.type)->prefix;
  if (!prefix_rule) {
    error(parser, "Expected expression");
    return NULL;
  }

  AstNode_* exp = prefix_rule(parser, scanner, scope);
  while (precedence <= get_rule(parser->current.type)->precedence) {
    ParseFn infix_rule = get_rule(parser->current.type)->infix;
    if (!infix_rule) {
      break;
    }
    advance(parser, scanner);

    if (parser->previous.type == TK_LPAREN) {
      AstFunctionCall_* new_exp = MAKE_AST_EXPR(parser->allocator, AstFunctionCall_, scope, parser->previous.line);
      new_exp->prefix = (AstExpr_*)exp;
      new_exp->args = (AstFunctionCallArgs_*)infix_rule(parser, scanner, scope);      
      exp = (AstNode_*)new_exp;
    } else if (parser->previous.type == TK_EQUAL) {
      AstAssignmentExpr_* new_exp = MAKE_AST_EXPR(parser->allocator, AstAssignmentExpr_, scope, parser->previous.line);
      new_exp->left = (AstExpr_*)exp;
      new_exp->right = (AstExpr_*)infix_rule(parser, scanner, scope);
      exp = (AstNode_*)new_exp;
    } else if (parser->previous.type == TK_DOT) {
      AstVarExpr_* var_exp = MAKE_AST_EXPR(parser->allocator, AstVarExpr_, scope, parser->previous.line);
      AstDotExpr_* new_exp = MAKE_AST_EXPR(parser->allocator, AstDotExpr_, scope, parser->previous.line);
      var_exp->expr = (AstExpr_*)new_exp;
      new_exp->prefix = (AstExpr_*)exp;
      new_exp->id = parse_variable(parser, scanner, &parser->current);
      advance(parser, scanner);
      exp = (AstNode_*)var_exp;
    } else if (parser->previous.type == TK_LBRACE) {
      AstClassConstructor_* new_exp = AST_CAST(AstClassConstructor_, infix_rule(parser, scanner, scope));
      if (exp->cls == AST_CLS(AstTypeExpr_)) {
        AstTypeExpr_* type_expr = AST_CAST(AstTypeExpr_, exp);
        new_exp->base.type = type_expr->base.type;
      } else if (exp->cls == AST_CLS(AstVarExpr_)) {
        AstVarExpr_* var_expr = AST_CAST(AstVarExpr_, exp);
        AstIdExpr_* id_expr = AST_CAST(AstIdExpr_, var_expr->expr);
        new_exp->name = id_expr->name;
        new_exp->base.type = make_placeholder_ty(new_exp->name, parser->allocator);
      }
      dealloc(parser->allocator, exp);
      exp = (AstNode_*)new_exp;
    } else if (parser->previous.type == TK_LBRACKET) {
      AstIndexExpr_* new_exp = AST_CAST(AstIndexExpr_, infix_rule(parser, scanner, scope));
      new_exp->prefix = (AstExpr_*)exp;

      AstVarExpr_* var_exp = MAKE_AST_EXPR(parser->allocator, AstVarExpr_, scope, parser->previous.line);
      var_exp->expr = (AstExpr_*)new_exp;
      exp = (AstNode_*)var_exp;
    } else if (parser->previous.type >= TK_PLUS_EQUAL && parser->previous.type <= TK_SLASH_SLASH_EQUAL) {
      AstInPlaceBinaryStmt_* bin_exp =
        MAKE_AST_EXPR(parser->allocator, AstInPlaceBinaryStmt_, scope, parser->previous.line);

      bin_exp->base.type = make_unknown_ty(parser->allocator);
      bin_exp->op = parser->previous.type;
      bin_exp->left = (AstExpr_*)exp;
      bin_exp->right = (AstExpr_*)infix_rule(parser, scanner, scope);

      AstTmpDecl_* tmp_decl = MAKE_AST_EXPR(parser->allocator, AstTmpDecl_, scope, parser->previous.line);
      tmp_decl->expr = (AstExpr_*)bin_exp;

      exp = (AstNode_*)tmp_decl;
    } else {
      AstBinaryExp_* bin_exp = MAKE_AST_EXPR(parser->allocator, AstBinaryExp_, scope, parser->previous.line);
      bin_exp->base.type = make_unknown_ty(parser->allocator);
      bin_exp->op = parser->previous.type;
      bin_exp->left = (AstExpr_*)exp;
      bin_exp->right = (AstExpr_*)infix_rule(parser, scanner, scope);

      AstTmpDecl_* tmp_decl = MAKE_AST_EXPR(parser->allocator, AstTmpDecl_, scope, parser->previous.line);
      tmp_decl->expr = (AstExpr_*)bin_exp;

      exp = (AstNode_*)tmp_decl;
    }
  }

  return exp;
}

static ParseRule_ rules[] = {
  [TK_EOF]               = {NULL,        NULL,              PREC_NONE},
  [TK_ERR]               = {NULL,        NULL,              PREC_NONE},
  [TK_ID]                = {Id,          NULL,              PREC_NONE},
  [TK_LPAREN]            = {grouping,    FunctionCallArgs,  PREC_CALL},
  [TK_RPAREN]            = {NULL,        NULL,              PREC_NONE},
  [TK_LBRACKET]          = {ArrayValue,  IndexExpr,         PREC_CALL},
  [TK_RBRACKET]          = {NULL,        NULL,              PREC_NONE},
  [TK_LBRACE]            = {NULL,        ClassConstructor,  PREC_CALL},
  [TK_RBRACE]            = {NULL,        NULL,              PREC_NONE},
  [TK_SEMICOLON]         = {NULL,        NULL,              PREC_NONE},
  [TK_COLON]             = {NULL,        NULL,              PREC_NONE},
  [TK_DOT]               = {NULL,        BinaryOp,          PREC_CALL},
  [TK_COMMA]             = {NULL,        NULL,              PREC_NONE},
  [TK_PLUS]              = {NULL,        BinaryOp,          PREC_TERM},
  [TK_MINUS]             = {UnaryOp,     BinaryOp,          PREC_TERM},
  [TK_AMPERSAND]         = {NULL,        BinaryOp,          PREC_BITWISE_AND},
  [TK_PIPE]              = {NULL,        BinaryOp,          PREC_BITWISE_OR},
  [TK_HAT]               = {NULL,        BinaryOp,          PREC_BITWISE_XOR},
  [TK_TILDE]             = {UnaryOp,     NULL,              PREC_UNARY},
  [TK_STAR]              = {NULL,        BinaryOp,          PREC_FACTOR},
  [TK_PERCENT]           = {NULL,        BinaryOp,          PREC_FACTOR},
  [TK_QUESTION]          = {NULL,        NULL,              PREC_NONE},
  [TK_BANG]              = {NULL,        NULL,              PREC_NONE},
  [TK_BANG_EQUAL]        = {NULL,        BinaryOp,          PREC_EQUALITY},
  [TK_EQUAL]             = {NULL,        BinaryOp,          PREC_ASSIGNMENT},
  [TK_EQUAL_EQUAL]       = {NULL,        BinaryOp,          PREC_EQUALITY},
  [TK_GT]                = {NULL,        BinaryOp,          PREC_COMPARISON},
  [TK_GTE]               = {NULL,        BinaryOp,          PREC_COMPARISON},
  [TK_RSHIFT]            = {NULL,        BinaryOp,          PREC_SHIFT},
  [TK_LT]                = {NULL,        BinaryOp,          PREC_COMPARISON},
  [TK_LTE]               = {NULL,        BinaryOp,          PREC_COMPARISON},
  [TK_LSHIFT]            = {NULL,        BinaryOp,          PREC_SHIFT},
  [TK_ARROW]             = {NULL,        NULL,              PREC_NONE},
  [TK_FAT_ARROW]         = {NULL,        NULL,              PREC_NONE},
  [TK_SLASH]             = {NULL,        BinaryOp,          PREC_FACTOR},
  [TK_DOUBLE_SLASH]      = {NULL,        BinaryOp,          PREC_FACTOR},
  [TK_DOUBLE_DOT]        = {NULL,        NULL,              PREC_NONE},
  [TK_TRIPLE_DOT]        = {NULL,        NULL,              PREC_NONE},
  [TK_QQ]                = {NULL,        NULL,              PREC_NONE},
  [TK_QQE]               = {NULL,        NULL,              PREC_NONE},
  [TK_PLUS_EQUAL]        = {NULL,        InPlaceBinaryOp,   PREC_ASSIGNMENT},
  [TK_MINUS_EQUAL]       = {NULL,        InPlaceBinaryOp,   PREC_ASSIGNMENT},
  [TK_AMPERSAND_EQUAL]   = {NULL,        InPlaceBinaryOp,   PREC_ASSIGNMENT},
  [TK_PIPE_EQUAL]        = {NULL,        InPlaceBinaryOp,   PREC_ASSIGNMENT},
  [TK_HAT_EQUAL]         = {NULL,        InPlaceBinaryOp,   PREC_ASSIGNMENT},
  [TK_TILDE_EQUAL]       = {NULL,        InPlaceBinaryOp,   PREC_ASSIGNMENT},
  [TK_STAR_EQUAL]        = {NULL,        InPlaceBinaryOp,   PREC_ASSIGNMENT},
  [TK_PERCENT_EQUAL]     = {NULL,        InPlaceBinaryOp,   PREC_ASSIGNMENT},
  [TK_SLASH_EQUAL]       = {NULL,        InPlaceBinaryOp,   PREC_ASSIGNMENT},
  [TK_SLASH_SLASH_EQUAL] = {NULL,        InPlaceBinaryOp,   PREC_ASSIGNMENT},
  [TK_STRING]            = {String,      NULL,              PREC_NONE},
  [TK_INTEGER]           = {Integer,     NULL,              PREC_NONE},
  [TK_NUMBER]            = {Number,      NULL,              PREC_NONE},
  [TK_DO]                = {NULL,        NULL,              PREC_NONE},
  [TK_END]               = {NULL,        NULL,              PREC_NONE},
  [TK_IF]                = {NULL,        NULL,              PREC_NONE},
  [TK_THEN]              = {NULL,        NULL,              PREC_NONE},
  [TK_ELIF]              = {NULL,        NULL,              PREC_NONE},
  [TK_ELSE]              = {NULL,        NULL,              PREC_NONE},
  [TK_FOR]               = {NULL,        NULL,              PREC_NONE},
  [TK_IN]                = {NULL,        NULL,              PREC_NONE},
  [TK_STEP]              = {NULL,        NULL,              PREC_NONE},
  [TK_WHILE]             = {NULL,        NULL,              PREC_NONE},
  [TK_REPEAT]            = {NULL,        NULL,              PREC_NONE},
  [TK_UNTIL]             = {NULL,        NULL,              PREC_NONE},
  [TK_CLASS]             = {NULL,        NULL,              PREC_NONE},
  [TK_IS]                = {NULL,        NULL,              PREC_NONE},
  [TK_MATCH]             = {NULL,        NULL,              PREC_NONE},
  [TK_CASE]              = {NULL,        NULL,              PREC_NONE},
  [TK_FUNCTION]          = {FunctionDef, NULL,              PREC_FUNCTION},
  [TK_RETURN]            = {NULL,        NULL,              PREC_NONE},
  [TK_PRINT]             = {NULL,        NULL,              PREC_NONE},
  [TK_AND]               = {NULL,        BinaryOp,          PREC_AND},
  [TK_OR]                = {NULL,        BinaryOp,          PREC_OR},
  [TK_XOR]               = {NULL,        BinaryOp,          PREC_XOR},
  [TK_NOT]               = {UnaryOp,     NULL,              PREC_UNARY},
  [TK_TRUE]              = {True,        NULL,              PREC_NONE},
  [TK_FALSE]             = {False,       NULL,              PREC_NONE},
  [TK_PASS]              = {NULL,        NULL,              PREC_NONE},
  [TK_VAL]               = {NULL,        NULL,              PREC_NONE},
  [TK_VAR]               = {NULL,        NULL,              PREC_NONE},
  [TK_REF]               = {NULL,        NULL,              PREC_NONE},
  [TK_NIL]               = {Nil,         NULL,              PREC_NONE},
  [TK_NEW]               = {UnaryOp,     NULL,              PREC_UNARY},
  [TK_DEL]               = {UnaryOp,     NULL,              PREC_UNARY},
  [TK_ASSERT]            = {NULL,        NULL,              PREC_NONE},
  [TK_BOOL]              = {NULL,        NULL,              PREC_NONE},
  [TK_INT]               = {Type,        NULL,              PREC_NONE},
  [TK_UINT]              = {Type,        NULL,              PREC_NONE},
  [TK_FLOAT]             = {Type,        NULL,              PREC_NONE},
  [TK_DOUBLE]            = {Type,        NULL,              PREC_NONE},
  [TK_STRING_TYPE]       = {Type,        NULL,              PREC_NONE},
  [TK_LIST]              = {Type,        NULL,              PREC_NONE},
  [TK_MAP]               = {Type,        NULL,              PREC_NONE},
  [TK_SET]               = {Type,        NULL,              PREC_NONE},
  [TK_ASYNC]             = {NULL,        NULL,              PREC_NONE},
  [TK_AWAIT]             = {NULL,        NULL,              PREC_NONE},
  [TK_YIELD]             = {NULL,        NULL,              PREC_NONE},
};
// Static assert to make sure that all token types are accounted for.
STATIC_ASSERT(
  sizeof(rules) / sizeof(ParseRule_) == __TK_COUNT__,
  CHECK_TOKEN_COUNT);

static ParseRule_* get_rule(TokenType_ type) {
  return &rules[type];
}

///////////////////////////////////////////////////////////////////////////////

static void error_at(Parser_* parser, Token_* token, const char* const message, va_list argp) {
  if (parser->panic_mode) return;
  parser->panic_mode = true;
  fprintf(stderr, "[line %d] Error", token->line);

  if (token->type == TK_EOF) {
    fprintf(stderr, " at end");
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
  error_at(parser, &parser->current, message, argp);
  va_end(argp);
}

static void error(Parser_* parser, const char* message, ...) {
  va_list argp;
  va_start(argp, message);
  error_at(parser, &parser->previous, message, argp);
  va_end(argp);
}
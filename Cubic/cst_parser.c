#include "parser.h"
#include "memory.h"
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

typedef CstNode_* (*ParseFn)(Parser_*, Scanner_*);

typedef struct ParseRule_ {
  ParseFn prefix;
  ParseFn infix;
  Precedence precedence;
} ParseRule_;

static CstNode_* Number(Parser_* parser, Scanner_* scanner);
static CstNode_* UnaryOp(Parser_* parser, Scanner_* scanner);
static CstNode_* BinaryOp(Parser_* parser, Scanner_* scanner);
static CstNode_* InPlaceBinaryOp(Parser_* parser, Scanner_* scanner);
static CstNode_* grouping(Parser_* parser, Scanner_* scanner);
static CstNode_* Expr(Parser_* parser, Scanner_* scanner);
static CstNode_* parse_precedence(Parser_* parser, Scanner_* scanner, Precedence precedence);
static CstNode_* Statement(Parser_* parser, Scanner_* scanner);
static CstNode_* Block(Parser_* parser, Scanner_* scanner);
static CstNode_* Id(Parser_* parser, Scanner_* scanner);
static CstNode_* FunctionDef(Parser_* parser, Scanner_* scanner);
static CstNode_* ReturnStatement(Parser_* parser, Scanner_* scanner);
static CstNode_* ClassDef(Parser_* parser, Scanner_* scanner);
static CstNode_* FunctionCallArgs(Parser_* parser, Scanner_* scanner);
static CstNode_* FunctionCallArg(Parser_* parser, Scanner_* scanner);
static CstNode_* TypeDef(Parser_* parser, Scanner_* scanner);

static bool parse_value(Parser_* parser, Value_* ret);
static bool parse_value_expr(Parser_* parser, Value_* ret);

static void advance(Parser_* parser, Scanner_* scanner);
static void consume(Parser_* parser, Scanner_* scanner, TokenType_ type, const char* message);
static bool match(Parser_* parser, Scanner_* scanner, TokenType_ type);
static bool check(Parser_* parser, TokenType_ type);

static ParseRule_* get_rule(TokenType_ type);

static void error_at_current(Parser_* parser, const char* message, ...);
static void error(Parser_* parser, const char* message, ...);
static bool block_follow(Parser_* parser, Scanner_* scanner, bool withuntil);
static void statement_list(Parser_* parser, Scanner_* scanner, CstList_* statements);

CstList_* cstlist_create(struct MemoryAllocator_* allocator) {
  CstList_* ret = alloc(allocator, sizeof(CstList_));
  memset(ret, 0, sizeof(CstList_));
  ret->allocator = allocator;
  return ret;
}

CstList_* cstlist_copy(const CstList_* from, MemoryAllocator_* allocator) {
  CstList_* ret = alloc(allocator, sizeof(CstList_));
  memset(ret, 0, sizeof(CstList_));
  ret->allocator = allocator;

  for (CstListNode_* n = from->head; n != NULL; n = n->next) {
    cstlist_append(ret, n->node);
  }

  return ret;
}

void cstlist_copyto(CstList_* to, const CstList_* from, MemoryAllocator_* allocator) {
  cstlist_init(to, allocator);

  for (CstListNode_* n = from->head; n != NULL; n = n->next) {
    cstlist_append(to, n->node);
  }
}

void cstlist_init(CstList_* list, struct MemoryAllocator_* allocator) {
  memset(list, 0, sizeof(CstList_));
  list->allocator = allocator;
}

void cstlist_clear(CstList_* list) {
  struct MemoryAllocator_* allocator = list->allocator;

  CstListNode_* cur = list->head;
  while (cur) {
    CstListNode_* next = cur->next;
    dealloc(allocator, cur);
    cur = next;
  }

  list->count = 0;
}

void cstlist_destroy(CstList_** list) {
  struct MemoryAllocator_* allocator = (*list)->allocator;

  cstlist_clear(*list);
  dealloc(allocator, *list);
  *list = NULL;
}

void cstlist_append(CstList_* list, CstNode_* node) {
  if (!node) return;

  struct MemoryAllocator_* allocator = list->allocator;
  CstListNode_* n = alloc(allocator, sizeof(CstListNode_));
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

CstNode_* parse_cst(Parser_* parser, Scanner_* scanner, struct Scope_* root_scope, const char* source) {
  advance(parser, scanner);  
  CstProgram_* root = MAKE_CST_NODE(parser->allocator, CstProgram_, parser->current.line);

  root->block = (CstBlock_*)Block(parser, scanner);

  consume(parser, scanner, TK_EOF, "Expected end of file.");
  return (CstNode_*)root;
}

static Token_ parse_variable(Parser_* parser, Token token) {
  Token_ ret = *token;
  ret.start = alloc(parser->allocator, ret.length);
  memcpy((char*)ret.start, token->start, token->length);

  return ret;
}

CstNode_* make_primitive_node(TokenType_ tk, MemoryAllocator_* allocator, int line) {
  CstPrimitiveType_* ret = MAKE_CST_NODE(allocator, CstPrimitiveType_, line);
  ret->type = tk;

  return (CstNode_*)ret;
}

CstNode_* make_id_node(Token_ id, MemoryAllocator_* allocator, int line) {
  CstIdType_* ret = MAKE_CST_NODE(allocator, CstIdType_, line);
  ret->id = id;

  return (CstNode_*)ret;
}

static CstNode_* parse_unary_type(Parser_* parser, Scanner_* scanner) {
  Token_ token = parser->previous;
  CstNode_* ret = NULL;
  switch (token.type) {
    case TK_BOOL:
    case TK_INT:
    case TK_UINT:
    case TK_FLOAT:
    case TK_DOUBLE:
    case TK_STRING_TYPE:
    case TK_NIL:
      ret = make_primitive_node(token.type, parser->allocator, parser->current.line);
      break;
    case TK_FUNCTION:
    {
      error_at_current(parser, "Function typed values are unimplemented.");
      return ret;
    }
    case TK_ID:
      ret = make_id_node(parse_variable(parser, &token), parser->allocator, parser->current.line);
      break;
    default:
      error(parser, "encountered unknown token type when parsing type.");
      return ret;
  }

  if (match(parser, scanner, TK_LBRACKET)) {
    do {
      CstGenericOrArrayType_* generic_type = MAKE_CST_NODE(parser->allocator, CstGenericOrArrayType_, token.line);
      generic_type->prefix = ret;
      cstlist_init(&generic_type->args, parser->allocator);
      do {
        advance(parser, scanner);

        CstNode_* arg = NULL;
        Value_ maybe_val = { 0 };
        if (parse_value(parser, &maybe_val)) {
          CstPrimaryExp_* exp = MAKE_CST_NODE(parser->allocator, CstPrimaryExp_, parser->previous.line);
          exp->value = maybe_val;
          exp->type = parser->previous.type;
          arg = (CstNode_*)exp;
        } else {
          arg = parse_unary_type(parser, scanner);
        }
        cstlist_append(&generic_type->args, arg);

      } while (match(parser, scanner, TK_COMMA));

      ret = (CstNode_*)generic_type;

      if (!match(parser, scanner, TK_RBRACKET)) {
        error(parser, "type malformed, expected ']' at end of type.");
        return ret;
      }
    } while (match(parser, scanner, TK_LBRACKET));
  }

  if (!ret) {
    error(parser, "Encountered unknown token type when trying to parse type expression: %d", token.type);
  }

  return ret;
}

static CstNode_* parse_singleton_or_union_type(Parser_* parser, Scanner_* scanner) {
  CstNode_* single_type = NULL;
  CstUnionType_* union_type = NULL;
  CstList_ union_types = { 0 };
  
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
    union_type = MAKE_CST_NODE(parser->allocator, CstUnionType_, parser->current.line);
    union_type->types = union_types;
    return (CstNode_*)union_type;
  } else {
    return single_type;
  }
}

static CstNode_* parse_singleton_or_tuple_type(Parser_* parser, Scanner_* scanner) {
  CstNode_* single_type = NULL;
  CstTupleType_* tuple_type = NULL;
  CstList_ tuple_types = { 0 };

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
    tuple_type = MAKE_CST_NODE(parser->allocator, CstTupleType_, parser->current.line);
    tuple_type->types = tuple_types;
    return (CstNode_*)tuple_type;
  } else {
    return single_type;
  }
}

static CstNode_* parse_type_expr(Parser_* parser, Scanner_* scanner) {
  return parse_singleton_or_union_type(parser, scanner);
}

static CstNode_* parse_type(Parser_* parser, Scanner_* scanner) {
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
      case TK_TYPE:
        return;
      default:
        ;
    }

    advance(parser, scanner);
  }
}

static void statement_list(Parser_* parser, Scanner_* scanner, CstList_* statements) {
  // TODO: how to handle the 'pass' keyword? Should it stop parsing statement lists?
  while (!block_follow(parser, scanner, true)) {
    
    CstNode_* statement = Statement(parser, scanner);
    cstlist_append(statements, statement);

    if (parser->panic_mode) {
      synchronize(parser, scanner);
    }

    if (parser->previous.type == TK_RETURN) {
      return;
    }
  }
}


static CstNode_* Block(Parser_* parser, Scanner_* scanner) {
  CstBlock_* block = MAKE_CST_NODE(parser->allocator, CstBlock_, parser->current.line);

  cstlist_init(&block->statements, parser->allocator);
  statement_list(parser, scanner, &block->statements);

  return (CstNode_*)block;
}

static CstNode_* BlockStatement(Parser_* parser, Scanner_* scanner) {
  CstNode_* block = Block(parser, scanner);
  consume(parser, scanner, TK_END, "Expected 'end' at the end of a block.");
  return block;
}

static void ExprList(Parser_* parser, Scanner_* scanner, CstList_* exprs) {
  TokenType_ tk = parser->current.type;
  do {
    cstlist_append(exprs, Expr(parser, scanner));
  } while (match(parser, scanner, TK_COMMA));
}

static CstNode_* GenericParam(Parser_* parser, Scanner_* scanner) {
  if (!match(parser, scanner, TK_ID)) {
    error(parser, "");
    return NULL;
  }

  CstGenericParam_* param = MAKE_CST_NODE(parser->allocator, CstGenericParam_, parser->current.line);
  param->name = parse_variable(parser, &parser->previous);
  cstlist_init(&param->constraints, parser->allocator);

  if (match(parser, scanner, TK_COLON)) {
    do {
      CstNode_* type = parse_type(parser, scanner);
      cstlist_append(&param->constraints, type);
    } while (match(parser, scanner, TK_AMPERSAND));
  }
  
  return (CstNode_*)param;
}

void GenericParams(Parser_* parser, Scanner_* scanner, CstList_* params) {
  if (!match(parser, scanner, TK_LBRACKET)) {
    return;
  }

  do {
    CstNode_* param = GenericParam(parser, scanner);
    if (param) {
      cstlist_append(params, param);
    }
  } while (match(parser, scanner, TK_COMMA));

  consume(parser, scanner, TK_RBRACKET, "expected ']' at end of generic parameters");
}

static CstNode_* FunctionParam(Parser_* parser, Scanner_* scanner) {
  CstFunctionParam_* param = MAKE_CST_NODE(parser->allocator, CstFunctionParam_, parser->current.line);

  if (match(parser, scanner, TK_IN)) {
    param->kind = TK_IN;
  } else if (match(parser, scanner, TK_OUT)) {
    param->kind = TK_OUT;
  } else {
    param->kind = 0;
  }

  consume(parser, scanner, TK_ID, "Expecting parameter name");
  param->name = parse_variable(parser, &parser->previous);

  consume(parser, scanner, TK_COLON, "Expecting colon after parameter name.");
  param->type = parse_type(parser, scanner);

  return (CstNode_*)param;
}

static CstNode_* FunctionDef(Parser_* parser, Scanner_* scanner) {
  MemoryAllocator_* allocator = parser->allocator;
  CstFunctionDef_* def = MAKE_CST_NODE(allocator, CstFunctionDef_, parser->current.line);
  CstGenericFunctionDef_* generic_def = NULL;

  if (match(parser, scanner, TK_ID)) {
    def->name = parse_variable(parser, &parser->previous);
  } else {
    def->name = (Token_) { 0 };
  }

  if (check(parser, TK_LBRACKET)) {
    generic_def = MAKE_CST_NODE(allocator, CstGenericFunctionDef_, parser->current.line);
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
        error(parser, "Variadic args are unimplemented.");
        break;
      }

      CstFunctionParam_* param = (CstFunctionParam_*)FunctionParam(parser, scanner);
      cstlist_append(&def->function_params, (CstNode_*)param);
    } while (match(parser, scanner, TK_COMMA));
    consume(parser, scanner, TK_RPAREN, "Expected ')' after arguments.");
  }

  if (match(parser, scanner, TK_ARROW)) {
    def->return_type = parse_type(parser, scanner);
  } else {
    def->return_type = make_primitive_node(TK_NIL, allocator, parser->current.line);
  }

  if (match(parser, scanner, TK_END)) {
    def->body = MAKE_CST_NOOP(allocator);
  } else {
    def->body = BlockStatement(parser, scanner);
  }

  return generic_def ? (CstNode_*)generic_def : (CstNode_*)def;
}

static CstNode_* ExpressionStmt(Parser_* parser, Scanner_* scanner) {
  CstExpressionStmt_* stmt = MAKE_CST_NODE(parser->allocator, CstExpressionStmt_, parser->current.line);
  stmt->expr = Expr(parser, scanner);
  return (CstNode_*)stmt;
}

static CstNode_* ReturnStatement(Parser_* parser, Scanner_* scanner) {
  CstReturnStmt_* stmt = MAKE_CST_NODE(parser->allocator, CstReturnStmt_, parser->current.line);
  if (block_follow(parser, scanner, true) || parser->current.type == ';') {
    stmt->expr = (CstNode_*)MAKE_CST_NODE(parser->allocator, CstNoopExpr_, 0);
  } else {
    stmt->expr = Expr(parser, scanner);
  }

  return (CstNode_*)stmt;
}

static CstNode_* ClassMemberDecl(Parser_* parser, Scanner_* scanner) {
  CstClassMemberDecl_* decl = MAKE_CST_NODE(parser->allocator, CstClassMemberDecl_, parser->current.line);

  TokenType_ kind = 0;
  if (match(parser, scanner, TK_VAL)) {
    kind = TK_VAL;
  } else if (match(parser, scanner, TK_VAR)) {
    kind = TK_VAR;
  }

  consume(parser, scanner, TK_ID, "Class field must include a name.");
  decl->name = parse_variable(parser, &parser->previous);

  consume(parser, scanner, TK_COLON, "Expected a ':' after class member name.");
  CstNode_* parsed_type = parse_type(parser, scanner);
  if (kind == TK_VAR) {
    CstReferenceType_* ref = MAKE_CST_NODE(parser->allocator, CstReferenceType_, parser->current.line);
    ref->type = parsed_type;
    decl->field_type = (CstNode_*)ref;
  } else {
    decl->field_type = parsed_type;
  }

  if (match(parser, scanner, TK_EQUAL)) {
    decl->opt_expr = Expr(parser, scanner);
  }

  return (CstNode_*)decl;
}

static CstNode_* ClassDef(Parser_* parser, Scanner_* scanner) {
  CstClassDef_* def = MAKE_CST_NODE(parser->allocator, CstClassDef_, parser->current.line);
  cstlist_init(&def->members, parser->allocator); 

  consume(parser, scanner, TK_ID, "Class definition must include a name.");
  def->name = parse_variable(parser, &parser->previous);
  cstlist_init(&def->generic_params, parser->allocator);
  GenericParams(parser, scanner, &def->generic_params);

  while (check(parser, TK_ID) || check(parser, TK_VAL) || check(parser, TK_VAR)) {
    CstClassMemberDecl_* decl = (CstClassMemberDecl_*)ClassMemberDecl(parser, scanner);
    cstlist_append(&def->members, (CstNode_*)decl);

    if (check(parser, TK_END)) {
      break;
    }

    match(parser, scanner, TK_COMMA);
  }

  consume(parser, scanner, TK_END, "Expected 'end' at the end of a class definition.");

  return (CstNode_*)def;
}

static CstNode_* VarDecl(Parser_* parser, Scanner_* scanner, TokenType_ decl_token) {  
  CstVarDeclStmt_* stmt = MAKE_CST_NODE(parser->allocator, CstVarDeclStmt_, parser->current.line);
  consume(parser, scanner, TK_ID, "Expected variable name.");
  stmt->name = parse_variable(parser, &parser->previous);
  stmt->decl_token = decl_token;

  if (!match(parser, scanner, TK_COLON)) {
    error(parser, "Expected a ':' for a variable declaration.");
    return (CstNode_*)stmt;
  }
  

  if (!check(parser, TK_EQUAL)) {
    stmt->opt_type = parse_type(parser, scanner);
  }

  if (match(parser, scanner, TK_EQUAL)) {
    stmt->expr = Expr(parser, scanner);
  } else if (!stmt->opt_type) {
    error_at_current(parser, "Expected an expression for a deduced type variable.");
  }

  return (CstNode_*)stmt;
}

static CstNode_* Statement(Parser_* parser, Scanner_* scanner) {
  TokenType_ tk = parser->current.type;
  MemoryAllocator_* allocator = parser->allocator;
  CstNode_* ret = NULL;

  switch (tk) {
    case TK_DO:
    {
      advance(parser, scanner);
      return BlockStatement(parser, scanner);
    }

    case TK_VAL:
    {
      advance(parser, scanner);
      return (CstNode_*)VarDecl(parser, scanner, TK_VAL);
    }

    case TK_VAR:
    {
      advance(parser, scanner);
      return (CstNode_*)VarDecl(parser, scanner, TK_VAR);
    }

    case TK_END:
    {
      error_at_current(parser, "Unmatched 'do'");
      return MAKE_CST_NOOP(parser->allocator);
    }

    case TK_FUNCTION:
    {
      advance(parser, scanner);
      return FunctionDef(parser, scanner);
    }

    case TK_WHILE:
    {
      advance(parser, scanner);
      CstWhileStmt_* stmt = MAKE_CST_NODE(parser->allocator, CstWhileStmt_, parser->current.line);
      stmt->condition_expr = Expr(parser, scanner);

      consume(parser, scanner, TK_DO, "Expected 'do' at end of while statement.");

      stmt->block_stmt = Block(parser, scanner);

      consume(parser, scanner, TK_END, "Expected 'end' at end of while definition.");

      return (CstNode_*)stmt;
    }

    case TK_FOR:
    {
      // ForStmt ::= 'for' [VarDecl | ExpressionStmt] ';' [Expr] ';' [Expr] 'do' Block 'end'
      CstForStmt_* stmt = MAKE_CST_NODE(parser->allocator, CstForStmt_, parser->current.line);
      advance(parser, scanner);
      if (!match(parser, scanner, TK_SEMICOLON)) {
        if (match(parser, scanner, TK_VAL) || match(parser, scanner, TK_VAR)) {
          stmt->opt_var_decl = VarDecl(parser, scanner, parser->previous.type);
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

      return (CstNode_*)stmt;
    }

    case TK_ASSERT:
    {
      advance(parser, scanner);
      CstAssertStmt_* stmt = MAKE_CST_NODE(parser->allocator, CstAssertStmt_, parser->current.line);
      stmt->expr = Expr(parser, scanner);
      return (CstNode_*)stmt;
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
      CstIfStmt_* stmt = MAKE_CST_NODE(parser->allocator, CstIfStmt_, parser->current.line);

      stmt->condition_expr = Expr(parser, scanner);
      consume(parser, scanner, TK_THEN, "Expected 'then' after condition.");

      stmt->if_stmt = (CstNode_*)Block(parser, scanner);
      cstlist_init(&stmt->elif_stmts, parser->allocator);
      cstlist_init(&stmt->elif_exprs, parser->allocator);

      for(;;) {
        if (!match(parser, scanner, TK_ELIF)) {
          break;
        }
        cstlist_append(&stmt->elif_exprs, Expr(parser, scanner));
        consume(parser, scanner, TK_THEN, "Expected 'then' after condition.");
        cstlist_append(&stmt->elif_stmts, Block(parser, scanner));
      }

      if (match(parser, scanner, TK_ELSE)) {
        stmt->else_stmt = (CstNode_*)Block(parser, scanner);
      } else {
        stmt->else_stmt = NULL;
      }

      consume(parser, scanner, TK_END, "Expected 'end' after if-statement.");

      return (CstNode_*)stmt;
    }

    case TK_PRINT:
    {
      advance(parser, scanner);
      CstPrintStmt_* stmt = MAKE_CST_NODE(parser->allocator, CstPrintStmt_, parser->current.line);
      stmt->expr = Expr(parser, scanner);
      return (CstNode_*)stmt;
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

    default:
      return ExpressionStmt(parser, scanner);
  }
  
  return NULL;
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
static CstNode_* Expr(Parser_* parser, Scanner_* scanner) {
  //CstExpr_* ret = MAKE_CST_NODE(parser->allocator, CstExpr_, parser->current.line);
  CstNode_* ret = parse_precedence(parser, scanner, PREC_NONE);
  if (!ret) {
    return NULL;
  }
  return (CstNode_*)ret;
}

static bool parse_value(Parser_* parser, Value_* ret) {
  Token_ tk = parser->previous;
  switch (parser->previous.type) {
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
      *ret = OBJ_VAL(objstring_from(tk.start + 1, tk.length - 2));
      break;

    case TK_NUMBER:
      if (tk.start[tk.length - 1] == 'f') {
        *ret = FLOAT_VAL(strtof(tk.start, NULL));
      } else {
        *ret = DOUBLE_VAL(strtod(tk.start, NULL));
      }
      break;

    case TK_INTEGER:
      *ret = INT_VAL(strtoll(tk.start, NULL, 10));
      break;

    default:
      return false;
  }

  return true;
}

static bool parse_value_expr(Parser_* parser, Value_* val) {
  bool ret = parse_value(parser, val);
  if (!ret) {
    error(parser, "Trying to parse unknown token as value: %d", parser->previous.type);
  }
  return ret;
}

static CstPrintStmt_* Print(Parser_* parser, Scanner_* scanner) {
  CstPrintStmt_* stmt = MAKE_CST_NODE(parser->allocator, CstPrintStmt_, parser->current.line);
  stmt->expr = Expr(parser, scanner);

  return stmt;
}

static CstNode_* Number(Parser_* parser, Scanner_* scanner) {
  Token_ token = parser->previous;
  CstPrimaryExp_* expr = MAKE_CST_NODE(parser->allocator, CstPrimaryExp_, parser->previous.line);

  if (token.start[token.length - 1] == 'f') {
    float value = strtof(parser->previous.start, NULL);
    expr->type = TK_FLOAT;
    parse_value_expr(parser, &expr->value);
  } else {
    double value = strtod(parser->previous.start, NULL);
    expr->type = TK_DOUBLE;
    parse_value_expr(parser, &expr->value);
  }
  
  return (CstNode_*)expr;
}

static CstNode_* Integer(Parser_* parser, Scanner_* scanner) {
  int64_t value = strtoll(parser->previous.start, NULL, 10);
  CstPrimaryExp_* expr = MAKE_CST_NODE(parser->allocator, CstPrimaryExp_, parser->previous.line);
  expr->type = TK_INT;
  parse_value_expr(parser, &expr->value);
  return (CstNode_*)expr;
}

static CstNode_* Nil(Parser_* parser, Scanner_* scanner) {
  CstPrimaryExp_* expr = MAKE_CST_NODE(parser->allocator, CstPrimaryExp_, parser->previous.line);
  expr->type = TK_NIL;
  expr->value = NIL_VAL;
  return (CstNode_*)expr;
}

static CstNode_* True(Parser_* parser, Scanner_* scanner) {
  CstPrimaryExp_* expr = MAKE_CST_NODE(parser->allocator, CstPrimaryExp_, parser->previous.line);
  expr->type = TK_BOOL;
  expr->value = TRUE_VAL;
  return (CstNode_*)expr;
}

static CstNode_* False(Parser_* parser, Scanner_* scanner) {
  CstPrimaryExp_* expr = MAKE_CST_NODE(parser->allocator, CstPrimaryExp_, parser->previous.line);
  expr->type = TK_BOOL;
  expr->value = FALSE_VAL;
  return (CstNode_*)expr;
}

static CstNode_* String(Parser_* parser, Scanner_* scanner) {
  CstPrimaryExp_* expr = MAKE_CST_NODE(parser->allocator, CstPrimaryExp_, parser->previous.line);
  expr->type = TK_STRING_TYPE;
  parse_value_expr(parser, &expr->value);
  return (CstNode_*)expr;
}

static CstNode_* Type(Parser_* parser, Scanner_* scanner) {
  CstType_* type = MAKE_CST_NODE(parser->allocator, CstType_, parser->previous.line);
  type->impl = parse_type_expr(parser, scanner);
  return (CstNode_*)type;
}

static CstNode_* grouping(Parser_* parser, Scanner_* scanner) {
  CstNode_* expr = Expr(parser, scanner);
  consume(parser, scanner, TK_RPAREN, "Expect ')' after expression.");

  return expr;
}

static CstNode_* ArrayValue(Parser_* parser, Scanner_* scanner) {
  CstArrayValueExpr_* array = MAKE_CST_NODE(parser->allocator, CstArrayValueExpr_, parser->previous.line);
  cstlist_init(&array->values, parser->allocator);
  if (match(parser, scanner, TK_RBRACKET)) {
    return (CstNode_*)array;
  }

  do {
    cstlist_append(&array->values, Expr(parser, scanner));
  } while (match(parser, scanner, TK_COMMA));

  consume(parser, scanner, TK_RBRACKET, "Expected ']' at end of array definition.");
  return (CstNode_*)array;
}

static CstNode_* IndexExpr(Parser_* parser, Scanner_* scanner) {
  CstIndexExpr_* array = MAKE_CST_NODE(parser->allocator, CstIndexExpr_, parser->previous.line);
  array->index = Expr(parser, scanner);
  consume(parser, scanner, TK_RBRACKET, "Expected ']' at end of array definition.");
  return (CstNode_*)array;
}

static CstNode_* IndexOrGenericArgs(Parser_* parser, Scanner_* scanner) {
  CstIndexOrGenericArgs_* node = MAKE_CST_NODE(parser->allocator, CstIndexOrGenericArgs_, parser->previous.line);
  cstlist_init(&node->args, parser->allocator);
  do {
    cstlist_append(&node->args, Expr(parser, scanner));
  } while (!match(parser, scanner, TK_RBRACKET) && match(parser, scanner, TK_COMMA));

  if (parser->previous.type != TK_RBRACKET) {
    error_at_current(parser, "Malformed type or array. Expected ']' at end.");
  }
  return (CstNode_*)node;
}

static CstNode_* UnaryOp(Parser_* parser, Scanner_* scanner) {
  TokenType_ operator_type = parser->previous.type;

  CstUnaryExp_* unary = MAKE_CST_NODE(parser->allocator, CstUnaryExp_, parser->previous.line);
  unary->op = operator_type;
  unary->expr = parse_precedence(parser, scanner, PREC_UNARY);

  return (CstNode_*)unary;
}

static CstNode_* BinaryOp(Parser_* parser, Scanner_* scanner) {
  TokenType_ operator_type = parser->previous.type;
  ParseRule_* rule = get_rule(operator_type);  
  CstNode_* expr = parse_precedence(parser, scanner, (Precedence)(rule->precedence + 1));
  return expr;
}

static CstNode_* InPlaceBinaryOp(Parser_* parser, Scanner_* scanner) {
  TokenType_ operator_type = parser->previous.type;
  ParseRule_* rule = get_rule(operator_type);
  CstNode_* expr = parse_precedence(parser, scanner, (Precedence)(rule->precedence + 1));
  return expr;
}

static CstNode_* Id(Parser_* parser, Scanner_* scanner) {
  CstVarExpr_* var_expr = MAKE_CST_NODE(parser->allocator, CstVarExpr_, parser->previous.line);
  CstIdExpr_* expr = MAKE_CST_NODE(parser->allocator, CstIdExpr_, parser->previous.line);
  var_expr->expr = (CstNode_*)expr;
  expr->name = parse_variable(parser, &parser->previous);
  return (CstNode_*)var_expr;
}

static CstNode_* FunctionCallArgs(Parser_* parser, Scanner_* scanner) {
  CstFunctionCallArgs_* args = MAKE_CST_NODE(parser->allocator, CstFunctionCallArgs_, parser->current.line);
  cstlist_init(&args->args, parser->allocator);

  if (match(parser, scanner, TK_RPAREN)) {
    return (CstNode_*)args;
  }

  do {
    cstlist_append(&args->args, Expr(parser, scanner));
  } while (match(parser, scanner, TK_COMMA));

  consume(parser, scanner, TK_RPAREN, "Expected a ')' after a function call.");
  return (CstNode_*)args;
}

static CstNode_* ClassConstructorParam(Parser_* parser, Scanner_* scanner) {
  CstClassConstructorParam_* param = MAKE_CST_NODE(parser->allocator, CstClassConstructorParam_, parser->previous.line);
  CstNode_* field_expr = Expr(parser, scanner);

  if (field_expr->cls == CST_CLS(CstAssignmentExpr_)) {
    CstAssignmentExpr_* assignment_exp = CST_CAST(CstAssignmentExpr_, field_expr);
    CstVarExpr_* var_exp = CST_CAST(CstVarExpr_, assignment_exp->left);
    CstIdExpr_* id_exp = CST_CAST(CstIdExpr_, var_exp->expr);
    param->expr = assignment_exp->right;
    param->name = id_exp->name;
  } else {
    param->expr = field_expr;
  }

  return (CstNode_*)param;
}

static CstNode_* ClassConstructor(Parser_* parser, Scanner_* scanner) {
  CstClassConstructor_* constructor = MAKE_CST_NODE(parser->allocator, CstClassConstructor_, parser->previous.line);  
  cstlist_init(&constructor->params, parser->allocator);
  
  if (match(parser, scanner, TK_RBRACE)) {
    return (CstNode_*)constructor;
  }

  bool parsing_named_params = false;
  do {
    // Allow for trailing commas in field list.
    if (check(parser, TK_RBRACE)) {
      break;
    }

    CstClassConstructorParam_* param = CST_CAST(
      CstClassConstructorParam_, ClassConstructorParam(parser, scanner));

    if (param->name.start) {
      parsing_named_params = true;
    } else if (parsing_named_params) {
      error(parser, "Constructor parameters must have all named parameters at the end.");
    }
    cstlist_append(&constructor->params, (CstNode_*)param);
  } while (match(parser, scanner, TK_COMMA));

  consume(parser, scanner, TK_RBRACE, "Expected a '}' to terminate a struct constructor.");
  return (CstNode_*)constructor;
}

static CstTypeMemberDecl_* TypeMemberDecl(Parser_* parser, Scanner_* scanner) {
  CstTypeMemberDecl_* member = MAKE_CST_NODE(parser->allocator, CstTypeMemberDecl_, parser->previous.line);
  
  if (match(parser, scanner, TK_ID)) {
    if (check(parser, TK_COLON)) {
      member->opt_name = parse_variable(parser, &parser->previous);
      advance(parser, scanner);
    }
  }
  member->type = parse_type(parser, scanner);
  return member;
}

static CstNode_* TypeDef(Parser_* parser, Scanner_* scanner) {
  CstTypeDef_* type_def = MAKE_CST_NODE(parser->allocator, CstTypeDef_, parser->previous.line);
  type_def->name = parse_variable(parser, &parser->current);
  advance(parser, scanner);

  cstlist_init(&type_def->generic_params, parser->allocator);
  GenericParams(parser, scanner, &type_def->generic_params);

  if (match(parser, scanner, TK_END)) {
    return (CstNode_*)type_def;
  }
  advance(parser, scanner);
  type_def->type = parse_singleton_or_tuple_type(parser, scanner);
  consume(parser, scanner, TK_END, "Expected 'end' at the end of a type definition.");

  return (CstNode_*)type_def;
}

static CstNode_* RangeExpr(Parser_* parser, Scanner_* scanner) {
  return NULL;
}

static CstNode_* PrefixRangeExpr(Parser_* parser, Scanner_* scanner) {
  CstRangeExpr_* expr = MAKE_CST_NODE(parser->allocator, CstRangeExpr_, parser->previous.line);
  
  expr->left = NULL;
  expr->right = Expr(parser, scanner);

  return (CstNode_*)expr;
}

// Recursively replaces all CstVarOrTypeExpr_ nodes with a CstGenericOrArrayExpr_.
// This is for parsing expressions like:
//   val a := foo[int](1, 2, 3) OR
//   val a := foo[1][3][4](1, 2, 3) OR
//   val a := b.foo[1][3][4](1, 2, 3) OR
// Where the parse tree is ((((foo[int]) [1]) [3]) [4]) (1, 2, 3)
static void reduce_to_var_expr(CstNode_* expr, MemoryAllocator_* allocator) {
  
  switch (expr->cls) {
    case CST_CLS(CstUnaryExp_):
      reduce_to_var_expr(CST_CAST(CstUnaryExp_, expr)->expr, allocator);
      return;
    case CST_CLS(CstBinaryExp_):
    {
      CstBinaryExp_* exp = (CstBinaryExp_*)expr;
      reduce_to_var_expr(exp->left, allocator);
      reduce_to_var_expr(exp->right, allocator);
      return;
    }
    case CST_CLS(CstVarExpr_):
      reduce_to_var_expr(CST_CAST(CstVarExpr_, expr)->expr, allocator);
      return;

    case CST_CLS(CstIdExpr_):
      return;

    case CST_CLS(CstAssignmentExpr_):
    {
      CstAssignmentExpr_* exp = (CstAssignmentExpr_*)expr;
      reduce_to_var_expr(exp->left, allocator);
      reduce_to_var_expr(exp->right, allocator);
      return;
    }
    case CST_CLS(CstClassConstructor_):
    {
      CstClassConstructor_* exp = (CstClassConstructor_*)expr;
      for (CstListNode_* n = exp->params.head; n != NULL; n = n->next) {
        reduce_to_var_expr(n->node, allocator);
      }
      return;
    }
    case CST_CLS(CstClassConstructorParam_):
      reduce_to_var_expr(CST_CAST(CstClassConstructorParam_, expr)->expr, allocator);
      return;
    case CST_CLS(CstDotExpr_):
      reduce_to_var_expr(CST_CAST(CstDotExpr_, expr)->prefix, allocator);
      return;
    case CST_CLS(CstArrayValueExpr_):
    {
      CstArrayValueExpr_* exp = (CstArrayValueExpr_*)expr;
      for (CstListNode_* n = exp->values.head; n != NULL; n = n->next) {
        reduce_to_var_expr(n->node, allocator);
      }
      return;
    }
    case CST_CLS(CstRangeExpr_):
    {
      CstRangeExpr_* exp = (CstRangeExpr_*)expr;
      reduce_to_var_expr(exp->left, allocator);
      reduce_to_var_expr(exp->right, allocator);
      return;
    }
    case CST_CLS(CstVarOrTypeExpr_):
      break;

    default:
      assertf(false, "Unknown node.");
  }

  CstVarOrTypeExpr_* var_or_expr = CST_CAST(CstVarOrTypeExpr_, expr);
  var_or_expr->base.cls = CST_CLS(CstVarExpr_);

  CstGenericOrArrayExpr_* generic_or_array = MAKE_CST_NODE(allocator, CstGenericOrArrayExpr_, var_or_expr->base.line);
  generic_or_array->prefix = var_or_expr->prefix;
  generic_or_array->args = var_or_expr->index_args->args;
  var_or_expr->prefix = NULL;
  var_or_expr->index_args = NULL;

  CstVarExpr_* var_expr = CST_CAST(CstVarExpr_, var_or_expr);
  var_expr->expr = (CstNode_*)generic_or_array;

  reduce_to_var_expr((CstNode_*)generic_or_array->prefix, allocator);
}

// Recursively replaces all CstVarOrTypeExpr_ nodes with a CstGenericOrArrayType_.
// This is for parsing expressions like:
//   val a := foo[1][3][4]{1, 2, 3} OR
// Where the parse tree is ((((foo[int]) [1]) [3]) [4]) {1, 2, 3}
static void reduce_to_type_expr(CstNode_* expr, MemoryAllocator_* allocator) {
  if (expr->cls != CST_CLS(CstVarOrTypeExpr_)) return;

  CstVarOrTypeExpr_* index_or_expr = CST_CAST(CstVarOrTypeExpr_, expr);
  index_or_expr->base.cls = CST_CLS(CstType_);

  CstGenericOrArrayType_* generic_or_array = MAKE_CST_NODE(allocator, CstGenericOrArrayType_, expr->line);
  generic_or_array->prefix = index_or_expr->prefix;
  generic_or_array->args = index_or_expr->index_args->args;
  index_or_expr->prefix = NULL;
  index_or_expr->index_args = NULL;

  CstType_* cst_type = CST_CAST(CstType_, expr);
  cst_type->impl = (CstNode_*)generic_or_array;

  reduce_to_var_expr((CstNode_*)generic_or_array->prefix, allocator);
}

static CstNode_* parse_precedence(Parser_* parser, Scanner_* scanner, Precedence precedence) {
  advance(parser, scanner);
  ParseFn prefix_rule = get_rule(parser->previous.type)->prefix;
  if (!prefix_rule) {
    error(parser, "Expected expression");
    return NULL;
  }

  CstNode_* exp = prefix_rule(parser, scanner);
  if (exp->cls == CST_CLS(CstType_) && parser->current.type != TK_LBRACE && parser->current.type != TK_RBRACKET) {
    error_at_current(parser, "Bad type constructor.");
  }

  while (precedence <= get_rule(parser->current.type)->precedence) {
    ParseFn infix_rule = get_rule(parser->current.type)->infix;
    if (!infix_rule) {
      break;
    }
    advance(parser, scanner);

    if (parser->previous.type == TK_LPAREN) {
      CstFunctionCall_* new_exp = MAKE_CST_NODE(parser->allocator, CstFunctionCall_, parser->previous.line);
      new_exp->prefix = exp;
      new_exp->args = infix_rule(parser, scanner);

      if (exp->cls == CST_CLS(CstVarOrTypeExpr_)) {
        reduce_to_var_expr(exp, parser->allocator);
      }

      exp = (CstNode_*)new_exp;
    } else if (parser->previous.type == TK_EQUAL) {
      CstAssignmentExpr_* new_exp = MAKE_CST_NODE(parser->allocator, CstAssignmentExpr_, parser->previous.line);
      new_exp->left = exp;
      new_exp->right = infix_rule(parser, scanner);
      if (new_exp->right->cls == CST_CLS(CstClassConstructor_)) {
        CstVarExpr_* var_expr = CST_CAST(CstVarExpr_, new_exp->left);
        CstClassConstructor_* c_expr = CST_CAST(CstClassConstructor_, new_exp->right);
        c_expr->name = CST_CAST(CstIdExpr_, var_expr->expr)->name;
      }
      exp = (CstNode_*)new_exp;
    } else if (parser->previous.type == TK_DOT) {
      CstVarExpr_* var_exp = MAKE_CST_NODE(parser->allocator, CstVarExpr_, parser->previous.line);
      CstDotExpr_* new_exp = MAKE_CST_NODE(parser->allocator, CstDotExpr_, parser->previous.line);
      var_exp->expr = (CstNode_*)new_exp;
      new_exp->prefix = exp;
      new_exp->id = parse_variable(parser, &parser->current);
      advance(parser, scanner);
      exp = (CstNode_*)var_exp;
    } else if (parser->previous.type == TK_LBRACE) {
      CstClassConstructor_* new_exp = CST_CAST(CstClassConstructor_, infix_rule(parser, scanner));
      new_exp->prefix = exp;
      if (exp->cls == CST_CLS(CstVarExpr_)) {
        CstVarExpr_* var_expr = CST_CAST(CstVarExpr_, exp);
        CstIdExpr_* id_expr = CST_CAST(CstIdExpr_, var_expr->expr);
        new_exp->name = id_expr->name;
      } else if (exp->cls == CST_CLS(CstVarOrTypeExpr_)) {
        reduce_to_type_expr(exp, parser->allocator);
      }
      dealloc(parser->allocator, exp);
      exp = (CstNode_*)new_exp;
    } else if (parser->previous.type == TK_LBRACKET) {
      CstVarOrTypeExpr_* new_exp = MAKE_CST_NODE(parser->allocator, CstVarOrTypeExpr_, parser->previous.line);
      new_exp->prefix = exp;
      new_exp->index_args = CST_CAST(CstIndexOrGenericArgs_, infix_rule(parser, scanner));
      exp = (CstNode_*)new_exp;
    } else if (parser->previous.type >= TK_PLUS_EQUAL && parser->previous.type <= TK_SLASH_SLASH_EQUAL) {
      CstInPlaceBinaryStmt_* bin_exp =
        MAKE_CST_NODE(parser->allocator, CstInPlaceBinaryStmt_, parser->previous.line);

      bin_exp->op = parser->previous.type;
      bin_exp->left = exp;
      bin_exp->right = infix_rule(parser, scanner);

      exp = (CstNode_*)bin_exp;
    } else if (parser->previous.type == TK_DOUBLE_DOT) {
      CstRangeExpr_* expr = MAKE_CST_NODE(parser->allocator, CstRangeExpr_, parser->previous.line);

      expr->left = exp;
      expr->right = Expr(parser, scanner);

      exp = (CstNode_*)expr;
    } else {
      CstBinaryExp_* bin_exp = MAKE_CST_NODE(parser->allocator, CstBinaryExp_, parser->previous.line);
      bin_exp->op = parser->previous.type;
      bin_exp->left = exp;
      bin_exp->right = infix_rule(parser, scanner);

      exp = (CstNode_*)bin_exp;
    }
  }

  return exp;
}

static ParseRule_ rules[] = {
  [TK_EOF]               = {NULL,             NULL,               PREC_NONE},
  [TK_ERR]               = {NULL,             NULL,               PREC_NONE},
  [TK_ID]                = {Id,               NULL,               PREC_NONE},
  [TK_LPAREN]            = {grouping,         FunctionCallArgs,   PREC_CALL},
  [TK_RPAREN]            = {NULL,             NULL,               PREC_NONE},
  [TK_LBRACKET]          = {ArrayValue,       IndexOrGenericArgs, PREC_CALL},
  [TK_RBRACKET]          = {NULL,             NULL,               PREC_NONE},
  [TK_LBRACE]            = {ClassConstructor, ClassConstructor,   PREC_CALL},
  [TK_RBRACE]            = {NULL,             NULL,               PREC_NONE},
  [TK_SEMICOLON]         = {NULL,             NULL,               PREC_NONE},
  [TK_COLON]             = {NULL,             NULL,               PREC_NONE},
  [TK_DOT]               = {NULL,             BinaryOp,           PREC_CALL},
  [TK_COMMA]             = {NULL,             NULL,               PREC_NONE},
  [TK_PLUS]              = {NULL,             BinaryOp,           PREC_TERM},
  [TK_MINUS]             = {UnaryOp,          BinaryOp,           PREC_TERM},
  [TK_AMPERSAND]         = {NULL,             BinaryOp,           PREC_BITWISE_AND},
  [TK_PIPE]              = {NULL,             BinaryOp,           PREC_BITWISE_OR},
  [TK_HAT]               = {NULL,             BinaryOp,           PREC_BITWISE_XOR},
  [TK_TILDE]             = {UnaryOp,          NULL,               PREC_UNARY},
  [TK_STAR]              = {NULL,             BinaryOp,           PREC_FACTOR},
  [TK_PERCENT]           = {NULL,             BinaryOp,           PREC_FACTOR},
  [TK_QUESTION]          = {NULL,             NULL,               PREC_NONE},
  [TK_BANG]              = {NULL,             NULL,               PREC_NONE},
  [TK_BANG_EQUAL]        = {NULL,             BinaryOp,           PREC_EQUALITY},
  [TK_EQUAL]             = {NULL,             BinaryOp,           PREC_ASSIGNMENT},
  [TK_EQUAL_EQUAL]       = {NULL,             BinaryOp,           PREC_EQUALITY},
  [TK_GT]                = {NULL,             BinaryOp,           PREC_COMPARISON},
  [TK_GTE]               = {NULL,             BinaryOp,           PREC_COMPARISON},
  [TK_RSHIFT]            = {NULL,             BinaryOp,           PREC_SHIFT},
  [TK_LT]                = {NULL,             BinaryOp,           PREC_COMPARISON},
  [TK_LTE]               = {NULL,             BinaryOp,           PREC_COMPARISON},
  [TK_LSHIFT]            = {NULL,             BinaryOp,           PREC_SHIFT},
  [TK_ARROW]             = {NULL,             NULL,               PREC_NONE},
  [TK_FAT_ARROW]         = {NULL,             NULL,               PREC_NONE},
  [TK_SLASH]             = {NULL,             BinaryOp,           PREC_FACTOR},
  [TK_DOUBLE_SLASH]      = {NULL,             BinaryOp,           PREC_FACTOR},
  [TK_DOUBLE_DOT]        = {NULL,             RangeExpr,          PREC_NONE},
  [TK_TRIPLE_DOT]        = {NULL,             NULL,               PREC_NONE},
  [TK_QQ]                = {NULL,             NULL,               PREC_NONE},
  [TK_QQE]               = {NULL,             NULL,               PREC_NONE},
  [TK_PLUS_EQUAL]        = {NULL,             InPlaceBinaryOp,    PREC_ASSIGNMENT},
  [TK_MINUS_EQUAL]       = {NULL,             InPlaceBinaryOp,    PREC_ASSIGNMENT},
  [TK_AMPERSAND_EQUAL]   = {NULL,             InPlaceBinaryOp,    PREC_ASSIGNMENT},
  [TK_PIPE_EQUAL]        = {NULL,             InPlaceBinaryOp,    PREC_ASSIGNMENT},
  [TK_HAT_EQUAL]         = {NULL,             InPlaceBinaryOp,    PREC_ASSIGNMENT},
  [TK_TILDE_EQUAL]       = {NULL,             InPlaceBinaryOp,    PREC_ASSIGNMENT},
  [TK_STAR_EQUAL]        = {NULL,             InPlaceBinaryOp,    PREC_ASSIGNMENT},
  [TK_PERCENT_EQUAL]     = {NULL,             InPlaceBinaryOp,    PREC_ASSIGNMENT},
  [TK_SLASH_EQUAL]       = {NULL,             InPlaceBinaryOp,    PREC_ASSIGNMENT},
  [TK_SLASH_SLASH_EQUAL] = {NULL,             InPlaceBinaryOp,    PREC_ASSIGNMENT},
  [TK_STRING]            = {String,           NULL,               PREC_NONE},
  [TK_INTEGER]           = {Integer,          NULL,               PREC_NONE},
  [TK_NUMBER]            = {Number,           NULL,               PREC_NONE},
  [TK_DO]                = {NULL,             NULL,               PREC_NONE},
  [TK_END]               = {NULL,             NULL,               PREC_NONE},
  [TK_IF]                = {NULL,             NULL,               PREC_NONE},
  [TK_THEN]              = {NULL,             NULL,               PREC_NONE},
  [TK_ELIF]              = {NULL,             NULL,               PREC_NONE},
  [TK_ELSE]              = {NULL,             NULL,               PREC_NONE},
  [TK_FOR]               = {NULL,             NULL,               PREC_NONE},
  [TK_IN]                = {NULL,             NULL,               PREC_NONE},
  [TK_STEP]              = {NULL,             NULL,               PREC_NONE},
  [TK_WHILE]             = {NULL,             NULL,               PREC_NONE},
  [TK_REPEAT]            = {NULL,             NULL,               PREC_NONE},
  [TK_UNTIL]             = {NULL,             NULL,               PREC_NONE},
  [TK_CLASS]             = {NULL,             NULL,               PREC_NONE},
  [TK_IS]                = {NULL,             NULL,               PREC_NONE},
  [TK_MATCH]             = {NULL,             NULL,               PREC_NONE},
  [TK_CASE]              = {NULL,             NULL,               PREC_NONE},
  [TK_FUNCTION]          = {FunctionDef,      NULL,               PREC_FUNCTION},
  [TK_RETURN]            = {NULL,             NULL,               PREC_NONE},
  [TK_PRINT]             = {NULL,             NULL,               PREC_NONE},
  [TK_AND]               = {NULL,             BinaryOp,           PREC_AND},
  [TK_OR]                = {NULL,             BinaryOp,           PREC_OR},
  [TK_XOR]               = {NULL,             BinaryOp,           PREC_XOR},
  [TK_NOT]               = {UnaryOp,          NULL,               PREC_UNARY},
  [TK_TRUE]              = {True,             NULL,               PREC_NONE},
  [TK_FALSE]             = {False,            NULL,               PREC_NONE},
  [TK_PASS]              = {NULL,             NULL,               PREC_NONE},
  [TK_VAL]               = {NULL,             NULL,               PREC_NONE},
  [TK_VAR]               = {NULL,             NULL,               PREC_NONE},
  [TK_REF]               = {NULL,             NULL,               PREC_NONE},
  [TK_NIL]               = {Nil,              NULL,               PREC_NONE},
  [TK_NEW]               = {UnaryOp,          NULL,               PREC_UNARY},
  [TK_DEL]               = {UnaryOp,          NULL,               PREC_UNARY},
  [TK_ASSERT]            = {NULL,             NULL,               PREC_NONE},
  [TK_TYPE]              = {NULL,             NULL,               PREC_NONE},
  [TK_BOOL]              = {NULL,             NULL,               PREC_NONE},
  [TK_INT]               = {Type,             NULL,               PREC_NONE},
  [TK_UINT]              = {Type,             NULL,               PREC_NONE},
  [TK_FLOAT]             = {Type,             NULL,               PREC_NONE},
  [TK_DOUBLE]            = {Type,             NULL,               PREC_NONE},
  [TK_STRING_TYPE]       = {Type,             NULL,               PREC_NONE},
  [TK_LIST]              = {Type,             NULL,               PREC_NONE},
  [TK_MAP]               = {Type,             NULL,               PREC_NONE},
  [TK_SET]               = {Type,             NULL,               PREC_NONE},
  [TK_ASYNC]             = {NULL,             NULL,               PREC_NONE},
  [TK_AWAIT]             = {NULL,             NULL,               PREC_NONE},
  [TK_YIELD]             = {NULL,             NULL,               PREC_NONE},
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
  error_at(parser, &parser->current, message, argp);
  va_end(argp);
}

static void error(Parser_* parser, const char* message, ...) {
  va_list argp;
  va_start(argp, message);
  error_at(parser, &parser->previous, message, argp);
  va_end(argp);
}
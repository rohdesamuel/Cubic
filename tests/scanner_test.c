#include "scanner.h"

#include <ctype.h>
#include <stdio.h>
#include <string.h>

#include "utest.h"

Scanner_ scanner;

void lower_string(char* source) {
  while (*source != '\0') {
    *source = tolower(*source);
    ++source;
  }
}

void test_token(TokenType_ expected_token, const char* source, int* utest_result) {
  Scanner_ scanner;
  scanner_init(&scanner, source);

  Token_ actual = scanner_scan(&scanner);
  if (actual.type != expected_token) {
    EXPECT_TRUE(actual.type != expected_token);
    fprintf(stderr, "Tokens did not match. Got %d expected %d\n",
      actual.type, expected_token);
  }
}

void test_literal(TokenType_ expected_token, const char* expected_lexeme, const char* source, int* utest_result) {
  Scanner_ scanner;
  scanner_init(&scanner, source);

  Token_ actual = scanner_scan(&scanner);
  if (actual.type != expected_token) {
    EXPECT_EQ(actual.type, expected_token);
    fprintf(stderr, "Tokens did not match. Got %d expected %d\n",
      actual.type, expected_token);
  } else if (strncmp(actual.start, expected_lexeme, actual.length) != 0) {
    EXPECT_EQ(strncmp(actual.start, expected_lexeme, actual.length), 0);
    fprintf(stderr, "Lexeme did not match. Got %.*s expected %s\n",
      actual.length, actual.start, expected_lexeme);
  }
}

void test_token_stream(TokenType_ expected_tokens[], const char* source, int* utest_result) {
  Scanner_ scanner;
  scanner_init(&scanner, source);

  int pos = 0;
  bool ok = true;
  for (;;) {
    Token_ token = scanner_scan(&scanner);

    if (expected_tokens[pos] != TK_ERR && token.type == TK_ERR) {
      EXPECT_TRUE(expected_tokens[pos] == TK_ERR || token.type != TK_ERR);
      fprintf(stderr, "Got scanner error at position %d: %s\n", pos, token.start);
      break;
    }

    if (token.type != expected_tokens[pos]) {
      EXPECT_TRUE(token.type == expected_tokens[pos]);
      fprintf(stderr, "At position %d Tokens did not match. Got %d expected %d\n",
        pos, token.type, expected_tokens[pos]);
      ok = false;
      break;
    }

    if (token.type == TK_EOF || expected_tokens[pos] == TK_EOF) {
      break;
    }

    ++pos;
  }
}

#define TEST_TOKEN_AS_STRING(TOKEN) \
UTEST_F(ScannerTest, CorrectlyMatches_##TOKEN) { \
  char* source = _strdup(#TOKEN); \
  lower_string(source); \
  test_token(TK_ ## TOKEN, source, utest_result); \
  free(source); \
}

#define TEST_TOKEN_COMPOSITE(TOKEN, SOURCE) \
UTEST_F(ScannerTest, CorrectlyMatchesComposite_##TOKEN) { \
  test_token(TK_ ## TOKEN, SOURCE, utest_result); \
}

#define TEST_LITERAL(TEST_NAME, TOKEN, EXPECTED_LEXEME, SOURCE) \
UTEST_F(ScannerTest, CorrectlyMatchesLiteral_##TEST_NAME) { \
  test_literal(TOKEN, EXPECTED_LEXEME, SOURCE, utest_result); \
}

#define TEST_LITERAL_SIMPLE(TOKEN, EXPECTED_LEXEME) \
UTEST_F(ScannerTest, CorrectlyMatchesLiteral_##TOKEN) { \
  test_literal(TOKEN, EXPECTED_LEXEME, EXPECTED_LEXEME, utest_result); \
}

#define TEST_LITERAL_SIMPLE_NAMED(NAME, TOKEN, EXPECTED_LEXEME) \
UTEST_F(ScannerTest, CorrectlyMatchesLiteral_##NAME) { \
  test_literal(TOKEN, EXPECTED_LEXEME, EXPECTED_LEXEME, utest_result); \
}

#define TEST_TOKEN_STREAM(NAME, SOURCE, ...) \
UTEST_F(ScannerTest, CorrectlyMatchesStream_##NAME) { \
  TokenType_ expected_tokens[] = {__VA_ARGS__};\
  test_token_stream(expected_tokens, SOURCE, utest_result);\
}

typedef struct ScannerTest {
  int unused;
} ScannerTest;

UTEST_F_SETUP(ScannerTest) {
}

UTEST_F_TEARDOWN(ScannerTest) {}

#if 0
TEST_TOKEN_AS_STRING(STRING);
TEST_TOKEN_AS_STRING(ASYNC);
TEST_TOKEN_AS_STRING(AWAIT);
TEST_TOKEN_AS_STRING(YIELD);
TEST_TOKEN_AS_STRING(BOOL);
TEST_TOKEN_AS_STRING(INT);
TEST_TOKEN_AS_STRING(UINT);
TEST_TOKEN_AS_STRING(FLOAT);
TEST_TOKEN_AS_STRING(DOUBLE);

TEST_TOKEN_AS_STRING(DO);
TEST_TOKEN_AS_STRING(END);
TEST_TOKEN_AS_STRING(IF);
TEST_TOKEN_AS_STRING(THEN);
TEST_TOKEN_AS_STRING(ELIF);
TEST_TOKEN_AS_STRING(ELSE);
TEST_TOKEN_AS_STRING(FOR);
TEST_TOKEN_AS_STRING(IN);
TEST_TOKEN_AS_STRING(STEP);
TEST_TOKEN_AS_STRING(WHILE);
TEST_TOKEN_AS_STRING(REPEAT);
TEST_TOKEN_AS_STRING(UNTIL);
TEST_TOKEN_AS_STRING(CLASS);
TEST_TOKEN_AS_STRING(MATCH);
TEST_TOKEN_AS_STRING(CASE);
TEST_TOKEN_AS_STRING(FUNCTION);
TEST_TOKEN_AS_STRING(RETURN);
TEST_TOKEN_AS_STRING(AND);
TEST_TOKEN_AS_STRING(OR);
TEST_TOKEN_AS_STRING(XOR);
TEST_TOKEN_AS_STRING(NOT);
TEST_TOKEN_AS_STRING(TRUE);
TEST_TOKEN_AS_STRING(FALSE);
TEST_TOKEN_AS_STRING(PASS);
TEST_TOKEN_AS_STRING(VAR);
TEST_TOKEN_AS_STRING(REF);
TEST_TOKEN_AS_STRING(NIL);
TEST_TOKEN_AS_STRING(ASSERT);

TEST_TOKEN_COMPOSITE(LIST, "List");
TEST_TOKEN_COMPOSITE(MAP, "Map");
TEST_TOKEN_COMPOSITE(SET, "Set");

TEST_LITERAL(RealNoNumbersAfterDecimal, TK_NUMBER, "1.", "1.");
TEST_LITERAL(RealNumbersAfterDecimal, TK_NUMBER, "1.23", "1.23");
TEST_LITERAL(ExtraDots, TK_NUMBER, "1.23", "1.2.3..");
TEST_LITERAL(ExtraNumbers, TK_NUMBER, ".2", ".2.3..");
TEST_LITERAL(Float, TK_NUMBER, "1.f", "1.f");
TEST_LITERAL(FloatWithDecimal, TK_NUMBER, "1.23f", "1.23f");
TEST_LITERAL(FloatExtraDots, TK_NUMBER, "1.23f", "1.2.3f..");
TEST_LITERAL(RealExtraDotsAndNumbers, TK_NUMBER, ".2", ".2.3..");
TEST_LITERAL(IntegerDoesNotMatchDot, TK_INTEGER, "12", "12 .");
TEST_LITERAL(DoubleDot, TK_DOUBLE_DOT, "..", "..");
TEST_LITERAL(TripleDot, TK_TRIPLE_DOT, "...", "...");
TEST_LITERAL(TripleDotExtraDots, TK_TRIPLE_DOT, "...", "....");
TEST_LITERAL(TripleDotExtraDotsNotDouble, TK_TRIPLE_DOT, "...", ".....");

TEST_LITERAL_SIMPLE(TK_BANG, "!");
TEST_LITERAL_SIMPLE(TK_BANG_EQUAL, "!=");
TEST_LITERAL_SIMPLE(TK_EQUAL, "=");
TEST_LITERAL_SIMPLE(TK_EQUAL_EQUAL, "==");
TEST_LITERAL_SIMPLE(TK_GT, ">");
TEST_LITERAL_SIMPLE(TK_GTE, ">=");
TEST_LITERAL_SIMPLE(TK_LT, "<");
TEST_LITERAL_SIMPLE(TK_LTE, "<=");
TEST_LITERAL_SIMPLE(TK_LSHIFT, "<<");
TEST_LITERAL_SIMPLE(TK_RSHIFT, ">>");
TEST_LITERAL_SIMPLE(TK_ARROW, "->");
TEST_LITERAL_SIMPLE(TK_FAT_ARROW, "=>");
TEST_LITERAL_SIMPLE(TK_SLASH, "/");
TEST_LITERAL_SIMPLE(TK_DOUBLE_SLASH, "//");
TEST_LITERAL_SIMPLE(TK_QQ, "??");
TEST_LITERAL_SIMPLE(TK_QQE, "??=");

TEST_LITERAL_SIMPLE(TK_PLUS_EQUAL, "+=");
TEST_LITERAL_SIMPLE(TK_MINUS_EQUAL, "-=");
TEST_LITERAL_SIMPLE(TK_AMPERSAND_EQUAL, "&=");
TEST_LITERAL_SIMPLE(TK_PIPE_EQUAL, "|=");
TEST_LITERAL_SIMPLE(TK_HAT_EQUAL, "^=");
TEST_LITERAL_SIMPLE(TK_TILDE_EQUAL, "~=");
TEST_LITERAL_SIMPLE(TK_STAR_EQUAL, "*=");
UTEST_F(ScannerTest, CorrectlyMatchesLiteral_TK_PERCENT_EQUAL) {
  test_literal(TK_PERCENT_EQUAL, "%=", "%=", utest_result);
}

TEST_LITERAL_SIMPLE(TK_SLASH_EQUAL, "/=");
TEST_LITERAL_SIMPLE(TK_SLASH_SLASH_EQUAL, "//=");

TEST_LITERAL_SIMPLE(TK_LPAREN, "(");
TEST_LITERAL_SIMPLE(TK_RPAREN, ")");
TEST_LITERAL_SIMPLE(TK_LBRACKET, "[");
TEST_LITERAL_SIMPLE(TK_RBRACKET, "]");
TEST_LITERAL_SIMPLE(TK_LBRACE, "{");
TEST_LITERAL_SIMPLE(TK_RBRACE, "}");
TEST_LITERAL_SIMPLE(TK_SEMICOLON, ";");
TEST_LITERAL_SIMPLE(TK_COLON, ":");
TEST_LITERAL_SIMPLE(TK_DOT, ".");
TEST_LITERAL_SIMPLE(TK_COMMA, ",");
TEST_LITERAL_SIMPLE(TK_PLUS, "+");
TEST_LITERAL_SIMPLE(TK_MINUS, "-");
TEST_LITERAL_SIMPLE(TK_QUESTION, "?");
TEST_LITERAL_SIMPLE(TK_AMPERSAND, "&");
TEST_LITERAL_SIMPLE(TK_PIPE, "|");
TEST_LITERAL_SIMPLE(TK_HAT, "^");
TEST_LITERAL_SIMPLE(TK_TILDE, "~");
TEST_LITERAL_SIMPLE(TK_STAR, "*");
TEST_LITERAL_SIMPLE(TK_PERCENT, "%%");
TEST_LITERAL_SIMPLE_NAMED(StringWithSingleQuote, TK_STRING, "'hello'");
TEST_LITERAL_SIMPLE_NAMED(StringWithDoubleQuote, TK_STRING, "\"hello\"");

TEST_LITERAL_SIMPLE_NAMED(If, TK_IF, "if");
TEST_LITERAL_SIMPLE_NAMED(IfWithExtraChar, TK_ID, "iff");
TEST_LITERAL_SIMPLE_NAMED(Or, TK_OR, "or");
TEST_LITERAL_SIMPLE_NAMED(OrWithExtraChar, TK_ID, "ord");
TEST_LITERAL_SIMPLE(TK_OUT, "out");

TEST_TOKEN_STREAM(ValDecl, "val x : int = 1234",
  TK_VAL, TK_ID, TK_COLON, TK_INT, TK_EQUAL, TK_INTEGER, TK_EOF);

TEST_TOKEN_STREAM(LambdaDecl, "val x := function (x) -> int x",
  TK_VAL, TK_ID, TK_COLON, TK_EQUAL,
  TK_FUNCTION, TK_LPAREN, TK_ID, TK_RPAREN, TK_ARROW, TK_INT, TK_ID, TK_EOF);

TEST_TOKEN_STREAM(Addition, "1 + 1",
  TK_INTEGER, TK_PLUS, TK_INTEGER, TK_EOF);

TEST_TOKEN_STREAM(Equals, "1 == 1",
  TK_INTEGER, TK_EQUAL_EQUAL, TK_INTEGER, TK_EOF);

TEST_TOKEN_STREAM(Div, "1 / 1",
  TK_INTEGER, TK_SLASH, TK_INTEGER, TK_EOF);

TEST_TOKEN_STREAM(IDiv, "1 // 1",
  TK_INTEGER, TK_DOUBLE_SLASH, TK_INTEGER, TK_EOF);

TEST_TOKEN_STREAM(Assert, "assert 6 / 3 == 2",
  TK_ASSERT, TK_INTEGER, TK_SLASH, TK_INTEGER, TK_EQUAL_EQUAL, TK_INTEGER, TK_EOF);

TEST_TOKEN_STREAM(BadComment, "/#//", TK_ERR, TK_EOF);
TEST_TOKEN_STREAM(DoubleComment, "/##/", TK_EOF);
TEST_TOKEN_STREAM(TripleComment, "/###/", TK_EOF);
TEST_TOKEN_STREAM(BlockComment, "/###### #######/", TK_EOF);
TEST_TOKEN_STREAM(BlockCommentWithInnerComment, "/#/##/", TK_EOF);
TEST_TOKEN_STREAM(NestedBlockComment, "/# /# #/", TK_EOF);
TEST_TOKEN_STREAM(NestedBlockCommentDoesNotExcludeSurroundingTokens, "1 /# /# #/ 2.0", TK_INTEGER, TK_NUMBER, TK_EOF);

TEST_TOKEN_STREAM(VarDecl, "var a? : int", TK_VAR, TK_ID, TK_QUESTION, TK_COLON, TK_INT, TK_EOF);
TEST_TOKEN_STREAM(QQ, "a ?? b", TK_ID, TK_QQ, TK_ID, TK_EOF);
TEST_TOKEN_STREAM(QQE, "a ??= b", TK_ID, TK_QQE, TK_ID, TK_EOF);
TEST_TOKEN_STREAM(VariableAssignment, "a = 1.f + 2.f", TK_ID, TK_EQUAL, TK_NUMBER, TK_PLUS, TK_NUMBER, TK_EOF);

TEST_TOKEN_STREAM(DoubleDots, "1..1", TK_INTEGER, TK_DOUBLE_DOT, TK_INTEGER, TK_EOF);
TEST_TOKEN_STREAM(DoubleDotsMultipleDigits, "1234..1", TK_INTEGER, TK_DOUBLE_DOT, TK_INTEGER, TK_EOF);
TEST_TOKEN_STREAM(DoubleDotsSpaces, "1 .. 1", TK_INTEGER, TK_DOUBLE_DOT, TK_INTEGER, TK_EOF);
#endif
TEST_TOKEN_STREAM(DoubleDotsStatementAfter, "1 .. val a := 0", TK_INTEGER, TK_DOUBLE_DOT, TK_VAL, TK_ID, TK_COLON, TK_EQUAL, TK_INTEGER, TK_EOF);
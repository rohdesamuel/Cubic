#ifndef CHUNK__H
#define CHUNK__H

#include "common.h"
#include "value.h"

#define OPCODE_LIST(OPCODE) \
    OPCODE(OP_NOP) \
    OPCODE(OP_NIL)  \
    OPCODE(OP_TRUE)  \
    OPCODE(OP_FALSE)  \
    OPCODE(OP_MOVE)   /* R(A) <- R(A)                   */ \
    OPCODE(OP_LEA)    /* R(A) <- R(A) + R(B) * IMM(C)   */ \
    OPCODE(OP_MEMCPY) /* &R(A)[0..n-1] <- &R(B)[0..n-1] */ \
    OPCODE(OP_MEMSET) /* &R(A)[0..n-1] <- R(B)          */ \
    OPCODE(OP_LOAD)   /* R(A) <- *(R(B) + IMM(C))       */ \
    OPCODE(OP_LOADA)  /* R(A) <- &R(B) + IMM(C)         */ \
    OPCODE(OP_STORE)  /* *(R(A) + IMM(B)) <- R(C)       */ \
    OPCODE(OP_RLOAD)  /* R(A) <- *(R(B).ptr + IMM(C))   */ \
    OPCODE(OP_RLOADA) /* R(A) <- R(B).ptr + IMM(C)      */ \
    OPCODE(OP_RSTORE) /* *(R(A).ptr + IMM(B)) <- R(C)   */ \
    OPCODE(OP_REF_MAKE)  \
    OPCODE(OP_REF_DEL)  \
    OPCODE(OP_CONSTANT)  \
    OPCODE(OP_EQ)  \
    OPCODE(OP_GT)  \
    OPCODE(OP_GTE)  \
    OPCODE(OP_LT)  \
    OPCODE(OP_LTE)  \
    OPCODE(OP_CMP)  \
    OPCODE(OP_ICMP)  \
    OPCODE(OP_FCMP)  \
    OPCODE(OP_DCMP)  \
    OPCODE(OP_ADD)  \
    OPCODE(OP_ADDIMM)  \
    OPCODE(OP_SUB)  \
    OPCODE(OP_MUL)  \
    OPCODE(OP_DIV)  \
    OPCODE(OP_IMUL)  \
    OPCODE(OP_IDIV)  \
    OPCODE(OP_FADD)  \
    OPCODE(OP_FSUB)  \
    OPCODE(OP_FMUL)  \
    OPCODE(OP_FDIV)  \
    OPCODE(OP_DADD)  \
    OPCODE(OP_DSUB)  \
    OPCODE(OP_DMUL)  \
    OPCODE(OP_DDIV)  \
    OPCODE(OP_CONCAT)  \
    OPCODE(OP_MOD)  \
    OPCODE(OP_IMOD)  \
    OPCODE(OP_BITWISE_AND)  \
    OPCODE(OP_BITWISE_OR)  \
    OPCODE(OP_BITWISE_XOR)  \
    OPCODE(OP_BITWISE_NOT)  \
    OPCODE(OP_LSHIFT)  \
    OPCODE(OP_RSHIFT)  \
    OPCODE(OP_AND)  \
    OPCODE(OP_OR)  \
    OPCODE(OP_XOR)  \
    OPCODE(OP_NOT)  \
    OPCODE(OP_NEG)  \
    OPCODE(OP_FNEG)  \
    OPCODE(OP_DNEG)  \
    OPCODE(OP_CAST_f2i)  \
    OPCODE(OP_CAST_f2d)  \
    OPCODE(OP_CAST_d2i)  \
    OPCODE(OP_CAST_d2f)  \
    OPCODE(OP_CAST_i2f)  \
    OPCODE(OP_CAST_i2d)  \
    OPCODE(OP_RETURN)  \
    OPCODE(OP_POP)  \
    OPCODE(OP_JMP)  \
    OPCODE(OP_JMP_IF_FALSE)  \
    OPCODE(OP_LOOP)  \
    OPCODE(OP_PUSH_PARAM)  \
    OPCODE(OP_CALL)  \
    OPCODE(OP_PROLOGUE)  \
    OPCODE(OP_EPILOGUE)  \
    OPCODE(OP_ADD_OFFSET)  \
    OPCODE(OP_GET_OFFSET)  \
    OPCODE(OP_SET_OFFSET)  \
    OPCODE(OP_GET_VAR)  \
    OPCODE(OP_SET_VAR)  \
    OPCODE(OP_NEW_VAR)  \
    OPCODE(OP_DESTROY_VAR)  \
    OPCODE(OP_ALLOC_PTR)  \
    OPCODE(OP_DEREF_PTR)  \
    OPCODE(OP_REF_PTR)  \
    OPCODE(OP_COPY_REF)  \
    OPCODE(OP_COPY_VAL)  \
    OPCODE(OP_COPY)  \
    OPCODE(OP_GET_REF)  \
    OPCODE(OP_SET_REF)  \
    OPCODE(OP_STACK_TOP)  \
    OPCODE(OP_ADDROF_VAR)  \
    OPCODE(OP_ADDROF_REF)  \
    OPCODE(OP_REF_INC)  \
    OPCODE(OP_REF_DEC)  \
    OPCODE(OP_PRINT)  \
    OPCODE(OP_ASSERT)  \
    OPCODE(OP_BEGIN_SCOPE)  \
    OPCODE(OP_END_SCOPE)  \
    OPCODE(OP_CONSTANT_LONG)  \
    OPCODE(OP_OBJ_EQ)  \
    OPCODE(__OP_CODE_COUNT_)

#define GENERATE_ENUM(ENUM) ENUM,
#define GENERATE_STRING(STRING) #STRING,

typedef enum {
  OPCODE_LIST(GENERATE_ENUM)
} OpCode;

static const char* OPCODE_STRING[] = {
  OPCODE_LIST(GENERATE_STRING)
};

#define OPCODE_STR(OPCODE)  (OPCODE_STRING[(OPCODE)])

typedef struct Chunk_ {
  int count;
  int capacity;

  uint8_t* code;

  ValueArray_ constants;
  int* lines;
} Chunk_, *Chunk;

void chunk_init(Chunk chunk);
void chunk_free(Chunk chunk);
void chunk_write(Chunk chunk, uint8_t byte, int line);
void chunk_writeconstant(Chunk chunk, Value_ value, int line);
int chunk_addconstant(Chunk chunk, Value_ value);

void chunk_protect(Chunk chunk);

#endif  // CHUNK__H
#include "type.h"

#include "utest.h"

typedef struct TypeTest {
  int unused;
} TypeTest;

UTEST_F_SETUP(TypeTest) {}

UTEST_F_TEARDOWN(TypeTest) {}

UTEST_F(TypeTest, TypeIdPrimitives) {
  uint64_t primitive_ids[] = {
    type_id((Type_*)Unknown_Ty),
    type_id((Type_*)Nil_Ty),
    type_id((Type_*)Bool_Ty),
    type_id((Type_*)Int_Ty),
    type_id((Type_*)Int8_Ty),
    type_id((Type_*)Int16_Ty),
    type_id((Type_*)Int32_Ty),
    type_id((Type_*)Int64_Ty),
    type_id((Type_*)Uint_Ty),
    type_id((Type_*)Uint8_Ty),
    type_id((Type_*)Uint16_Ty),
    type_id((Type_*)Uint32_Ty),
    type_id((Type_*)Uint64_Ty),
    type_id((Type_*)Float_Ty),
    type_id((Type_*)Double_Ty),
    type_id((Type_*)String_Ty),
  };

  size_t count = sizeof(primitive_ids) / sizeof(primitive_ids[0]);
  for (size_t i = 0; i < count; ++i) {
    for (size_t j = i + 1; j < count; ++j) {
      EXPECT_NE(primitive_ids[i], primitive_ids[j]);
    }
  }
}

UTEST_F(TypeTest, TypeIdUnknownAndPlaceholdersArePoison) {
  PageAllocator_ a;
  pageallocator_init(&a, 4096);
  MemoryAllocator_* m = (MemoryAllocator_*)&a;

  EXPECT_EQ(0, type_id((Type_*)Unknown_Ty));
  EXPECT_EQ(0, type_id(make_in_ty((Type_*)Unknown_Ty, m)));
  EXPECT_EQ(0, type_id(make_tuple_ty(m, 1, (Type_*)Unknown_Ty)));
  EXPECT_EQ(0, type_id(make_union_ty(m, 1, (Type_*)Unknown_Ty)));
  EXPECT_EQ(0, type_id(make_in_ty(make_union_ty(m, 2, (Type_*)Int_Ty, (Type_*)Unknown_Ty), m)));

  pageallocator_deinit(&a);
}

UTEST_F(TypeTest, TypeIdUnary) {
  PageAllocator_ a;
  pageallocator_init(&a, 4096);
  MemoryAllocator_* m = (MemoryAllocator_*)&a;

  Type_* ty = (Type_*)Int_Ty;
  uint64_t ids[] = {
    type_id(make_const_ty(ty, m)),
    type_id(make_in_ty(ty, m)),
    type_id(make_out_ty(ty, m)),
    type_id(make_var_ty(ty, m)),
    type_id(make_field_ty((Token_) { 0 }, ty, m))
  };

  size_t count = sizeof(ids) / sizeof(ids[0]);
  for (size_t i = 0; i < count; ++i) {
    for (size_t j = i + 1; j < count; ++j) {
      EXPECT_NE(ids[i], ids[j]);
      if (ids[i] == ids[j]) {
        printf("collision: ids[%lld] == ids[%lld]\n\n", i, j);
      }
    }
  }

  pageallocator_deinit(&a);
}

UTEST_F(TypeTest, TypeIdClasses) {
  PageAllocator_ a;
  pageallocator_init(&a, 4096);
  MemoryAllocator_* m = (MemoryAllocator_*)&a;

  Type_* ty = (Type_*)Int_Ty;
  uint64_t ids[] = {
    type_id(make_class_ty(token_string("Foo"), m)),
    type_id(make_class_ty(token_string("foo"), m)),
    type_id(make_class_ty(token_string("Foooo"), m)),        
  };

  size_t count = sizeof(ids) / sizeof(ids[0]);
  for (size_t i = 0; i < count; ++i) {
    for (size_t j = i + 1; j < count; ++j) {
      EXPECT_NE(ids[i], ids[j]);
      if (ids[i] == ids[j]) {
        printf("collision: ids[%lld] == ids[%lld]\n\n", i, j);
      }
    }
  }

  pageallocator_deinit(&a);
}

UTEST_F(TypeTest, TypeIdArrays) {
  PageAllocator_ a;
  pageallocator_init(&a, 4096);
  MemoryAllocator_* m = (MemoryAllocator_*)&a;

  Type_* ty = (Type_*)&Int_Ty;
  uint64_t ids[] = {
    type_id(make_array_ty(ty, 1, m)),
    type_id(make_array_ty(ty, 2, m)),
  };

  size_t count = sizeof(ids) / sizeof(ids[0]);
  for (size_t i = 0; i < count; ++i) {
    for (size_t j = i + 1; j < count; ++j) {
      EXPECT_NE(ids[i], ids[j]);
      if (ids[i] == ids[j]) {
        printf("collision: ids[%lld] == ids[%lld]\n\n", i, j);
      }
    }
  }

  pageallocator_deinit(&a);
}

UTEST_F(TypeTest, TypeIdTuple) {
  PageAllocator_ a;
  pageallocator_init(&a, 4096);
  MemoryAllocator_* m = (MemoryAllocator_*)&a;

  Type_* ty = (Type_*)&Int_Ty;
  uint64_t ids[] = {
    type_id(make_tuple_ty(m, 2, (Type_*)(&Int_Ty), (Type_*)(&Int_Ty))),
    type_id(make_tuple_ty(m, 2, (Type_*)(&Int_Ty), (Type_*)(&Uint_Ty))),
    type_id(make_tuple_ty(m, 2, (Type_*)(&Uint_Ty), (Type_*)(&Int_Ty))),
  };

  size_t count = sizeof(ids) / sizeof(ids[0]);
  for (size_t i = 0; i < count; ++i) {
    for (size_t j = i + 1; j < count; ++j) {
      EXPECT_NE(ids[i], ids[j]);
      if (ids[i] == ids[j]) {
        printf("collision: ids[%lld] == ids[%lld]\n\n", i, j);
      }
    }
  }

  pageallocator_deinit(&a);
}

UTEST_F(TypeTest, TypeIdUnion) {
  PageAllocator_ a;
  pageallocator_init(&a, 4096);
  MemoryAllocator_* m = (MemoryAllocator_*)&a;

  {
    uint64_t ids[] = {
      type_id(make_union_ty(m, 2, (Type_*)(&Int_Ty), (Type_*)(&Uint_Ty))),
      type_id(make_union_ty(m, 2, (Type_*)(&Uint_Ty), (Type_*)(&Int_Ty))),
    };

    size_t count = sizeof(ids) / sizeof(ids[0]);
    for (size_t i = 0; i < count; ++i) {
      for (size_t j = i + 1; j < count; ++j) {
        EXPECT_EQ(ids[i], ids[j]);
        if (ids[i] != ids[j]) {
          printf("ids[%lld] != ids[%lld]\n\n", i, j);
        }
      }
    }
  }

  {
    uint64_t ids[] = {
      type_id(make_union_ty(m, 1, (Type_*)(&Int_Ty))),
      type_id(make_union_ty(m, 1, (Type_*)(&Uint_Ty))),
    };

    size_t count = sizeof(ids) / sizeof(ids[0]);
    for (size_t i = 0; i < count; ++i) {
      for (size_t j = i + 1; j < count; ++j) {
        EXPECT_NE(ids[i], ids[j]);
        if (ids[i] == ids[j]) {
          printf("ids[%lld] == ids[%lld]\n\n", i, j);
        }
      }
    }
  }

  pageallocator_deinit(&a);
}

UTEST_F(TypeTest, TypeIdUnionManyTypes) {
  PageAllocator_ a;
  pageallocator_init(&a, 4096);
  MemoryAllocator_* m = (MemoryAllocator_*)&a;

  uint64_t ids[] = {
    type_id(make_union_ty(m, 10, (Type_*)(&Int_Ty), (Type_*)(&Uint_Ty),
        (Type_*)(&Uint_Ty), (Type_*)(&Uint_Ty), (Type_*)(&Uint_Ty), (Type_*)(&Uint_Ty),
        (Type_*)(&Uint_Ty), (Type_*)(&Uint_Ty), (Type_*)(&Uint_Ty), (Type_*)(&Uint_Ty))),
    type_id(make_union_ty(m, 10, (Type_*)(&Int_Ty), (Type_*)(&Uint_Ty),
        (Type_*)(&Uint_Ty), (Type_*)(&Uint_Ty), (Type_*)(&Uint_Ty), (Type_*)(&Uint_Ty),
        (Type_*)(&Uint_Ty), (Type_*)(&Uint_Ty), (Type_*)(&Uint_Ty), (Type_*)(&Uint_Ty))), };

  size_t count = sizeof(ids) / sizeof(ids[0]);
  for (size_t i = 0; i < count; ++i) {
    for (size_t j = i + 1; j < count; ++j) {
      EXPECT_EQ(ids[i], ids[j]);
      if (ids[i] != ids[j]) {
        printf("ids[%lld] != ids[%lld]\n\n", i, j);
      }
    }
  }

  pageallocator_deinit(&a);
}

UTEST_F(TypeTest, TypeIdAll) {
  PageAllocator_ a;
  pageallocator_init(&a, 4096);
  MemoryAllocator_* m = (MemoryAllocator_*)&a;

  Type_* ty = (Type_*)&Int_Ty;
  uint64_t ids[] = {
    type_id((Type_*)&Unknown_Ty),
    type_id((Type_*)&Nil_Ty),
    type_id((Type_*)&Bool_Ty),
    type_id((Type_*)&Int_Ty),
    type_id((Type_*)&Int8_Ty),
    type_id((Type_*)&Int16_Ty),
    type_id((Type_*)&Int32_Ty),
    type_id((Type_*)&Int64_Ty),
    type_id((Type_*)&Uint_Ty),
    type_id((Type_*)&Uint8_Ty),
    type_id((Type_*)&Uint16_Ty),
    type_id((Type_*)&Uint32_Ty),
    type_id((Type_*)&Uint64_Ty),
    type_id((Type_*)&Float_Ty),
    type_id((Type_*)&Double_Ty),
    type_id((Type_*)&String_Ty),
    type_id(make_const_ty(ty, m)),
    type_id(make_in_ty(ty, m)),
    type_id(make_out_ty(ty, m)),
    type_id(make_var_ty(ty, m)),
    type_id(make_field_ty((Token_) { 0 }, ty, m)),
  };

  size_t count = sizeof(ids) / sizeof(ids[0]);
  for (size_t i = 0; i < count; ++i) {
    for (size_t j = i + 1; j < count; ++j) {
      EXPECT_NE(ids[i], ids[j]);
      if (ids[i] == ids[j]) {
        printf("collision: ids[%lld] == ids[%lld]\n\n", i, j);
      }
    }
  }

  pageallocator_deinit(&a);
}
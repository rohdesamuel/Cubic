#ifndef MEMORY__H
#define MEMORY__H

#include "common.h"

#define GROW_CAPACITY(capacity) \
    ((capacity) < 8 ? 8 : (capacity) * 2)

#define GROW_ARRAY(info, pointer, old_count, new_count) \
    (info*)reallocate(pointer, sizeof(info) * (old_count), \
        sizeof(info) * (new_count))

#define FREE_ARRAY(info, pointer, old_count) \
    reallocate(pointer, sizeof(info) * (old_count), 0)

void* reallocate(void* pointer, size_t old_size, size_t new_size);

#define alloc_ty(allocator, ty) alloc_((struct MemoryAllocator_*)(allocator), sizeof(ty))
#define alloc(allocator, size)  alloc_((struct MemoryAllocator_*)(allocator), size)
#define dealloc(allocator, ptr) dealloc_((struct MemoryAllocator_*)(allocator), ptr)

void* alloc_(struct MemoryAllocator_* allocator, size_t size);
void dealloc_(struct MemoryAllocator_* allocator, void* ptr);

#define ARRAY_GROW(arr, ty) (ty*)array_grow(arr)
#define ARRAY_FREE(arr) array_free(arr)

typedef struct Array_ {
  int count;
  int capacity;
  size_t elm_size;
  uint8_t* data;
} Array_;

void array_init(Array_* array, struct MemoryAllocator_* allocator, size_t elm_size);
void* array_grow(Array_* array);

typedef struct ListNode_ {
  struct ListNode_* next;
  void* data;
} ListNode_;

typedef struct List_ {
  size_t val_size;
  size_t node_size;
  struct ListNode_* head;
  struct ListNode_* tail;
  struct MemoryAllocator_* allocator;

  int count;
} List_;


#define ListOf_(TY) List_

#define list_of(PLIST, TY, ALLOCATOR) do {list_init(PLIST, sizeof(TY), ALLOCATOR);} while(0)
#define list_ptr(NODE, TY) ((TY*)(&(NODE)->data))
#define list_val(NODE, TY) (*list_ptr(NODE, TY))

void list_init(struct List_* list, size_t val_size, struct MemoryAllocator_* allocator);
void list_clear(List_* list);
ListNode_* list_push(List_* list, void* pval);

typedef struct MemoryAllocator_ {
  // Allocate a block of memory with `size`.
  void* (*allocate)(struct MemoryAllocator_* base, size_t size);

  // Deallocate a block of memory.
  void (*deallocate)(struct MemoryAllocator_* base, void* ptr);

  // Invalidates all currently allocated memory without deleting the buffer.
  void (*clear)(struct MemoryAllocator_* base);
} *MemoryAllocator, MemoryAllocator_;

typedef struct __PageAllocator_Page {
  char* memory;
  size_t offset;
  struct __PageAllocator_Page* next;
} __PageAllocator_Page;

typedef struct PageAllocator_ {
  MemoryAllocator_ base;

  struct __PageAllocator_Page* head;
  struct __PageAllocator_Page* oversized;
  size_t page_size;
} PageAllocator_;

typedef struct LinearAllocator_ {
  MemoryAllocator_ base;

  char* slab;
  size_t offset;
  size_t max_size;
} LinearAllocator_;

void memory_initialize();

extern MemoryAllocator_ NoopAllocator;
extern MemoryAllocator_ DefaultAllocator;

// Returns the default memory allocator using malloc/free.
// This is registered with the name "default".
MemoryAllocator memallocator_default();

// Creates a memory pool allocator.
// 
// Description: A memory pool allocator keeps track of slabs of memory in a
//              linked list. Every allocation searches in the list of allocated
//              blocks and finds the slab with the best fit. If not, allocates
//              a new slab.
// 
// Behavior:
//   - allocate(size): Allocates a block of memory of size `size`. Returns nullptr
//                  if host is OOM.
//   - deallocate(ptr): deallocates the given pointer.
//   - flush(frame): noop
//   - clear(): noop
// MemoryAllocator memallocator_pool();

// Creates a linear allocator.
// 
// Description: A linear allocator is a simple stack allocator. The memory is
//              pre-allocated and an frame_offset is moved based on how much memory
//              is `alloc`ed. Memory cannot be deallocated. 
// 
// Behavior:
//   - allocate(size): Allocates a block of memory of size `size`. Returns nullptr
//                  if size + total allocated is greater than max size.
//   - deallocate(ptr): noop
//   - flush(frame): noop
//   - clear(): resets frame_offset to zero.
void linearallocator_init(LinearAllocator_* allocator, size_t max_size);
void linearallocator_deinit(LinearAllocator_* allocator);

// Creates a paged allocator.
// 
// Description: A paged allocator is a linear allocator without a max size. The
//              memory is allocated in `page_size` increments, OR `size` if the
//              size is greater than the page size.
// 
// Behavior:
//   - allocate(size): Allocates a block of memory of size `size`.
//   - deallocate(ptr): noop
//   - flush(frame): noop
//   - clear(): Deallocates all memory.
void pageallocator_init(PageAllocator_* allocator, size_t page_size);
void pageallocator_deinit(PageAllocator_* allocator);

// Creates a stack allocator.
// 
// Description: A stack allocator acts like a linear allocator that allows
//              deallocation. This is implemented by tracking the size of each
//              allocation in a header. Memory needs to be popped off in order
//              of allocation.
// 
// Behavior:
//   - allocate(size): Allocates a block of memory of size `size` on top of the
//                  stack. Returns nullptr if size + total allocated is
//                  greater than max size.
//   - deallocate(ptr): Deallocates the given pointer. Assumes that the pointer
//                   is at the top of the stack. The stack becomes corrupted if
//                   trying to deallocate a pointer not at the top.
//   - flush(frame): noop
//   - clear(): resets frame_offset to zero.
// MemoryAllocator memallocator_stack(size_t max_size);

// Creates a object pool allocator.
// 
// Description: An object pool allocator holds a preallocated number of
//              objects. Allocating from the object pool retrieves an object
//              from the pool. The pool has a `pool_count` objects of size
//              `object_size`.
// 
// Behavior:
//   - allocate(unused): Returns an object if available. Returns nullptr if the
//                    pool has allocated all objects already.
//   - deallocate(ptr): Returns the object to the pool.
//   - flush(frame): noop
//   - clear(): Returns all allocated objects to the pool. Any reference to an
//              allocated object becomes invalid.
// MemoryAllocator memallocator_objectpool(size_t object_size, size_t pool_count);

#endif  // MEMORY__H
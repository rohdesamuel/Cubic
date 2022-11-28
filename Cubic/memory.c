#include <varargs.h>
#include <stdlib.h>
#include <string.h>

#include "memory.h"

void* reallocate(void* pointer, size_t old_size, size_t new_size) {
  if (new_size == 0) {
    free(pointer);
    return NULL;
  }

  void* result = realloc(pointer, new_size);
  if (result == NULL) exit(1);

  return result;
}

#if 0
struct MemoryPoolAllocator {
  // https://github.com/Isty001/mem-pool
  MemoryAllocator_ self;

  MemoryPoolAllocator() {
    pool_variable_init(&pool, 4096, 75);

    self.destroy = MemoryPoolAllocator::destroy;
    self.alloc = MemoryPoolAllocator::alloc;
    self.dealloc = MemoryPoolAllocator::dealloc;
    self.flush = nullptr;
    self.clear = nullptr;
  }

  static inline VariableMemPool* to_impl(MemoryAllocator_* self) {
    return ((MemoryPoolAllocator*)self)->pool;
  }

  static void destroy(MemoryAllocator_* self) {
    pool_variable_destroy(to_impl(self));
    delete ((MemoryPoolAllocator*)self);
  }

  static void* alloc(MemoryAllocator_* self, size_t size) {
    void* ret;
    pool_variable_alloc(to_impl(self), size, &ret);
    return ret;
  }

  static void dealloc(MemoryAllocator_* self, void* ptr) {
    pool_variable_free(to_impl(self), ptr);
  }

  VariableMemPool* pool;
};

MemoryAllocator _memallocator_pool() {
  return (MemoryAllocator)(new MemoryPoolAllocator());
}

struct MemoryStackAllocator {
  MemoryAllocator_ self;

  struct Header {
    uint32_t size;
  };

  MemoryStackAllocator(size_t max_size) {
    slab_ = new char[max_size];
    offset_ = 0;
    max_size_ = max_size;

    self.destroy = MemoryStackAllocator::destroy;
    self.alloc = MemoryStackAllocator::alloc;
    self.dealloc = MemoryStackAllocator::dealloc;
    self.clear = MemoryStackAllocator::clear;
    self.flush = nullptr;
  }

  static void destroy(MemoryAllocator_* allocator) {
    MemoryStackAllocator* self = (MemoryStackAllocator*)allocator;

    delete[] self->slab_;
    delete self;
  }

  static void* alloc(MemoryAllocator_* allocator, size_t size) {
    MemoryStackAllocator* self = (MemoryStackAllocator*)allocator;
    size_t alloc_size = size + sizeof(Header);

    if (self->offset_ + alloc_size > self->max_size_) {
      return nullptr;
    }

    char* new_memory = ((MemoryStackAllocator*)self)->slab_ + self->offset_;
    void* ret = new_memory + sizeof(Header);
    ((Header*)new_memory)->size = alloc_size;

    self->offset_ += alloc_size;
    return ret;
  }

  static void dealloc(MemoryAllocator_* allocator, void* ptr) {
    MemoryStackAllocator* self = (MemoryStackAllocator*)allocator;
    Header* header = (Header*)((char*)(ptr)-sizeof(Header));

    DEBUG_ASSERT((char*)header == self->slab_ + self->offset_ - header->size, -1);

    self->offset_ -= header->size;
  }

  static void clear(MemoryAllocator_* allocator) {
    MemoryStackAllocator* self = (MemoryStackAllocator*)allocator;
    self->offset_ = 0;
  }

  char* slab_;
  size_t offset_;
  size_t max_size_;
};

MemoryAllocator _memallocator_stack(size_t max_size) {
  return (MemoryAllocator)(new MemoryStackAllocator(max_size));
}
#endif

void list_init(struct List_* list, size_t val_size, struct MemoryAllocator_* allocator) {
  memset(list, 0, sizeof(List_));
  list->val_size = val_size;
  list->node_size = val_size <= sizeof(void*) ? 0 : val_size + sizeof(struct ListNode_);
  list->allocator = allocator;
}

void list_clear(List_* list) {
  struct ListNode_* cur = list->head;
  while (cur) {
    struct ListNode_* next = cur->next;
    if (list->allocator) {
      dealloc(list->allocator, cur);
    } else {
      free(cur);
    }
    cur = next;
  }
  list->head = NULL;
  list->tail = NULL;
}

ListNode_* list_push(List_* list, void* val) {
  struct ListNode_* n;
  if (list->allocator) {
    n = alloc(list->allocator, list->node_size);
  } else {
    n = malloc(list->node_size);
  }  
  n->next = NULL;

  if (val) {
    memcpy(&n->data, val, list->val_size);
  } else {
    n->data = NULL;
  }  

  if (!list->head) list->head = n;
  if (list->tail) list->tail->next = n;

  list->tail = n;
  return n;
}

///////////////////////////////////////////////////////////////////////////////
static void* pageallocator_alloc(MemoryAllocator_* base, size_t size);
static void pageallocator_dealloc(MemoryAllocator_* base, void* ptr) {}
static void pageallocator_clear(MemoryAllocator_* base);
static struct __PageAllocator_Page* pageallocator_newpage(size_t page_size);

void pageallocator_init(PageAllocator_* allocator, size_t page_size) {
  memset(allocator, 0, sizeof(PageAllocator_));
  allocator->page_size = page_size;
  allocator->base.alloc = pageallocator_alloc;
  allocator->base.dealloc = pageallocator_dealloc;
  allocator->base.clear = pageallocator_clear;

  allocator->head = pageallocator_newpage(page_size);
}

void pageallocator_deinit(PageAllocator_* allocator) {
  pageallocator_clear(&allocator->base);
  free(allocator->head);
  memset(allocator, 0, sizeof(PageAllocator_));
}

static void* pageallocator_alloc(MemoryAllocator_* base, size_t size) {
  PageAllocator_* self = (PageAllocator_*)base;
  size_t page_size = self->page_size;
  struct __PageAllocator_Page* head = self->head;

  if (size > page_size) {
    struct __PageAllocator_Page* page = pageallocator_newpage(size);
    page->next = self->oversized;
    self->oversized = page;
    return page->memory;
  }

  if (head->offset + size > page_size) {
    struct __PageAllocator_Page* page = pageallocator_newpage(self->page_size);
    page->next = self->head;
    self->head = page;
    head = self->head;
  }

  void* ret = head->memory + head->offset;
  head->offset += size;
  return ret;
}

void pageallocator_clear(MemoryAllocator_* base) {
  PageAllocator_* self = (PageAllocator_*)base;

  struct __PageAllocator_Page* cur = self->head;
  while (cur->next) {
    struct __PageAllocator_Page* next = cur->next;
    free(cur);
    cur = next;
  }
  self->head = cur;
  self->head->offset = 0;

  cur = self->oversized;
  while (cur) {
    struct __PageAllocator_Page* next = cur->next;
    free(cur);
    cur = next;
  }
  self->oversized = NULL;
}

static struct __PageAllocator_Page* pageallocator_newpage(size_t page_size) {
  struct __PageAllocator_Page* page = calloc(1, sizeof(struct __PageAllocator_Page) + page_size);
  page->memory = (char*)page + sizeof(struct __PageAllocator_Page);
  
  return page;
}

///////////////////////////////////////////////////////////////////////////////
static void* stackallocator_alloc(MemoryAllocator_* base, size_t size);
static void  stackallocator_dealloc(MemoryAllocator_* base, void* ptr);
static void  stackallocator_clear(MemoryAllocator_* base);


static void* stackallocator_alloc(MemoryAllocator_* base, size_t size) {
  LinearAllocator_* self = (LinearAllocator_*)base;
  size_t offset = self->offset;

  if (offset + size > self->max_size) {
    return NULL;
  }

  void* ret = self->slab + offset;
  self->offset += size;
  return ret;
}

static void stackallocator_dealloc(MemoryAllocator_* base, void* ptr) {}

static void stackallocator_clear(MemoryAllocator_* base) {
  LinearAllocator_* self = (LinearAllocator_*)base;
  self->offset = 0;
}

void linearallocator_init(LinearAllocator_* allocator, size_t max_size) {
  memset(allocator, 0, sizeof(LinearAllocator_));
  allocator->max_size = max_size;

  allocator->slab = malloc(max_size);
  allocator->offset = 0;
  allocator->max_size = max_size;

  allocator->base.alloc = stackallocator_alloc;
  allocator->base.dealloc = stackallocator_dealloc;
  allocator->base.clear = stackallocator_clear;
}

void linearallocator_deinit(LinearAllocator_* allocator) {
  free(allocator->slab);
  memset(allocator, 0, sizeof(LinearAllocator_));
}

///////////////////////////////////////////////////////////////////////////////
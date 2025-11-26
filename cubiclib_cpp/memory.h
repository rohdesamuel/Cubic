#ifndef MEMORY__H
#define MEMORY__H

#include <utility>

#include "common.h"

class MemoryAllocator {
public:
  virtual ~MemoryAllocator() = default;
  virtual void* alloc(size_t size) = 0;
  virtual void dealloc(void* ptr) = 0;
  virtual void clear() = 0;

  template <class Ty_>
  Ty_* alloc(size_t count = 1) {
    void* mem = alloc(count * sizeof(Ty_));
    assert(mem);

    return (Ty_*)mem;
  }

  template <class Ty_, class... Args_>
  Ty_* make(Args_&&... args) {
    void* mem = alloc(sizeof(Ty_));
    assert(mem);

    return new (mem)Ty_(std::forward<Args_>(args)...);
  }
};

// Creates a paged allocator.
// 
// Description: A paged allocator is a linear allocator without a max size. The
//              memory is allocated in `page_size` increments, OR `size` if the
//              size is greater than the page size.
// 
// Behavior:
//   - alloc(size): Allocates a block of memory of size `size`.
//   - dealloc(ptr): noop
//   - clear(): Deallocates all memory.
class PageAllocator : MemoryAllocator {
public:
  PageAllocator(size_t page_size);
  ~PageAllocator() override;

  void* alloc(size_t size) override;
  void dealloc(void* ptr) override;
  void clear() override;

private:
  struct __PageAllocator_Page {
    char* memory;
    size_t offset;
    struct __PageAllocator_Page* next;
  };

  __PageAllocator_Page* newpage(size_t page_size);

  struct __PageAllocator_Page* head_ = nullptr;
  struct __PageAllocator_Page* oversized_ = nullptr;
  size_t page_size_ = 0;
};

// Creates a linear allocator.
// 
// Description: A linear allocator is a simple stack allocator. The memory is
//              pre-allocated and an frame_offset is moved based on how much memory
//              is `alloc`ed. Memory cannot be deallocated. 
// 
// Behavior:
//   - alloc(size): Allocates a block of memory of size `size`. Returns nullptr
//                  if size + total allocated is greater than max size.
//   - dealloc(ptr): noop
//   - clear(): resets frame_offset to zero.
class LinearAllocator : MemoryAllocator {
public:
  LinearAllocator(size_t max_size);
  ~LinearAllocator() override;

  void* alloc(size_t size) override;
  void dealloc(void* ptr) override;
  void clear() override;

private:
  char* slab_;
  size_t offset_;
  size_t max_size_;
};

#endif  // MEMORY__H
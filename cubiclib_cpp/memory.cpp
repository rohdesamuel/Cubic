#include <varargs.h>
#include <stdlib.h>
#include <string.h>

#include "memory.h"

///////////////////////////////////////////////////////////////////////////////
static struct __PageAllocator_Page* pageallocator_newpage(size_t page_size);

PageAllocator::PageAllocator(size_t page_size): page_size_(page_size) {
  head_ = newpage(page_size);
}

PageAllocator::~PageAllocator() {
  clear();
  free(head_);
}

void* PageAllocator::alloc(size_t size) {
  size_t page_size = page_size_;
  __PageAllocator_Page* head = head_;

  if (size > page_size) {
    __PageAllocator_Page* page = newpage(size);
    page->next = oversized_;
    oversized_ = page;
    return page->memory;
  }

  if (head->offset + size > page_size) {
    __PageAllocator_Page* page = newpage(page_size_);
    page->next = head_;
    head_ = page;
    head = head_;
  }

  void* ret = head->memory + head->offset;
  head->offset += size;
  return ret;
}

void PageAllocator::dealloc(void*) {}

void PageAllocator::clear() {
  struct __PageAllocator_Page* cur = head_;
  while (cur->next) {
    struct __PageAllocator_Page* next = cur->next;
    free(cur);
    cur = next;
  }
  head_ = cur;
  head_->offset = 0;

  cur = oversized_;
  while (cur) {
    struct __PageAllocator_Page* next = cur->next;
    free(cur);
    cur = next;
  }
  oversized_ = nullptr;
}

PageAllocator::__PageAllocator_Page* PageAllocator::newpage(size_t page_size) {
  __PageAllocator_Page* page = (__PageAllocator_Page*)calloc(1, sizeof(struct __PageAllocator_Page) + page_size);
  page->memory = (char*)page + sizeof(struct __PageAllocator_Page);

  return page;
}

///////////////////////////////////////////////////////////////////////////////

LinearAllocator::LinearAllocator(size_t max_size): offset_(0), max_size_(max_size) {
  slab_ = (char*)malloc(max_size);
}

LinearAllocator::~LinearAllocator() {
  free(slab_);
}

void* LinearAllocator::alloc(size_t size) {
  size_t offset = offset_;

  if (offset + size > max_size_) {
    return NULL;
  }

  void* ret = slab_ + offset;
  offset_ += size;
  return ret;
}

void LinearAllocator::dealloc(void* ptr) {}

void LinearAllocator::clear() {
  offset_ = 0;
}
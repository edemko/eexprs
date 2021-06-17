#include "dynarr.h"

#include <stdlib.h>
#include <string.h>

#include "common.h"

void _dynarr_init(_dynarr* arr, size_t initialCapacity, size_t elemSize) {
  arr->data = malloc(initialCapacity * elemSize);
  checkOom(arr->data);
  arr->cap = initialCapacity;
  arr->len = 0;
}

void _dynarr_deinit(_dynarr* arr) {
  if (arr->data != NULL) {
    arr->cap = 0;
    arr->len = 0;
    free(arr->data);
    arr->data = NULL;
  }
}

void _dynarr_push(_dynarr* arr, const void* elem, size_t elemSize) {
  if (arr->len == arr->cap) {
    if (arr->cap == 0) { arr->cap = 4; }
    else { arr->cap *= 2; }
    arr->data = realloc(arr->data, arr->cap * elemSize);
    checkOom(arr->data);
  }
  memcpy(&arr->data[elemSize * arr->len], elem, elemSize);
  arr->len += 1;
}

void* _dynarr_peek(const _dynarr* arr, size_t elemSize) {
  if (arr->len == 0) { return NULL; }
  return &arr->data[elemSize * (arr->len - 1)];
}

void* _dynarr_pop(_dynarr* arr, size_t elemSize) {
  if (arr->len == 0) { return NULL; }
  arr->len -= 1;
  return &arr->data[elemSize * arr->len];
}

/*
A malloc-backed polymorphic resizable array list for C that keeps elements unboxed.

## Usage

Make sure that the C file is included in your object files
(either by compiling as its own translation unit, or as part of a larger unit).

Then, instantiate slabboc at a `typeName` (which must be a type identifier, not a type expression) with:

```
#define TYPE <typeName>
#include <this header>
```

This sequence both declares and defines everything necessary to work with 
It is not necessary to include the header without `TYPE` defined, nor should you include the C file with `TYPE` defined.
The header will automatically undefine `TYPE` when it is done.
*/
#ifndef SHIM_DYNARR_H
#define SHIM_DYNARR_H

#include <stddef.h>


#define dynarr(T) __dynarr(T)
#define __dynarr(T) dynarr_ ## T
typedef struct _dynarr {
  size_t cap;
  size_t len;
  char* data;
} _dynarr;

#define dynarr_init(T) __dynarr_init(T)
#define __dynarr_init(T) dynarr_init_ ## T
// mallocs new internal data structures, and initialize length and capacity
// it does not attempt to clean up previous data
void _dynarr_init(_dynarr* arr, size_t initialCapacity, size_t elemSize);

#define dynarr_deinit(T) __dynarr_deinit(T)
#define __dynarr_deinit(T) dynarr_deinit_ ## T
// frees internal data structures used by the dynarr
// makes no attempt to free any pointers owned by the elements
void _dynarr_deinit(_dynarr* arr);

#define dynarr_push(T) __dynarr_push(T)
#define __dynarr_push(T) dynarr_push_ ## T
// copies an element to the end of the dynamic array, resizing if necessary
void _dynarr_push(_dynarr* arr, const void* elem, size_t elemSize);

#define dynarr_peek(T) __dynarr_peek(T)
#define __dynarr_peek(T) dynarr_peek_ ## T
// return a reference to the last element of the array
// return NULL if length is zero
void* _dynarr_peek(const _dynarr* arr, size_t elemSize);

#define dynarr_pop(T) __dynarr_pop(T)
#define __dynarr_pop(T) dynarr_pop_ ## T
// removes the last element and returns a reference to it, which the caller must take ownership of
// the reference only lasts until the next time elements are added to the array, or the array shrinks
// returns NULL if length is zero
void* _dynarr_pop(_dynarr* arr, size_t elemSize);

#endif


#ifdef TYPE

typedef struct dynarr(TYPE) {
  size_t cap;
  size_t len;
  TYPE* data;
} dynarr(TYPE);

// sanity check on compiler struct layout algorithm
_Static_assert( __builtin_offsetof(dynarr(TYPE), cap) == __builtin_offsetof(_dynarr, cap)
              , "layout of polymorphic dynarr does not match _dynarr");
_Static_assert( __builtin_offsetof(dynarr(TYPE), len) == __builtin_offsetof(_dynarr, len)
              , "layout of polymorphic dynarr does not match _dynarr");
_Static_assert( __builtin_offsetof(dynarr(TYPE), data) == __builtin_offsetof(_dynarr, data)
              , "layout of polymorphic dynarr does not match _dynarr");

static inline
void dynarr_init(TYPE)(dynarr(TYPE)* arr, size_t initialCapacity) {
  _dynarr_init((_dynarr*)arr, initialCapacity, sizeof(TYPE));
}

static inline
void dynarr_deinit(TYPE)(dynarr(TYPE)* arr) {
  _dynarr_deinit((_dynarr*)arr);
}


static inline
void dynarr_push(TYPE)(dynarr(TYPE)* arr, const TYPE* elem) {
  _dynarr_push((_dynarr*)arr, (const void*)elem, sizeof(TYPE));
}

static inline
TYPE* dynarr_peek(TYPE)(const dynarr(TYPE)* arr) {
  return (TYPE*)_dynarr_peek((_dynarr*)arr, sizeof(TYPE));
}

static inline
TYPE* dynarr_pop(TYPE)(dynarr(TYPE)* arr) {
  return (TYPE*)_dynarr_pop((_dynarr*)arr, sizeof(TYPE));
}

  #undef TYPE
#endif

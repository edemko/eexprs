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


typedef struct _dynarr {
  size_t cap;
  size_t len;
  char* data;
} _dynarr;

// mallocs new internal data structures, and initialize length and capacity
// it does not attempt to clean up previous data
void _dynarr_init(_dynarr* arr, size_t initialCapacity, size_t elemSize);

// frees internal data structures used by the dynarr
// makes no attempt to free any pointers owned by the elements
void _dynarr_deinit(_dynarr* arr);

// copies an element to the end of the dynamic array, resizing if necessary
void _dynarr_push(_dynarr* arr, const void* elem, size_t elemSize);

// return a reference to the last element of the array
// return NULL if length is zero
void* _dynarr_peek(const _dynarr* arr, size_t elemSize);

// removes the last element and returns a reference to it, which the caller must take ownership of
// the reference only lasts until the next time elements are added to the array, or the array shrinks
// returns NULL if length is zero
void* _dynarr_pop(_dynarr* arr, size_t elemSize);

#endif


#ifdef TYPE
  // macros to paste expanded arguments
  #define _dynarr_paste(T) dynarr_ ## T
  #define _dynarr_init_paste(T) dynarr_init_ ## T
  #define _dynarr_deinit_paste(T) dynarr_deinit_ ## T
  #define _dynarr_push_paste(T) dynarr_push_ ## T
  #define _dynarr_peek_paste(T) dynarr_peek_ ## T
  #define _dynarr_pop_paste(T) dynarr_pop_ ## T
  // macros I actually use
  #define dynarr(T) _dynarr_paste(T)
  #define dynarr_init(T) _dynarr_init_paste(T)
  #define dynarr_deinit(T) _dynarr_deinit_paste(T)
  #define dynarr_push(T) _dynarr_push_paste(T)
  #define dynarr_peek(T) _dynarr_peek_paste(T)
  #define dynarr_pop(T) _dynarr_pop_paste(T)

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

  #undef dynarr
  #undef dynarr_init
  #undef dynarr_deinit
  #undef dynarr_push
  #undef dynarr_peek
  #undef dynarr_pop
  #undef _dynarr_paste
  #undef _dynarr_init_paste
  #undef _dynarr_deinit_paste
  #undef _dynarr_push_paste
  #undef _dynarr_peek_paste
  #undef _dynarr_pop_paste
  #undef TYPE
#endif

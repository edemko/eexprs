/*
A malloc-backed polymorphic doubly-linked list for C that keeps elements unboxed.

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
#ifndef SHIM_DLLIST_H
#define SHIM_DLLIST_H

#include <stddef.h>


typedef struct _dllistNode _dllistNode;
struct _dllistNode {
  _dllistNode* next; // owned, null for end of stream
  _dllistNode* prev; // aliased, null for start of stream
  char here[];
};

typedef struct _dllist {
  _dllistNode* start; // null for empty list
  _dllistNode* end; // null for empty list
} _dllist;


// copies the element into a new singleton list
_dllist _dllist_singleton(const void* elem, size_t elemSize);

// Concatenate two lists.
// Ownership of the nodes is taken, and given to the return value.
_dllist _dllist_cat(_dllist* l1, _dllist* l2);

// Copies an element into a new node placed just before the given node.
// If the given node is NULL, inserts at the start of the list.
// It is undefined behaviour for the node to not be neither in the list nor NULL.
// Returns a reference to the new node (which is owned by the list).
_dllistNode* _dllist_insertBefore(_dllist* list, const void* elem, _dllistNode* node, size_t elemSize);

// Copies an element into a new node placed just after the given node.
// If the given node is NULL, inserts at the end of the list.
// It is undefined behaviour for the node to not be neither in the list nor NULL.
// Returns a reference to the new node (which is owned by the list).
_dllistNode* _dllist_insertAfter(_dllist* list, _dllistNode* node, const void* elem, size_t elemSize);

// Move the source node from the source list to the destination list, inserting directly after the destination node.
// The relationship between the destination list and node is like that of _dllist_insertAfter.
// Also, if the source node is not part of the source list, that is undefined behavior as well.
// Ownership of the memory for the node is transferred from the source list to the destination list.
void _dllist_moveAfter(_dllist* dstList, _dllistNode* dstNode, _dllist* srcList, _dllistNode* srcNode);

// Removes the first element of a (non-null, non-empty) list and copies it into the given address.
// If the address is NULL, the copy does not occur.
// The memory used by the node is freed.
void _dllist_popStart(_dllist* list, void* into, size_t elemSize);

// Removes the last element of a (non-null, non-empty) list and copies it into the given address.
// If the address is NULL, the copy does not occur.
// The memory used by the node is freed.
void _dllist_popEnd(_dllist* list, void*, size_t elemSize);

// free the memory used for the list, and re-initialize as empty
// does not attempt to free any memory owned by the elements
void _dllist_del(_dllist* list);

#endif


#ifdef TYPE
  // macros to paste expanded arguments
  #define _dllistNode_paste(T) dllistNode_ ## T
  #define _dllist_paste(T) dllist_ ## T
  #define _dllist_empty_paste(T) dllist_empty_ ## T
  #define _dllist_singleton_paste(T) dllist_singleton_ ## T
  #define _dllist_cat_paste(T) dllist_cat_ ## T
  #define _dllist_insertBefore_paste(T) dllist_insertBefore_ ## T
  #define _dllist_insertAfter_paste(T) dllist_insertAfter_ ## T
  #define _dllist_moveAfter_paste(T) dllist_moveAfter_ ## T
  #define _dllist_popStart_paste(T) dllist_popStart_ ## T
  #define _dllist_popEnd_paste(T) dllist_popEnd_ ## T
  #define _dllist_del_paste(T) dllist_del_ ## T
  // macros I actually use
  #define dllistNode(T) _dllistNode_paste(T)
  #define dllist(T) _dllist_paste(T)
  #define dllist_empty(T) _dllist_empty_paste(T)
  #define dllist_singleton(T) _dllist_singleton_paste(T)
  #define dllist_cat(T) _dllist_cat_paste(T)
  #define dllist_insertBefore(T) _dllist_insertBefore_paste(T)
  #define dllist_insertAfter(T) _dllist_insertAfter_paste(T)
  #define dllist_moveAfter(T) _dllist_moveAfter_paste(T)
  #define dllist_popStart(T) _dllist_popStart_paste(T)
  #define dllist_popEnd(T) _dllist_popEnd_paste(T)
  #define dllist_del(T) _dllist_del_paste(T)

typedef struct dllistNode(TYPE) dllistNode(TYPE);
struct dllistNode(TYPE) {
  dllistNode(TYPE)* next;
  dllistNode(TYPE)* prev;
  TYPE here;
};

// sanity check on compiler struct layout algorithm
_Static_assert( __builtin_offsetof(dllistNode(TYPE), next) == __builtin_offsetof(_dllistNode, next)
              , "layout of polymorphic dllist does not match _dllist");
_Static_assert( __builtin_offsetof(dllistNode(TYPE), prev) == __builtin_offsetof(_dllistNode, prev)
              , "layout of polymorphic dllist does not match _dllist");
_Static_assert( __builtin_offsetof(dllistNode(TYPE), here) == __builtin_offsetof(_dllistNode, here)
              , "layout of polymorphic dllist does not match _dllist");

typedef struct dllist(TYPE) {
  dllistNode(TYPE)* start;
  dllistNode(TYPE)* end;
} dllist(TYPE);

// an empty list
static inline
dllist(TYPE) dllist_empty(TYPE)() {
  dllist(TYPE) out = {.start = NULL, .end = NULL};
  return out;
}

static inline
dllist(TYPE) dllist_singleton(TYPE)(const TYPE* elem) {
  _dllist it = _dllist_singleton((const void*)elem, sizeof(TYPE));
  return *(dllist(TYPE)*)&it;
}

static inline
dllist(TYPE) dllist_cat(TYPE)(dllist(TYPE)* l1, dllist(TYPE)* l2) {
  _dllist it = _dllist_cat((_dllist*)l1, (_dllist*)l2);
  return *(dllist(TYPE)*)&it;
}

static inline
dllistNode(TYPE)* dllist_insertBefore(TYPE)(dllist(TYPE)* list, const TYPE* elem, dllistNode(TYPE)* node) {
  return (dllistNode(TYPE)*)_dllist_insertBefore((_dllist*)list, (void*)elem, (_dllistNode*)node, sizeof(TYPE));
}

static inline
dllistNode(TYPE)* dllist_insertAfter(TYPE)(dllist(TYPE)* list, dllistNode(TYPE)* node, const TYPE* elem) {
  return (dllistNode(TYPE)*)_dllist_insertAfter((_dllist*)list, (_dllistNode*)node, (void*)elem, sizeof(TYPE));
}

static inline
void dllist_moveAfter(TYPE)(dllist(TYPE)* dstList, dllistNode(TYPE)* dstNode, dllist(TYPE)* srcList, dllistNode(TYPE)* srcNode) {
  _dllist_moveAfter((_dllist*)dstList, (_dllistNode*)dstNode, (_dllist*)srcList, (_dllistNode*)srcNode);
}

static inline
void dllist_popStart(TYPE)(dllist(TYPE)* list, TYPE* into) {
  _dllist_popStart((_dllist*)list, (void*)into, sizeof(TYPE));
}
static inline
void dllist_popEnd(TYPE)(dllist(TYPE)* list, TYPE* into) {
  _dllist_popEnd((_dllist*)list, (void*)into, sizeof(TYPE));
}


static inline
void dllist_del(TYPE)(dllist(TYPE)* list) {
  _dllist_del((_dllist*) list);
}

  #undef dllistNode
  #undef dllist
  #undef dllist_empty
  #undef dllist_singleton
  #undef dllist_cat
  #undef dllist_insertBefore
  #undef dllist_insertAfter
  #undef dllist_moveAfter
  #undef dllist_popStart
  #undef dllist_popEnd
  #undef dllist_del
  #undef _dllistNode_paste
  #undef _dllist_paste
  #undef _dllist_empty_paste
  #undef _dllist_singleton_paste
  #undef _dllist_cat_paste
  #undef _dllist_insertBefore_paste
  #undef _dllist_insertAfter_paste
  #undef _dllist_moveAfter_paste
  #undef _dllist_popStart_paste
  #undef _dllist_popEnd_paste
  #undef _dllist_del_paste
  #undef TYPE
#endif

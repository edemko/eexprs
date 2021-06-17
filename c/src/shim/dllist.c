#include "dllist.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"

static
_dllistNode* newNode(const void* elem, size_t elemSize) {
  _dllistNode* new = malloc(sizeof(_dllistNode) + elemSize);
  checkOom(new);
  memcpy(&new->here, elem, elemSize);
  return new;
}

_dllist _dllist_singleton(const void* elem, size_t elemSize) {
  _dllistNode* new = newNode(elem, elemSize);
  new->prev = NULL;
  new->next = NULL;
  _dllist out = {.start = new, .end = new };
  return out;
}

_dllist _dllist_cat(_dllist* l1, _dllist* l2) {
  if (l1->start == NULL) { return *l2; }
  if (l2->start == NULL) { return *l1; }
  l1->end->next = l2->start;
  l2->start->prev = l1->end;
  _dllist out = {.start = l1->start, .end = l2->end};
  return out;
}

_dllistNode* _dllist_insertBefore(_dllist* list, const void* elem, _dllistNode* node, size_t elemSize) {
  _dllistNode* new = newNode(elem, elemSize);
  if (node == NULL) {
    new->prev = NULL;
    new->next = list->start;
    if (list->start != NULL) { list->start->prev = new; }
    else { list->end = new; }
    list->start = new;
  }
  else {
    new->next = node;
    new->prev = node->prev;
    if (node->prev != NULL) { node->prev->next = new; }
    else { list->start = new; }
    node->prev = new;
  }
  return new;
}

_dllistNode* _dllist_insertAfter(_dllist* list, _dllistNode* node, const void* elem, size_t elemSize) {
  _dllistNode* new = newNode(elem, elemSize);
  if (node == NULL) {
    new->next = NULL;
    new->prev = list->end;
    if (list->end != NULL) { list->end->next = new; }
    else { list->start = new; }
    list->end = new;
  }
  else {
    new->prev = node;
    new->next = node->next;
    if (node->next != NULL) { node->next->prev = new; }
    else { list->end = new; }
    node->next = new;
  }
  return new;
}

void _dllist_moveAfter(_dllist* dstList, _dllistNode* dstNode, _dllist* srcList, _dllistNode* srcNode) {
  assert(srcNode != NULL);
  // extricate the source node
  if (srcNode->prev != NULL) { srcNode->prev->next = srcNode->next; }
  else { srcList->start = srcNode->next; }
  if (srcNode->next != NULL) { srcNode->next->prev = srcNode->prev; }
  else { srcList->end = srcNode->prev; }
  // place the source node into the destination list
  if (dstNode == NULL) {
    srcNode->next = NULL;
    srcNode->prev = dstList->end;
    if (dstList->end != NULL) { dstList->end->next = srcNode; }
    else { dstList->start = srcNode; }
    dstList->end = srcNode;
  }
  else {
    srcNode->prev = dstNode;
    srcNode->next = dstNode->next;
    if (dstNode->next != NULL) { dstNode->next->prev = srcNode; }
    else { dstList->end = srcNode; }
    dstNode->next = srcNode;
  }
}

void _dllist_popStart(_dllist* list, void* into, size_t elemSize) {
  _dllistNode* first = list->start;
  assert(first != NULL);
  if (first->next != NULL) {
    list->start = first->next;
    list->start->prev = NULL;
  }
  else {
    list->start = NULL;
    list->end = NULL;
  }
  if (into != NULL) {
    memcpy(into, &first->here, elemSize);
  }
  free(first);
}

void _dllist_popEnd(_dllist* list, void* into, size_t elemSize) {
  _dllistNode* last = list->end;
  assert(last != NULL);
  if (last->prev != NULL) {
    list->end = last->prev;
    list->end->next = NULL;
  }
  else {
    list->start = NULL;
    list->end = NULL;
  }
  if (into != NULL) {
    memcpy(into, &last->here, elemSize);
  }
  free(last);
}

void _dllist_del(_dllist* list) {
  _dllistNode* node = list->start;
  while (node != NULL) {
    _dllistNode* next = node->next;
    free(node);
    node = next;
  }
  list->start = NULL;
  list->end = NULL;
}

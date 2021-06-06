#ifndef COMMON_H
#define COMMON_H

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>


static inline
void checkOom(void* ptr) {
  if (ptr == NULL) {
    fprintf(stderr, "out of memory\n");
    exit(255);
  }
}


#endif

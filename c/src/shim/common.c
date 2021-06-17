#include "common.h"

#include <stdlib.h>
#include <stdio.h>

inline
void checkOom(void* ptr) {
  if (ptr == NULL) {
    fprintf(stderr, "out of memory\n");
    exit(255);
  }
}

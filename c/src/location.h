#include <stdlib.h>

typedef struct filelocPoint {
  size_t line;
  size_t col;
} filelocPoint;

typedef struct fileloc {
  filelocPoint start;
  filelocPoint end;
} fileloc;

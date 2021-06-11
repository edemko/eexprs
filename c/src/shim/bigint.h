#ifndef SHIM_BIGINT_H
#define SHIM_BIGINT_H

#include <stdbool.h>
#include <stdint.h>

#include "shim/strstuff.h"


typedef struct bigint {
  uint32_t* buf; // owned, little-endian
  bool pos; // is false for zero, since then everything will be zero (save `.len`)
  uint16_t len;
} bigint;

// malloc a zero bigint with mag zero
bigint bigint_new();
// malloc a zero bigint with mag zero
void bigint_del(bigint* obj);

// add a small number
void bigint_inc(bigint* base, uint32_t amt);

// subtract a small number
void bigint_dec(bigint* base, uint32_t amt);

// multiply by a small positive number
void bigint_scale(bigint* base, uint8_t amt);

// render in base 10, the str has a malloc'd buf pointer
str bigint_toDecimal(bigint val);

#endif

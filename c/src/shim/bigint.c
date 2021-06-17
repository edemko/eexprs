#include "bigint.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"


bigint bigint_new() {
  bigint new = {.pos = false, .len = 0, .buf = NULL};
  return new;
}

void bigint_del(bigint* obj) {
  if (obj->buf != NULL) {
    free(obj->buf);
    obj->buf = NULL;
  }
}


bigint bigint_clone(bigint orig) {
  bigint new = {.len = orig.len, .pos = orig.pos};
  for (; new.len > 0; --new.len) {
    if (orig.buf[new.len-1] != 0) { break; }
  }
  if (new.len == 0) {
    new.buf = NULL;
    new.pos = false;
  }
  else {
    new.buf = malloc(new.len * sizeof(uint32_t));
    checkOom(new.buf);
    for (size_t i = 0; i < new.len; ++i) {
      new.buf[i] = orig.buf[i];
    }
  }
  return new;
}

// increase the size of a bigint's buffer
void grow(bigint* a, uint32_t next) {
  if (a->len == 0) {
    a->buf = malloc(sizeof(uint32_t));
    checkOom(a->buf);
  }
  else {
    a->buf = realloc(a->buf, (a->len + 1) * sizeof(uint32_t));
    checkOom(a->buf);
  }
  a->buf[a->len] = next;
  a->len++;
}

void addMag(bigint* base, uint32_t amt) {
  uint32_t carry = amt;
  for (size_t i = 0; carry && i < base->len; ++i) {
    uint64_t b = base->buf[i];
    uint64_t c = b + carry;
    base->buf[i] = (uint32_t)c;
    carry = (uint32_t)(c >> 32);
  }
  if (carry) {
    grow(base, carry);
  }
}
void subMag(bigint* base, uint32_t amt) {
  uint32_t borrow = amt;
  for (size_t i = 0; borrow && i < base->len; ++i) {
    uint64_t b = base->buf[i];
    uint64_t c;
    if (borrow > b) {
      c = ((uint64_t)1<<32 | b) - borrow;
      borrow = 1;
    }
    else {
      c = b - borrow;
      borrow = 0;
    }
    base->buf[i] = (uint32_t)c;
  }
  if (base->buf[base->len-1] == 0) {
    base->len -= 1;
  }
}

void bigint_inc(bigint* base, uint32_t amt) {
  if (amt == 0) {/* do nothing */}
  else if (base->len == 0) {
    assert(base->buf == NULL);
    grow(base, amt);
    base->pos = true;
  }
  else if (!base->pos && base->len == 1 && base->buf[0] <= amt) {
    base->buf[0] = amt - base->buf[0];
    base->pos = true;
    if (base->buf[0] == 0) {
      free(base->buf);
      base->len = 0;
      base->pos = false;
    }
  }
  else {
    (base->pos ? addMag : subMag)(base, amt);
  }

}

void bigint_dec(bigint* base, uint32_t amt) {
  if (amt == 0) {/* do nothing */}
  else if (base->len == 0) {
    assert(base->buf == NULL);
    grow(base, amt);
    base->pos = false;
  }
  else if (base->pos && base->len == 1 && base->buf[0] <= amt) {
    base->buf[0] = amt - base->buf[0];
    base->pos = false;
    if (base->buf[0] == 0) {
      free(base->buf);
      base->len = 0;
      base->pos = false;
    }
  }
  else {
    (base->pos ? subMag : addMag)(base, amt);
  }
}

void bigint_scale(bigint* base, uint8_t amt) {
  if (base->buf == NULL) { return; }
  if (amt == 0) {
    free(base->buf);
    base->len = 0;
    base->pos = false;
    return;
  }
  uint32_t carry = 0;
  for (size_t i = 0; i < base -> len; ++i) {
    uint64_t b = base->buf[i];
    uint64_t c = b * (uint64_t)amt + (uint64_t)carry;
    base->buf[i] = (uint32_t)c;
    carry = (uint32_t)(c >> 32);
  }
  if (carry) {
    grow(base, carry);
  }
}

uint8_t extract(bigint* x) {
  if (x->len == 0) { return 0; }
  uint64_t r = 0;
  for (size_t i = x->len; i > 0; --i) {
    uint64_t b = (r << 32) + x->buf[i-1];
    uint64_t d = b / 10;
    r = b % 10;
    x->buf[i-1] = d;
  }
  if (x->buf[x->len-1] == 0) { x->len -= 1; }
  return r;
}
str bigint_toDecimal(bigint val) {
  if (val.len == 0) {
    assert(val.buf == NULL);
    str out = {.len = 1};
    out.bytes = malloc(1);
    checkOom(out.bytes);
    out.bytes[0] = '0';
    return out;
  }
  bigint tmp = bigint_clone(val);
  str out; uint8_t* next; {
    size_t maxBufLen = 1 + 9 * tmp.len;
    out.bytes = malloc(maxBufLen);
    checkOom(out.bytes);
    out.len = 0;
    next = &out.bytes[maxBufLen];
  }
  { // the core algorithm
    while (tmp.len > 0) {
      uint8_t d = extract(&tmp);
      *(--next) = '0' + d;
      out.len += 1;
    }
    if (!tmp.pos) {
      *(--next) = '-';
      out.len += 1;
    }
  }
  memmove(out.bytes, next, out.len);
  free(tmp.buf);
  return out;
}

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "shim/common.h"
#include "shim/strstuff.h"


str readFile(const char* filename) {
  str out = {
    .len = 0,
    .bytes = NULL
  };
  FILE* fp; {
    fp = fopen(filename, "r");
    if (fp == NULL) { return out; }
  }
  size_t fileSize; {
    fseek(fp, 0, SEEK_END);
    fileSize = ftell(fp);
    rewind(fp);
  }
  { // allocate space for the whole file
    out.bytes = malloc(fileSize);
    if (out.bytes == NULL) { fclose(fp); return out; }
  }
  out.len = fread(out.bytes, 1 /* element size in bytes */, fileSize /* number of elements to read */, fp);
  fclose(fp);
  if (out.len != fileSize) {
    out.len = 0;
    free(out.bytes);
    out.bytes = NULL;
  }
  return out;
}

str str_clone(const str orig) {
  str out = { .len = orig.len, .bytes = malloc(orig.len) };
  checkOom(out.bytes);
  memcpy(out.bytes, orig.bytes, orig.len);
  return out;
}

bool isPrefixOf(str s, str prefix) {
  if (s.len < prefix.len) { return false; }
  for (size_t i = 0; i < prefix.len; ++i) {
    if (s.bytes[i] != prefix.bytes[i]) { return false; }
  }
  return true;
}


strBuilder strBuilder_new(size_t cap0) {
  assert(cap0 > 0);
  strBuilder out = {.len = 0, .cap = cap0};
  out.bytes = malloc(out.cap * sizeof(uint8_t));
  checkOom(out.bytes);
  return out;
}

void strBuilder_appendByte(strBuilder* self, uint8_t c) {
  if (self->len == self->cap) {
    self->bytes = realloc(self->bytes, 2 * self->cap * sizeof(uint8_t));
    checkOom(self->bytes);
  }
  self->bytes[self->len++] = c;
}

void strBuilder_append(strBuilder* self, str other) {
  if (self->len + other.len > self->cap) {
    while (self->len + other.len > self->cap) { self->cap *= 2; }
    self->bytes = realloc(self->bytes, self->cap * sizeof(uint8_t));
    checkOom(self->bytes);
  }
  for (size_t i = 0; i < other.len; ++i) {
    self->bytes[self->len+i] = other.bytes[i];
  }
  self->len += other.len;
}



size_t peekUchar(char32_t* out, str in) {
  if (in.len == 0) {
    *out = UCHAR_NULL;
    return 0;
  }
  else if (!(in.bytes[0] & 0x80) /*0b0xxxxxxx*/) {
    *out = (int32_t)in.bytes[0];
    return 1;
  }
  else if (!(in.bytes[0] & 0x40) /* 0b10xxxxxx */) {
    goto badbyte;
  }
  else if (!(in.bytes[0] & 0x20) /* 0b110xxxxx */) {
    if ( in.len < 2
      || (in.bytes[1] & 0xC0) != 0x80
       ) {
      goto badbyte;
    }
    *out = ((int32_t)in.bytes[0] & 0x1F)<<6
         | ((int32_t)in.bytes[1] & 0x3F)
         ;
    return 2;
  }
  else if (!(in.bytes[0] & 0x10) /* 0b1110xxxx */) {
    if ( in.len < 3
      || ( ((in.bytes[1] & 0xC0) != 0x80)
         | ((in.bytes[2] & 0xC0) != 0x80)
         )
       ) {
      goto badbyte;
    }
    *out = ((int32_t)in.bytes[0] & 0x0F)<<12
         | ((int32_t)in.bytes[1] & 0x3F)<<6
         | ((int32_t)in.bytes[2] & 0x3F)
         ;
    return 3;
  }
  else if (!(in.bytes[0] & 0x08) /* 0b11110xxx */) {
    if ( in.len < 4
      || ( ((in.bytes[1] & 0xC0) != 0x80)
         | ((in.bytes[2] & 0xC0) != 0x80)
         | ((in.bytes[3] & 0xC0) != 0x80)
         )
       ) {
      goto badbyte;
    }
    *out = ((int32_t)in.bytes[0] & 0x07)<<18
         | ((int32_t)in.bytes[1] & 0x3F)<<12
         | ((int32_t)in.bytes[2] & 0x3F)<<6
         | ((int32_t)in.bytes[3] & 0x3F)
         ;
    return 4;
  }
  else badbyte: {
    *out = UCHAR_NULL;
    return 1;
  }
}

size_t peekUchars(char32_t* out, size_t n, str in) {
  size_t adv = 0;
  for (size_t i = 0; i < n; ++i) {
    size_t adv1 = peekUchar(&out[i], in);
    in.bytes += adv1;
    in.len -= adv1;
    adv += adv1;
  }
  return adv;
}


bool ucharElem(char32_t c, const char32_t* set) {
  // if (c == UCHAR_NULL) { return false; } // TODO would this help performance?
  for (size_t i = 0; set[i] != UCHAR_NULL; ++i) {
    if (set[i] == c) { return true; }
  }
  return false;
}

size_t ucharFind(char32_t c, const char32_t* set) {
  for (size_t i = 0; set[i] != UCHAR_NULL; ++i) {
    if (set[i] == c) { return i; }
  }
  assert(false);
}


utf8Char encodeUchar(char32_t c) {
  utf8Char out = {.nbytes = 0, .codeunits = {0, 0, 0, 0}};
  if (c < 0) {
    return out;
  }
  else if (c < 0x000080) {
    out.nbytes = 1;
    out.codeunits[0] = c;
  }
  else if (c < 0x000800) {
    out.nbytes = 2;
    out.codeunits[0] = 0xC0 | (c>>6 & 0x1F);
    out.codeunits[1] = 0x80 | (c    & 0x3F);
  }
  else if (c < 0x010000) {
    out.nbytes = 3;
    out.codeunits[0] = 0xE0 | (c>>12 & 0x0F);
    out.codeunits[1] = 0x80 | (c>>6  & 0x3F);
    out.codeunits[2] = 0x80 | (c     & 0x3F);
  }
  else if (c < 0x110000) {
    out.nbytes = 4;
    out.codeunits[0] = 0xF0 | (c>>18 & 0x07);
    out.codeunits[1] = 0x80 | (c>>12 & 0x3F);
    out.codeunits[2] = 0x80 | (c>>6  & 0x3F);
    out.codeunits[3] = 0x80 | (c     & 0x3F);
  }
  return out;
}

/* A driver for scheme_entry */

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>

/* Memory management includes */
#include <sys/mman.h>
#include <unistd.h>

#define wordsize 8

// fixnums
#define fixnum_mask  0x03 // 11
#define fixnum_tag   0x00 // 00
// empty list
#define fixnum_shift 2
// booleans
#define bool_t       0x9F // 10011111
#define bool_f       0x1F // 00011111
// characters
#define char_mask    0xFF // 11110000
#define char_tag     0x0F // 11110000
#define char_shift   8
// lists
#define obj_mask     0x7  // 111
#define pair_tag     0x01 // 001
#define car_offset   (0 - pair_tag) // -1
#define cdr_offset   (wordsize - pair_tag) // 7
#define vector_tag   0x05 // 101

typedef long long scm_val;

typedef struct {
  scm_val length;
  scm_val buf[1];
} vector;

#define empty_list   0x2F // 00101111
const char *ascii_table[0x7F] = {
  "nul",    "soh",    "stx",     "etx",    "eot",    "enq",    "ack",    "bel",
  "bs",     "tab",    "newline", "vt",     "ff",     "return", "so",     "si",
  "dle",    "dc1",    "dc2",     "dc3",    "dc4",    "nak",    "syn",    "etb",
  "can",    "em",     "sub",     "esc",    "fs",     "gs",     "rs",     "us",
  "space"
};

/* typedef uint8_t LEVEL; */

static void prn(scm_val, int);

static void prn_list(scm_val val, int level) {
  if (level == 0) {
    printf("(");
  }

  scm_val car_ = *(scm_val*) (val + car_offset);
  scm_val cdr_ = *(scm_val*) (val + cdr_offset);

  prn(car_, level);

  if ((cdr_ & obj_mask) == pair_tag) {
    printf(" ");
    prn_list(cdr_, level+1);
  } else if (cdr_ != empty_list) {
    printf(" . ");
    prn(cdr_, level);
  }

  if (level == 0) {
    printf(")");
  }
}

static void prn_vector(scm_val val, int level) {
  printf("#(");

  // TODO: clean this up
  vector* p = (vector*)(val - vector_tag);
  int len = p->length >> fixnum_shift;
  for (int i = 0; i < len; i++) {
    if (i > 0) {
      printf(" ");
    }
    prn(p->buf[i], level);
  }

  printf(")");
}

// TODO: clean up
static void prn(scm_val val, int level){
  if((val & fixnum_mask) == fixnum_tag){
    printf("%lld", val >> fixnum_shift);
  } else if ((val & obj_mask) == pair_tag){
    prn_list(val, level);
  } else if ((val & obj_mask) == vector_tag){
    prn_vector(val, level);
  } else if (val == bool_t){
    printf("#t");
  } else if (val == bool_f){
    printf("#f");
  } else if (val == empty_list){
    printf("()");
  } else if ((val & char_mask) == char_tag){
    char c = (val >> char_shift);
    if (iscntrl(c) || isspace(c)) {
      if ((int)c == 127) { // HACK
        printf("#\\del");
      } else {
        printf("#\\%s", ascii_table[(unsigned char)c]);
      }
    } else if (!(strcmp(&c, "\\\\"))) { // HACK
      printf("#\\\\");
    } else {
      printf("#\\%c", c);
    }
  } else {
    printf("FAIL!");
  }
}

static char* allocate_protected_space(int size){
  int page = getpagesize();
  int status;
  int aligned_size = ((size + page - 1) / page) * page;
  char* p = mmap(0, aligned_size + 2 * page,
                 PROT_READ | PROT_WRITE,
                 MAP_ANONYMOUS | MAP_PRIVATE,
                 0, 0);
  if (p == MAP_FAILED){ perror("mmap failed :'("); };
  status = mprotect(p, page, PROT_NONE);
  if(status != 0){ perror("failed to protect mmapped space"); };
  status = mprotect(p + page + aligned_size, page, PROT_NONE);
  if(status != 0){ perror("failed to protect mmapped space (2)"); };
  return (p + page);
}

static void deallocate_protected_space(char* p, int size){
  int page = getpagesize();
  int status;
  int aligned_size = ((size + page - 1) / page) * page;
  status = munmap(p - page, aligned_size + 2 * page);
  if(status != 0){ perror("failed to deallocate protected space :'("); };
}

typedef struct {
  void* rax; /* 0  scratch */
  void* rbx; /* 8  preserve */
  void* rcx; /* 16 scratch */
  void* rdx; /* 24 scratch */
  void* rsi; /* 32 preserve */
  void* rdi; /* 40 preserve */
  void* rbp; /* 48 preserve */
  void* rsp; /* 56 preserve */
} context;

extern scm_val scheme_entry();

int main(int argc, char** argv){
  int stack_size = (16 * 4096);  /* holds 16K cells */
  int heap_size = (64 * 4096);  /* holds 64K cells */

  char* stack_top = allocate_protected_space(stack_size);
  char* stack_base = stack_top + stack_size;
  char* heap_base = allocate_protected_space(heap_size);
  context ctx;

  /* printf("heap_base: %9llx\n", (scm_val)heap_base); */
  scm_val ret = scheme_entry(&ctx, stack_base, heap_base);
  /* printf("      ret: %9llx\n",  ret); */
  prn(ret, 0);
  printf("\n");

  deallocate_protected_space(stack_top, stack_size);
  deallocate_protected_space(heap_base, heap_size);
  return 0;
}

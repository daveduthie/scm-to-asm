/* A driver for scheme_entry */

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>

/* Memory management includes */
#include <sys/mman.h>
#include <unistd.h>

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

#define empty_list   0x2F // 00101111

extern int scheme_entry();


const char *ascii_table[0x7F] = {
  "nul",    "soh",    "stx",     "etx",    "eot",    "enq",    "ack",    "bel",
  "bs",     "tab",    "newline", "vt",     "ff",     "return", "so",     "si",
  "dle",    "dc1",    "dc2",     "dc3",    "dc4",    "nak",    "syn",    "etb",
  "can",    "em",     "sub",     "esc",    "fs",     "gs",     "rs",     "us",
  "space"
};

static void prn(int val){
  if((val & fixnum_mask) == fixnum_tag){
    printf("%d", val >> fixnum_shift);
  } else if (val == bool_t){
    printf("#t");
  } else if (val == bool_f){
    printf("#f");
  } else if (val == empty_list){
    printf("()");
  } else if ((val & char_mask) == char_tag){
    char c = (val >> char_shift);
    if (iscntrl(c) || isspace(c)) {
      if ((int)c == 127) {
        printf("#\\del");
      } else {
        printf("#\\%s", ascii_table[(unsigned char)c]);
      }
    } else if (!(strcmp(&c, "\\\\"))) {
      printf("#\\\\");
    } else {
      printf("#\\%c", c);
    }
  } else {
    printf("FAIL!");
  }
  printf("\n");

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

int main(int argc, char** argv){
  int stack_size = (16 * 4096);  /* holds 16K cells */
  char* stack_top = allocate_protected_space(stack_size);
  char* stack_base = stack_top + stack_size;
  prn(scheme_entry(stack_base));
  deallocate_protected_space(stack_top, stack_size);
  return 0;
}

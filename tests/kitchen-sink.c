#include <stdlib.h>

struct root {
  union {
    struct {
      int a;
    };
    char b;
  };
  size_t c;

  struct {
    union {
      int d;
      char e;
    } f;
    char g;
  } h;
  size_t i;
};

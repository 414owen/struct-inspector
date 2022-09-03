#define RESET "\033[0m"
#define BLACK "\033[30m"
#define RED "\033[31m"
#define GREEN "\033[32m"
#define YELLOW "\033[33m"
#define BLUE "\033[34m"
#define MAGENTA "\033[35m"
#define CYAN "\033[36m"
#define WHITE "\033[37m"
#define BOLDBLACK "\033[1m\033[30m"
#define BOLDRED "\033[1m\033[31m"
#define BOLDGREEN "\033[1m\033[32m"
#define BOLDYELLOW "\033[1m\033[33m"
#define BOLDBLUE "\033[1m\033[34m"
#define BOLDMAGENTA "\033[1m\033[35m"
#define BOLDCYAN "\033[1m\033[36m"
#define BOLDWHITE "\033[1m\033[37m"
#include <stdio.h>
// End of prelude

#include <stdlib.h>
struct a {
  struct {
    int e;
  } d;
  size_t b;
  int c;
  char f;
};

int main(void) {
  {
    struct a __s[2];
    {
      size_t __size_of = sizeof(__s[0]);
      size_t __uses    = ((size_t)&__s[1]) - ((size_t)&__s[0]);
      size_t __padding = __uses - __size_of;
        printf("// sizeof: %zu\n", __size_of);
        printf("// uses: %zu\n", __uses);
        printf("// padding: %s%zu" RESET "\n", __padding > 0 ? RED : "", __padding);
    }
    puts("struct a {");
    printf("%2s", "");
    puts("struct {");
    {
      size_t __size_of = sizeof(__s[0].d.e);
      size_t __uses    = ((size_t)&__s[0].b) - ((size_t)&__s[0].d.e);
      size_t __padding = __uses - __size_of;
      printf("%4s", "");
      printf("// sizeof: %zu\n", __size_of);
      printf("%4s", "");
      printf("// uses: %zu\n", __uses);
      printf("%4s", "");
      printf("// padding: %s%zu" RESET "\n", __padding > 0 ? RED : "", __padding);
    }
    printf("%4s", "");
    puts("int e;");
    printf("%2s", "");
    puts("} d;");
    {
      size_t __size_of = sizeof(__s[0].b);
      size_t __uses    = ((size_t)&__s[0].c) - ((size_t)&__s[0].b);
      size_t __padding = __uses - __size_of;
      printf("%2s", "");
      printf("// sizeof: %zu\n", __size_of);
      printf("%2s", "");
      printf("// uses: %zu\n", __uses);
      printf("%2s", "");
      printf("// padding: %s%zu" RESET "\n", __padding > 0 ? RED : "", __padding);
    }
    printf("%2s", "");
    puts("size_t b;");
    {
      size_t __size_of = sizeof(__s[0].c);
      size_t __uses    = ((size_t)&__s[0].f) - ((size_t)&__s[0].c);
      size_t __padding = __uses - __size_of;
      printf("%2s", "");
      printf("// sizeof: %zu\n", __size_of);
      printf("%2s", "");
      printf("// uses: %zu\n", __uses);
      printf("%2s", "");
      printf("// padding: %s%zu" RESET "\n", __padding > 0 ? RED : "", __padding);
    }
    printf("%2s", "");
    puts("int c;");
    {
      size_t __size_of = sizeof(__s[0].f);
      size_t __uses    = ((size_t)&__s[1]) - ((size_t)&__s[0].f);
      size_t __padding = __uses - __size_of;
      printf("%2s", "");
      printf("// sizeof: %zu\n", __size_of);
      printf("%2s", "");
      printf("// uses: %zu\n", __uses);
      printf("%2s", "");
      printf("// padding: %s%zu" RESET "\n", __padding > 0 ? RED : "", __padding);
    }
    printf("%2s", "");
    puts("char f;");
    puts("};\n");
  }
}

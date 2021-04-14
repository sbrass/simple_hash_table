#include <stddef.h>
#ifdef __cplusplus
extern "C" {
#endif
  size_t keyhash(char *k);

  // https://git.musl-libc.org/cgit/musl/tree/src/search/hsearch.c
  size_t keyhash(char *k)
  {
    unsigned char *p = (void *)k;
    size_t h = 0;

    while (*p)
      h = 31*h + *p++;
    return h;
  }
#ifdef __cplusplus
}
#endif

#include <stdlib.h>

void *BSL_RT_MALLOC(size_t sz) {
  static void *base, *top;
  if (sz < top - base) {
    return top -= sz;
  } else {
    base = malloc(1 << 23);
    top = base + (1 << 23);
    if (sz < top - base) {
      return top -= sz;
    } else {
      return malloc(sz);
    }
  }
}

typedef void* BSL_RT_VAR_T;
typedef BSL_RT_VAR_T (*BSL_RT_FUN_T)(BSL_RT_VAR_T, BSL_RT_VAR_T[]);
typedef struct {
  BSL_RT_FUN_T fun;
  BSL_RT_VAR_T env[];
} * BSL_RT_CLOSURE_T;

BSL_RT_VAR_T BSL_RT_CALL(BSL_RT_CLOSURE_T c, BSL_RT_VAR_T a) {
  return c->fun(a, c->env);
}

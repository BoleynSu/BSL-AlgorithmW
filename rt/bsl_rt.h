#include <stdlib.h>

void *BSL_RT_MALLOC(size_t sz) {
  static void *base = 1 << 23, *top = 1 << 23;
  if ((top -= sz) < base) {
    base = malloc(1 << 23);
    top = base + (1 << 23) - sz;
  }
  return top;
}

typedef void *BSL_RT_VAR_T;
typedef BSL_RT_VAR_T (*BSL_RT_FUN_T)(BSL_RT_VAR_T, BSL_RT_VAR_T[]);
typedef struct {
  BSL_RT_FUN_T fun;
  BSL_RT_VAR_T env[];
} * BSL_RT_CLOSURE_T;

BSL_RT_VAR_T BSL_RT_CALL(BSL_RT_CLOSURE_T c, BSL_RT_VAR_T a) {
  return c->fun(a, c->env);
}

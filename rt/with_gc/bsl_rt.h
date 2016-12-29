#ifndef BSL_RT_HEADER
#define BSL_RT_HEADER

#include <gc.h>

#define BSL_RT_MALLOC GC_MALLOC

typedef void* BSL_RT_VAR_T;
typedef BSL_RT_VAR_T (*BSL_RT_FUN_T)(BSL_RT_VAR_T, BSL_RT_VAR_T[]);
typedef struct {
  BSL_RT_FUN_T fun;
  BSL_RT_VAR_T env[];
} * BSL_RT_CLOSURE_T;

BSL_RT_VAR_T BSL_RT_CALL(BSL_RT_CLOSURE_T c, BSL_RT_VAR_T a) {
  return c->fun(a, c->env);
}

#endif

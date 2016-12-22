#ifndef SU_BOLEYN_BSL_OPTIMIZE_H
#define SU_BOLEYN_BSL_OPTIMIZE_H

#include <memory>

#include "ds/expr.h"

using namespace std;

struct Optimizer {
  shared_ptr<Expr> optimize(shared_ptr<Expr> e) { return e; }
};

#endif

#ifndef SU_BOLEYN_BSL_OPTIMIZE_H
#define SU_BOLEYN_BSL_OPTIMIZE_H

#include <cstdlib>
#include <iostream>
#include <memory>
#include <set>
#include <sstream>
#include <string>
#include <utility>
#include <vector>

#include "ds/expr.h"

using namespace std;

struct Optimizer {
  shared_ptr<Expr> optimize(shared_ptr<Expr> e) { return e; }
};

#endif

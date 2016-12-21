#ifndef SU_BOLEYN_BSL_DS_UNIT_H
#define SU_BOLEYN_BSL_DS_UNIT_H

#include <cassert>
#include <map>
#include <memory>
#include <string>

#include "data.h"

struct Expr;

struct Unit {
  map<string, shared_ptr<Data>> data;
  map<string, shared_ptr<Constructor>> cons;
  shared_ptr<Expr> expr;
};

#endif

#ifndef SU_BOLEYN_BSL_DATA_H
#define SU_BOLEYN_BSL_DATA_H

#include <memory>
#include <string>
#include <utility>
#include <vector>

#include "expr.h"

using namespace std;

struct Data {
  bool is_ffi;
  string name;
  int arg;
  vector<pair<string, shared_ptr<Poly> > > constructors;
  string ffi;
};

#endif

#ifndef SU_BOLEYN_BSL_DATA_H
#define SU_BOLEYN_BSL_DATA_H

#include <memory>
#include <string>
#include <utility>
#include <vector>

#include "expr.h"

using namespace std;

struct Constructor {
  string name;
  size_t arg;
  shared_ptr<Poly> type;
  string data_name;
};

struct Data {
  bool is_ffi;
  string name;
  size_t arg;
  vector<shared_ptr<Constructor>> constructors;
  string ffi;
};

#endif

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
  bool has_data(const string &d) { return data.count(d); }
  shared_ptr<Data> get_data(const string &d) {
    assert(has_data(d));
    return data.find(d)->second;
  }
  void set_data(const string &d, shared_ptr<Data> da) { data[d] = da; }
  bool has_constructor(const string &c) { return cons.count(c); }
  shared_ptr<Constructor> get_constructor(const string &c) {
    assert(has_constructor(c));
    return cons.find(c)->second;
  }
  void set_constructor(const string &c, shared_ptr<Constructor> co) {
    cons[c] = co;
  }
};

#endif

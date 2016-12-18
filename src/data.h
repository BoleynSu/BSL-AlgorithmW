#ifndef SU_BOLEYN_BSL_DATA_H
#define SU_BOLEYN_BSL_DATA_H

#include <memory>
#include <string>
#include <vector>

struct Mono_;
struct Poly_;

using namespace std;

struct Constructor {
  string name;
  size_t arg;
  shared_ptr<Poly_> sig, type;
  string data_name;
};

struct Data {
  string name;
  size_t arg;
  vector<shared_ptr<Mono_>> exists;
  vector<shared_ptr<Constructor>> constructors;
  size_t maxarg;
  size_t to_ptr;
};

#endif

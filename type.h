#ifndef SU_BOLEYN_BSL_TYPE_H
#define SU_BOLEYN_BSL_TYPE_H

#include <memory>
#include <string>
#include <vector>

struct Mono {
  bool is_const;
  shared_ptr<Mono> alpha;
  string D;
  vector<shared_ptr<Mono>> tau;
};

struct Poly {
  bool is_poly;
  shared_ptr<Mono> tau;
  shared_ptr<Mono> alpha;
  shared_ptr<Poly> sigma;
};

#endif

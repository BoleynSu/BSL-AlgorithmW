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

shared_ptr<Mono> find(shared_ptr<Mono> x) {
  if (x->alpha != nullptr) {
    x->alpha = find(x->alpha);
    return x->alpha;
  } else {
    return x;
  }
}

string to_string(shared_ptr<Mono> tau) {
  tau = find(tau);
  if (tau->is_const) {
    if (tau->D == "->") {
      return "(" + to_string(tau->tau[0]) + ")->(" + to_string(tau->tau[1]) +
             ")";
    } else {
      string ret = tau->D;
      for (auto t : tau->tau) {
        ret += " (" + to_string(t) + ")";
      }
      return ret;
    }
  } else {
    stringstream s;
    s << "t" << find(tau);
    return s.str();
  }
}

string to_string(shared_ptr<Poly> sigma) {
  if (sigma->is_poly) {
    stringstream s;
    s << "t" << find(sigma->alpha);
    return "forall " + s.str() + " . " + to_string(sigma->sigma);
  } else {
    return to_string(sigma->tau);
  }
}

#endif

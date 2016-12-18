#ifndef SU_BOLEYN_BSL_TYPE_H
#define SU_BOLEYN_BSL_TYPE_H

#include <cassert>
#include <map>
#include <memory>
#include <set>
#include <sstream>
#include <string>
#include <utility>
#include <vector>

using namespace std;

struct Mono {
  shared_ptr<Mono> par;
  bool is_const;
  string D;
  vector<shared_ptr<Mono>> tau;
  bool is_forall;
};

struct Poly {
  bool is_mono;
  shared_ptr<Mono> tau;
  shared_ptr<Mono> alpha;
  shared_ptr<Poly> sigma;
};

struct Rank2Poly {
  bool is_forall;
  shared_ptr<Mono> alpha;
  shared_ptr<Rank2Poly> sigma;

  shared_ptr<Poly> poly;
  shared_ptr<Mono> mono;
};

bool is_c(shared_ptr<Mono> x) { return x->is_const; }
bool is_f(shared_ptr<Mono> x) { return (!x->is_const) && x->is_forall; }
bool is_e(shared_ptr<Mono> x) { return (!x->is_const) && (!x->is_forall); }

shared_ptr<Mono> find(shared_ptr<Mono> x) {
  if (x->par != nullptr) {
    x->par = find(x->par);
    return x->par;
  } else {
    return x;
  }
}

string to_string(shared_ptr<Poly>);

string to_string(shared_ptr<Mono> tau) {
  tau = find(tau);
  if (is_c(tau)) {
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
    s << (is_f(tau) ? "f" : "e") << tau;
    return s.str();
  }
}

string to_string(shared_ptr<Poly> sigma) {
  if (sigma->is_mono) {
    return to_string(sigma->tau);
  } else {
    stringstream s;
    assert(is_f(sigma->alpha));
    s << sigma->alpha;
    return "forall " + s.str() + ". " + to_string(sigma->sigma);
  }
}

string to_string(shared_ptr<Rank2Poly> sigma) {
  if (sigma->is_forall) {
    stringstream s;
    assert(is_f(sigma->alpha));
    s << sigma->alpha;
    return "forall " + s.str() + ". " + to_string(sigma->sigma);
  } else {
    stringstream s;
    return "(" + to_string(sigma->poly) + ")->(" + to_string(sigma->mono) + ")";
  }
}

shared_ptr<Mono> new_const_var(const string &D) {
  auto t = make_shared<Mono>();
  t->is_const = true;
  t->D = D;
  return t;
}

shared_ptr<Mono> new_forall_var() {
  auto t = make_shared<Mono>();
  t->is_const = false;
  t->is_forall = true;
  return t;
}

shared_ptr<Mono> new_exists_var() {
  auto t = make_shared<Mono>();
  t->is_const = false;
  t->is_forall = false;
  return t;
}

shared_ptr<Poly> new_poly(shared_ptr<Mono> m) {
  auto p = make_shared<Poly>();
  p->is_mono = true;
  p->tau = m;
  return p;
}

shared_ptr<Poly> new_poly(shared_ptr<Mono> alpha, shared_ptr<Poly> sigma) {
  auto p = make_shared<Poly>();
  p->is_mono = false;
  p->alpha = alpha;
  p->sigma = sigma;
  return p;
}

shared_ptr<Rank2Poly> new_rank2poly(shared_ptr<Poly> p, shared_ptr<Mono> m) {
  auto t = make_shared<Rank2Poly>();
  t->is_forall = false;
  t->poly = p;
  t->mono = m;
  return t;
}

shared_ptr<Rank2Poly> new_rank2poly(shared_ptr<Mono> alpha,
                                    shared_ptr<Rank2Poly> sigma) {
  auto t = make_shared<Rank2Poly>();
  t->is_forall = true;
  t->alpha = alpha;
  t->sigma = sigma;
  return t;
}

shared_ptr<Mono> get_mono(shared_ptr<Poly> t) {
  while (!t->is_mono) {
    t = t->sigma;
  }
  return t->tau;
}

shared_ptr<Mono> get_mono(shared_ptr<Rank2Poly> t) {
  while (t->is_forall) {
    t = t->sigma;
  }
  return t->mono;
}

shared_ptr<Mono> inst(shared_ptr<Mono> tau,
                      map<shared_ptr<Mono>, shared_ptr<Mono>> &m) {
  tau = find(tau);
  if (is_c(tau)) {
    auto t = make_shared<Mono>(*tau);
    for (size_t i = 0; i < tau->tau.size(); i++) {
      t->tau[i] = inst(tau->tau[i], m);
    }
    return t;
  } else {
    if (m.count(tau)) {
      return m[tau];
    } else {
      return tau;
    }
  }
}

shared_ptr<Mono> inst(shared_ptr<Poly> sigma,
                      map<shared_ptr<Mono>, shared_ptr<Mono>> &m) {
  if (sigma->is_mono) {
    return inst(sigma->tau, m);
  } else {
    if (!m.count(find(sigma->alpha))) {
      m[find(sigma->alpha)] = new_forall_var();
    }
    return inst(sigma->sigma, m);
  }
}

shared_ptr<Mono> inst(shared_ptr<Poly> sigma) {
  map<shared_ptr<Mono>, shared_ptr<Mono>> m;
  return inst(sigma, m);
}

shared_ptr<Mono> inst(shared_ptr<Poly> sigma,
                      vector<shared_ptr<Mono>> &exists) {
  map<shared_ptr<Mono>, shared_ptr<Mono>> m;
  for (auto e : exists) {
    m[e] = new_exists_var();
  }
  return inst(sigma, m);
}

pair<shared_ptr<Poly>, shared_ptr<Mono>> rank2inst(
    shared_ptr<Rank2Poly> sigma, map<shared_ptr<Mono>, shared_ptr<Mono>> &m) {
  if (sigma->is_forall) {
    if (!m.count(find(sigma->alpha))) {
      m[find(sigma->alpha)] = new_forall_var();
    }
    return rank2inst(sigma->sigma, m);
  } else {
    auto t = make_shared<Poly>(*sigma->poly);
    while (!t->is_mono) {
      t->sigma = make_shared<Poly>(*t->sigma);
      t = t->sigma;
    }
    t->tau = inst(t->tau, m);
    return make_pair(t, inst(sigma->mono, m));
  }
}

pair<shared_ptr<Poly>, shared_ptr<Mono>> rank2inst(
    shared_ptr<Rank2Poly> sigma) {
  map<shared_ptr<Mono>, shared_ptr<Mono>> m;
  return rank2inst(sigma, m);
}

pair<shared_ptr<Poly>, shared_ptr<Mono>> rank2inst(
    shared_ptr<Rank2Poly> sigma, vector<shared_ptr<Mono>> &exists) {
  map<shared_ptr<Mono>, shared_ptr<Mono>> m;
  for (auto e : exists) {
    m[e] = new_exists_var();
  }
  return rank2inst(sigma, m);
}

void ftv(set<shared_ptr<Mono>> &, shared_ptr<Poly>);

void ftv(set<shared_ptr<Mono>> &f, shared_ptr<Mono> tau) {
  tau = find(tau);
  if (is_c(tau)) {
    for (size_t i = 0; i < tau->tau.size(); i++) {
      ftv(f, tau->tau[i]);
    }
  } else if (is_f(tau)) {
    f.insert(tau);
  }
}

void ftv(set<shared_ptr<Mono>> &f, shared_ptr<Poly> sigma) {
  if (sigma->is_mono) {
    ftv(f, sigma->tau);
  } else {
    ftv(f, sigma->sigma);
    f.erase(sigma->alpha);
  }
}

#endif

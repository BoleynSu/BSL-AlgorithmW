#ifndef SU_BOLEYN_BSL_DS_TYPE_H
#define SU_BOLEYN_BSL_DS_TYPE_H

#include <cassert>
#include <map>
#include <memory>
#include <set>
#include <sstream>
#include <string>
#include <utility>
#include <vector>

using namespace std;

struct Poly;

struct Mono {
  shared_ptr<Mono> par;
  bool is_const;
  bool is_poly;
  shared_ptr<Poly> sigma;
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

bool is_c(shared_ptr<Mono> x) { return x->is_const; }
bool is_p(shared_ptr<Mono> x) { return x->is_const && x->is_poly; }
bool is_f(shared_ptr<Mono> x) { return (!x->is_const) && x->is_forall; }
bool is_e(shared_ptr<Mono> x) { return (!x->is_const) && (!x->is_forall); }
bool is_fun(shared_ptr<Mono> x) {
  return x->is_const && !x->is_poly && x->D == "->";
}

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
    if (is_p(tau)) {
      return "(" + to_string(tau->sigma) + ")";
    } else {
      if (is_fun(tau)) {
        return "(" + to_string(tau->tau[0]) + ")->(" + to_string(tau->tau[1]) +
               ")";
      } else {
        string ret = tau->D;
        for (auto t : tau->tau) {
          ret += " (" + to_string(t) + ")";
        }
        return ret;
      }
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

shared_ptr<Mono> new_const(const string &D) {
  auto t = make_shared<Mono>();
  t->is_const = true;
  t->is_poly = false;
  t->D = D;
  return t;
}

shared_ptr<Mono> new_const(shared_ptr<Poly> sigma) {
  auto t = make_shared<Mono>();
  t->is_const = true;
  t->is_poly = true;
  t->sigma = sigma;
  return t;
}

shared_ptr<Mono> new_fun() {
  auto t = make_shared<Mono>();
  t->is_const = true;
  t->is_poly = false;
  t->D = "->";
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

shared_ptr<Mono> get_mono(shared_ptr<Poly> t) {
  while (!t->is_mono) {
    t = t->sigma;
  }
  return t->tau;
}

shared_ptr<Mono> inst_with_exists(shared_ptr<Mono> tau,
                                  map<shared_ptr<Mono>, shared_ptr<Mono>> &m) {
  tau = find(tau);
  if (is_c(tau)) {
    auto t = make_shared<Mono>(*tau);
    for (size_t i = 0; i < tau->tau.size(); i++) {
      t->tau[i] = inst_with_exists(tau->tau[i], m);
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

shared_ptr<Mono> inst_with_exists(shared_ptr<Poly> sigma,
                                  map<shared_ptr<Mono>, shared_ptr<Mono>> &m) {
  if (sigma->is_mono) {
    return inst_with_exists(sigma->tau, m);
  } else {
    if (!m.count(find(sigma->alpha))) {
      m[find(sigma->alpha)] = new_forall_var();
    }
    return inst_with_exists(sigma->sigma, m);
  }
}

shared_ptr<Mono> inst(shared_ptr<Poly> sigma) {
  map<shared_ptr<Mono>, shared_ptr<Mono>> m;
  return inst_with_exists(sigma, m);
}

shared_ptr<Mono> inst_get_set(shared_ptr<Poly> sigma,
                              map<shared_ptr<Mono>, shared_ptr<Mono>> &m,
                              set<shared_ptr<Mono>> &st) {
  if (sigma->is_mono) {
    return inst_with_exists(sigma->tau, m);
  } else {
    if (!m.count(find(sigma->alpha))) {
      m[find(sigma->alpha)] = new_forall_var();
      st.insert(m[find(sigma->alpha)]);
    }
    return inst_with_exists(sigma->sigma, m);
  }
}

shared_ptr<Mono> inst_get_set(shared_ptr<Poly> sigma,
                              set<shared_ptr<Mono>> &st) {
  map<shared_ptr<Mono>, shared_ptr<Mono>> m;
  return inst_with_exists(sigma, m);
}

shared_ptr<Mono> inst_with_exists(shared_ptr<Poly> sigma,
                                  set<shared_ptr<Mono>> &exists) {
  map<shared_ptr<Mono>, shared_ptr<Mono>> m;
  for (auto e : exists) {
    m[e] = new_exists_var();
  }
  return inst_with_exists(sigma, m);
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

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

struct Kind {
  shared_ptr<Kind> par;
  bool is_const;
  bool is_arrow;
  string k;
  shared_ptr<Kind> left, right;
};

struct Poly;

struct Mono {
  shared_ptr<Mono> par;
  shared_ptr<Kind> kind;
  bool is_const;
  bool is_poly;
  shared_ptr<Poly> sigma;
  struct {
    bool is_const;
    string D;
    shared_ptr<Mono> d;
  } D;
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
bool is_cd(shared_ptr<Mono> x) {
  return x->is_const && !x->is_poly && x->D.is_const;
}
bool is_p(shared_ptr<Mono> x) { return x->is_const && x->is_poly; }
bool is_f(shared_ptr<Mono> x) { return (!x->is_const) && x->is_forall; }
bool is_e(shared_ptr<Mono> x) { return (!x->is_const) && (!x->is_forall); }
bool is_fun(shared_ptr<Mono> x) {
  return x->is_const && !x->is_poly && is_cd(x) && x->D.D == "->";
}

shared_ptr<Mono> find(shared_ptr<Mono> x) {
  if (x->par != nullptr) {
    x->par = find(x->par);
    return x->par;
  } else {
    return x;
  }
}

shared_ptr<Kind> find(shared_ptr<Kind> x) {
  if (x->par != nullptr) {
    x->par = find(x->par);
    return x->par;
  } else {
    return x;
  }
}

string to_string(shared_ptr<Kind> kind) {
  kind = find(kind);
  if (kind->is_const) {
    if (kind->is_arrow) {
      return "(" + to_string(kind->left) + "->" + to_string(kind->right) + ")";
    } else {
      return kind->k;
    }
  } else {
    stringstream s;
    s << "k" << kind;
    return s.str();
  }
}

string to_string(shared_ptr<Poly>);

string to_string(shared_ptr<Mono> tau, bool fp = false, bool dp = false) {
  tau = find(tau);
  if (is_c(tau)) {
    if (is_p(tau)) {
      return "(" + to_string(tau->sigma) + ")";
    } else {
      if (is_fun(tau)) {
        return (fp ? "(" : "") + to_string(tau->tau[0], true) + "->" +
               to_string(tau->tau[1]) + (fp ? ")" : "");
      } else {
        string ret;
        if (is_cd(tau)) {
          ret = tau->D.D;
        } else {
          ret = to_string(tau->D.d);
        }
        for (auto t : tau->tau) {
          ret += " " + to_string(t, true, true);
        }
        return (dp && tau->tau.size() ? "(" : "") + ret +
               (dp && tau->tau.size() ? ")" : "");
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

shared_ptr<Kind> new_kind() {
  auto k = make_shared<Kind>();
  k->is_const = false;
  return k;
}

shared_ptr<Kind> new_const_kind() {
  auto k = make_shared<Kind>();
  k->is_const = true;
  k->k = "*";
  return k;
}

shared_ptr<Kind> new_kind(shared_ptr<Kind> l, shared_ptr<Kind> r) {
  auto k = make_shared<Kind>();
  k->is_const = true;
  k->is_arrow = true;
  k->left = l;
  k->right = r;
  return k;
}

shared_ptr<Mono> new_const(const string &D, shared_ptr<Kind> kind) {
  auto t = make_shared<Mono>();
  t->kind = kind;
  t->is_const = true;
  t->is_poly = false;
  t->D.is_const = true;
  t->D.D = D;
  return t;
}

shared_ptr<Mono> new_const(shared_ptr<Mono> d, shared_ptr<Kind> kind) {
  auto t = make_shared<Mono>();
  t->kind = kind;
  t->is_const = true;
  t->is_poly = false;
  t->D.is_const = false;
  t->D.d = d;
  return t;
}

shared_ptr<Mono> new_const(shared_ptr<Poly> sigma, shared_ptr<Kind> kind) {
  auto t = make_shared<Mono>();
  t->kind = kind;
  t->is_const = true;
  t->is_poly = true;
  t->sigma = sigma;
  return t;
}

shared_ptr<Mono> new_fun() { return new_const("->", new_const_kind()); }

shared_ptr<Mono> new_forall_var(shared_ptr<Kind> kind) {
  auto t = make_shared<Mono>();
  t->kind = kind;
  t->is_const = false;
  t->is_forall = true;
  return t;
}

shared_ptr<Mono> new_exists_var(shared_ptr<Kind> kind) {
  auto t = make_shared<Mono>();
  t->kind = kind;
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

shared_ptr<Mono> inst_get_set(shared_ptr<Poly> sigma,
                              map<shared_ptr<Mono>, shared_ptr<Mono>> &m,
                              set<shared_ptr<Mono>> &st);

shared_ptr<Mono> inst(shared_ptr<Mono> tau,
                      map<shared_ptr<Mono>, shared_ptr<Mono>> &m) {
  tau = find(tau);
  if (is_c(tau)) {
    if (is_p(tau)) {
      set<shared_ptr<Mono>> st;
      auto mo = inst_get_set(tau->sigma, m, st);
      auto po = new_poly(mo);
      for (auto f : st) {
        po = new_poly(f, po);
      }
      return new_const(po, mo->kind);
    } else {
      shared_ptr<Mono> t;
      if (is_cd(tau)) {
        t = new_const(tau->D.D, tau->kind);
      } else {
        auto d = find(tau->D.d);
        if (m.count(d)) {
          t = new_const(m[d], tau->kind);
        } else {
          t = new_const(d, tau->kind);
        }
      }
      for (size_t i = 0; i < tau->tau.size(); i++) {
        t->tau.push_back(inst(tau->tau[i], m));
      }
      return t;
    }
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
    if (!m.count(sigma->alpha)) {
      m[sigma->alpha] = new_forall_var(sigma->alpha->kind);
    } else {
      assert(is_e(m[sigma->alpha]));
    }
    return inst(sigma->sigma, m);
  }
}

shared_ptr<Mono> inst(shared_ptr<Poly> sigma) {
  map<shared_ptr<Mono>, shared_ptr<Mono>> m;
  return inst(sigma, m);
}

shared_ptr<Mono> inst_get_set(shared_ptr<Poly> sigma,
                              map<shared_ptr<Mono>, shared_ptr<Mono>> &m,
                              set<shared_ptr<Mono>> &st) {
  if (sigma->is_mono) {
    return inst(sigma->tau, m);
  } else {
    if (!m.count(sigma->alpha)) {
      m[sigma->alpha] = new_forall_var(sigma->alpha->kind);
    } else {
      assert(is_e(m[sigma->alpha]));
    }
    st.insert(m[sigma->alpha]);
    auto r = inst_get_set(sigma->sigma, m, st);
    m.erase(sigma->alpha);
    return r;
  }
}

shared_ptr<Mono> inst_get_set(shared_ptr<Poly> sigma,
                              set<shared_ptr<Mono>> &st) {
  map<shared_ptr<Mono>, shared_ptr<Mono>> m;
  return inst_get_set(sigma, m, st);
}

shared_ptr<Mono> inst_with_exists(shared_ptr<Poly> sigma,
                                  set<shared_ptr<Mono>> &exists,
                                  set<shared_ptr<Mono>> &exists_var) {
  map<shared_ptr<Mono>, shared_ptr<Mono>> m;
  for (auto e : exists) {
    m[e] = new_exists_var(e->kind);
    exists_var.insert(m[e]);
  }
  return inst(sigma, m);
}

void ftv(set<shared_ptr<Mono>> &, shared_ptr<Poly>);

void ftv(set<shared_ptr<Mono>> &f, shared_ptr<Mono> tau) {
  tau = find(tau);
  if (is_c(tau)) {
    if (is_p(tau)) {
      ftv(f, tau->sigma);
    } else {
      if (!is_cd(tau)) {
        ftv(f, tau->D.d);
      }
      for (size_t i = 0; i < tau->tau.size(); i++) {
        ftv(f, tau->tau[i]);
      }
    }
  } else {
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

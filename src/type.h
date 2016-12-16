#ifndef SU_BOLEYN_BSL_TYPE_H
#define SU_BOLEYN_BSL_TYPE_H

#include <iostream>
#include <memory>
#include <string>
#include <vector>

struct Poly_;

struct Mono_ {
  shared_ptr<Mono_> par;
  bool is_const;
  string D;
  vector<shared_ptr<Poly_>> tau;
  bool is_forall;
};

struct Poly_ {
  bool is_mono;
  shared_ptr<Mono_> tau;
  shared_ptr<Mono_> alpha;
  shared_ptr<Poly_> sigma;
};

bool is_c(shared_ptr<Mono_> x) { return x->is_const; }
bool is_f(shared_ptr<Mono_> x) { return (!x->is_const) && x->is_forall; }
bool is_e(shared_ptr<Mono_> x) { return (!x->is_const) && (!x->is_forall); }

shared_ptr<Mono_> find(shared_ptr<Mono_> x) {
  if (x->par != nullptr) {
    x->par = find(x->par);
    return x->par;
  } else {
    return x;
  }
}

string to_string(shared_ptr<Poly_>);

string to_string(shared_ptr<Mono_> tau) {
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
    s << (tau->is_forall ? "f" : "e") << tau;
    return s.str();
  }
}

string to_string(shared_ptr<Poly_> sigma) {
  if (sigma->is_mono) {
    return to_string(sigma->tau);
  } else {
    stringstream s;
    s << (sigma->alpha->is_forall ? "f" : "e") << sigma->alpha;
    return (sigma->alpha->is_forall ? "forall " : "exist ") + s.str() + ". " +
           to_string(sigma->sigma);
  }
}

shared_ptr<Mono_> new_const_var(const string &D) {
  auto t = make_shared<Mono_>();
  t->is_const = true;
  t->D = D;
  return t;
}

shared_ptr<Mono_> new_forall_var() {
  auto t = make_shared<Mono_>();
  t->is_const = false;
  t->is_forall = true;
  return t;
}

shared_ptr<Mono_> new_exist_var() {
  auto t = make_shared<Mono_>();
  t->is_const = false;
  t->is_forall = false;
  return t;
}

shared_ptr<Poly_> to_poly(shared_ptr<Mono_> m) {
  auto p = make_shared<Poly_>();
  p->is_mono = true;
  p->tau = m;
  return p;
}

shared_ptr<Poly_> to_poly(shared_ptr<Mono_> alpha, shared_ptr<Poly_> sigma) {
  auto p = make_shared<Poly_>();
  p->is_mono = false;
  p->alpha = alpha;
  p->sigma = sigma;
  return p;
}

shared_ptr<Mono_> get_mono(shared_ptr<Poly_> t) {
  while (!t->is_mono) {
    t = t->sigma;
  }
  return t->tau;
}

shared_ptr<Mono_> inst(shared_ptr<Mono_>,
                       const map<shared_ptr<Mono_>, shared_ptr<Mono_>> &);

shared_ptr<Poly_> inst(shared_ptr<Poly_> sigma,
                       const map<shared_ptr<Mono_>, shared_ptr<Mono_>> &m) {
  auto s = make_shared<Poly_>(*sigma);
  if (sigma->is_mono) {
    s->tau = inst(sigma->tau, m);
    return s;
  } else {
    s->sigma = inst(sigma->sigma, m);
    return s;
  }
}

shared_ptr<Mono_> inst(shared_ptr<Mono_> tau,
                       const map<shared_ptr<Mono_>, shared_ptr<Mono_>> &m) {
  tau = find(tau);
  if (tau->is_const) {
    auto t = make_shared<Mono_>(*tau);
    for (size_t i = 0; i < tau->tau.size(); i++) {
      t->tau[i] = inst(tau->tau[i], m);
    }
    return t;
  } else {
    if (m.count(tau)) {
      return m.find(tau)->second;
    } else {
      return tau;
    }
  }
}

shared_ptr<Mono_> inst(shared_ptr<Poly_> sigma) {
  map<shared_ptr<Mono_>, shared_ptr<Mono_>> m;
  while (!sigma->is_mono) {
    if (!m.count(sigma->alpha)) {
      m[sigma->alpha] =
          sigma->alpha->is_forall ? new_forall_var() : new_exist_var();
    }
    sigma = sigma->sigma;
  }
  return inst(sigma->tau, m);
}

void ftv(set<shared_ptr<Mono_>> &, shared_ptr<Poly_>);

void ftv(set<shared_ptr<Mono_>> &f, shared_ptr<Mono_> tau) {
  tau = find(tau);
  if (tau->is_const) {
    for (size_t i = 0; i < tau->tau.size(); i++) {
      ftv(f, tau->tau[i]);
    }
  } else {
    f.insert(tau);
  }
}

void ftv(set<shared_ptr<Mono_>> &f, shared_ptr<Poly_> sigma) {
  if (sigma->is_mono) {
    ftv(f, sigma->tau);
  } else {
    ftv(f, sigma->sigma);
    f.erase(sigma->alpha);
  }
}

shared_ptr<Poly_> gen(shared_ptr<map<string, shared_ptr<Poly_>>> context,
                      shared_ptr<Mono_> tau) {
  tau = find(tau);
  set<shared_ptr<Mono_>> f;
  for (auto &c : *context) {
    set<shared_ptr<Mono_>> fi;
    ftv(fi, c.second);
    f.insert(fi.begin(), fi.end());
  }
  set<shared_ptr<Mono_>> fp;
  ftv(fp, tau);
  for (auto i : f) {
    fp.erase(i);
  }
  map<shared_ptr<Mono_>, shared_ptr<Mono_>> m;
  for (auto f : fp) {
    if (f->is_forall) {  // FIXME is this correct?
      m[f] = new_forall_var();
    }
  }
  auto g = to_poly(inst(tau, m));
  for (auto f : m) {
    g = to_poly(f.second, g);
  }
  return g;
}

#endif

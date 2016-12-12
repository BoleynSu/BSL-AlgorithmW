#ifndef SU_BOLEYN_BSL_TYPE_INFERENCE_H
#define SU_BOLEYN_BSL_TYPE_INFERENCE_H

#include <initializer_list>
#include <iostream>
#include <map>
#include <memory>
#include <set>
#include <sstream>
#include <string>
#include <utility>
#include <vector>

#include "expr.h"
#include "type.h"

using namespace std;

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

shared_ptr<Mono> newvar() { return make_shared<Mono>(Mono{false}); }

shared_ptr<Mono> inst(shared_ptr<Mono> tau,
                      map<shared_ptr<Mono>, shared_ptr<Mono>>& m) {
  tau = find(tau);
  if (tau->is_const) {
    auto t = make_shared<Mono>();
    t->is_const = tau->is_const;
    t->D = tau->D;
    for (size_t i = 0; i < tau->tau.size(); i++) {
      t->tau.push_back(inst(tau->tau[i], m));
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
                      map<shared_ptr<Mono>, shared_ptr<Mono>>& m) {
  if (sigma->is_poly) {
    if (!m.count(find(sigma->alpha))) {
      m[find(sigma->alpha)] = newvar();
    }
    return inst(sigma->sigma, m);
  } else {
    return inst(sigma->tau, m);
  }
}

shared_ptr<Mono> inst(shared_ptr<Poly> sigma) {
  map<shared_ptr<Mono>, shared_ptr<Mono>> m;
  return inst(sigma, m);
}

void ftv(set<shared_ptr<Mono>>& f, shared_ptr<Mono> tau) {
  tau = find(tau);
  if (tau->is_const) {
    for (size_t i = 0; i < tau->tau.size(); i++) {
      ftv(f, tau->tau[i]);
    }
  } else {
    f.insert(tau);
  }
}

void ftv(set<shared_ptr<Mono>>& f, shared_ptr<Poly> sigma) {
  if (sigma->is_poly) {
    ftv(f, sigma->sigma);
    f.erase(find(sigma->alpha));
  } else {
    ftv(f, sigma->tau);
  }
}

shared_ptr<Poly> gen(shared_ptr<map<string, shared_ptr<Poly>>> context,
                     shared_ptr<Mono> tau) {
  tau = find(tau);
  set<shared_ptr<Mono>> f;
  for (auto c : *context) {
    set<shared_ptr<Mono>> fi;
    ftv(fi, c.second);
    f.insert(fi.begin(), fi.end());
  }
  set<shared_ptr<Mono>> fp;
  ftv(fp, tau);
  for (auto i : f) {
    fp.erase(i);
  }
  map<shared_ptr<Mono>, shared_ptr<Mono>> m;
  for (auto f : fp) {
    m[f] = newvar();
  }
  auto g = make_shared<Poly>(Poly{false, inst(tau, m)});
  for (auto f : m) {
    g = make_shared<Poly>(Poly{true, nullptr, f.second, g});
  }
  return g;
}

bool occ(shared_ptr<Mono> a, shared_ptr<Mono> b) {
  if (b->is_const) {
    for (size_t i = 0; i < b->tau.size(); i++) {
      if (occ(a, find(b->tau[i]))) {
        return true;
      }
    }
    return false;
  } else {
    return a == b;
  }
}

void unify(shared_ptr<Mono> a, shared_ptr<Mono> b) {
  a = find(a);
  b = find(b);
  if (a != b) {
    if (a->is_const && b->is_const && a->D == b->D &&
        a->tau.size() == b->tau.size()) {
      for (size_t i = 0; i < a->tau.size(); i++) {
        unify(a->tau[i], b->tau[i]);
      }
    } else if (!a->is_const) {
      if (occ(a, b)) {
        cerr << "//"
             << "ERROR!" << endl;  // TODO
        cerr << "//" << to_string(a) << " ~ " << to_string(b) << endl;
      } else {
        a->alpha = b;
      }
    } else if (!b->is_const) {
      if (occ(b, a)) {
        cerr << "//"
             << "ERROR!" << endl;  // TODO
        cerr << "//" << to_string(b) << " ~ " << to_string(a) << endl;
      } else {
        b->alpha = a;
      }
    } else {
      cerr << "//"
           << "ERROR!" << endl;  // TODO
      cerr << "//" << to_string(a) << " != " << to_string(b) << endl;
    }
  }
}

void unify_sig(shared_ptr<Mono> a, shared_ptr<Mono> b,
               set<shared_ptr<Mono>>& st) {
  a = find(a);
  b = find(b);
  st.insert(b);
  if (a != b) {
    if (a->is_const && b->is_const && a->D == b->D &&
        a->tau.size() == b->tau.size()) {
      for (size_t i = 0; i < a->tau.size(); i++) {
        unify_sig(a->tau[i], b->tau[i], st);
      }
    } else if (!a->is_const) {
      if (occ(a, b)) {
        cerr << "//"
             << "ERROR!" << endl;  // TODO
        cerr << "//" << to_string(a) << " ~ " << to_string(b) << endl;
      } else {
        if (st.count(a) && a != b) {
          cerr << "//"
               << "ERROR!" << endl;  // TODO
          cerr << "//" << to_string(a) << " != " << to_string(b) << endl;
        } else {
          a->alpha = b;
        }
      }
    } else if (!b->is_const) {
      if (occ(b, a)) {
        cerr << "//"
             << "ERROR!" << endl;  // TODO
        cerr << "//" << to_string(b) << " ~ " << to_string(a) << endl;
      } else {
        cerr << "//"
             << "ERROR!" << endl;  // TODO
        cerr << "//" << to_string(b) << " !< " << to_string(a) << endl;
      }
    } else {
      cerr << "//"
           << "ERROR!" << endl;  // TODO
      cerr << "//" << to_string(a) << " /= " << to_string(b) << endl;
    }
  }
}

void infer(shared_ptr<Expr> expr,
           shared_ptr<map<string, shared_ptr<Poly>>> context,
           map<string, size_t>& cl) {
  switch (expr->T) {
    case ExprType::VAR:
      if (context->count(expr->x)) {
        expr->type = inst((*context)[expr->x]);
      } else {
        cerr << "//"
             << "ERROR!" << endl;  // TODO
        cerr << "//" << expr->x << " not in context" << endl;
      }
      break;
    case ExprType::APP: {
      infer(expr->e1, context, cl);
      infer(expr->e2, context, cl);
      expr->type = newvar();
      auto t = make_shared<Mono>();
      t->is_const = true;
      t->D = "->";
      t->tau.push_back(expr->e2->type);
      t->tau.push_back(expr->type);
      unify(expr->e1->type, t);
      break;
    }
    case ExprType::ABS: {
      auto tau = newvar();
      auto contextx = context->count(expr->x) ? (*context)[expr->x] : nullptr;
      (*context)[expr->x] = make_shared<Poly>(Poly{false, tau});
      infer(expr->e, context, cl);
      expr->type = make_shared<Mono>();
      expr->type->is_const = true;
      expr->type->D = "->";
      expr->type->tau.push_back(tau);
      expr->type->tau.push_back(expr->e->type);
      if (contextx == nullptr) {
        context->erase(expr->x);
      } else {
        (*context)[expr->x] = contextx;
      }
      break;
    }
    case ExprType::LET: {
      infer(expr->e1, context, cl);
      auto contextx = context->count(expr->x) ? (*context)[expr->x] : nullptr;
      (*context)[expr->x] = gen(context, expr->e1->type);
      cerr << "//" << expr->x << " : " << to_string((*context)[expr->x])
           << endl;
      infer(expr->e2, context, cl);
      expr->type = expr->e2->type;
      if (contextx == nullptr) {
        context->erase(expr->x);
      } else {
        (*context)[expr->x] = contextx;
      }
      break;
    }
    case ExprType::REC: {
      // begin rec check
      set<string> v;
      for (size_t i = 0; i < expr->xes.size(); i++) {
        if (v.count(expr->xes[i].first)) {
          cerr << "//"
               << "ERROR!" << endl;  // TODO
          cerr << "//" << expr->xes[i].first << " variable names conflict"
               << endl;
          return;
        }
        v.insert(expr->xes[i].first);
      }
      // end rec check
      vector<shared_ptr<Mono>> taus_1;
      vector<shared_ptr<Poly>> contextx_1;
      for (size_t i = 0; i < expr->xes.size(); i++) {
        if (context->count(expr->xes[i].first)) {
          contextx_1.push_back((*context)[expr->xes[i].first]);
        } else {
          contextx_1.push_back(nullptr);
        }
        taus_1.push_back(newvar());
        if (expr->xes[i].second->sig != nullptr) {
          (*context)[expr->xes[i].first] = expr->xes[i].second->sig;
        } else {
          (*context)[expr->xes[i].first] =
              make_shared<Poly>(Poly{false, taus_1[i]});
        }
      }
      for (size_t i = 0; i < expr->xes.size(); i++) {
        infer(expr->xes[i].second, context, cl);
        unify(expr->xes[i].second->type, taus_1[i]);
      }
      for (size_t i = 0; i < expr->xes.size(); i++) {
        if (contextx_1[i] == nullptr) {
          context->erase(expr->xes[i].first);
        } else {
          (*context)[expr->xes[i].first] = contextx_1[i];
        }
      }
      vector<shared_ptr<Poly>> taus_2;
      vector<shared_ptr<Poly>> contextx_2;
      for (size_t i = 0; i < expr->xes.size(); i++) {
        taus_2.push_back(gen(context, expr->xes[i].second->type));
      }
      for (size_t i = 0; i < expr->xes.size(); i++) {
        if (context->count(expr->xes[i].first)) {
          contextx_2.push_back((*context)[expr->xes[i].first]);
        } else {
          contextx_2.push_back(nullptr);
        }
        (*context)[expr->xes[i].first] = taus_2[i];
        cerr << "//" << expr->xes[i].first << " : "
             << to_string((*context)[expr->xes[i].first]) << endl;
      }
      infer(expr->e, context, cl);
      expr->type = expr->e->type;
      for (size_t i = 0; i < expr->xes.size(); i++) {
        if (contextx_2[i] == nullptr) {
          context->erase(expr->xes[i].first);
        } else {
          (*context)[expr->xes[i].first] = contextx_2[i];
        }
      }
      break;
    }
    case ExprType::CASE: {
      // TODO FIXME exhaustive check
      // begin con check
      for (size_t i = 0; i < expr->pes.size(); i++) {
        if (!cl.count(expr->pes[i].first[0]) ||
            cl[expr->pes[i].first[0]] != expr->pes[i].first.size() - 1) {
          cerr << "//"
               << "ERROR!" << endl;  // TODO
          cerr << "//" << expr->pes[i].first[0]
               << " data constructor not does exist or does not match" << endl;
          return;
        }
        set<string> v;
        for (size_t j = 1; j < expr->pes[i].first.size(); j++) {
          if (v.count(expr->pes[i].first[j])) {
            cerr << "//"
                 << "ERROR!" << endl;  // TODO
            cerr << "//" << expr->pes[i].first[0] << " variable names conflict"
                 << endl;
            return;
          }
          v.insert(expr->pes[i].first[j]);
        }
      }
      // end con check
      if (expr->gadt == nullptr) {
        vector<shared_ptr<Poly>> fns;
        for (size_t i = 0; i < expr->pes.size(); i++) {
          auto tau = inst((*context)[expr->pes[i].first[0]]);
          vector<shared_ptr<Mono>> taus_1;
          vector<shared_ptr<Poly>> contextx_1;
          for (size_t j = 1; j < expr->pes[i].first.size(); j++) {
            taus_1.push_back(newvar());
            auto t1 = make_shared<Mono>(), t2 = newvar();
            t1->is_const = true;
            t1->D = "->";
            t1->tau.push_back(taus_1[j - 1]);
            t1->tau.push_back(t2);
            unify(t1, tau);
            tau = t2;
          }
          shared_ptr<Mono> fn = make_shared<Mono>();
          fn->is_const = true;
          fn->D = "->";
          fn->tau.push_back(tau);
          for (size_t j = 1; j < expr->pes[i].first.size(); j++) {
            if (context->count(expr->pes[i].first[j])) {
              contextx_1.push_back((*context)[expr->pes[i].first[j]]);
            } else {
              contextx_1.push_back(nullptr);
            }
            (*context)[expr->pes[i].first[j]] =
                make_shared<Poly>(Poly{false, taus_1[j - 1]});
          }
          infer(expr->pes[i].second, context, cl);
          for (size_t j = 1; j < expr->pes[i].first.size(); j++) {
            if (contextx_1[j - 1] == nullptr) {
              context->erase(expr->pes[i].first[j]);
            } else {
              (*context)[expr->pes[i].first[j]] = contextx_1[j - 1];
            }
          }
          fn->tau.push_back(expr->pes[i].second->type);
          fns.push_back(gen(context, fn));
        }
        infer(expr->e, context, cl);
        expr->type = newvar();
        shared_ptr<Mono> fn = make_shared<Mono>();
        fn->is_const = true;
        fn->D = "->";
        fn->tau.push_back(expr->e->type);
        fn->tau.push_back(expr->type);
        for (size_t i = 0; i < expr->pes.size(); i++) {
          unify(fn, inst(fns[i]));
        }
        for (size_t i = 0; i < expr->pes.size(); i++) {
          cerr << "//case " << expr->pes[i].first[0] << " : "
               << to_string(fns[i]) << endl;
        }
      } else {
        vector<shared_ptr<Poly>> fns;
        for (size_t i = 0; i < expr->pes.size(); i++) {
          auto gadt = inst(expr->gadt);
          auto tau = inst((*context)[expr->pes[i].first[0]]);
          vector<shared_ptr<Mono>> taus_1;
          vector<shared_ptr<Poly>> contextx_1;
          for (size_t j = 1; j < expr->pes[i].first.size(); j++) {
            taus_1.push_back(newvar());
            auto t1 = make_shared<Mono>(), t2 = newvar();
            t1->is_const = true;
            t1->D = "->";
            t1->tau.push_back(taus_1[j - 1]);
            t1->tau.push_back(t2);
            unify(t1, tau);
            tau = t2;
          }
          shared_ptr<Mono> fn = make_shared<Mono>(), ret = newvar();
          fn->is_const = true;
          fn->D = "->";
          fn->tau.push_back(tau);
          fn->tau.push_back(ret);
          unify(fn, gadt);
          fns.push_back(gen(context, fn));
          for (size_t j = 1; j < expr->pes[i].first.size(); j++) {
            if (context->count(expr->pes[i].first[j])) {
              contextx_1.push_back((*context)[expr->pes[i].first[j]]);
            } else {
              contextx_1.push_back(nullptr);
            }
            (*context)[expr->pes[i].first[j]] =
                make_shared<Poly>(Poly{false, taus_1[j - 1]});
          }
          infer(expr->pes[i].second, context, cl);
          for (size_t j = 1; j < expr->pes[i].first.size(); j++) {
            if (contextx_1[j - 1] == nullptr) {
              context->erase(expr->pes[i].first[j]);
            } else {
              (*context)[expr->pes[i].first[j]] = contextx_1[j - 1];
            }
          }
          unify(expr->pes[i].second->type, ret);
          set<shared_ptr<Mono>> st;
          unify_sig(fn, inst(fns.back()), st);
        }
        infer(expr->e, context, cl);
        expr->type = newvar();
        shared_ptr<Mono> fn = make_shared<Mono>();
        fn->is_const = true;
        fn->D = "->";
        fn->tau.push_back(expr->e->type);
        fn->tau.push_back(expr->type);
        unify(fn, inst(expr->gadt));
        for (size_t i = 0; i < expr->pes.size(); i++) {
          cerr << "//case " << expr->pes[i].first[0] << " : "
               << to_string(fns[i]) << endl;
        }
      }
      break;
    }
    case ExprType::FFI: {
      expr->type = newvar();
      break;
    }
  }
  if (expr->sig != nullptr) {
    set<shared_ptr<Mono>> st;
    unify_sig(expr->type, inst(expr->sig), st);
  }
}

#endif

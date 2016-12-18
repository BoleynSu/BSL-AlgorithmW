#ifndef SU_BOLEYN_BSL_TYPE_INFER_H
#define SU_BOLEYN_BSL_TYPE_INFER_H

#include <cassert>
#include <cstdlib>
#include <iostream>
#include <map>
#include <memory>
#include <set>
#include <string>
#include <utility>
#include <vector>

#include "context.h"
#include "data.h"
#include "expr.h"
#include "type.h"

using namespace std;

shared_ptr<Poly> gen(shared_ptr<Context> context, shared_ptr<Mono> tau) {
  tau = find(tau);
  set<shared_ptr<Mono>> f;
  for (auto &c : *context) {
    set<shared_ptr<Mono>> fi;
    if (context->has_poly(c.first)) {
      ftv(fi, context->get_poly(c.first));
    }
    f.insert(fi.begin(), fi.end());
  }
  set<shared_ptr<Mono>> fp;
  ftv(fp, tau);
  for (auto i : f) {
    fp.erase(i);
  }
  map<shared_ptr<Mono>, shared_ptr<Mono>> m;
  for (auto f : fp) {
    if (is_f(f)) {
      m[f] = new_forall_var();
    }
  }
  auto g = new_poly(inst(tau, m));
  for (auto f : m) {
    g = new_poly(f.second, g);
  }
  return g;
}

bool occ(shared_ptr<Mono>, shared_ptr<Mono>);

bool occ(shared_ptr<Mono> a, shared_ptr<Poly> b) {
  if (b->is_mono) {
    return occ(a, b->tau);
  } else {
    return occ(a, b->sigma);
  }
}

bool occ(shared_ptr<Mono> a, shared_ptr<Mono> b) {
  b = find(b);
  if (is_c(b)) {
    for (size_t i = 0; i < b->tau.size(); i++) {
      if (occ(a, b->tau[i])) {
        return true;
      }
    }
    return false;
  } else {
    return a == b;
  }
}

bool unify(shared_ptr<Mono> a, shared_ptr<Mono> b, ostream *cerr,
           set<shared_ptr<Mono>> *st = nullptr) {
  a = find(a);
  b = find(b);
  if (st != nullptr) {
    st->insert(a);
  }
  if (a != b) {
    if (is_c(a)) {
      if (is_c(b)) {
        if (a->D == b->D && a->tau.size() == b->tau.size()) {
          for (size_t i = 0; i < a->tau.size(); i++) {
            if (!unify(a->tau[i], b->tau[i], cerr, st)) {
              return false;
            }
          }
          return true;
        } else {
          if (cerr != nullptr) {
            (*cerr) << "type error: " << to_string(a) << " /= " << to_string(b)
                    << endl;
          }
          return false;
        }
      } else if (is_f(b)) {
        if (occ(b, a)) {
          if (cerr != nullptr) {
            (*cerr) << "type error: " << to_string(a) << " ~ " << to_string(b)
                    << endl;
          }
          return false;
        } else {
          if (st != nullptr && st->count(b) && a != b) {
            if (cerr != nullptr) {
              (*cerr) << "type error: " << to_string(a) << " !< "
                      << to_string(b) << endl;
            }
            return false;
          } else {
            b->par = a;
            return true;
          }
        }
      } else {
        if (cerr != nullptr) {
          (*cerr) << "type error: " << to_string(a) << " /= " << to_string(b)
                  << endl;
        }
        return false;
      }
    } else if (is_f(a)) {
      if (is_c(b)) {
        if (occ(a, b)) {
          if (cerr != nullptr) {
            (*cerr) << "type error: " << to_string(a) << " ~ " << to_string(b)
                    << endl;
          }
          return false;
        } else {
          a->par = b;
          return true;
        }
      } else if (is_f(b)) {
        if (st != nullptr && st->count(b) && a != b) {
          if (cerr != nullptr) {
            (*cerr) << "type error: " << to_string(a) << " !< " << to_string(b)
                    << endl;
          }
          return false;
        } else {
          b->par = a;
          return true;
        }
      } else {
        a->par = b;
        return true;
      }
    } else {
      if (is_c(b)) {
        if (cerr != nullptr) {
          (*cerr) << "type error: " << to_string(a) << " /= " << to_string(b)
                  << endl;
        }
        return false;
      } else if (is_f(b)) {
        if (st != nullptr && st->count(b) && a != b) {
          if (cerr != nullptr) {
            (*cerr) << "type error: " << to_string(a) << " !< " << to_string(b)
                    << endl;
          }
          return false;
        } else {
          b->par = a;
          return true;
        }
      } else {
        if (cerr != nullptr) {
          (*cerr) << "type error: " << to_string(a) << " /= " << to_string(b)
                  << endl;
        }
        return false;
      }
    }
  } else {
    return true;
  }
}

void infer(shared_ptr<Expr> expr, shared_ptr<Context> context,
           pair<shared_ptr<map<string, shared_ptr<Data>>>,
                shared_ptr<map<string, shared_ptr<Constructor>>>> &dnc) {
  switch (expr->T) {
    case ExprType::VAR:
      if (context->has_rank2poly(expr->x)) {
        cerr << "type error: " << expr->x << " is of rank-2 type" << endl;
        exit(EXIT_FAILURE);
      } else if (context->has_poly(expr->x)) {
        auto t = context->get_poly(expr->x);
        expr->type = inst(t);
      } else {
        cerr << "type error: " << expr->x << " is not in context" << endl;
        exit(EXIT_FAILURE);
      }
      break;
    case ExprType::APP: {
      if (expr->e1->T == ExprType::VAR && context->has_rank2poly(expr->e1->x)) {
        //        infer(expr->e1, context, dnc);
        infer(expr->e2, context, dnc);
        expr->type = new_forall_var();
        auto t = rank2inst(context->get_rank2poly(expr->e1->x));
        set<shared_ptr<Mono>> st;
        unify(t.second, expr->type, &cerr);
        unify(inst(t.first), expr->e2->type, &cerr, &st);
      } else {
        infer(expr->e1, context, dnc);
        infer(expr->e2, context, dnc);
        expr->type = new_forall_var();
        auto t = new_const_var("->");
        t->tau.push_back(expr->e2->type);
        t->tau.push_back(expr->type);
        if (!unify(expr->e1->type, t, &cerr)) {
          exit(EXIT_FAILURE);
        }
      }
      break;
    }
    case ExprType::ABS: {
      auto tau = new_forall_var();
      context->set_poly(expr->x, new_poly(tau));
      infer(expr->e, context, dnc);
      expr->type = new_const_var("->");
      expr->type->tau.push_back(tau);
      expr->type->tau.push_back(expr->e->type);
      context->unset(expr->x);
      break;
    }
    case ExprType::LET: {
      infer(expr->e1, context, dnc);
      if (expr->e1->sig != nullptr) {
        context->set_poly(expr->x, expr->e1->sig);
      } else {
        context->set_poly(expr->x, gen(context, expr->e1->type));
      }
      cerr << "//" << expr->x << " : " << to_string(context->get_poly(expr->x))
           << endl;
      infer(expr->e2, context, dnc);
      expr->type = expr->e2->type;
      context->unset(expr->x);
      break;
    }
    case ExprType::REC: {
      for (size_t i = 0; i < expr->xes.size(); i++) {
        if (expr->xes[i].second->sig != nullptr) {
          context->set_poly(expr->xes[i].first, expr->xes[i].second->sig);
        } else {
          context->set_poly(expr->xes[i].first, new_poly(new_forall_var()));
        }
      }
      for (size_t i = 0; i < expr->xes.size(); i++) {
        infer(expr->xes[i].second, context, dnc);
      }
      for (size_t i = 0; i < expr->xes.size(); i++) {
        context->unset(expr->xes[i].first);
      }
      for (size_t i = 0; i < expr->xes.size(); i++) {
        context->set_poly(expr->xes[i].first,
                          gen(context, expr->xes[i].second->type));
        cerr << "//" << expr->xes[i].first << " : "
             << to_string(context->get_poly(expr->xes[i].first)) << endl;
      }
      infer(expr->e, context, dnc);
      expr->type = expr->e->type;
      for (size_t i = 0; i < expr->xes.size(); i++) {
        context->unset(expr->xes[i].first);
      }
      break;
    }
    case ExprType::CASE: {
      map<string, shared_ptr<Poly>> fns;
      for (auto &pes_ : expr->pes) {
        auto pes = pes_.second;
        assert(dnc.second->count(pes_.first));
        if ((*dnc.second)[pes_.first]->rank2sig != nullptr) {
          auto tau = rank2inst(
              (*dnc.second)[pes_.first]->rank2sig,
              (*dnc.first)[(*dnc.second)[pes_.first]->data_name]->exists);
          context->set_poly(pes.first.front(), tau.first);
          // TODO FIXME
          infer(pes.second, context, dnc);
          context->unset(pes.first.front());
          auto fn = new_const_var("->");
          fn->tau.push_back(tau.second);
          fn->tau.push_back(pes.second->type);
          fns[pes_.first] = gen(context, fn);
        } else {
          auto tau = inst((*dnc.second)[pes_.first]->sig,
                          (*dnc.first)

                              [(*dnc.second)[pes_.first]->data_name]
                                  ->exists);
          vector<shared_ptr<Mono>> taus;
          for (size_t i = 0; i < pes.first.size(); i++) {
            taus.push_back(new_forall_var());
            auto t1 = new_const_var("->"), t2 = new_forall_var();
            t1->tau.push_back(taus[i]);
            t1->tau.push_back(t2);
            unify(t1, tau, &cerr);
            tau = t2;
          }
          auto fn = new_const_var("->");
          fn->tau.push_back(tau);
          for (size_t i = 0; i < pes.first.size(); i++) {
            context->set_poly(pes.first[i], new_poly(taus[i]));
          }
          infer(pes.second, context, dnc);
          for (size_t i = 0; i < pes.first.size(); i++) {
            context->unset(pes.first[i]);
          }
          fn->tau.push_back(pes.second->type);
          fns[pes_.first] = gen(context, fn);
        }
      }
      if (expr->gadt != nullptr) {
        auto t = get_mono(expr->gadt);
        if (!(t->is_const && t->D == "->" && is_c(t->tau[0]) &&
              dnc.first->count(t->tau[0]->D))) {
          cerr << "type error: invaliad signature for case expression" << endl
               << to_string(expr->gadt) << endl;
          exit(EXIT_FAILURE);
        }
      } else {
        auto gadt = new_forall_var();
        for (auto &fn : fns) {
          unify(gadt, inst(fn.second), &cerr);
        }
        expr->gadt = gen(context, gadt);
        auto t = get_mono(expr->gadt);
        assert(t->is_const && t->D == "->" && is_c(t->tau[0]) &&
               dnc.first->count(t->tau[0]->D));
      }
      for (auto &fn : fns) {
        cerr << "//case " << fn.first << " : " << to_string(fn.second) << endl;
      }
      cerr << "//: " << to_string(expr->gadt) << endl;
      for (auto c : (*dnc.first)[(*dnc.second)[fns.begin()->first]->data_name]
                        ->constructors) {
        if (c->rank2sig != nullptr) {
          auto tau = rank2inst(c->rank2sig, (*dnc.first)[c->data_name]->exists);
          assert(c->arg == 1);
          auto fn = new_const_var("->"), ret = new_forall_var();
          fn->tau.push_back(tau.second);
          fn->tau.push_back(ret);
          if (unify(fn, inst(expr->gadt), nullptr)) {
            if (fns.count(c->name)) {
              set<shared_ptr<Mono>> st;
              unify(inst(fns[c->name]), fn, &cerr, &st);
            } else {
              cerr << "type error: non-exhaustive patterns `" << c->name << "`"
                   << endl;
              exit(EXIT_FAILURE);
            }
          } else {
            if (fns.count(c->name)) {
              unify(inst(expr->gadt), fn, &cerr);
              assert(false);
            }
          }
        } else {
          auto tau = inst(c->sig, (*dnc.first)[c->data_name]->exists);
          vector<shared_ptr<Mono>> taus;
          for (size_t i = 0; i < c->arg; i++) {
            taus.push_back(new_forall_var());
            auto t1 = new_const_var("->"), t2 = new_forall_var();
            t1->tau.push_back(taus[i]);
            t1->tau.push_back(t2);
            unify(t1, tau, &cerr);
            tau = t2;
          }
          auto fn = new_const_var("->"), ret = new_forall_var();
          fn->tau.push_back(tau);
          fn->tau.push_back(ret);
          if (unify(fn, inst(expr->gadt), nullptr)) {
            if (fns.count(c->name)) {
              set<shared_ptr<Mono>> st;
              unify(inst(fns[c->name]), fn, &cerr, &st);
            } else {
              cerr << "type error: non-exhaustive patterns `" << c->name << "`"
                   << endl;
              exit(EXIT_FAILURE);
            }
          } else {
            if (fns.count(c->name)) {
              unify(inst(expr->gadt), fn, &cerr);
              assert(false);
            }
          }
        }
      }
      infer(expr->e, context, dnc);
      expr->type = new_forall_var();
      auto fn = new_const_var("->");
      fn->tau.push_back(expr->e->type);
      fn->tau.push_back(expr->type);
      unify(fn, inst(expr->gadt), &cerr);
      break;
    }
    case ExprType::FFI: {
      expr->type = new_forall_var();
      break;
    }
  }
  if (expr->sig != nullptr) {
    set<shared_ptr<Mono>> st;
    unify(inst(expr->sig), expr->type, &cerr, &st);
  }
}

#endif

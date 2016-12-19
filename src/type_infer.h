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

#include "ds/context.h"
#include "ds/data.h"
#include "ds/expr.h"
#include "ds/type.h"
#include "ds/unit.h"

using namespace std;

struct TypeInfer {
  shared_ptr<Unit> unit;
  Context context;
  TypeInfer(shared_ptr<Unit> unit) : unit(unit), context() {
    for (auto dai : unit->data) {
      auto da = dai.second;
      for (auto &c : da->constructors) {
        if (c->rank2sig != nullptr) {
          //            cerr << "//" << c->name << " : " <<
          //            to_string(c->rank2sig) << endl;
          context.set_rank2poly(c->name, c->rank2sig);
        } else {
          //            cerr << "//" << c->name << " : " << to_string(c->sig)
          //            << endl;
          context.set_poly(c->name, c->sig);
        }
      }
    }
    infer(unit->expr);
    for (auto dai : unit->data) {
      auto da = dai.second;
      for (auto &c : da->constructors) {
        context.unset(c->name);
      }
    }
  }

  void check(shared_ptr<Constructor> c, shared_ptr<Mono> p,
             set<shared_ptr<Mono>> &st, bool m, bool r,
             shared_ptr<map<string, shared_ptr<Data>>> data) {
    if (is_c(p)) {
      if (p->D == "->") {
        assert(p->tau.size() == 2);
        check(c, p->tau[0], st, m && false, r, data);
        check(c, p->tau[1], st, m && true, r, data);
      } else {
        if (data->count(p->D)) {
          if (m) {
            if (p->D != c->data_name) {
              cerr << "type error: in constructor " << c->name << ":"
                   << (c->rank2sig != nullptr ? to_string(c->rank2sig)
                                              : to_string(c->sig))
                   << endl
                   << "return type is `" << p->D << "` instead of `"
                   << c->data_name << "`" << endl;
              exit(EXIT_FAILURE);
            }
          }
          if (p->tau.size() != (*data)[p->D]->arg) {
            cerr << "type error: in constructor " << c->name << ":"
                 << (c->rank2sig != nullptr ? to_string(c->rank2sig)
                                            : to_string(c->sig))
                 << endl
                 << "type `" << p->D << "` expects " << (*data)[p->D]->arg
                 << " arguments, but gets " << p->tau.size() << endl;
            exit(EXIT_FAILURE);
          }
          for (size_t i = 0; i < p->tau.size(); i++) {
            check(c, p->tau[i], st, false, m || r, data);
          }
          if (m) {
            for (auto t : st) {
              assert(is_f(t));
              (*data)[c->data_name]->exists.push_back(t);
            }
          }
        } else {
          cerr << "type error: in constructor " << c->name << ":"
               << (c->rank2sig != nullptr ? to_string(c->rank2sig)
                                          : to_string(c->sig))
               << endl
               << "`" << p->D << "` is not a type" << endl;
          exit(EXIT_FAILURE);
        }
      }
    } else {
      if (m) {
        cerr << "type error: in constructor " << c->name << ":"
             << (c->rank2sig != nullptr ? to_string(c->rank2sig)
                                        : to_string(c->sig))
             << endl
             << "return type is a type variable instead of " << c->data_name
             << endl;
        exit(EXIT_FAILURE);
      }
      if (r) {
        st.erase(p);
      }
    }
  }

  void check(shared_ptr<Poly> t, shared_ptr<Mono> p, set<shared_ptr<Mono>> &st,
             shared_ptr<map<string, shared_ptr<Data>>> data) {
    if (is_c(p)) {
      if (p->D == "->") {
        assert(p->tau.size() == 2);
        check(t, p->tau[0], st, data);
        check(t, p->tau[1], st, data);
      } else if (data->count(p->D)) {
        if (p->tau.size() != (*data)[p->D]->arg) {
          cerr << "type error: in signature " << to_string(t) << endl
               << "type `" << p->D << "` expects " << (*data)[p->D]->arg
               << " arguments, but gets " << p->tau.size() << endl;
          exit(EXIT_FAILURE);
        }
        for (size_t i = 0; i < p->tau.size(); i++) {
          check(t, p->tau[i], st, data);
        }
      } else {
        cerr << "type error: in signature " << to_string(t) << endl
             << "`" << p->D << "` is not a type" << endl;
        exit(EXIT_FAILURE);
      }
    } else {
      st.erase(p);
    }
  }
  void check(shared_ptr<map<string, shared_ptr<Data>>> data,
             shared_ptr<Poly> t) {
    auto p = t;
    set<shared_ptr<Mono>> st;
    while (!p->is_mono) {
      st.insert(p->alpha);
      p = p->sigma;
    }
    check(t, p->tau, st, data);
    if (st.size()) {
      cerr << "type error: in signature " << to_string(t) << endl
           << "free type variables are found" << endl;
    }
  }

  void check(shared_ptr<map<string, shared_ptr<Data>>> data,
             shared_ptr<map<string, shared_ptr<Constructor>>> cons) {
    for (auto &c : *cons) {
      if (c.second->rank2sig != nullptr) {
        auto p = c.second->rank2sig;
        set<shared_ptr<Mono>> st;
        while (p->is_forall) {
          st.insert(p->alpha);
          p = p->sigma;
        }
        check(c.second, get_mono(p->poly), st, false, false, data);
        auto mo = p->mono;
        check(c.second, p->mono, st, true, false, data);
      } else {
        auto p = c.second->sig;
        set<shared_ptr<Mono>> st;
        while (!p->is_mono) {
          st.insert(p->alpha);
          p = p->sigma;
        }
        check(c.second, p->tau, st, true, false, data);
      }
    }
  }

  shared_ptr<Poly> gen(shared_ptr<Mono> tau) {
    tau = find(tau);
    set<shared_ptr<Mono>> f;
    for (auto &c : context) {
      set<shared_ptr<Mono>> fi;
      if (context.has_poly(c.first)) {
        ftv(fi, context.get_poly(c.first));
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
              (*cerr) << "type error: " << to_string(a)
                      << " /= " << to_string(b) << endl;
              exit(EXIT_FAILURE);
            }
            return false;
          }
        } else if (is_f(b)) {
          if (occ(b, a)) {
            if (cerr != nullptr) {
              (*cerr) << "type error: " << to_string(a) << " ~ " << to_string(b)
                      << endl;
              exit(EXIT_FAILURE);
            }
            return false;
          } else {
            if (st != nullptr && st->count(b) && a != b) {
              if (cerr != nullptr) {
                (*cerr) << "type error: " << to_string(a) << " !< "
                        << to_string(b) << endl;
                exit(EXIT_FAILURE);
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
            exit(EXIT_FAILURE);
          }
          return false;
        }
      } else if (is_f(a)) {
        if (is_c(b)) {
          if (occ(a, b)) {
            if (cerr != nullptr) {
              (*cerr) << "type error: " << to_string(a) << " ~ " << to_string(b)
                      << endl;
              exit(EXIT_FAILURE);
            }
            return false;
          } else {
            if (st != nullptr) {
              if (cerr != nullptr) {
                (*cerr) << "type error: " << to_string(a) << " !< "
                        << to_string(b) << endl;
                exit(EXIT_FAILURE);
              }
              return false;
            } else {
              a->par = b;
              return true;
            }
          }
        } else if (is_f(b)) {
          if (st != nullptr && st->count(b) && a != b) {
            if (cerr != nullptr) {
              (*cerr) << "type error: " << to_string(a) << " !< "
                      << to_string(b) << endl;
              exit(EXIT_FAILURE);
            }
            return false;
          } else {
            b->par = a;
            return true;
          }
        } else {
          if (st != nullptr) {
            if (cerr != nullptr) {
              (*cerr) << "type error: " << to_string(a) << " !< "
                      << to_string(b) << endl;
              exit(EXIT_FAILURE);
            }
            return false;
          } else {
            a->par = b;
            return true;
          }
        }
      } else {
        if (is_c(b)) {
          if (cerr != nullptr) {
            (*cerr) << "type error: " << to_string(a) << " /= " << to_string(b)
                    << endl;
            exit(EXIT_FAILURE);
          }
          return false;
        } else if (is_f(b)) {
          if (st != nullptr && st->count(b) && a != b) {
            if (cerr != nullptr) {
              (*cerr) << "type error: " << to_string(a) << " !< "
                      << to_string(b) << endl;
              exit(EXIT_FAILURE);
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
            exit(EXIT_FAILURE);
          }
          return false;
        }
      }
    } else {
      return true;
    }
  }

  void infer(shared_ptr<Expr> expr) {
    switch (expr->T) {
      case ExprType::VAR:
        if (context.has_rank2poly(expr->x)) {
          cerr << "type error: " << expr->x << " is of rank-2 type" << endl;
          exit(EXIT_FAILURE);
        } else if (context.has_poly(expr->x)) {
          auto t = context.get_poly(expr->x);
          expr->type = inst(t);
        } else {
          cerr << "type error: " << expr->x << " is not in context" << endl;
          exit(EXIT_FAILURE);
        }
        break;
      case ExprType::APP: {
        if (expr->e1->T == ExprType::VAR &&
            context.has_rank2poly(expr->e1->x)) {
          //        infer(expr->e1, context, dnc);
          infer(expr->e2);
          expr->type = new_forall_var();
          auto t = rank2inst(context.get_rank2poly(expr->e1->x));
          set<shared_ptr<Mono>> st;
          unify(t.second, expr->type, &cerr);
          unify(inst(t.first), expr->e2->type, &cerr, &st);
        } else {
          infer(expr->e1);
          infer(expr->e2);
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
        context.set_poly(expr->x, new_poly(tau));
        infer(expr->e);
        expr->type = new_const_var("->");
        expr->type->tau.push_back(tau);
        expr->type->tau.push_back(expr->e->type);
        context.unset(expr->x);
        break;
      }
      case ExprType::LET: {
        infer(expr->e1);
        if (expr->e1->sig != nullptr) {
          context.set_poly(expr->x, expr->e1->sig);
        } else {
          context.set_poly(expr->x, gen(expr->e1->type));
        }
        //      cerr << "//" << expr->x << " : " <<
        //      to_string(context.get_poly(expr->x))
        //           << endl;
        infer(expr->e2);
        expr->type = expr->e2->type;
        context.unset(expr->x);
        break;
      }
      case ExprType::REC: {
        for (auto &xe : expr->xes) {
          if (xe.second->sig != nullptr) {
            context.set_poly(xe.first, xe.second->sig);
          } else {
            context.set_poly(xe.first, new_poly(new_forall_var()));
          }
        }
        for (auto &xe : expr->xes) {
          infer(xe.second);
        }
        for (auto &xe : expr->xes) {
          context.unset(xe.first);
        }
        for (auto &xe : expr->xes) {
          context.set_poly(xe.first, gen(xe.second->type));
          //        cerr << "//" << xe.first << " : "
          //             << to_string(context.get_poly(xe.first))
          //             <<
          //             endl;
        }
        infer(expr->e);
        expr->type = expr->e->type;
        for (auto &xe : expr->xes) {
          context.unset(xe.first);
        }
        break;
      }
      case ExprType::CASE: {
        map<string, shared_ptr<Poly>> fns;
        for (auto &pes_ : expr->pes) {
          auto pes = pes_.second;
          assert(unit->has_constructor(pes_.first));
          auto c = unit->get_constructor(pes_.first);
          assert(c->arg == pes.first.size());
          if (c->rank2sig != nullptr) {
            auto tau =
                rank2inst(c->rank2sig, unit->get_data(c->data_name)->exists);
            vector<shared_ptr<Mono>> taus;
            taus.push_back(nullptr);
            for (size_t i = 1; i < pes.first.size(); i++) {
              taus.push_back(new_forall_var());
              auto t1 = new_const_var("->"), t2 = new_forall_var();
              t1->tau.push_back(taus[i]);
              t1->tau.push_back(t2);
              unify(t1, tau.second, &cerr);
              tau.second = t2;
            }
            auto fn = new_const_var("->");
            fn->tau.push_back(tau.second);
            context.set_poly(pes.first.front(), tau.first);
            for (size_t i = 1; i < pes.first.size(); i++) {
              context.set_poly(pes.first[i], new_poly(taus[i]));
            }
            infer(pes.second);
            for (size_t i = 0; i < pes.first.size(); i++) {
              context.unset(pes.first[i]);
            }
            fn->tau.push_back(pes.second->type);
            fns[pes_.first] = gen(fn);
          } else {
            auto tau = inst(c->sig, unit->get_data(c->data_name)->exists);
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
              context.set_poly(pes.first[i], new_poly(taus[i]));
            }
            infer(pes.second);
            for (size_t i = 0; i < pes.first.size(); i++) {
              context.unset(pes.first[i]);
            }
            fn->tau.push_back(pes.second->type);
            fns[pes_.first] = gen(fn);
          }
        }
        if (expr->gadt != nullptr) {
          auto t = get_mono(expr->gadt);
          if (!(t->is_const && t->D == "->" && is_c(t->tau[0]) &&
                unit->has_data(t->tau[0]->D))) {
            cerr << "type error: invaliad signature for case expression" << endl
                 << to_string(expr->gadt) << endl;
            exit(EXIT_FAILURE);
          }
        } else {
          auto gadt = new_forall_var();
          for (auto &fn : fns) {
            unify(gadt, inst(fn.second), &cerr);
          }
          expr->gadt = gen(gadt);
          auto t = get_mono(expr->gadt);
          assert(t->is_const && t->D == "->" && is_c(t->tau[0]) &&
                 unit->has_data(t->tau[0]->D));
        }
        //      for (auto &fn : fns) {
        //        cerr << "//case " << fn.first << " : " <<
        //        to_string(fn.second)
        //        << endl;
        //      }
        //      cerr << "//: " << to_string(expr->gadt) << endl;
        for (auto c :
             unit->get_data(
                     unit->get_constructor(fns.begin()->first)->data_name)
                 ->constructors) {
          if (c->rank2sig != nullptr) {
            auto tau =
                rank2inst(c->rank2sig, unit->get_data(c->data_name)->exists);
            for (size_t i = 1; i < c->arg; i++) {
              auto t1 = new_const_var("->"), t2 = new_forall_var();
              t1->tau.push_back(new_forall_var());
              t1->tau.push_back(t2);
              unify(t1, tau.second, &cerr);
              tau.second = t2;
            }
            auto fn = new_const_var("->"), ret = new_forall_var();
            fn->tau.push_back(tau.second);
            fn->tau.push_back(ret);
            if (unify(fn, inst(expr->gadt), nullptr)) {
              if (fns.count(c->name)) {
                set<shared_ptr<Mono>> st;
                unify(fn, inst(fns[c->name]), &cerr, &st);
              } else {
                cerr << "type error: non-exhaustive patterns `" << c->name
                     << "`" << endl;
                exit(EXIT_FAILURE);
              }
            } else {
              if (fns.count(c->name)) {
                unify(inst(expr->gadt), fn, &cerr);
                assert(false);
              }
            }
          } else {
            auto tau = inst(c->sig, unit->get_data(c->data_name)->exists);
            for (size_t i = 0; i < c->arg; i++) {
              auto t1 = new_const_var("->"), t2 = new_forall_var();
              t1->tau.push_back(new_forall_var());
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
                unify(fn, inst(fns[c->name]), &cerr, &st);
              } else {
                cerr << "type error: non-exhaustive patterns `" << c->name
                     << "`" << endl;
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
        infer(expr->e);
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
};

#endif

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

#include "ds/data.h"
#include "ds/expr.h"
#include "ds/type.h"
#include "ds/unit.h"

using namespace std;

struct TypeInfer {
  struct Context {
    map<string, vector<pair<bool, shared_ptr<void>>>> c;
    map<shared_ptr<Expr>, shared_ptr<Mono>> type;
    map<shared_ptr<Constructor>, set<shared_ptr<Mono>>> exists;
    bool has_poly(const string &key) {
      auto it = c.find(key);
      if (it != c.end() && it->second.size() && it->second.back().first) {
        return true;
      } else {
        return false;
      }
    }
    bool has_rank2poly(const string &key) {
      auto it = c.find(key);
      if (it != c.end() && it->second.size() && !it->second.back().first) {
        return true;
      } else {
        return false;
      }
    }
    shared_ptr<Poly> get_poly(const string &key) {
      auto it = c.find(key);
      if (it != c.end() && it->second.size() && it->second.back().first) {
        return static_pointer_cast<Poly>(it->second.back().second);
      } else {
        return nullptr;
      }
    }
    shared_ptr<Rank2Poly> get_rank2poly(const string &key) {
      auto it = c.find(key);
      if (it != c.end() && it->second.size() && !it->second.back().first) {
        return static_pointer_cast<Rank2Poly>(it->second.back().second);
      } else {
        return nullptr;
      }
    }
    void set_poly(const string &key, shared_ptr<Poly> value) {
      c[key].push_back(make_pair(true, static_pointer_cast<void>(value)));
    }
    void set_rank2poly(const string &key, shared_ptr<Rank2Poly> value) {
      c[key].push_back(make_pair(false, static_pointer_cast<void>(value)));
    }
    void unset(const string &key) {
      auto &v = c[key];
      v.pop_back();
      if (v.empty()) {
        c.erase(key);
      }
    }
    shared_ptr<Mono> get_type(shared_ptr<Expr> e) { return type[e]; }
    void set_type(shared_ptr<Expr> e, shared_ptr<Mono> t) { type[e] = t; }
    void add_exists(shared_ptr<Constructor> c, shared_ptr<Mono> t) {
      exists[c].insert(t);
    }
    set<shared_ptr<Mono>> &get_exists(shared_ptr<Constructor> c) {
      return exists[c];
    }
  } context;
  shared_ptr<Unit> unit;
  TypeInfer(shared_ptr<Unit> unit) : unit(unit) {
    for (auto &c : unit->cons) {
      if (c.second->rank2sig != nullptr) {
        auto p = c.second->rank2sig;
        set<shared_ptr<Mono>> st;
        while (p->is_forall) {
          st.insert(p->alpha);
          p = p->sigma;
        }
        check(c.second, get_mono(p->poly), st, false, false);
        auto mo = p->mono;
        check(c.second, p->mono, st, true, false);
      } else {
        auto p = c.second->sig;
        set<shared_ptr<Mono>> st;
        while (!p->is_mono) {
          st.insert(p->alpha);
          p = p->sigma;
        }
        check(c.second, p->tau, st, true, false);
      }
    }
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
             set<shared_ptr<Mono>> &st, bool m, bool r) {
    if (is_c(p)) {
      if (is_fun(p)) {
        assert(p->tau.size() == 2);
        check(c, p->tau[0], st, m && false, r);
        check(c, p->tau[1], st, m && true, r);
      } else {
        if (unit->data.count(p->D)) {
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
          if (p->tau.size() != unit->data[p->D]->arg) {
            cerr << "type error: in constructor " << c->name << ":"
                 << (c->rank2sig != nullptr ? to_string(c->rank2sig)
                                            : to_string(c->sig))
                 << endl
                 << "type `" << p->D << "` expects " << unit->data[p->D]->arg
                 << " arguments, but gets " << p->tau.size() << endl;
            exit(EXIT_FAILURE);
          }
          for (size_t i = 0; i < p->tau.size(); i++) {
            check(c, p->tau[i], st, false, m || r);
          }
          if (m) {
            for (auto t : st) {
              assert(is_f(t));
              context.add_exists(c, t);
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

  shared_ptr<Poly> gen(shared_ptr<Mono> tau) {
    tau = find(tau);
    set<shared_ptr<Mono>> f;
    for (auto &c : context.c) {
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

  void infer(shared_ptr<Expr> e) {
    switch (e->T) {
      case ExprType::VAR:
        if (context.has_rank2poly(e->x)) {
          cerr << "type error: " << e->x << " is of rank-2 type" << endl;
          exit(EXIT_FAILURE);
        } else if (context.has_poly(e->x)) {
          auto t = context.get_poly(e->x);
          context.set_type(e, inst(t));
        } else {
          cerr << "type error: " << e->x << " is not in context" << endl;
          exit(EXIT_FAILURE);
        }
        break;
      case ExprType::APP: {
        if (e->e1->T == ExprType::VAR && context.has_rank2poly(e->e1->x)) {
          //        infer(expr->e1, context, dnc);
          infer(e->e2);
          context.set_type(e, new_forall_var());
          auto t = rank2inst(context.get_rank2poly(e->e1->x));
          set<shared_ptr<Mono>> st;
          unify(t.second, context.get_type(e), &cerr);
          unify(inst(t.first), context.get_type(e->e2), &cerr, &st);
        } else {
          infer(e->e1);
          infer(e->e2);
          context.set_type(e, new_forall_var());
          auto t = new_fun_var();
          t->tau.push_back(context.get_type(e->e2));
          t->tau.push_back(context.get_type(e));
          if (!unify(context.get_type(e->e1), t, &cerr)) {
            exit(EXIT_FAILURE);
          }
        }
        break;
      }
      case ExprType::ABS: {
        auto tau = new_forall_var();
        context.set_poly(e->x, new_poly(tau));
        infer(e->e);
        context.set_type(e, new_fun_var());
        context.get_type(e)->tau.push_back(tau);
        context.get_type(e)->tau.push_back(context.get_type(e->e));
        context.unset(e->x);
        break;
      }
      case ExprType::LET: {
        infer(e->e1);
        if (e->e1->sig != nullptr) {
          context.set_poly(e->x, e->e1->sig);
        } else {
          context.set_poly(e->x, gen(context.get_type(e->e1)));
        }
        //      cerr << "//" << expr->x << " : " <<
        //      to_string(context.get_poly(expr->x))
        //           << endl;
        infer(e->e2);
        context.set_type(e, context.get_type(e->e2));
        context.unset(e->x);
        break;
      }
      case ExprType::REC: {
        for (auto &xe : e->xes) {
          if (xe.second->sig != nullptr) {
            context.set_poly(xe.first, xe.second->sig);
          } else {
            context.set_poly(xe.first, new_poly(new_forall_var()));
          }
        }
        for (auto &xe : e->xes) {
          infer(xe.second);
        }
        for (auto &xe : e->xes) {
          auto t = find(context.get_type(xe.second));
          if (xe.second->T != ExprType::ABS) {
            cerr << "type error: rec of this type is not supported" << endl;
            exit(EXIT_FAILURE);
          }
        }
        for (auto &xe : e->xes) {
          context.unset(xe.first);
        }
        for (auto &xe : e->xes) {
          context.set_poly(xe.first, gen(context.get_type(xe.second)));
          //        cerr << "//" << xe.first << " : "
          //             << to_string(context.get_poly(xe.first))
          //             <<
          //             endl;
        }
        infer(e->e);
        context.set_type(e, context.get_type(e->e));
        for (auto &xe : e->xes) {
          context.unset(xe.first);
        }
        break;
      }
      case ExprType::CASE: {
        map<string, shared_ptr<Poly>> fns;
        for (auto &pes_ : e->pes) {
          auto pes = pes_.second;
          assert(unit->cons.count(pes_.first));
          auto c = unit->cons[pes_.first];
          assert(c->arg == pes.first.size());
          if (c->rank2sig != nullptr) {
            auto tau = rank2inst(c->rank2sig, context.get_exists(c));
            vector<shared_ptr<Mono>> taus;
            taus.push_back(nullptr);
            for (size_t i = 1; i < pes.first.size(); i++) {
              taus.push_back(new_forall_var());
              auto t1 = new_fun_var(), t2 = new_forall_var();
              t1->tau.push_back(taus[i]);
              t1->tau.push_back(t2);
              unify(t1, tau.second, &cerr);
              tau.second = t2;
            }
            auto fn = new_fun_var();
            fn->tau.push_back(tau.second);
            context.set_poly(pes.first.front(), tau.first);
            for (size_t i = 1; i < pes.first.size(); i++) {
              context.set_poly(pes.first[i], new_poly(taus[i]));
            }
            infer(pes.second);
            for (size_t i = 0; i < pes.first.size(); i++) {
              context.unset(pes.first[i]);
            }
            fn->tau.push_back(context.get_type(pes.second));
            fns[pes_.first] = gen(fn);
          } else {
            auto tau = inst(c->sig, context.get_exists(c));
            vector<shared_ptr<Mono>> taus;
            for (size_t i = 0; i < pes.first.size(); i++) {
              taus.push_back(new_forall_var());
              auto t1 = new_fun_var(), t2 = new_forall_var();
              t1->tau.push_back(taus[i]);
              t1->tau.push_back(t2);
              unify(t1, tau, &cerr);
              tau = t2;
            }
            auto fn = new_fun_var();
            fn->tau.push_back(tau);
            for (size_t i = 0; i < pes.first.size(); i++) {
              context.set_poly(pes.first[i], new_poly(taus[i]));
            }
            infer(pes.second);
            for (size_t i = 0; i < pes.first.size(); i++) {
              context.unset(pes.first[i]);
            }
            fn->tau.push_back(context.get_type(pes.second));
            fns[pes_.first] = gen(fn);
          }
        }
        auto gadt = e->gadt;
        if (gadt != nullptr) {
          auto t = get_mono(gadt);
          if (!(is_fun(t) && is_c(t->tau[0]) &&
                unit->data.count(t->tau[0]->D))) {
            cerr << "type error: invaliad signature for case expression" << endl
                 << to_string(gadt) << endl;
            exit(EXIT_FAILURE);
          }
        } else {
          auto gadt_ = new_forall_var();
          for (auto &fn : fns) {
            unify(gadt_, inst(fn.second), &cerr);
          }
          gadt = gen(gadt_);
          auto t = get_mono(gadt);
          assert(is_fun(t) && is_c(t->tau[0]) &&
                 unit->data.count(t->tau[0]->D));
        }
        //      for (auto &fn : fns) {
        //        cerr << "//case " << fn.first << " : " <<
        //        to_string(fn.second)
        //        << endl;
        //      }
        //      cerr << "//: " << to_string(expr->gadt) << endl;
        for (auto c : unit->data[unit->cons[fns.begin()->first]->data_name]
                          ->constructors) {
          if (c->rank2sig != nullptr) {
            auto tau = rank2inst(c->rank2sig, context.get_exists(c));
            for (size_t i = 1; i < c->arg; i++) {
              auto t1 = new_fun_var(), t2 = new_forall_var();
              t1->tau.push_back(new_forall_var());
              t1->tau.push_back(t2);
              unify(t1, tau.second, &cerr);
              tau.second = t2;
            }
            auto fn = new_fun_var(), ret = new_forall_var();
            fn->tau.push_back(tau.second);
            fn->tau.push_back(ret);
            if (unify(fn, inst(gadt), nullptr)) {
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
                unify(inst(gadt), fn, &cerr);
                assert(false);
              }
            }
          } else {
            auto tau = inst(c->sig, context.get_exists(c));
            for (size_t i = 0; i < c->arg; i++) {
              auto t1 = new_fun_var(), t2 = new_forall_var();
              t1->tau.push_back(new_forall_var());
              t1->tau.push_back(t2);
              unify(t1, tau, &cerr);
              tau = t2;
            }
            auto fn = new_fun_var(), ret = new_forall_var();
            fn->tau.push_back(tau);
            fn->tau.push_back(ret);
            if (unify(fn, inst(gadt), nullptr)) {
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
                unify(inst(gadt), fn, &cerr);
                assert(false);
              }
            }
          }
        }
        infer(e->e);
        context.set_type(e, new_forall_var());
        auto fn = new_fun_var();
        fn->tau.push_back(context.get_type(e->e));
        fn->tau.push_back(context.get_type(e));
        unify(fn, inst(gadt), &cerr);
        break;
      }
      case ExprType::FFI: {
        for (auto fv : e->ffi->fv) {
          if (!context.has_poly(fv) && !context.has_rank2poly(fv)) {
            cerr << "type error: " << fv << " is not in context" << endl;
            exit(EXIT_FAILURE);
          }
        }
        context.set_type(e, new_forall_var());
        break;
      }
    }
    if (e->sig != nullptr) {
      set<shared_ptr<Mono>> st;
      unify(inst(e->sig), context.get_type(e), &cerr, &st);
    }
  }
};

#endif

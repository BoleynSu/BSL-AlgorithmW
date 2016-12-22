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
    map<string, set<shared_ptr<Mono>>> exists;
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
    void add_exists(string c, shared_ptr<Mono> t) { exists[c].insert(t); }
    set<shared_ptr<Mono>> &get_exists(string c) { return exists[c]; }
  } context;
  shared_ptr<Unit> unit;
  TypeInfer(shared_ptr<Unit> unit) : unit(unit) {
    for (auto dai : unit->data) {
      auto da = dai.second;
      for (auto &c : da->constructors) {
        if (c->rank2sig != nullptr) {
          auto p = c->rank2sig;
          set<shared_ptr<Mono>> st;
          while (p->is_forall) {
            st.insert(p->alpha);
            p = p->sigma;
          }
          check(da, c, get_mono(p->poly), st, false, false);
          auto mo = p->mono;
          check(da, c, p->mono, st, true, false);
        } else {
          auto p = c->sig;
          set<shared_ptr<Mono>> st;
          while (!p->is_mono) {
            st.insert(p->alpha);
            p = p->sigma;
          }
          check(da, c, p->tau, st, true, false);
        }
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
  void check(shared_ptr<Data> da, shared_ptr<Constructor> c, shared_ptr<Mono> p,
             set<shared_ptr<Mono>> &st, bool m, bool r) {
    if (is_c(p)) {
      if (is_fun(p)) {
        assert(p->tau.size() == 2);
        check(da, c, p->tau[0], st, m && false, r);
        check(da, c, p->tau[1], st, m && true, r);
      } else {
        if (unit->data.count(p->D)) {
          if (m) {
            if (p->D != da->name) {
              cerr << "type error: in constructor " << c->name << ":"
                   << (c->rank2sig != nullptr ? to_string(c->rank2sig)
                                              : to_string(c->sig))
                   << endl
                   << "return type is `" << p->D << "` instead of `" << da->name
                   << "`" << endl;
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
            check(da, c, p->tau[i], st, false, m || r);
          }
          if (m) {
            for (auto t : st) {
              assert(is_f(t));
              context.add_exists(c->name, t);
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
             << "return type is a type variable instead of " << da->name
             << endl;
        exit(EXIT_FAILURE);
      }
      if (r) {
        st.erase(p);
      }
    }
  }

  void check(shared_ptr<Poly> t, shared_ptr<Mono> p,
             set<shared_ptr<Mono>> &st) {
    if (is_c(p)) {
      if (is_fun(p)) {
        assert(p->tau.size() == 2);
        check(t, p->tau[0], st);
        check(t, p->tau[1], st);
      } else if (unit->data.count(p->D)) {
        if (p->tau.size() != unit->data[p->D]->arg) {
          cerr << "type error: in signature " << to_string(t) << endl
               << "type `" << p->D << "` expects " << unit->data[p->D]->arg
               << " arguments, but gets " << p->tau.size() << endl;
          exit(EXIT_FAILURE);
        }
        for (size_t i = 0; i < p->tau.size(); i++) {
          check(t, p->tau[i], st);
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
  void check(shared_ptr<Poly> t) {
    auto p = t;
    set<shared_ptr<Mono>> st;
    while (!p->is_mono) {
      st.insert(p->alpha);
      p = p->sigma;
    }
    check(t, p->tau, st);
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

  shared_ptr<Mono> infer(shared_ptr<Expr> e) {
    shared_ptr<Mono> ty;
    if (e->sig != nullptr) {
      check(e->sig);
    }
    switch (e->T) {
      case ExprType::VAR:
        if (context.has_rank2poly(e->x)) {
          cerr << "type error: " << e->x << " is of rank-2 type" << endl;
          exit(EXIT_FAILURE);
        } else if (context.has_poly(e->x)) {
          auto t = context.get_poly(e->x);
          ty = inst(t);
        } else {
          cerr << "type error: " << e->x << " is not in context" << endl;
          exit(EXIT_FAILURE);
        }
        break;
      case ExprType::APP: {
        shared_ptr<Mono> ty1, ty2;
        if (e->e1->T == ExprType::VAR && context.has_rank2poly(e->e1->x)) {
          ty2 = infer(e->e2);
          ty = new_forall_var();
          auto t = rank2inst(context.get_rank2poly(e->e1->x));
          set<shared_ptr<Mono>> st;
          unify(t.second, ty, &cerr);             // unify?
          unify(inst(t.first), ty2, &cerr, &st);  // unify?
        } else {
          ty1 = infer(e->e1);
          ty2 = infer(e->e2);
          ty = new_forall_var();
          auto t = new_fun_var();
          t->tau.push_back(ty2);
          t->tau.push_back(ty);
          if (!unify(ty1, t, &cerr)) {
            exit(EXIT_FAILURE);
          }
        }
        break;
      }
      case ExprType::ABS: {
        shared_ptr<Mono> ty_;
        auto tau = new_forall_var();
        context.set_poly(e->x, new_poly(tau));
        ty_ = infer(e->e);
        ty = new_fun_var();
        ty->tau.push_back(tau);
        ty->tau.push_back(ty_);
        context.unset(e->x);
        break;
      }
      case ExprType::LET: {
        shared_ptr<Mono> ty1, ty2;
        ty1 = infer(e->e1);
        if (e->e1->sig != nullptr) {
          context.set_poly(e->x, e->e1->sig);
        } else {
          context.set_poly(e->x, gen(ty1));
        }
        //      cerr << "//" << expr->x << " : " <<
        //      to_string(context.get_poly(expr->x))
        //           << endl;
        ty2 = infer(e->e2);
        ty = ty2;
        context.unset(e->x);
        break;
      }
      case ExprType::REC: {
        map<string, shared_ptr<Mono>> tys;
        shared_ptr<Mono> ty_;
        for (auto &xe : e->xes) {
          if (xe.second->sig != nullptr) {
            context.set_poly(xe.first, xe.second->sig);
          } else {
            context.set_poly(xe.first, new_poly(new_forall_var()));
          }
        }
        for (auto &xe : e->xes) {
          tys[xe.first] = infer(xe.second);
        }
        for (auto &xe : e->xes) {
          auto t = find(tys[xe.first]);
          if (xe.second->T != ExprType::ABS) {
            cerr << "type error: rec of this type is not supported" << endl;
            exit(EXIT_FAILURE);
          }
        }
        for (auto &xe : e->xes) {
          context.unset(xe.first);
        }
        for (auto &xe : e->xes) {
          context.set_poly(xe.first, gen(tys[xe.first]));
          //        cerr << "//" << xe.first << " : "
          //             << to_string(context.get_poly(xe.first))
          //             <<
          //             endl;
        }
        ty_ = infer(e->e);
        ty = ty_;
        for (auto &xe : e->xes) {
          context.unset(xe.first);
        }
        break;
      }
      case ExprType::CASE: {
        map<string, shared_ptr<Mono>> tys;
        shared_ptr<Mono> ty_;
        map<string, shared_ptr<Poly>> fns;
        for (auto &pes_ : e->pes) {
          auto pes = pes_.second;
          assert(unit->cons.count(pes_.first));
          auto c = unit->cons[pes_.first];
          assert(c->arg == pes.first.size());
          if (c->rank2sig != nullptr) {
            auto tau = rank2inst(c->rank2sig, context.get_exists(c->name));
            vector<shared_ptr<Poly>> taus;
            taus.push_back(tau.first);
            auto t = tau.second;
            while (is_fun(t)) {
              taus.push_back(new_poly(t->tau[0]));
              t = t->tau[1];
            }
            auto fn = new_fun_var();
            fn->tau.push_back(t);
            for (size_t i = 0; i < pes.first.size(); i++) {
              context.set_poly(pes.first[i], taus[i]);
            }
            tys[pes_.first] = infer(pes.second);
            for (size_t i = 0; i < pes.first.size(); i++) {
              context.unset(pes.first[i]);
            }
            fn->tau.push_back(tys[pes_.first]);
            fns[pes_.first] = gen(fn);
          } else {
            auto tau = inst(c->sig, context.get_exists(c->name));
            vector<shared_ptr<Poly>> taus;
            auto t = tau;
            while (is_fun(t)) {
              taus.push_back(new_poly(t->tau[0]));
              t = t->tau[1];
            }
            auto fn = new_fun_var();
            fn->tau.push_back(t);
            for (size_t i = 0; i < pes.first.size(); i++) {
              context.set_poly(pes.first[i], taus[i]);
            }
            tys[pes_.first] = infer(pes.second);
            for (size_t i = 0; i < pes.first.size(); i++) {
              context.unset(pes.first[i]);
            }
            fn->tau.push_back(tys[pes_.first]);
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
            unify(gadt_, inst(fn.second), &cerr);  // unify?
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
            auto tau = rank2inst(c->rank2sig, context.get_exists(c->name));
            for (size_t i = 1; i < c->arg; i++) {
              auto t1 = new_fun_var(), t2 = new_forall_var();
              t1->tau.push_back(new_forall_var());
              t1->tau.push_back(t2);
              unify(t1, tau.second, &cerr);  // unify?
              tau.second = t2;
            }
            auto fn = new_fun_var(), ret = new_forall_var();
            fn->tau.push_back(tau.second);
            fn->tau.push_back(ret);
            if (unify(fn, inst(gadt), nullptr)) {  // unify?
              if (fns.count(c->name)) {
                set<shared_ptr<Mono>> st;
                unify(fn, inst(fns[c->name]), &cerr, &st);  // unify?
              } else {
                cerr << "type error: non-exhaustive patterns `" << c->name
                     << "`" << endl;
                exit(EXIT_FAILURE);
              }
            } else {
              if (fns.count(c->name)) {
                unify(inst(gadt), fn, &cerr);  // unify?
                assert(false);
              }
            }
          } else {
            auto tau = inst(c->sig, context.get_exists(c->name));
            for (size_t i = 0; i < c->arg; i++) {
              auto t1 = new_fun_var(), t2 = new_forall_var();
              t1->tau.push_back(new_forall_var());
              t1->tau.push_back(t2);
              unify(t1, tau, &cerr);  // unify?
              tau = t2;
            }
            auto fn = new_fun_var(), ret = new_forall_var();
            fn->tau.push_back(tau);
            fn->tau.push_back(ret);
            if (unify(fn, inst(gadt), nullptr)) {  // unify?
              if (fns.count(c->name)) {
                set<shared_ptr<Mono>> st;
                unify(fn, inst(fns[c->name]), &cerr, &st);  // unify?
              } else {
                cerr << "type error: non-exhaustive patterns `" << c->name
                     << "`" << endl;
                exit(EXIT_FAILURE);
              }
            } else {
              if (fns.count(c->name)) {
                unify(inst(gadt), fn, &cerr);  // unify?
                assert(false);
              }
            }
          }
        }
        ty_ = infer(e->e);
        ty = new_forall_var();
        auto fn = new_fun_var();
        fn->tau.push_back(ty_);
        fn->tau.push_back(ty);
        unify(fn, inst(gadt), &cerr);
        break;
      }
      case ExprType::FFI: {
        size_t idx = 0;
        while (idx < e->ffi->source.length() &&
               (idx = e->ffi->source.find("$", idx)) != string::npos) {
          if (++idx < e->ffi->source.length()) {
            char c = e->ffi->source[idx];
            if (('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z') || c == '_') {
              string v;
              v.push_back(c);
              idx++;
              while (idx < e->ffi->source.length()) {
                c = e->ffi->source[idx];
                if (!(('0' <= c && c <= '9') || ('A' <= c && c <= 'Z') ||
                      ('a' <= c && c <= 'z') || c == '_' || c == '\'')) {
                  break;
                }
                v.push_back(c);
                idx++;
              }
              if (!context.has_poly(v) && !context.has_rank2poly(v)) {
                cerr << "type error: " << v << " is not in context" << endl;
                exit(EXIT_FAILURE);
              }
            } else {
              assert(false);
            }
          } else {
            assert(false);
          }
        }
        ty = new_forall_var();
        break;
      }
    }
    if (e->sig != nullptr) {
      set<shared_ptr<Mono>> st;
      unify(inst(e->sig), ty, &cerr, &st);
    }
    return ty;
  }
};

#endif

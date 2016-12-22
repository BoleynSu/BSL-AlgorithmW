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
    map<string, vector<shared_ptr<Poly>>> c;
    map<string, set<shared_ptr<Mono>>> exists;
    bool has_env(const string &key) {
      auto it = c.find(key);
      if (it != c.end() && it->second.size()) {
        return true;
      } else {
        return false;
      }
    }
    shared_ptr<Poly> get_env(const string &key) {
      auto it = c.find(key);
      if (it != c.end() && it->second.size()) {
        return it->second.back();
      } else {
        return nullptr;
      }
    }
    void set_env(const string &key, shared_ptr<Poly> value) {
      c[key].push_back(value);
    }
    void unset_env(const string &key) {
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
        auto p = c->sig;
        set<shared_ptr<Mono>> st;
        while (!p->is_mono) {
          st.insert(p->alpha);
          p = p->sigma;
        }
        check(da, c, p->tau, st, true, false);
      }
    }
    for (auto dai : unit->data) {
      auto da = dai.second;
      for (auto &c : da->constructors) {
        //            cerr << "//" << c->name << " : " << to_string(c->sig)
        //            << endl;
        context.set_env(c->name, c->sig);
      }
    }
    infer(unit->expr, nullptr);
    for (auto dai : unit->data) {
      auto da = dai.second;
      for (auto &c : da->constructors) {
        context.unset_env(c->name);
      }
    }
  }
  void check(shared_ptr<Data> da, shared_ptr<Constructor> c, shared_ptr<Mono> p,
             set<shared_ptr<Mono>> &st, bool m, bool r) {
    if (is_c(p)) {
      if (is_p(p)) {
        check(c->sig, p->sigma, st);
      } else {
        if (is_fun(p)) {
          assert(p->tau.size() == 2);
          check(da, c, p->tau[0], st, m && false, r);
          check(da, c, p->tau[1], st, m && true, r);
        } else {
          if (unit->data.count(p->D)) {
            if (m) {
              if (p->D != da->name) {
                cerr << "type error: in constructor " << c->name << ":"
                     << to_string(c->sig) << endl
                     << "return type is `" << p->D << "` instead of `"
                     << da->name << "`" << endl;
                exit(EXIT_FAILURE);
              }
            }
            if (p->tau.size() != unit->data[p->D]->arg) {
              cerr << "type error: in constructor " << c->name << ":"
                   << to_string(c->sig) << endl
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
                 << to_string(c->sig) << endl
                 << "`" << p->D << "` is not a type" << endl;
            exit(EXIT_FAILURE);
          }
        }
      }
    } else {
      if (m) {
        cerr << "type error: in constructor " << c->name << ":"
             << to_string(c->sig) << endl
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
      if (is_p(p)) {
        check(t, p->sigma, st);
      } else {
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
      }
    } else {
      st.erase(p);
    }
  }
  void check(shared_ptr<Poly> t, shared_ptr<Poly> p,
             set<shared_ptr<Mono>> &st) {
    while (!p->is_mono) {
      st.insert(p->alpha);
      p = p->sigma;
    }
    check(t, p->tau, st);
  }
  void check(shared_ptr<Poly> t) {
    set<shared_ptr<Mono>> st;
    check(t, t, st);
  }

  shared_ptr<Poly> gen(shared_ptr<Mono> tau) {
    tau = find(tau);
    set<shared_ptr<Mono>> f;
    for (auto &c : context.c) {
      set<shared_ptr<Mono>> fi;
      if (context.has_env(c.first)) {
        ftv(fi, context.get_env(c.first));
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
    auto g = new_poly(inst_with_exists(tau, m));
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
      if (is_p(b)) {
        return occ(a, b->sigma);
      } else {
        for (size_t i = 0; i < b->tau.size(); i++) {
          if (occ(a, b->tau[i])) {
            return true;
          }
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
    if (a != b) {
      if (is_c(a)) {
        if (is_c(b)) {
          if (is_p(a) && is_p(b)) {
            if (st != nullptr) {
              if (!unify(inst_get_set(a->sigma, *st), inst(b->sigma), cerr,
                         st)) {
                return false;
              } else {
                return true;
              }
            } else {
              set<shared_ptr<Mono>> sta, stb;
              if (!unify(inst_get_set(a->sigma, sta), inst(b->sigma), cerr,
                         &sta) ||
                  !unify(inst_get_set(b->sigma, stb), inst(a->sigma), cerr,
                         &stb)) {
                return false;
              } else {
                return true;
              }
            }
          } else if (is_p(a)) {
            if (st != nullptr) {
              if (cerr != nullptr) {
                (*cerr) << "type error: " << to_string(a) << " !< "
                        << to_string(b) << endl;
              }
              return false;
            } else {
              if (!unify(inst_get_set(a->sigma, *st), b, cerr, st)) {
                return false;
              } else {
                return true;
              }
            }
          } else if (is_p(b)) {
            if (!unify(a, inst(b->sigma), cerr, st)) {
              return false;
            } else {
              return true;
            }
          } else {
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
              }
              return false;
            }
          }
        } else if (is_f(b)) {
          if (occ(b, a)) {
            if (cerr != nullptr) {
              (*cerr) << "type error: " << to_string(a) << " ~ " << to_string(b)
                      << endl;
            }
            return false;
          } else {
            if (is_p(a)) {
              b->par = a;
              return true;
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
            if (st != nullptr) {
              if (cerr != nullptr) {
                (*cerr) << "type error: " << to_string(a) << " !< "
                        << to_string(b) << endl;
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
          }
          return false;
        } else if (is_f(b)) {
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

  shared_ptr<Mono> infer(shared_ptr<Expr> e, shared_ptr<Poly> sig) {
    shared_ptr<Mono> ty;
    if (e->sig != nullptr) {
      check(e->sig);
      sig = e->sig;
    }
    switch (e->T) {
      case ExprType::VAR:
        if (context.has_env(e->x)) {
          auto t = context.get_env(e->x);
          ty = inst(t);
        } else {
          cerr << "type error: " << e->x << " is not in context" << endl;
          exit(EXIT_FAILURE);
        }
        break;
      case ExprType::APP: {
        shared_ptr<Mono> ty1, ty2;
        ty1 = infer(e->e1, nullptr);
        ty2 = infer(e->e2, nullptr);
        ty = new_forall_var();
        if (is_fun(find(ty1)) && is_p(find(find(ty1)->tau[0]))) {
          set<shared_ptr<Mono>> st;
          unify(find(ty1)->tau[1], ty, &cerr);
          unify(inst(find(find(ty1)->tau[0])->sigma), ty2, &cerr, &st);
        } else {
          auto t = new_fun();
          t->tau.push_back(ty2);
          t->tau.push_back(ty);
          if (!unify(ty1, t, &cerr)) {
            string data = to_string(e, 0, "  ");
            if (data.length() > 78) {
              data = data.substr(0, 75) + "...";
            }
            cerr << "`" << data << "`" << endl;
            exit(EXIT_FAILURE);
          }
        }
        break;
      }
      case ExprType::ABS: {
        shared_ptr<Mono> ty_;
        if (sig != nullptr) {
          ty = inst(sig);
          if (is_fun(ty)) {
            if (is_p(ty->tau[0])) {
              context.set_env(e->x, ty->tau[0]->sigma);
            } else {
              context.set_env(e->x, new_poly(ty->tau[0]));
            }
            if (is_p(ty->tau[1])) {
              ty_ = infer(e->e, ty->tau[1]->sigma);
            } else {
              ty_ = infer(e->e, new_poly(ty->tau[1]));
            }
            context.unset_env(e->x);
          } else {
          }
        } else {
          auto tau = new_forall_var();
          context.set_env(e->x, new_poly(tau));
          ty_ = infer(e->e, nullptr);
          ty = new_fun();
          ty->tau.push_back(tau);
          ty->tau.push_back(ty_);
          context.unset_env(e->x);
        }
        break;
      }
      case ExprType::LET: {
        shared_ptr<Mono> ty1, ty2;
        ty1 = infer(e->e1, nullptr);
        if (e->e1->sig != nullptr) {
          context.set_env(e->x, e->e1->sig);
        } else {
          context.set_env(e->x, gen(ty1));
        }
        //        cerr << "//" << e->x << " : "
        //             << (e->e1->sig != nullptr ? to_string(e->e1->sig)
        //                                       : to_string(gen(ty1)))
        //             << endl;
        ty2 = infer(e->e2, sig);
        ty = ty2;
        context.unset_env(e->x);
        break;
      }
      case ExprType::REC: {
        map<string, shared_ptr<Mono>> tys;
        shared_ptr<Mono> ty_;
        for (auto &xe : e->xes) {
          if (xe.second->sig != nullptr) {
            context.set_env(xe.first, xe.second->sig);
          } else {
            tys[xe.first] = new_forall_var();
            context.set_env(xe.first, new_poly(tys[xe.first]));
          }
        }
        for (auto &xe : e->xes) {
          infer(xe.second, nullptr);
        }
        for (auto &xe : e->xes) {
          auto t = xe.second->sig != nullptr ? inst(xe.second->sig)
                                             : find(tys[xe.first]);
          if (xe.second->T != ExprType::ABS) {
            cerr << "type error: rec of this type is not supported" << endl;
            exit(EXIT_FAILURE);
          }
          //          cerr << "//" << xe.first << " : "
          //               << (xe.second->sig != nullptr ?
          //               to_string(xe.second->sig)
          //                                             :
          //                                             to_string(tys[xe.first]))
          //               << endl;
        }
        for (auto &xe : e->xes) {
          context.unset_env(xe.first);
        }
        for (auto &xe : e->xes) {
          if (xe.second->sig != nullptr) {
            context.set_env(xe.first, xe.second->sig);
          } else {
            context.set_env(xe.first, gen(tys[xe.first]));
          }
        }
        ty_ = infer(e->e, sig);
        ty = ty_;
        for (auto &xe : e->xes) {
          context.unset_env(xe.first);
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
          auto tau = inst_with_exists(c->sig, context.get_exists(c->name));
          vector<shared_ptr<Mono>> taus;
          auto t = tau;
          while (is_fun(t)) {
            taus.push_back(t->tau[0]);
            t = t->tau[1];
          }
          auto fn = new_fun();
          fn->tau.push_back(t);
          for (size_t i = 0; i < c->arg; i++) {
            if (!is_p(taus[i])) {
              context.set_env(pes.first[i], new_poly(taus[i]));
            } else {
              context.set_env(pes.first[i], taus[i]->sigma);
            }
          }
          tys[pes_.first] = infer(pes.second, nullptr);
          for (size_t i = 0; i < c->arg; i++) {
            context.unset_env(pes.first[i]);
          }
          fn->tau.push_back(tys[pes_.first]);
          fns[pes_.first] = gen(fn);
        }
        auto gadt = e->gadt;
        if (gadt != nullptr) {
          auto t = find(get_mono(gadt));
          if (!(is_fun(t) && is_c(find(t->tau[0])) && !is_p(find(t->tau[0])) &&
                unit->data.count(find(t->tau[0])->D))) {
            cerr << "type error: invaliad signature for case expression" << endl
                 << to_string(gadt) << endl;
            exit(EXIT_FAILURE);
          }
        } else {
          auto gadt_ = new_forall_var();
          for (auto &fn : fns) {
            if (!unify(gadt_, inst(fn.second), &cerr)) {
              string data = to_string(e, 0, "  ");
              if (data.length() > 78) {
                data = data.substr(0, 75) + "...";
              }
              cerr << "`" << data << "`" << endl;
              exit(EXIT_FAILURE);
            }
          }
          gadt = gen(gadt_);
          auto t = find(get_mono(gadt));
          assert(is_fun(t) && is_c(find(t->tau[0])) && !is_p(find(t->tau[0])) &&
                 unit->data.count(find(t->tau[0])->D));
        }
        //        for (auto &fn : fns) {
        //          cerr << "//case " << fn.first << " : " <<
        //          to_string(fn.second)
        //               << endl;
        //        }
        //        cerr << "//: " << to_string(gadt) << endl;
        for (auto c : unit->data[unit->cons[fns.begin()->first]->data_name]
                          ->constructors) {
          auto tau = inst_with_exists(c->sig, context.get_exists(c->name));
          auto t = tau;
          while (is_fun(t)) {
            t = t->tau[1];
          }
          auto fn = new_fun(), ret = new_forall_var();
          fn->tau.push_back(t);
          fn->tau.push_back(ret);
          if (unify(inst(gadt), fn, nullptr)) {
            if (fns.count(c->name)) {
              set<shared_ptr<Mono>> st;
              if (!unify(fn, inst_get_set(fns[c->name], st), &cerr, &st)) {
                string data = to_string(e, 0, "  ");
                if (data.length() > 78) {
                  data = data.substr(0, 75) + "...";
                }
                cerr << "`" << data << "`" << endl;
                exit(EXIT_FAILURE);
              }
            } else {
              cerr << "type error: non-exhaustive patterns `" << c->name << "`"
                   << endl;
              string data = to_string(e, 0, "  ");
              if (data.length() > 78) {
                data = data.substr(0, 75) + "...";
              }
              cerr << "`" << data << "`" << endl;
              exit(EXIT_FAILURE);
            }
          } else {
            if (fns.count(c->name)) {
              unify(inst(gadt), fn, &cerr);
              string data = to_string(e, 0, "  ");
              if (data.length() > 78) {
                data = data.substr(0, 75) + "...";
              }
              cerr << "`" << data << "`" << endl;
              exit(EXIT_FAILURE);
            }
          }
        }
        ty_ = infer(e->e, nullptr);
        ty = new_forall_var();
        auto fn = new_fun();
        fn->tau.push_back(ty_);
        fn->tau.push_back(ty);
        if (!unify(fn, inst(gadt), &cerr)) {
          string data = to_string(e, 0, "  ");
          if (data.length() > 78) {
            data = data.substr(0, 75) + "...";
          }
          cerr << "`" << data << "`" << endl;
          exit(EXIT_FAILURE);
        }
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
              if (!context.has_env(v)) {
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
    if (sig != nullptr) {
      set<shared_ptr<Mono>> st;
      if (!unify(inst_get_set(sig, st), ty, &cerr, &st)) {
        string data = to_string(e, 0, "  ");
        if (data.length() > 78) {
          data = data.substr(0, 75) + "...";
        }
        cerr << "`" << data << "`" << endl;
        exit(EXIT_FAILURE);
      }
    }
    return ty;
  }
};

#endif

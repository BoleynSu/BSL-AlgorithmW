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
    map<string, vector<shared_ptr<Poly>>> type;
    map<string, shared_ptr<Kind>> kind;
    map<string, set<shared_ptr<Mono>>> exists;
    bool has_type_env(const string &key) {
      auto it = type.find(key);
      if (it != type.end() && it->second.size()) {
        return true;
      } else {
        return false;
      }
    }
    shared_ptr<Poly> get_type_env(const string &key) {
      auto it = type.find(key);
      if (it != type.end() && it->second.size()) {
        return it->second.back();
      } else {
        assert(false);
      }
    }
    void set_type_env(const string &key, shared_ptr<Poly> value) {
      type[key].push_back(value);
    }
    void unset_type_env(const string &key) {
      auto &v = type[key];
      v.pop_back();
      if (v.empty()) {
        type.erase(key);
      }
    }
    void add_exists(string c, shared_ptr<Mono> t) { exists[c].insert(t); }
    set<shared_ptr<Mono>> &get_exists(string c) { return exists[c]; }
  } context;
  shared_ptr<Unit> unit;
  TypeInfer(shared_ptr<Unit> unit) : unit(unit) {
    context.kind["->"] = new_kind(new_const_kind(), new_const_kind());
    for (auto dai : unit->data) {
      auto da = dai.second;
      context.kind[da->name] = new_kind();
      auto k = context.kind[da->name];
      for (size_t i = 0; i < da->arg; i++) {
        auto ret = new_kind();
        unify(k, new_kind(new_kind(), ret), nullptr);
        k = ret;
      }
      unify(k, new_const_kind(), nullptr);
    }
    for (auto dai : unit->data) {
      auto da = dai.second;
      for (auto &c : da->constructors) {
        check(da, c);
      }
      //      cerr << da->name << ":" << to_string(context.kind[da->name]) <<
      //      endl;
    }
    for (auto dai : unit->data) {
      auto da = dai.second;
      for (auto &c : da->constructors) {
        cerr << c->name << " : " << to_string(c->sig) << endl;
        context.set_type_env(c->name, c->sig);
      }
    }
    infer(unit->expr, nullptr);
    for (auto dai : unit->data) {
      auto da = dai.second;
      for (auto &c : da->constructors) {
        context.unset_type_env(c->name);
      }
    }
  }
  void check(shared_ptr<Data> da, shared_ptr<Constructor> c, shared_ptr<Mono> p,
             set<shared_ptr<Mono>> &st) {
    if (is_fun(p)) {
      assert(p->tau.size() == 2);
      if (is_p(p->tau[1])) {
        cerr << "type error: a `forall` can be moved to left hand of `->`"
             << endl
             << "in constructor " << c->name << ":" << to_string(c->sig)
             << endl;
        exit(EXIT_FAILURE);
      }
      check(da, c, p->tau[1], st);
      if (!unify(p->tau[1]->kind, new_const_kind(), &cerr)) {
        cerr << "in constructor " << c->name << ":" << to_string(c->sig)
             << endl;
        exit(EXIT_FAILURE);
      }
      check(c->sig, p->tau[0], st);
      if (!unify(p->tau[0]->kind, new_const_kind(), &cerr)) {
        cerr << "in constructor " << c->name << ":" << to_string(c->sig)
             << endl;
        exit(EXIT_FAILURE);
      }
      p->kind = new_const_kind();
    } else {
      if (!is_cd(p) || p->D.D != da->name) {
        cerr << "in constructor " << c->name << ":" << to_string(c->sig) << endl
             << "return type is not `" << da->name << "`" << endl;
        exit(EXIT_FAILURE);
      }
      check(c->sig, p, st);
      for (auto t : st) {
        assert(is_f(t));
        context.add_exists(c->name, t);
      }
    }
  }
  void check(shared_ptr<Data> da, shared_ptr<Constructor> c) {
    auto p = c->sig;
    set<shared_ptr<Mono>> st;
    while (!p->is_mono) {
      st.insert(p->alpha);
      p = p->sigma;
    }
    check(da, c, p->tau, st);
  }
  bool occ(shared_ptr<Kind> a, shared_ptr<Kind> b) {
    b = find(b);
    if (b->is_const) {
      if (b->is_arrow) {
        return occ(a, b->left) || occ(a, b->right);
      } else {
        return false;
      }
    } else {
      return a == b;
    }
  }
  bool unify(shared_ptr<Kind> a, shared_ptr<Kind> b, ostream *cerr) {
    a = find(a);
    b = find(b);
    if (a != b) {
      if (a->is_const && b->is_const) {
        if (a->is_arrow && b->is_arrow) {
          return unify(a->left, b->left, cerr) &&
                 unify(a->right, b->right, cerr);
        } else if (!a->is_arrow && !b->is_arrow) {
          if (a->k != b->k) {
            if (cerr != nullptr) {
              (*cerr) << "kind error: " << to_string(a) << " ~ " << to_string(b)
                      << endl;
            }
            return false;
          } else {
            return true;
          }
        } else {
          if (cerr != nullptr) {
            (*cerr) << "kind error: " << to_string(a) << " != " << to_string(b)
                    << endl;
          }
          return false;
        }
      } else if (b->is_const) {
        if (occ(a, b)) {
          if (cerr != nullptr) {
            (*cerr) << "kind error: " << to_string(a) << " ~ " << to_string(b)
                    << endl;
          }
          return false;
        } else {
          a->par = b;
          return true;
        }
      } else if (a->is_const) {
        if (occ(b, a)) {
          if (cerr != nullptr) {
            (*cerr) << "kind error: " << to_string(a) << " ~ " << to_string(b)
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
      return true;
    }
  }
  void check(shared_ptr<Poly> t, shared_ptr<Mono> p,
             set<shared_ptr<Mono>> &st) {
    if (is_c(p)) {
      if (is_p(p)) {
        check(t, p->sigma, st);
        p->kind = get_mono(p->sigma)->kind;
      } else {
        if (is_cd(p)) {
          if (is_fun(p)) {
            assert(p->tau.size() == 2);
            check(t, p->tau[0], st);
            if (!unify(p->tau[0]->kind, new_const_kind(), &cerr)) {
              cerr << "in signature " << to_string(t) << endl;
              exit(EXIT_FAILURE);
            }
            if (is_p(p->tau[1])) {
              cerr << "type error: a `forall` can be moved to left hand of `->`"
                   << endl
                   << "in signature " << to_string(t) << endl;
              exit(EXIT_FAILURE);
            }
            check(t, p->tau[1], st);
            if (!unify(p->tau[1]->kind, new_const_kind(), &cerr)) {
              cerr << "in signature " << to_string(t) << endl;
              exit(EXIT_FAILURE);
            }
            p->kind = new_const_kind();
          } else {
            if (unit->data.count(p->D.D)) {
              auto k = context.kind[p->D.D];
              for (size_t i = 0; i < p->tau.size(); i++) {
                auto ret = new_kind();
                check(t, p->tau[i], st);
                if (!unify(new_kind(p->tau[i]->kind, ret), k, &cerr)) {
                  cerr << "in signature " << to_string(t) << endl;
                  exit(EXIT_FAILURE);
                }
                k = ret;
              }
              if (!unify(k, p->kind, &cerr)) {
                cerr << "in signature " << to_string(t) << endl;
                exit(EXIT_FAILURE);
              }
            } else {
              cerr << "in signature " << to_string(t) << endl
                   << "`" << p->D.D << "` is not a type" << endl;
              exit(EXIT_FAILURE);
            }
          }
        } else {
          auto k = p->D.d->kind;
          for (size_t i = 0; i < p->tau.size(); i++) {
            auto ret = new_kind();
            check(t, p->tau[i], st);
            if (!unify(new_kind(p->tau[i]->kind, ret), k, &cerr)) {
              cerr << "in signature " << to_string(t) << endl;
              exit(EXIT_FAILURE);
            }
            k = ret;
          }
          if (!unify(k, p->kind, &cerr)) {
            cerr << "in signature " << to_string(t) << endl;
            exit(EXIT_FAILURE);
          }
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
    for (auto &c : context.type) {
      set<shared_ptr<Mono>> fi;
      assert(context.has_type_env(c.first));
      ftv(fi, context.get_type_env(c.first));
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
        m[f] = new_forall_var(f->kind);
      }
    }
    auto g = new_poly(inst(tau, m));
    for (auto f : m) {
      if (is_f(f.second)) {
        g = new_poly(f.second, g);
      }
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
        if (!is_cd(b)) {
          if (occ(a, b->D.d)) {
            return true;
          }
        }
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
                (*cerr) << "type error: " << to_string(a)
                        << " /= " << to_string(b) << endl;
              }
              return false;
            } else {
              set<shared_ptr<Mono>> sta;
              if (!unify(inst_get_set(a->sigma, sta), b, cerr, &sta)) {
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
            if (!is_cd(a) && is_cd(find(a->D.d))) {
              a->D.d = find(a->D.d);
              auto na = new_const(a->D.d->D.D, a->kind);
              for (size_t i = 0; i < a->D.d->tau.size(); i++) {
                na->tau.push_back(a->D.d->tau[i]);
              }
              for (size_t i = 0; i < a->tau.size(); i++) {
                na->tau.push_back(a->tau[i]);
              }
              return unify(na, b, cerr, st);
            } else if (!is_cd(b) && is_cd(find(b->D.d))) {
              b->D.d = find(b->D.d);
              auto nb = new_const(b->D.d->D.D, b->kind);
              for (size_t i = 0; i < b->D.d->tau.size(); i++) {
                nb->tau.push_back(b->D.d->tau[i]);
              }
              for (size_t i = 0; i < b->tau.size(); i++) {
                nb->tau.push_back(b->tau[i]);
              }
              return unify(a, nb, cerr, st);
            } else if (a->tau.size() == b->tau.size()) {
              if (is_cd(a) && is_cd(b)) {
                if (a->D.D != b->D.D) {
                  if (cerr != nullptr) {
                    (*cerr) << "type error: " << to_string(a)
                            << " /= " << to_string(b) << endl;
                  }
                  return false;
                }
              } else if (is_cd(a)) {
                b->D.d = find(b->D.d);
                if (st != nullptr && st->count(b->D.d)) {
                  if (cerr != nullptr) {
                    (*cerr)
                        << "type error: " << to_string(b->D.d) << " !< "
                        << to_string(new_const(a->D.D, context.kind[a->D.D]))
                        << endl;
                  }
                  return false;
                } else {
                  b->D.d->par = new_const(a->D.D, context.kind[a->D.D]);
                  b->D.is_const = true;
                  b->D.D = a->D.D;
                }
              } else if (is_cd(b)) {
                a->D.d = find(a->D.d);
                if (st != nullptr && st->count(a->D.d)) {
                  if (cerr != nullptr) {
                    (*cerr)
                        << "type error: " << to_string(a->D.d) << " !< "
                        << to_string(new_const(b->D.D, context.kind[b->D.D]))
                        << endl;
                  }
                  return false;
                } else {
                  a->D.d->par = new_const(b->D.D, context.kind[b->D.D]);
                  a->D.is_const = true;
                  a->D.D = b->D.D;
                }
              } else {
                a->D.d = find(a->D.d);
                b->D.d = find(b->D.d);
                if (a->D.d != b->D.d) {
                  if (st != nullptr && st->count(b->D.d)) {
                    if (st != nullptr && st->count(a->D.d)) {
                      if (cerr != nullptr) {
                        (*cerr) << "type error: " << to_string(a)
                                << " != " << to_string(b) << endl;
                      }
                      return false;
                    } else {
                      a->D.d->par = b->D.d;
                    }
                  } else {
                    b->D.d->par = a->D.d;
                  }
                }
              }
              for (size_t i = 0; i < a->tau.size(); i++) {
                if (!unify(a->tau[i], b->tau[i], cerr, st)) {
                  return false;
                }
              }
              return true;
            } else {
              if (a->tau.size() > b->tau.size()) {
                if (is_cd(b)) {
                  if (cerr != nullptr) {
                    (*cerr) << "type error: " << to_string(a)
                            << " /= " << to_string(b) << endl;
                  }
                  return false;
                }
                b->D.d = find(b->D.d);
                shared_ptr<Kind> k;
                shared_ptr<Mono> h, hnt;
                if (is_cd(a)) {
                  k = context.kind[a->D.D];
                  h = new_const(a->D.D, k);
                  hnt = new_const(a->D.D, k);
                } else {
                  a->D.d = find(a->D.d);
                  k = a->D.d->kind;
                  h = new_const(a->D.d, k);
                  hnt = new_const(a->D.d, k);
                }
                for (size_t i = 0; i + b->tau.size() < a->tau.size(); i++) {
                  h->tau.push_back(a->tau[i]);
                  hnt->tau.push_back(a->tau[i]);
                  auto t = new_kind();
                  if (!unify(k, new_kind(a->tau[i]->kind, t), cerr)) {
                    return false;
                  }
                  k = t;
                }
                if (st != nullptr && st->count(b->D.d)) {
                  if (cerr != nullptr) {
                    (*cerr) << "type error: " << to_string(b->D.d) << " !< "
                            << to_string(h) << endl;
                  }
                  return false;
                } else {
                  b->D.d->par = h;
                  if (!unify(k, b->D.d->kind, cerr)) {
                    return false;
                  }
                  for (size_t i = 0; i < b->tau.size(); i++) {
                    hnt->tau.push_back(
                        a->tau[a->tau.size() - b->tau.size() + i]);
                    if (!unify(a->tau[a->tau.size() - b->tau.size() + i],
                               b->tau[i], cerr, st)) {
                      return false;
                    }
                  }
                  b.swap(hnt);
                  return true;
                }
              } else {
                if (is_cd(a)) {
                  if (cerr != nullptr) {
                    (*cerr) << "type error: " << to_string(a)
                            << " /= " << to_string(b) << endl;
                  }
                  return false;
                }
                a->D.d = find(a->D.d);
                shared_ptr<Kind> k;
                shared_ptr<Mono> h, hnt;
                if (is_cd(b)) {
                  k = context.kind[b->D.D];
                  h = new_const(b->D.D, k);
                  hnt = new_const(b->D.D, k);
                } else {
                  b->D.d = find(b->D.d);
                  k = b->D.d->kind;
                  h = new_const(b->D.d, k);
                  hnt = new_const(b->D.d, k);
                }
                for (size_t i = 0; i + a->tau.size() < b->tau.size(); i++) {
                  h->tau.push_back(b->tau[i]);
                  hnt->tau.push_back(b->tau[i]);
                  auto t = new_kind();
                  if (!unify(k, new_kind(b->tau[i]->kind, t), cerr)) {
                    return false;
                  }
                  k = t;
                }
                if (st != nullptr && !st->count(a->D.d)) {
                  if (cerr != nullptr) {
                    (*cerr) << "type error: " << to_string(a->D.d) << " !< "
                            << to_string(h) << endl;
                  }
                  return false;
                } else {
                  a->D.d->par = h;
                  if (!unify(k, a->D.d->kind, cerr)) {
                    return false;
                  }
                  for (size_t i = 0; i < a->tau.size(); i++) {
                    hnt->tau.push_back(
                        b->tau[b->tau.size() - a->tau.size() + i]);
                    if (!unify(a->tau[i],
                               b->tau[b->tau.size() - a->tau.size() + i], cerr,
                               st)) {
                      return false;
                    }
                  }
                  a.swap(hnt);
                  return true;
                }
              }
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
              if (!unify(a->kind, b->kind, cerr)) {
                return false;
              } else {
                b->par = a;
                return true;
              }
            } else {
              if (!unify(a->kind, b->kind, cerr)) {
                return false;
              } else {
                if (st != nullptr && st->count(b)) {
                  if (cerr != nullptr) {
                    (*cerr) << "type error: " << to_string(b) << " !< "
                            << to_string(a) << endl;
                  }
                  return false;
                } else {
                  b->par = a;
                  return true;
                }
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
            if (!unify(a->kind, b->kind, cerr)) {
              return false;
            } else {
              if (st != nullptr && st->count(a)) {
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
          }
        } else if (is_f(b)) {
          if (!unify(a->kind, b->kind, cerr)) {
            return false;
          } else {
            if (st != nullptr && st->count(b)) {
              if (st != nullptr && st->count(a)) {
                if (cerr != nullptr) {
                  (*cerr) << "type error: " << to_string(a)
                          << " != " << to_string(b) << endl;
                }
                return false;
              } else {
                a->par = b;
                return true;
              }
            } else {
              b->par = a;
              return true;
            }
          }
        } else {
          if (!unify(a->kind, b->kind, cerr)) {
            return false;
          } else {
            if (st != nullptr && st->count(a)) {
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
        }
      } else {
        if (is_c(b)) {
          if (cerr != nullptr) {
            (*cerr) << "type error: " << to_string(a) << " /= " << to_string(b)
                    << endl;
          }
          return false;
        } else if (is_f(b)) {
          if (!unify(a->kind, b->kind, cerr)) {
            return false;
          } else {
            if (st != nullptr && st->count(b)) {
              if (cerr != nullptr) {
                (*cerr) << "type error: " << to_string(b) << " !< "
                        << to_string(a) << endl;
              }
              return false;
            } else {
              b->par = a;
              return true;
            }
          }
        } else {
          if (!unify(a->kind, b->kind, cerr)) {
            return false;
          } else {
            if (st != nullptr && st->count(b)) {
              if (st != nullptr && st->count(a)) {
                if (cerr != nullptr) {
                  (*cerr) << "type error: " << to_string(a)
                          << " != " << to_string(b) << endl;
                }
                return false;
              } else {
                a->par = b;
                return true;
              }
            } else {
              b->par = a;
              return true;
            }
          }
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
      if (sig != nullptr) {
        cerr << "type error: the signature cannot be here" << endl;
        string data = to_string(e, 0, "  ");
        if (data.length() > 78) {
          data = data.substr(0, 75) + "...";
        }
        cerr << "`" << data << "`" << endl;
        exit(EXIT_FAILURE);
      }
      sig = e->sig;
    }
    switch (e->T) {
      case ExprType::VAR:
        if (context.has_type_env(e->x)) {
          auto t = context.get_type_env(e->x);
          ty = inst(t);
        } else {
          cerr << "type error: " << e->x << " is not in context" << endl;
          string data = to_string(e, 0, "  ");
          if (data.length() > 78) {
            data = data.substr(0, 75) + "...";
          }
          cerr << "`" << data << "`" << endl;
          exit(EXIT_FAILURE);
        }
        break;
      case ExprType::APP: {
        shared_ptr<Mono> ty1, ty2;
        ty1 = infer(e->e1, nullptr);
        ty2 = infer(e->e2, nullptr);
        ty = new_forall_var(new_const_kind());
        if (is_fun(find(ty1)) && is_p(find(find(ty1)->tau[0]))) {
          set<shared_ptr<Mono>> st;
          if (!unify(find(ty1)->tau[1], ty, &cerr)) {
            string data = to_string(e, 0, "  ");
            if (data.length() > 78) {
              data = data.substr(0, 75) + "...";
            }
            cerr << "`" << data << "`" << endl;
            exit(EXIT_FAILURE);
          }
          if (!unify(inst_get_set(find(find(ty1)->tau[0])->sigma, st), ty2,
                     &cerr, &st)) {
            string data = to_string(e, 0, "  ");
            if (data.length() > 78) {
              data = data.substr(0, 75) + "...";
            }
            cerr << "`" << data << "`" << endl;
            exit(EXIT_FAILURE);
          }
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
              context.set_type_env(e->x, ty->tau[0]->sigma);
            } else {
              context.set_type_env(e->x, new_poly(ty->tau[0]));
            }
            if (is_p(ty->tau[1])) {
              ty_ = infer(e->e, ty->tau[1]->sigma);
            } else {
              ty_ = infer(e->e, new_poly(ty->tau[1]));
            }
            context.unset_type_env(e->x);
          } else {
            cerr << "type error: `" << to_string(ty)
                 << "` is not of a function type" << endl;
            string data = to_string(e, 0, "  ");
            if (data.length() > 78) {
              data = data.substr(0, 75) + "...";
            }
            cerr << "`" << data << "`" << endl;
            exit(EXIT_FAILURE);
          }
        } else {
          auto tau = new_forall_var(new_const_kind());
          context.set_type_env(e->x, new_poly(tau));
          ty_ = infer(e->e, nullptr);
          context.unset_type_env(e->x);
          ty = new_fun();
          ty->tau.push_back(tau);
          ty->tau.push_back(ty_);
        }
        break;
      }
      case ExprType::LET: {
        shared_ptr<Mono> ty1, ty2;
        ty1 = infer(e->e1, nullptr);
        if (e->e1->sig != nullptr) {
          context.set_type_env(e->x, e->e1->sig);
        } else {
          context.set_type_env(e->x, gen(ty1));
        }
        cerr << e->x << " : " << (e->e1->sig != nullptr ? to_string(e->e1->sig)
                                                        : to_string(gen(ty1)))
             << endl;
        ty2 = infer(e->e2, sig);
        context.unset_type_env(e->x);
        ty = ty2;
        break;
      }
      case ExprType::REC: {
        map<string, shared_ptr<Mono>> tys;
        shared_ptr<Mono> ty_;
        for (auto &xe : e->xes) {
          if (xe.second->sig != nullptr) {
            context.set_type_env(xe.first, xe.second->sig);
          } else {
            tys[xe.first] = new_forall_var(new_const_kind());
            context.set_type_env(xe.first, new_poly(tys[xe.first]));
          }
        }
        for (auto &xe : e->xes) {
          auto ty_ = infer(xe.second, nullptr);
          if (xe.second->sig == nullptr) {
            if (!unify(tys[xe.first], ty_, &cerr)) {
              string data = to_string(e, 0, "  ");
              if (data.length() > 78) {
                data = data.substr(0, 75) + "...";
              }
              cerr << "`" << data << "`" << endl;
              exit(EXIT_FAILURE);
            }
          }
        }
        for (auto &xe : e->xes) {
          context.unset_type_env(xe.first);
        }
        for (auto &xe : e->xes) {
          auto t = xe.second->sig != nullptr ? inst(xe.second->sig)
                                             : find(tys[xe.first]);
          if (xe.second->T != ExprType::ABS) {
            cerr << "type error: rec of this type is not supported" << endl;
            string data = to_string(e, 0, "  ");
            if (data.length() > 78) {
              data = data.substr(0, 75) + "...";
            }
            cerr << "`" << data << "`" << endl;
            exit(EXIT_FAILURE);
          }
          //          cerr << xe.first << " : "
          //               << (xe.second->sig != nullptr ?
          //               to_string(xe.second->sig)
          //                                             :
          //                                             to_string(tys[xe.first]))
          //               << endl;
        }
        for (auto &xe : e->xes) {
          if (xe.second->sig != nullptr) {
            context.set_type_env(xe.first, xe.second->sig);
          } else {
            context.set_type_env(xe.first, gen(tys[xe.first]));
          }
        }
        ty_ = infer(e->e, sig);
        for (auto &xe : e->xes) {
          context.unset_type_env(xe.first);
        }
        ty = ty_;
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
              context.set_type_env(pes.first[i], new_poly(taus[i]));
            } else {
              context.set_type_env(pes.first[i], taus[i]->sigma);
            }
          }
          auto ty_ = infer(pes.second, nullptr);
          for (size_t i = 0; i < c->arg; i++) {
            context.unset_type_env(pes.first[i]);
          }
          fn->tau.push_back(ty_);
          fns[pes_.first] = gen(fn);
        }
        auto gadt = e->gadt;
        if (gadt != nullptr) {
          check(gadt);
          auto t = find(get_mono(gadt));
          if (!(is_fun(t) && is_c(find(t->tau[0])) && !is_p(find(t->tau[0])) &&
                is_cd(find(t->tau[0])) &&
                unit->data.count(find(t->tau[0])->D.D))) {
            cerr << "type error: invaliad signature for case expression" << endl
                 << to_string(gadt) << endl;
            string data = to_string(e, 0, "  ");
            if (data.length() > 78) {
              data = data.substr(0, 75) + "...";
            }
            cerr << "`" << data << "`" << endl;
            exit(EXIT_FAILURE);
          }
        } else {
          auto gadt_ = new_forall_var(new_const_kind());
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
                 is_cd(find(t->tau[0])) &&
                 unit->data.count(find(t->tau[0])->D.D));
        }
        //        for (auto &fn : fns) {
        //          cerr << "case " << fn.first << " : " <<
        //          to_string(fn.second)
        //          << endl;
        //        }
        //        cerr << "gadt : " << to_string(gadt) << endl;
        for (auto c : unit->data[unit->cons[fns.begin()->first]->data_name]
                          ->constructors) {
          auto tau = inst_with_exists(c->sig, context.get_exists(c->name));
          auto t = tau;
          while (is_fun(t)) {
            t = t->tau[1];
          }
          auto fn = new_fun(), ret = new_forall_var(new_const_kind());
          fn->tau.push_back(t);
          fn->tau.push_back(ret);
          if (unify(inst(gadt), fn, nullptr)) {
            if (fns.count(c->name)) {
              set<shared_ptr<Mono>> st;
              if (!unify(inst_get_set(gen(fn), st), inst(fns[c->name]), &cerr,
                         &st)) {
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
        ty = new_forall_var(new_const_kind());
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
              if (!context.has_type_env(v)) {
                cerr << "type error: " << v << " is not in context" << endl;
                string data = to_string(e, 0, "  ");
                if (data.length() > 78) {
                  data = data.substr(0, 75) + "...";
                }
                cerr << "`" << data << "`" << endl;
                exit(EXIT_FAILURE);
              }
            } else {
              assert(false);
            }
          } else {
            assert(false);
          }
        }
        ty = new_forall_var(new_const_kind());
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

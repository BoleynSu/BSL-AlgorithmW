#ifndef SU_BOLEYN_BSL_OPTIMIZE_H
#define SU_BOLEYN_BSL_OPTIMIZE_H

#include <map>
#include <memory>
#include <set>
#include <string>
#include <utility>
#include <vector>

#include "ds/expr.h"

using namespace std;

struct Optimizer {
  struct Context {
    struct Env {
      size_t id;
      string x;
      bool is_abs_or_case;
      bool is_let;
      shared_ptr<Expr> expr;
    };
    struct FvCmp {
      bool operator()(shared_ptr<Env> a, shared_ptr<Env> b) {
        return a->id < b->id;
      }
    };
    size_t env_cnt;
    map<string, vector<shared_ptr<Env>>> c;
    Context() : env_cnt() {}
    shared_ptr<Context::Env> new_abs_or_case_env(const string &x) {
      auto e = make_shared<Context::Env>();
      e->id = ++env_cnt;
      e->x = x;
      e->is_abs_or_case = true;
      return e;
    }
    shared_ptr<Context::Env> new_let_env(const string &x,
                                         shared_ptr<Expr> expr) {
      auto e = make_shared<Context::Env>();
      e->id = ++env_cnt;
      e->x = x;
      e->is_abs_or_case = false;
      e->is_let = true;
      e->expr = expr;
      return e;
    }
    shared_ptr<Context::Env> new_rec_env(const string &x) {
      auto e = make_shared<Context::Env>();
      e->id = ++env_cnt;
      e->x = x;
      e->is_abs_or_case = false;
      e->is_let = false;
      return e;
    }
    shared_ptr<Env> get_env(const string &key) {
      return c.find(key)->second.back();
    }
    void set_env(const string &key, shared_ptr<Env> value) {
      c[key].push_back(value);
    }
    void unset_env(const string &key) {
      auto &v = c[key];
      v.pop_back();
      if (v.empty()) {
        c.erase(key);
      }
    }
  } context;

  string rename(shared_ptr<Context::Env> e) {
    stringstream s;
    s << "_2_" << e->id << "_" << e->x;
    return s.str();
  }

  string ffi(const string &f) {
    stringstream s;
    size_t idx = 0;
    while (idx < f.length()) {
      char c = f[idx];
      if (c != '$') {
        s << f[idx++];
      } else {
        if (++idx < f.length()) {
          c = f[idx];
          if (('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z') || c == '_') {
            string v;
            v.push_back(c);
            idx++;
            while (idx < f.length()) {
              c = f[idx];
              if (!(('0' <= c && c <= '9') || ('A' <= c && c <= 'Z') ||
                    ('a' <= c && c <= 'z') || c == '_' || c == '\'')) {
                break;
              }
              v.push_back(c);
              idx++;
            }
            s << "$" << rename(context.get_env(v));
          } else {
            assert(false);
          }
        } else {
          assert(false);
        }
      }
    }
    return s.str();
  }

  shared_ptr<Expr> inline_1(shared_ptr<Expr> e) {
    shared_ptr<Expr> ne;
    switch (e->T) {
      case ExprType::VAR: {
        auto env = context.get_env(e->x);

        ne = make_shared<Expr>();
        ne->T = e->T;
        ne->x = rename(env);
      } break;
      case ExprType::APP: {
        auto e1 = inline_1(e->e1);
        auto e2 = inline_1(e->e2);

        ne = make_shared<Expr>();
        ne->T = e->T;
        ne->e1 = e1;
        ne->e2 = e2;
      } break;
      case ExprType::ABS: {
        auto env = context.new_abs_or_case_env(e->x);
        context.set_env(e->x, env);
        auto e_ = inline_1(e->e);
        context.unset_env(e->x);

        ne = make_shared<Expr>();
        ne->T = e->T;
        ne->x = rename(env);
        ne->e = e_;
      } break;
      case ExprType::LET: {
        auto e1 = inline_1(e->e1);
        auto env = context.new_let_env(e->x, e1);
        context.set_env(e->x, env);
        auto e2 = inline_1(e->e2);
        context.unset_env(e->x);

        ne = make_shared<Expr>();
        ne->T = e->T;
        ne->x = rename(env);
        ne->e1 = e1;
        ne->e2 = e2;
      } break;
      case ExprType::REC: {
        vector<pair<string, shared_ptr<Expr>>> xes;

        for (auto &xe : e->xes) {
          auto env = context.new_rec_env(xe.first);
          context.set_env(xe.first, env);
        }
        for (auto &xe : e->xes) {
          xes.push_back(make_pair(rename(context.get_env(xe.first)),
                                  inline_1(xe.second)));
        }
        auto e_ = inline_1(e->e);
        for (auto &xe : e->xes) {
          context.unset_env(xe.first);
        }

        ne = make_shared<Expr>();
        ne->T = e->T;
        ne->xes = xes;
        ne->e = e_;
      } break;
      case ExprType::CASE: {
        map<string, pair<vector<string>, shared_ptr<Expr>>> pes;
        for (auto &pe : e->pes) {
          for (auto x : pe.second.first) {
            auto env = context.new_abs_or_case_env(x);
            context.set_env(x, env);
            pes[pe.first].first.push_back(rename(env));
          }
          pes[pe.first].second = inline_1(pe.second.second);
          for (auto x : pe.second.first) {
            context.unset_env(x);
          }
        }
        auto e_ = inline_1(e->e);

        ne = make_shared<Expr>();
        ne->T = e->T;
        ne->pes = pes;
        ne->e = e_;
      } break;
      case ExprType::FFI: {
        ne = make_shared<Expr>();
        ne->T = e->T;
        ne->ffi = make_shared<Ffi>();
        ne->ffi->source = ffi(e->ffi->source);
        for (auto &fv : e->ffi->fv) {
          ne->ffi->fv.insert(rename(context.get_env(fv)));
        }
      } break;
      default:
        assert(false);
    }
    return ne;
  }

  shared_ptr<Expr> clone(shared_ptr<Expr> e) {
    shared_ptr<Expr> ne;
    switch (e->T) {
      case ExprType::VAR: {
        ne = make_shared<Expr>();
        ne->T = e->T;
        ne->x = e->x;
      } break;
      case ExprType::APP: {
        auto e1 = clone(e->e1);
        auto e2 = clone(e->e2);

        ne = make_shared<Expr>();
        ne->T = e->T;
        ne->e1 = e1;
        ne->e2 = e2;
      } break;
      case ExprType::ABS: {
        auto e_ = clone(e->e);

        ne = make_shared<Expr>();
        ne->T = e->T;
        ne->x = e->x;
        ne->e = e_;
      } break;
      case ExprType::LET: {
        auto e1 = clone(e->e1);
        auto e2 = clone(e->e2);

        ne = make_shared<Expr>();
        ne->T = e->T;
        ne->x = e->x;
        ne->e1 = e1;
        ne->e2 = e2;
      } break;
      case ExprType::REC: {
        vector<pair<string, shared_ptr<Expr>>> xes;
        for (auto &xe : e->xes) {
          xes.push_back(make_pair(xe.first, clone(xe.second)));
        }
        auto e_ = clone(e->e);

        ne = make_shared<Expr>();
        ne->T = e->T;
        ne->xes = xes;
        ne->e = e_;
      } break;
      case ExprType::CASE: {
        map<string, pair<vector<string>, shared_ptr<Expr>>> pes;
        for (auto &pe : e->pes) {
          for (auto x : pe.second.first) {
            pes[pe.first].first.push_back(x);
          }
          pes[pe.first].second = clone(pe.second.second);
        }
        auto e_ = clone(e->e);

        ne = make_shared<Expr>();
        ne->T = e->T;
        ne->pes = pes;
        ne->e = e_;
      } break;
      case ExprType::FFI: {
        ne = make_shared<Expr>();
        ne->T = e->T;
        ne->ffi = make_shared<Ffi>();
        ne->ffi->source = e->ffi->source;
        for (auto &fv : e->ffi->fv) {
          ne->ffi->fv.insert(fv);
        }
      } break;
      default:
        assert(false);
    }
    return ne;
  }

  shared_ptr<Expr> inline_2(shared_ptr<Expr> e) {
    shared_ptr<Expr> ne;
    switch (e->T) {
      case ExprType::VAR: {
        auto env = context.get_env(e->x);

        if (!env->is_abs_or_case && env->is_let) {
          ne = clone(env->expr);
        } else {
          ne = e;
        }
      } break;
      case ExprType::APP: {
        auto e1 = inline_2(e->e1);
        auto e2 = inline_2(e->e2);

        ne = make_shared<Expr>();
        ne->T = e->T;
        ne->e1 = e1;
        ne->e2 = e2;
      } break;
      case ExprType::ABS: {
        auto env = context.new_abs_or_case_env(e->x);
        context.set_env(e->x, env);
        auto e_ = inline_2(e->e);
        context.unset_env(e->x);

        ne = make_shared<Expr>();
        ne->T = e->T;
        ne->x = e->x;
        ne->e = e_;
      } break;
      case ExprType::LET: {
        auto e1 = inline_2(e->e1);
        auto env = context.new_let_env(e->x, e1);
        context.set_env(e->x, env);
        auto e2 = inline_2(e->e2);
        context.unset_env(e->x);

        ne = make_shared<Expr>();
        ne->T = e->T;
        ne->x = e->x;
        ne->e1 = e1;
        ne->e2 = e2;
      } break;
      case ExprType::REC: {
        vector<pair<string, shared_ptr<Expr>>> xes;
        for (auto &xe : e->xes) {
          auto env = context.new_rec_env(xe.first);
          context.set_env(xe.first, env);
        }
        for (auto &xe : e->xes) {
          xes.push_back(make_pair(xe.first, inline_2(xe.second)));
        }
        auto e_ = inline_2(e->e);
        for (auto &xe : e->xes) {
          context.unset_env(xe.first);
        }

        ne = make_shared<Expr>();
        ne->T = e->T;
        ne->xes = xes;
        ne->e = e_;
      } break;
      case ExprType::CASE: {
        map<string, pair<vector<string>, shared_ptr<Expr>>> pes;
        for (auto &pe : e->pes) {
          for (auto x : pe.second.first) {
            auto env = context.new_abs_or_case_env(x);
            context.set_env(x, env);
            pes[pe.first].first.push_back(x);
          }
          pes[pe.first].second = inline_2(pe.second.second);
          for (auto x : pe.second.first) {
            context.unset_env(x);
          }
        }
        auto e_ = inline_2(e->e);

        ne = make_shared<Expr>();
        ne->T = e->T;
        ne->pes = pes;
        ne->e = e_;
      } break;
      case ExprType::FFI: {
        ne = e;
      } break;
      default:
        assert(false);
    }
    return ne;
  }
  shared_ptr<Expr> optimize(shared_ptr<Expr> e) {
    return e;
    e = inline_1(e);
    e = inline_2(e);
    cerr << to_string(e, 1, " ") << endl;
  }
};

#endif

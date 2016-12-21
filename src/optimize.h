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
      shared_ptr<Expr> expr;
      bool is_let;
    };
    struct FvCmp {
      bool operator()(shared_ptr<Env> a, shared_ptr<Env> b) {
        return a->id < b->id;
      }
    };
    size_t env_cnt;
    map<string, vector<shared_ptr<Env>>> c;
    map<shared_ptr<Expr>, set<shared_ptr<Env>, FvCmp>> fv;
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
      e->expr = expr;
      e->is_let = true;
      return e;
    }
    shared_ptr<Context::Env> new_rec_env(const string &x,
                                         shared_ptr<Expr> expr) {
      auto e = make_shared<Context::Env>();
      e->id = ++env_cnt;
      e->x = x;
      e->is_abs_or_case = false;
      e->expr = expr;
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
    set<shared_ptr<Env>, FvCmp> &get_fv(shared_ptr<Expr> e) { return fv[e]; }
    void add_fv(shared_ptr<Expr> e, shared_ptr<Env> env) { fv[e].insert(env); }
    void remove_fv(shared_ptr<Expr> e, shared_ptr<Env> env) {
      fv[e].erase(env);
    }
    void add_fv(shared_ptr<Expr> e, shared_ptr<Expr> e_) {
      fv[e].insert(fv[e_].begin(), fv[e_].end());
    }
  } context;
  shared_ptr<Expr> optimize(shared_ptr<Expr> e) { return e; }
};

#endif

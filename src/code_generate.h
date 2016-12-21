#ifndef SU_BOLEYN_BSL_CODE_GENERATE_H
#define SU_BOLEYN_BSL_CODE_GENERATE_H

#include <stddef.h>
#include <algorithm>
#include <cassert>
#include <cstdlib>
#include <iostream>
#include <limits>
#include <map>
#include <memory>
#include <set>
#include <sstream>
#include <string>
#include <utility>
#include <vector>

#include "ds/data.h"
#include "ds/expr.h"
#include "ds/ffi.h"
#include "ds/unit.h"
#include "optimize.h"

using namespace std;

const string BSL_RT_CLOSURE_T = "BSL_RT_CLOSURE_T";
const string BSL_RT_FUN_T = "BSL_RT_FUN_T";
const string BSL_RT_VAR_T = "BSL_RT_VAR_T";
const string BSL_RT_MALLOC = "BSL_RT_MALLOC";
const string BSL_RT_CALL = "BSL_RT_CALL";

const string BSL_TYPE_ = "BSL_TYPE_";
const string BSL_TAG_TYPE_ = "BSL_TAG_TYPE_";
const string BSL_TAG_ = "BSL_TAG_";
const string BSL_CON_ = "BSL_CON_";
const string BSL_FUN_ = "BSL_FUN_";
const string BSL_BLK_ = "BSL_BLK_";
const string BSL_VAR_ = "BSL_VAR_";
const string BSL_ENV = "BSL_ENV";

struct CodeGenerator {
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

  shared_ptr<Unit> unit;
  shared_ptr<Optimizer> optimizer;

  vector<shared_ptr<stringstream>> blks;
  vector<shared_ptr<stringstream>> fns;
  set<size_t> cons;

  CodeGenerator(ostream &out, shared_ptr<Unit> unit,
                shared_ptr<Optimizer> optimizer)
      : unit(unit), optimizer(optimizer) {
    codegen_unit(out);
  }

  string var(string v) {
    string nv = BSL_VAR_;
    for (size_t i = 0; i < v.length(); i++) {
      if (v[i] == '_') {
        nv.push_back('_');
        nv.push_back('_');
      } else if (v[i] == '\'') {
        nv.push_back('_');
        nv.push_back('0');
      } else {
        nv.push_back(v[i]);
      }
    }
    return nv;
  }
  string tmp() { return BSL_VAR_ + "_1"; }
  string type(const string &t) { return BSL_TYPE_ + t; }
  string tag_type(const string &t) { return BSL_TAG_TYPE_ + t; }
  string tag(const string &t) { return BSL_TAG_ + t; }
  string con(const string &c) {
    stringstream ss;
    ss << BSL_CON_ << c;
    return ss.str();
  }
  string con(size_t c) {
    stringstream ss;
    ss << BSL_CON_ << c;
    return ss.str();
  }
  string arg(size_t i) {
    stringstream ss;
    ss << "arg" << i;
    return ss.str();
  }
  string fun(size_t i) {
    stringstream ss;
    ss << BSL_FUN_ << i;
    return ss.str();
  }
  string blk(size_t i) {
    stringstream ss;
    ss << BSL_BLK_ << i;
    return ss.str();
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
            s << var(v);
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

  void codegen_unit(ostream &out) {
    // TODO handle include here someday
    out << "#include <bsl_rt.h>" << endl;

    stringstream delcs;
    codegen_data(delcs);

    stringstream main;
    codegen_expr(main);

    out << delcs.str() << endl;

    for (size_t i : cons) {
      out << "static " << BSL_RT_VAR_T << " " << con(i) << "("
          << BSL_RT_CLOSURE_T << " " << tmp() << ", " << BSL_RT_FUN_T << " fun";
      for (size_t j = 0; j < i; j++) {
        out << ", " << BSL_RT_VAR_T << " " << var(arg(j));
      }
      out << ") {" << endl
          << "  " << tmp() << "->fun"
          << " = fun;" << endl;
      for (size_t j = 0; j < i; j++) {
        out << "  " << tmp() << "->env[" << j << "]"
            << " = " << var(arg(j)) << ";" << endl;
      }
      out << "  return (" << BSL_RT_VAR_T << ") " << tmp() << ";" << endl
          << "}" << endl;
    }

    for (size_t i = 0; i < fns.size(); i++) {
      string fn = fns[i]->str();
      string header = fn.substr(0, fn.find('{'));
      header.back() = ';';
      out << "static " << header << endl;
    }
    for (size_t i = 0; i < blks.size(); i++) {
      string blk = blks[i]->str();
      string header = blk.substr(0, blk.find('{'));
      header.back() = ';';
      out << "static " << header << endl;
    }
    for (auto fn : fns) {
      out << "static " << fn->str();
    }
    for (auto blk : blks) {
      out << "static " << blk->str();
    }

    // TODO handle module here someday
    out << "int main() { " << main.str() << "; }" << endl;
  }

  void codegen_data(ostream &out) {
    for (auto dai : unit->data) {
      auto da = dai.second;
      if (da->constructors.size()) {
        out << "typedef enum {";
        bool first = true;
        if (da->to_ptr != numeric_limits<size_t>::max()) {
          auto c = da->constructors[da->to_ptr];
          out << (first ? " " : ", ") << tag(c->name);
          first = false;
        }
        for (size_t i = 0; i < da->constructors.size(); i++) {
          auto c = da->constructors[i];
          if (i != da->to_ptr) {
            out << (first ? " " : ", ") << tag(c->name);
            first = false;
          }
        }
        out << " } " << tag_type(da->name) << ";" << endl;

        out << "typedef struct {" << endl;
        if (da->maxarg == 0) {
        } else {
          if ((da->to_ptr == numeric_limits<size_t>::max() &&
               da->constructors.size() > 1) ||
              (da->to_ptr != numeric_limits<size_t>::max() &&
               da->constructors.size() > 2)) {
            out << tag_type(da->name) << " tag;" << endl;
            for (size_t i = 0; i < da->maxarg; i++) {
              out << "  " << BSL_RT_VAR_T << " " << arg(i) << ";" << endl;
            }
          } else {
            for (size_t i = 0; i < da->constructors.size(); i++) {
              if (i != da->to_ptr) {
                auto c = da->constructors[i];
                if (c->arg == 1 && da->constructors.size() == 1) {
                } else {
                  for (size_t i = 0; i < c->arg; i++) {
                    out << "  " << BSL_RT_VAR_T << " " << arg(i) << ";" << endl;
                  }
                }
              }
            }
          }
        }
        out << "} " << type(da->name) << ";" << endl;

        for (size_t i = 0; i < da->constructors.size(); i++) {
          auto c = da->constructors[i];
          out << BSL_RT_VAR_T << " " << con(c->name) << "(" << type(da->name)
              << " *" << tmp();
          for (size_t j = 0; j < c->arg; j++) {
            out << ", " << BSL_RT_VAR_T << " " << var(arg(j));
          }
          out << ") {" << endl;
          if (da->maxarg == 0) {
            out << "  return " << tag(c->name) << ";" << endl;
          } else {
            if (i != da->to_ptr) {
              if ((da->to_ptr == numeric_limits<size_t>::max() &&
                   da->constructors.size() > 1) ||
                  (da->to_ptr != numeric_limits<size_t>::max() &&
                   da->constructors.size() > 2)) {
                out << "  " << tmp() << "->tag = " << tag(c->name) << ";"
                    << endl;
                for (size_t j = 0; j < c->arg; j++) {
                  out << "  " << tmp() << "->" << arg(j) << " = " << var(arg(j))
                      << ";" << endl;
                }
              } else {
                if (c->arg == 1 && da->constructors.size() == 1) {
                  out << "  " << tmp() << " = " << var(arg(0)) << ";" << endl;
                } else {
                  for (size_t j = 0; j < c->arg; j++) {
                    out << "  " << tmp() << "->" << arg(j) << " = "
                        << var(arg(j)) << ";" << endl;
                  }
                }
              }
              out << "  return (" << BSL_RT_VAR_T << ")" << tmp() << ";"
                  << endl;
            } else {
              out << "  return NULL;" << endl;
            }
          }
          out << "}" << endl;
        }
      }
    }
  }

  void codegen_expr(ostream &out) {
    auto expr = unit->expr;
    for (auto dai : unit->data) {
      auto da = dai.second;
      if (da->constructors.size()) {
        for (size_t i = 0; i < da->constructors.size(); i++) {
          auto c = da->constructors[i];
          auto e = make_shared<Expr>();
          e->T = ExprType::LET;
          e->x = c->name;
          auto lam = make_shared<Expr>();
          auto cur = lam;
          for (size_t j = 0; j < c->arg; j++) {
            cur->T = ExprType::ABS;
            cur->x = arg(j);
            cur->e = make_shared<Expr>();
            cur = cur->e;
          }
          stringstream s;
          s << " " << con(c->name) << "(";
          if (da->maxarg == 0) {
            s << "NULL";
          } else {
            if (i != da->to_ptr) {
              if ((da->to_ptr == numeric_limits<size_t>::max() &&
                   da->constructors.size() > 1) ||
                  (da->to_ptr != numeric_limits<size_t>::max() &&
                   da->constructors.size() > 2)) {
                s << BSL_RT_MALLOC << "(sizeof(" << type(c->data_name) << "))";
              } else {
                if (c->arg == 1 && da->constructors.size() == 1) {
                  s << "NULL";
                } else {
                  s << BSL_RT_MALLOC << "(sizeof(" << type(c->data_name)
                    << "))";
                }
              }
            } else {
              s << "NULL";
            }
          }
          for (size_t j = 0; j < c->arg; j++) {
            s << ", "
              << "$" << arg(j);
          }
          s << ") ";
          cur->T = ExprType::FFI;
          cur->ffi = make_shared<Ffi>();
          cur->ffi->source = s.str();
          for (size_t j = 0; j < c->arg; j++) {
            cur->ffi->fv.insert(arg(j));
          }
          e->e1 = lam;
          e->e2 = expr;
          expr = e;
        }
      }
    }
    if (optimizer != nullptr) {
      expr = optimizer->optimize(expr);
    }
    codegen_expr_(out, expr);
  }

  void codegen_expr_(ostream &out, shared_ptr<Expr> e,
                     map<string, size_t> env = map<string, size_t>()) {
    switch (e->T) {
      case ExprType::VAR: {
        out << var(e->x);
        context.add_fv(e, context.get_env(e->x));
        break;
      }
      case ExprType::APP: {
        out << BSL_RT_CALL << "(";
        codegen_expr_(out, e->e1);
        context.add_fv(e, e->e1);
        out << ", ";
        codegen_expr_(out, e->e2);
        context.add_fv(e, e->e2);
        out << ")";
      } break;
      case ExprType::ABS: {
        auto env = context.new_abs_or_case_env(e->x);
        context.set_env(e->x, env);
        stringstream nnout;
        codegen_expr_(nnout, e->e);
        context.unset_env(e->x);

        context.add_fv(e, e->e);
        context.remove_fv(e, env);

        size_t fn_idx = fns.size();
        fns.push_back(make_shared<stringstream>());
        auto &nout = *fns.back();
        nout << BSL_RT_VAR_T << " " << fun(fn_idx) << "(" << BSL_RT_VAR_T << " "
             << var(e->x) << ", " << BSL_RT_VAR_T << " " << BSL_ENV << "[]) {"
             << endl;
        size_t fv_cnt = 0;
        for (auto &fv : context.get_fv(e)) {
          nout << "  " << BSL_RT_VAR_T << " " << var(fv->x) << " = " << BSL_ENV
               << "[" << fv_cnt << "];" << endl;
          fv_cnt++;
        }
        nout << "  return " << nnout.str() << ";" << endl << "}" << endl;

        cons.insert(fv_cnt);
        out << con(fv_cnt) << "(" << BSL_RT_MALLOC << "("
            << "sizeof(" << BSL_RT_FUN_T << ") + " << fv_cnt << " * sizeof("
            << BSL_RT_VAR_T << "))"
            << ", " << fun(fn_idx);
        for (auto &fv : context.get_fv(e)) {
          out << ", " << var(fv->x);
        }
        out << ")";
      } break;
      case ExprType::LET: {
        auto env = context.new_let_env(e->x, e);
        context.set_env(e->x, env);
        stringstream nnout;
        codegen_expr_(nnout, e->e2);
        context.add_fv(e, e->e2);
        context.remove_fv(e, env);
        context.unset_env(e->x);

        size_t blk_idx = blks.size();
        blks.push_back(make_shared<stringstream>());
        auto &nout = *blks.back();
        nout << BSL_RT_VAR_T << " " << blk(blk_idx) << "(" << BSL_RT_VAR_T
             << " " << var(e->x);
        for (auto &fv : context.get_fv(e)) {
          nout << ", " << BSL_RT_VAR_T << " " << var(fv->x);
        }
        nout << ") {" << endl
             << "  return " << nnout.str() << ";" << endl
             << "}" << endl;

        out << blk(blk_idx) << "(";
        codegen_expr_(out, e->e1);
        for (auto &fv : context.get_fv(e)) {
          out << ", " << var(fv->x);
        }
        out << ")";
        context.add_fv(e, e->e1);
      } break;
      case ExprType::REC: {
        for (auto &xe : e->xes) {
          auto env = context.new_rec_env(xe.first, xe.second);
          context.set_env(xe.first, env);
        }
        map<string, stringstream> nnouts;
        for (auto &xe : e->xes) {
          auto &nnout = nnouts[xe.first];

          auto env = context.new_abs_or_case_env(xe.second->x);
          context.set_env(xe.second->x, env);
          stringstream nnnout;
          codegen_expr_(nnnout, xe.second->e);
          context.unset_env(xe.second->x);

          context.add_fv(xe.second, xe.second->e);
          context.remove_fv(xe.second, env);

          size_t fn_idx = fns.size();
          fns.push_back(make_shared<stringstream>());
          auto &nout = *fns.back();
          nout << BSL_RT_VAR_T << " " << fun(fn_idx) << "(" << BSL_RT_VAR_T
               << " " << var(xe.second->x) << ", " << BSL_RT_VAR_T << " "
               << BSL_ENV << "[]) {" << endl;
          size_t fv_cnt = 0;
          for (auto &fv : context.get_fv(xe.second)) {
            nout << "  " << BSL_RT_VAR_T << " " << var(fv->x) << " = "
                 << BSL_ENV << "[" << fv_cnt << "];" << endl;
            fv_cnt++;
          }
          nout << "  return " << nnnout.str() << ";" << endl << "}" << endl;

          cons.insert(fv_cnt);
          nnout << con(fv_cnt) << "(" << var(xe.first) << ", " << fun(fn_idx);
          for (auto &fv : context.get_fv(xe.second)) {
            nnout << ", " << var(fv->x);
          }
          nnout << ")";
        }

        stringstream nnout;
        codegen_expr_(nnout, e->e);
        context.add_fv(e, e->e);

        for (auto &xe : e->xes) {
          context.add_fv(e, xe.second);
        }
        for (auto &xe : e->xes) {
          context.remove_fv(e, context.get_env(xe.first));
          context.unset_env(xe.first);
        }

        size_t blk_idx = blks.size();
        blks.push_back(make_shared<stringstream>());
        auto &nout = *blks.back();
        nout << BSL_RT_VAR_T << " " << blk(blk_idx) << "(";
        {
          bool first = true;
          for (auto &fv : context.get_fv(e)) {
            nout << (first ? "" : ", ") << BSL_RT_VAR_T << " " << var(fv->x);
            first = false;
          }
        }
        nout << ") {" << endl;
        for (auto &xe : e->xes) {
          nout << "  " << BSL_RT_VAR_T << " " << var(xe.first) << " = "
               << BSL_RT_MALLOC << "("
               << "sizeof(" << BSL_RT_FUN_T << ") + "
               << context.get_fv(xe.second).size() << " * sizeof("
               << BSL_RT_VAR_T << "));" << endl;
        }
        for (auto &xe : e->xes) {
          nout << "  " << var(xe.first) << " = " << nnouts[xe.first].str()
               << ";" << endl;
        }
        nout << "  return " << nnout.str() << ";" << endl << "}" << endl;

        out << blk(blk_idx) << "(";
        {
          bool first = true;
          for (auto &fv : context.get_fv(e)) {
            if (!first) {
              out << ", ";
            }
            out << var(fv->x);
            first = false;
          }
        }
        out << ")";
      } break;
      case ExprType::CASE: {
        map<string, stringstream> nouts;
        for (auto &pe : e->pes) {
          auto &nout = nouts[pe.first];
          for (auto x : pe.second.first) {
            auto env = context.new_abs_or_case_env(x);
            context.set_env(x, env);
          }
          codegen_expr_(nout, pe.second.second);
          context.add_fv(e, pe.second.second);
          for (auto x : pe.second.first) {
            context.remove_fv(e, context.get_env(x));
            context.unset_env(x);
          }
        }

        size_t blk_idx = blks.size();
        blks.push_back(make_shared<stringstream>());
        auto &nout = *blks.back();
        nout << BSL_RT_VAR_T << " " << blk(blk_idx) << "(" << BSL_RT_VAR_T
             << " " << tmp();
        for (auto &fv : context.get_fv(e)) {
          nout << ", " << BSL_RT_VAR_T << " " << var(fv->x);
        }
        nout << ") {" << endl;

        assert(e->pes.size() >= 1);
        auto t = get_mono(e->gadt);
        assert(is_fun(t) && is_c(t->tau[0]) && unit->data.count(t->tau[0]->D));
        auto da = unit->data[t->tau[0]->D];
        if (da->maxarg == 0) {
          if (da->to_ptr != numeric_limits<size_t>::max()) {
            assert(da->constructors.size());
            if (e->pes.count(da->constructors[da->to_ptr]->name)) {
              auto &pes =
                  e->pes.find(da->constructors[da->to_ptr]->name)->second;
              nout << "  if (" << tmp() << " == NULL) {" << endl;
              for (size_t i = 0; i < pes.first.size(); i++) {
                nout << "    " << BSL_RT_VAR_T << " " << var(pes.first[i])
                     << " = ((" << type(da->name) << "*)(" << tmp() << "))->"
                     << arg(i) << ";" << endl;
              }
              nout << "    return "
                   << nouts[da->constructors[da->to_ptr]->name].str() << ";"
                   << endl
                   << "  }" << endl;
            }
          }
          if ((da->to_ptr == numeric_limits<size_t>::max() &&
               da->constructors.size() > 1) ||
              (da->to_ptr != numeric_limits<size_t>::max() &&
               da->constructors.size() > 2)) {
            nout << "  switch ("
                 << "(" << tag_type(da->name) << ") " << tmp() << ") {" << endl;
            for (size_t i = 0; i < da->constructors.size(); i++) {
              auto c = da->constructors[i];
              if (i != da->to_ptr && e->pes.count(c->name)) {
                auto &pes = e->pes.find(c->name)->second;
                nout << "    case " << tag(c->name) << ": {" << endl;
                for (size_t i = 0; i < pes.first.size(); i++) {
                  nout << "      " << BSL_RT_VAR_T << " " << var(pes.first[i])
                       << " = ((" << type(da->name) << "*)(" << tmp() << "))->"
                       << arg(i) << ";" << endl;
                }
                nout << "      return "
                     << nouts[da->constructors[i]->name].str() << ";" << endl
                     << "    }" << endl;
              }
            }
            nout << "  }" << endl;
          } else {
            for (size_t i = 0; i < da->constructors.size(); i++) {
              auto c = da->constructors[i];
              if (i != da->to_ptr && e->pes.count(c->name)) {
                nout << "  return " << nouts[da->constructors[i]->name].str()
                     << ";" << endl;
              }
            }
          }
        } else {
          if (da->to_ptr != numeric_limits<size_t>::max()) {
            assert(da->constructors.size());
            if (e->pes.count(da->constructors[da->to_ptr]->name)) {
              auto &pes =
                  e->pes.find(da->constructors[da->to_ptr]->name)->second;
              nout << "  if (" << tmp() << " == NULL) {" << endl;
              for (size_t i = 0; i < pes.first.size(); i++) {
                nout << "    " << BSL_RT_VAR_T << " " << var(pes.first[i])
                     << " = ((" << type(da->name) << "*)(" << tmp() << "))->"
                     << arg(i) << ";" << endl;
              }
              nout << "    return "
                   << nouts[da->constructors[da->to_ptr]->name].str() << ";"
                   << endl
                   << "  }" << endl;
            }
          }
          if ((da->to_ptr == numeric_limits<size_t>::max() &&
               da->constructors.size() > 1) ||
              (da->to_ptr != numeric_limits<size_t>::max() &&
               da->constructors.size() > 2)) {
            nout << "  switch ("
                 << "((" << type(da->name) << "*) " << tmp() << ")->tag"
                 << ") {" << endl;
            for (size_t i = 0; i < da->constructors.size(); i++) {
              auto c = da->constructors[i];
              if (i != da->to_ptr && e->pes.count(c->name)) {
                auto &pes = e->pes.find(c->name)->second;
                nout << "    case " << tag(c->name) << ": {" << endl;
                for (size_t i = 0; i < pes.first.size(); i++) {
                  nout << "      " << BSL_RT_VAR_T << " " << var(pes.first[i])
                       << " = ((" << type(da->name) << "*)(" << tmp() << "))->"
                       << arg(i) << ";" << endl;
                }
                nout << "      return "
                     << nouts[da->constructors[i]->name].str() << ";" << endl
                     << "    }" << endl;
              }
            }
            nout << "  }" << endl;
          } else {
            for (size_t i = 0; i < da->constructors.size(); i++) {
              auto c = da->constructors[i];
              if (i != da->to_ptr && e->pes.count(c->name)) {
                auto &pes = e->pes.find(c->name)->second;
                if (c->arg == 1 && da->constructors.size() == 1) {
                  nout << "  " << BSL_RT_VAR_T << " " << var(pes.first.front())
                       << " = " << tmp() << ";" << endl;
                } else {
                  for (size_t i = 0; i < pes.first.size(); i++) {
                    nout << "  " << BSL_RT_VAR_T << " " << var(pes.first[i])
                         << " = ((" << type(da->name) << "*)(" << tmp()
                         << "))->" << arg(i) << ";" << endl;
                  }
                }
                nout << "  return " << nouts[da->constructors[i]->name].str()
                     << ";" << endl;
              }
            }
          }
        }
        nout << "}" << endl;

        out << blk(blk_idx) << "(";
        codegen_expr_(out, e->e);
        for (auto &fv : context.get_fv(e)) {
          out << ", " << var(fv->x);
        }
        out << ")";
        context.add_fv(e, e->e);
      } break;
      case ExprType::FFI: {
        for (auto &fv : e->ffi->fv) {
          context.add_fv(e, context.get_env(fv));
        }
        out << ffi(e->ffi->source);
      } break;
    }
  }
};

#endif

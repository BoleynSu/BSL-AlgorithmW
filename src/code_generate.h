#ifndef SU_BOLEYN_BSL_CODE_GENERATE_H
#define SU_BOLEYN_BSL_CODE_GENERATE_H

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

#include "data.h"
#include "expr.h"
#include "optimize.h"
#include "parse.h"
#include "sig_check.h"
#include "type.h"
#include "type_infer.h"

using namespace std;

const string BSL_RT_CLOSURE_T = "BSL_RT_CLOSURE_T";
const string BSL_RT_FUN_T = "BSL_RT_FUN_T";
const string BSL_RT_VAR_T = "BSL_RT_VAR_T";
const string BSL_RT_MALLOC = "BSL_RT_MALLOC";
const string BSL_RT_CALL = "BSL_RT_CALL";

const string BSL_TYPE_ = "BSL_TYPE_";
const string BSL_TAG_ = "BSL_TAG_";
const string BSL_CON_ = "BSL_CON_";
const string BSL_FUN_ = "BSL_FUN_";
const string BSL_BLK_ = "BSL_BLK_";
const string BSL_VAR_ = "BSL_VAR_";
const string BSL_ENV = "BSL_ENV";

struct CodeGenerator {
  Parser &parser;
  ostream &out;
  shared_ptr<map<string, shared_ptr<Data>>> data;
  vector<shared_ptr<stringstream>> blks;
  vector<shared_ptr<stringstream>> fns;
  set<size_t> cons;
  CodeGenerator(Parser &parser, ostream &out) : parser(parser), out(out) {
    codegen(parser.parse());
  }

  string var(string v, const map<string, size_t> &env = map<string, size_t>()) {
    string nv;
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
    if (env.count(v)) {
      size_t idx = env.find(v)->second;
      stringstream ss;
      ss << BSL_ENV << "[" << idx << "]";
      return ss.str();
    } else {
      return BSL_VAR_ + nv;
    }
  }
  string tmp() { return BSL_VAR_ + "_1"; }
  string type(const string &t) { return BSL_TYPE_ + t; }
  string tag(const string &t) { return BSL_TAG_ + t; }
  string con(const string &c) {
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
  string ffi(const string &f,
             const map<string, size_t> &env = map<string, size_t>()) {
    stringstream s;
    size_t idx = 0;
    while (idx < f.length()) {
      char c = f[idx];
      if (c != '$') {
        s << f[idx++];
      } else {
        idx++;
        if (idx < f.length()) {
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
            s << var(v, env);
          } else {
            string data = f;
            if (data.length() > 80) {
              data = data.substr(0, 77) + "...";
            }
            cerr << "code generator: error in ffi" << endl << data << endl;
            exit(EXIT_FAILURE);
          }
        }
      }
    }
    return s.str();
  }

  void codegen(ostream &out, shared_ptr<Expr> expr,
               const map<string, size_t> &env, string rec = "") {
    switch (expr->T) {
      case ExprType::VAR: {
        out << var(expr->x, env);
        return;
      }
      case ExprType::APP: {
        out << BSL_RT_CALL << "(";
        codegen(out, expr->e1, env);
        out << ",";
        codegen(out, expr->e2, env);
        out << ")";
      }
        return;
      case ExprType::ABS: {
        map<string, size_t> env_;
        for (auto &fv : expr->fv) {
          env_.insert(make_pair(fv, env_.size()));
        }
        size_t fn_idx = fns.size();
        fns.push_back(make_shared<stringstream>());
        auto &nout = *fns.back();
        nout << BSL_RT_VAR_T << " " << fun(fn_idx) << "(" << BSL_RT_VAR_T << " "
             << var(expr->x) << ", " << BSL_RT_VAR_T << " " << BSL_ENV
             << "[]) {" << endl
             << " return ";
        codegen(nout, expr->e, env_);
        nout << ";" << endl << "}" << endl;

        cons.insert(env_.size());
        out << BSL_CON_ << env_.size() << "(";
        if (rec.empty()) {
          out << BSL_RT_MALLOC << "("
              << "sizeof(" << BSL_RT_FUN_T << ") + " << env_.size()
              << " * sizeof(" << BSL_RT_VAR_T << "))";
        } else {
          out << var(rec, env);
        }
        out << ", " << fun(fn_idx);
        for (auto &fv : env_) {
          out << ", " << var(fv.first, env);
        }
        out << ")";
      }
        return;
      case ExprType::LET: {
        map<string, size_t> env_;
        for (auto &fv : expr->e2->fv) {
          if (fv != expr->x) {
            env_.insert(make_pair(fv, env_.size()));
          }
        }
        size_t blk_idx = blks.size();
        blks.push_back(make_shared<stringstream>());
        auto &nout = *blks.back();
        nout << BSL_RT_VAR_T << " " << blk(blk_idx) << "(" << BSL_RT_VAR_T
             << " " << var(expr->x);
        for (auto &e : env_) {
          nout << ", " << BSL_RT_VAR_T << " " << var(e.first);
        }
        nout << ") {" << endl << "  return ";
        codegen(nout, expr->e2, map<string, size_t>());
        nout << ";" << endl << "}" << endl;

        out << blk(blk_idx) << "(";
        codegen(out, expr->e1, env);
        for (auto &fv : env_) {
          out << ", " << var(fv.first, env);
        }
        out << ")";
      }
        return;
      case ExprType::REC: {
        set<string> fv, rec;
        for (size_t i = 0; i < expr->xes.size(); i++) {
          fv.insert(expr->xes[i].second->fv.begin(),
                    expr->xes[i].second->fv.end());
          rec.insert(expr->xes[i].first);
        }
        fv.insert(expr->e->fv.begin(), expr->e->fv.end());
        map<string, size_t> env_;
        for (auto &v : fv) {
          env_.insert(make_pair(v, env_.size()));
        }
        size_t blk_idx = blks.size();
        blks.push_back(make_shared<stringstream>());
        auto &nout = *blks.back();
        nout << BSL_RT_VAR_T << " " << blk(blk_idx) << "(";
        {
          bool first = true;
          for (auto &e : env_) {
            nout << (first ? "" : ", ") << BSL_RT_VAR_T << " " << var(e.first);
            first = false;
          }
        }
        nout << ") {" << endl;
        stringstream nnout;
        for (size_t i = 0; i < expr->xes.size(); i++) {
          auto t = find(expr->xes[i].second->type);
          if (is_c(t) && t->D == "->" &&
              expr->xes[i].second->T == ExprType::ABS) {
            if (env_.count(expr->xes[i].first)) {
              nout << "  " << var(expr->xes[i].first) << " = " << BSL_RT_MALLOC
                   << "(sizeof(" << BSL_RT_FUN_T << ") + "
                   << expr->xes[i].second->fv.size() << " * sizeof("
                   << BSL_RT_VAR_T << "));" << endl
                   << "  ";
              codegen(nnout, expr->xes[i].second, map<string, size_t>(),
                      expr->xes[i].first);
              nnout << ";" << endl;
            } else {
              string data = expr->to_string(0, "  ");
              if (data.length() > 80) {
                data = data.substr(0, 77) + "...";
              }
              cerr << "code generator: rec is not used" << endl << data << endl;
              exit(EXIT_FAILURE);
            }
          } else {
            string data = expr->to_string(0, "  ");
            if (data.length() > 80) {
              data = data.substr(0, 77) + "...";
            }
            cerr << "code generator: rec of this type is not supported" << endl
                 << data << endl;
            exit(EXIT_FAILURE);
          }
        }
        nout << nnout.str() << "  return ";
        codegen(nout, expr->e, map<string, size_t>());
        nout << ";" << endl << "}" << endl;

        out << blk(blk_idx) << "(";
        {
          bool first = true;
          for (auto &fv : env_) {
            if (!first) {
              out << ", ";
            }
            if (rec.count(fv.first)) {
              out << "NULL";
            } else {
              out << var(fv.first, env);
            }
            first = false;
          }
        }
        out << ")";
      }
        return;
      case ExprType::CASE: {
        set<string> fv;
        for (auto &pes_ : expr->pes) {
          auto &pes = pes_.second;
          set<string> fvi;
          fvi.insert(pes.second->fv.begin(), pes.second->fv.end());
          for (size_t i = 0; i < pes.first.size(); i++) {
            fvi.erase(pes.first[i]);
          }
          fv.insert(fvi.begin(), fvi.end());
        }
        map<string, size_t> env_;
        for (auto &v : fv) {
          env_.insert(make_pair(v, env_.size()));
        }
        size_t blk_idx = blks.size();
        blks.push_back(make_shared<stringstream>());
        auto &nout = *blks.back();
        nout << BSL_RT_VAR_T << " " << blk(blk_idx) << "(" << BSL_RT_VAR_T
             << " " << tmp();
        for (auto &e : env_) {
          nout << ", " << BSL_RT_VAR_T << " " << var(e.first);
        }
        nout << ") {" << endl;
        assert(expr->pes.size() >= 1);
        auto t = get_mono(expr->gadt);
        assert(is_c(t) && t->D == "->" && t->tau[0]->is_const &&
               data->count(t->tau[0]->D));
        auto da = (*data)[t->tau[0]->D];
        if (da->to_ptr != numeric_limits<size_t>::max()) {
          assert(da->constructors.size());
          if (expr->pes.count(da->constructors[da->to_ptr]->name)) {
            auto &pes =
                expr->pes.find(da->constructors[da->to_ptr]->name)->second;
            nout << "  if (" << tmp() << " == NULL) {" << endl;
            for (size_t i = 0; i < pes.first.size(); i++) {
              nout << "    " << BSL_RT_VAR_T << " " << var(pes.first[i])
                   << " = ((" << type(da->name) << "*)(" << tmp() << "))->"
                   << arg(i) << ";" << endl;
            }
            nout << "    return ";
            codegen(nout, pes.second, map<string, size_t>());
            nout << ";" << endl << "  }" << endl;
          }
        }

        if ((da->to_ptr == numeric_limits<size_t>::max() &&
             da->constructors.size() > 1) ||
            (da->to_ptr != numeric_limits<size_t>::max() &&
             da->constructors.size() > 2)) {
          nout << "  switch (";
          if (da->maxarg == 0) {
            nout << tmp();
          } else {
            nout << "((" << type(da->name) << "*) " << tmp() << ")->tag";
          }
          nout << ") {" << endl;
          for (size_t i = 0; i < da->constructors.size(); i++) {
            auto c = da->constructors[i];
            if (i != da->to_ptr && expr->pes.count(c->name)) {
              auto &pes = expr->pes.find(c->name)->second;
              nout << "    case " << tag(c->name) << ": {" << endl;
              for (size_t i = 0; i < pes.first.size(); i++) {
                nout << "      " << BSL_RT_VAR_T << " " << var(pes.first[i])
                     << " = ((" << type(da->name) << "*)(" << tmp() << "))->"
                     << arg(i) << ";" << endl;
              }
              nout << "      return ";
              codegen(nout, pes.second, map<string, size_t>());
              nout << ";" << endl << "    }" << endl;
            }
          }
          nout << "  }" << endl;
        } else {
          if (da->maxarg == 0) {
            for (size_t i = 0; i < da->constructors.size(); i++) {
              auto c = da->constructors[i];
              if (i != da->to_ptr && expr->pes.count(c->name)) {
                auto &pes = expr->pes.find(c->name)->second;
                nout << "  return ";
                codegen(nout, pes.second, map<string, size_t>());
                nout << ";" << endl;
              }
            }
          } else {
            for (size_t i = 0; i < da->constructors.size(); i++) {
              auto c = da->constructors[i];
              if (i != da->to_ptr && expr->pes.count(c->name)) {
                auto &pes = expr->pes.find(c->name)->second;
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
                nout << "  return ";
                codegen(nout, pes.second, map<string, size_t>());
                nout << ";" << endl;
              }
            }
          }
        }
        nout << "}" << endl;

        out << blk(blk_idx) << "(";
        codegen(out, expr->e, env);
        for (auto &fv : env_) {
          out << ", " << var(fv.first, env);
        }
        out << ")";
      }
        return;
      case ExprType::FFI: {
        out << ffi(expr->ffi, env);
      }
        return;
    }
  }

  void codegen(pair<pair<shared_ptr<map<string, shared_ptr<Data>>>,
                         shared_ptr<map<string, shared_ptr<Constructor>>>>,
                    shared_ptr<Expr>>
                   prog) {
    data = prog.first.first;
    auto expr = prog.second;
    auto context = make_shared<Context>();

    out << "#include <bsl_rt.h>" << endl;

    for (auto dai : *data) {
      auto da = dai.second;
      if (da->constructors.size()) {
        da->maxarg = 0;
        for (size_t i = 0; i < da->constructors.size(); i++) {
          auto c = da->constructors[i];
          da->maxarg = max(da->maxarg, c->arg);
        }
        da->to_ptr = numeric_limits<size_t>::max();
        for (size_t i = 0; i < da->constructors.size(); i++) {
          auto c = da->constructors[i];
          if (da->to_ptr == numeric_limits<size_t>::max() && c->arg == 0) {
            da->to_ptr = i;
          }
        }
        out << "typedef struct {" << endl;
        if (da->maxarg == 0) {
        } else {
          if ((da->to_ptr == numeric_limits<size_t>::max() &&
               da->constructors.size() > 1) ||
              (da->to_ptr != numeric_limits<size_t>::max() &&
               da->constructors.size() > 2)) {
            out << "  enum {";
            bool first = true;
            for (size_t i = 0; i < da->constructors.size(); i++) {
              auto c = da->constructors[i];
              if (i != da->to_ptr) {
                out << (first ? " " : ", ") << tag(c->name);
                first = false;
              }
            }
            out << " } tag;" << endl;
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
            out << "  return " << i << ";" << endl;
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
    for (auto dai : *data) {
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
          cur->ffi = s.str();
          e->e1 = lam;
          if (c->rank2sig != nullptr) {
            //            cerr << "//" << c->name << " : " <<
            //            to_string(c->rank2sig) << endl;
            context->set_rank2poly(c->name, c->rank2sig);
          } else {
            //            cerr << "//" << c->name << " : " << to_string(c->sig)
            //            << endl;
            context->set_poly(c->name, c->sig);
          }
          e->e2 = expr;
          expr = e;
        }
      }
    }

    infer(prog.second, context, prog.first);

    stringstream main;
    codegen(main, optimize(expr), map<string, size_t>());

    for (size_t i : cons) {
      out << BSL_RT_VAR_T << " " << BSL_CON_ << i << "("
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
      out << header << endl;
    }
    for (size_t i = 0; i < blks.size(); i++) {
      string blk = blks[i]->str();
      string header = blk.substr(0, blk.find('{'));
      header.back() = ';';
      out << "inline " << header << endl;
    }
    for (auto fn : fns) {
      out << fn->str();
    }
    for (auto blk : blks) {
      out << "inline " << blk->str();
    }
    out << "int main() { " << main.str() << "; }" << endl;
  }
};

#endif

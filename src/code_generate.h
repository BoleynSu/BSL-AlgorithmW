#ifndef SU_BOLEYN_BSL_CODE_GENERATE_H
#define SU_BOLEYN_BSL_CODE_GENERATE_H

#include <cassert>
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
  string ffi(const string &f, set<string> &fv) {
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
            fv.insert(v);
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
      out << "static " << BSL_RT_VAR_T << " " << con(i) << "(";
      for (size_t j = 0; j < i; j++) {
        out << BSL_RT_VAR_T << " " << var(arg(j)) << ", ";
      }
      out << BSL_RT_CLOSURE_T << " " << tmp() << ", " << BSL_RT_FUN_T << " fun"
          << ") {" << endl;
      for (size_t j = 0; j < i; j++) {
        out << "  " << tmp() << "->env[" << j << "]"
            << " = " << var(arg(j)) << ";" << endl;
      }
      out << "  " << tmp() << "->fun"
          << " = fun;" << endl
          << "  return (" << BSL_RT_VAR_T << ") " << tmp() << ";" << endl
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
          out << BSL_RT_VAR_T << " " << con(c->name) << "(";
          for (size_t j = 0; j < c->arg; j++) {
            out << BSL_RT_VAR_T << " " << var(arg(j)) << ", ";
          }
          out << type(da->name) << " *" << tmp() << ") {" << endl;
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
          for (size_t j = 0; j < c->arg; j++) {
            s << "$" << arg(j) << ", ";
          }
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
          s << ") ";
          cur->T = ExprType::FFI;
          cur->ffi = make_shared<Ffi>();
          cur->ffi->source = s.str();
          e->e1 = lam;
          e->e2 = expr;
          expr = e;
        }
      }
    }
    if (optimizer != nullptr) {
      expr = optimizer->optimize(expr);
    }
    set<string> fv;
    codegen_expr_(out, expr, fv);
  }

  void codegen_expr_(ostream &out, shared_ptr<Expr> e, set<string> &fv) {
    switch (e->T) {
      case ExprType::VAR: {
        out << var(e->x);
        fv.insert(e->x);
      } break;
      case ExprType::APP: {
        set<string> fv_;
        out << BSL_RT_CALL << "(";
        codegen_expr_(out, e->e1, fv);
        out << ", ";
        codegen_expr_(out, e->e2, fv_);
        out << ")";
        fv.insert(fv_.begin(), fv_.end());
      } break;
      case ExprType::ABS: {
        stringstream nnout;
        codegen_expr_(nnout, e->e, fv);
        fv.erase(e->x);

        size_t fn_idx = fns.size();
        fns.push_back(make_shared<stringstream>());
        auto &nout = *fns.back();
        nout << BSL_RT_VAR_T << " " << fun(fn_idx) << "(" << BSL_RT_VAR_T << " "
             << var(e->x) << ", " << BSL_RT_VAR_T << " " << BSL_ENV << "[]) {"
             << endl;
        size_t fv_cnt = 0;
        for (auto &f : fv) {
          nout << "  " << BSL_RT_VAR_T << " " << var(f) << " = " << BSL_ENV
               << "[" << fv_cnt << "];" << endl;
          fv_cnt++;
        }
        nout << "  return " << nnout.str() << ";" << endl << "}" << endl;

        cons.insert(fv_cnt);
        out << con(fv_cnt) << "(";
        for (auto &f : fv) {
          out << var(f) << ", ";
        }
        out << BSL_RT_MALLOC << "("
            << "sizeof(" << BSL_RT_FUN_T << ") + " << fv_cnt << " * sizeof("
            << BSL_RT_VAR_T << "))"
            << ", " << fun(fn_idx) << ")";
      } break;
      case ExprType::LET: {
        set<string> fv_;
        stringstream nnout;
        codegen_expr_(nnout, e->e2, fv);
        fv.erase(e->x);

        size_t blk_idx = blks.size();
        blks.push_back(make_shared<stringstream>());
        auto &nout = *blks.back();
        nout << BSL_RT_VAR_T << " " << blk(blk_idx) << "(";
        for (auto &f : fv) {
          nout << BSL_RT_VAR_T << " " << var(f) << ", ";
        }
        nout << BSL_RT_VAR_T << " " << var(e->x) << ") {" << endl
             << "  return " << nnout.str() << ";" << endl
             << "}" << endl;

        out << blk(blk_idx) << "(";
        for (auto &f : fv) {
          out << var(f) << ", ";
        }
        codegen_expr_(out, e->e1, fv_);
        out << ")";
        fv.insert(fv_.begin(), fv_.end());
      } break;
      case ExprType::REC: {
        map<string, set<string>> fvs;
        map<string, stringstream> nnouts;
        for (auto &xe : e->xes) {
          auto &nnout = nnouts[xe.first];
          stringstream nnnout;
          codegen_expr_(nnnout, xe.second->e, fvs[xe.first]);
          fvs[xe.first].erase(xe.second->x);

          size_t fn_idx = fns.size();
          fns.push_back(make_shared<stringstream>());
          auto &nout = *fns.back();
          nout << BSL_RT_VAR_T << " " << fun(fn_idx) << "(" << BSL_RT_VAR_T
               << " " << var(xe.second->x) << ", " << BSL_RT_VAR_T << " "
               << BSL_ENV << "[]) {" << endl;
          size_t fv_cnt = 0;
          for (auto &f : fvs[xe.first]) {
            nout << "  " << BSL_RT_VAR_T << " " << var(f) << " = " << BSL_ENV
                 << "[" << fv_cnt << "];" << endl;
            fv_cnt++;
          }
          nout << "  return " << nnnout.str() << ";" << endl << "}" << endl;

          cons.insert(fv_cnt);
          nnout << con(fv_cnt) << "(";
          for (auto &f : fvs[xe.first]) {
            nnout << var(f) << ", ";
          }
          nnout << var(xe.first) << ", " << fun(fn_idx) << ")";
        }

        stringstream nnout;
        codegen_expr_(nnout, e->e, fv);

        for (auto &fv_ : fvs) {
          fv.insert(fv_.second.begin(), fv_.second.end());
        }
        for (auto &xe : e->xes) {
          fv.erase(xe.first);
        }

        size_t blk_idx = blks.size();
        blks.push_back(make_shared<stringstream>());
        auto &nout = *blks.back();
        nout << BSL_RT_VAR_T << " " << blk(blk_idx) << "(";
        {
          bool first = true;
          for (auto &f : fv) {
            if (!first) {
              nout << ", ";
            }
            nout << BSL_RT_VAR_T << " " << var(f);
            first = false;
          }
        }
        nout << ") {" << endl;
        for (auto &xe : e->xes) {
          nout << "  " << BSL_RT_VAR_T << " " << var(xe.first) << " = "
               << BSL_RT_MALLOC << "("
               << "sizeof(" << BSL_RT_FUN_T << ") + " << fvs[xe.first].size()
               << " * sizeof(" << BSL_RT_VAR_T << "));" << endl;
        }
        for (auto &xe : e->xes) {
          nout << "  " << var(xe.first) << " = " << nnouts[xe.first].str()
               << ";" << endl;
        }
        nout << "  return " << nnout.str() << ";" << endl << "}" << endl;

        out << blk(blk_idx) << "(";
        {
          bool first = true;
          for (auto &f : fv) {
            if (!first) {
              out << ", ";
            }
            out << var(f);
            first = false;
          }
        }
        out << ")";
      } break;
      case ExprType::CASE: {
        set<string> fv_;
        map<string, set<string>> fvs;
        map<string, stringstream> nouts;
        for (auto &pe : e->pes) {
          auto &nout = nouts[pe.first];
          codegen_expr_(nout, pe.second.second, fvs[pe.first]);
          for (auto x : pe.second.first) {
            fvs[pe.first].erase(x);
          }
          fv.insert(fvs[pe.first].begin(), fvs[pe.first].end());
        }

        size_t blk_idx = blks.size();
        blks.push_back(make_shared<stringstream>());
        auto &nout = *blks.back();
        nout << BSL_RT_VAR_T << " " << blk(blk_idx) << "(";
        for (auto &f : fv) {
          nout << BSL_RT_VAR_T << " " << var(f) << ", ";
        }
        nout << BSL_RT_VAR_T << " " << tmp() << ") {" << endl;

        assert(e->pes.size() >= 1);
        auto da = unit->data[unit->cons[e->pes.begin()->first]->data_name];
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
        for (auto &f : fv) {
          out << var(f) << ", ";
        }
        codegen_expr_(out, e->e, fv_);
        fv.insert(fv_.begin(), fv_.end());
        out << ")";
      } break;
      case ExprType::FFI: {
        out << ffi(e->ffi->source, fv);
      } break;
    }
  }
};

#endif

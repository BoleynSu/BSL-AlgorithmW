#ifndef SU_BOLEYN_BSL_EXPR_H
#define SU_BOLEYN_BSL_EXPR_H

#include <memory>
#include <set>
#include <sstream>
#include <string>
#include <utility>
#include <vector>

#include "lex.h"

using namespace std;

struct Mono;
struct Poly;

enum class ExprType { VAR, APP, ABS, LET, REC, CASE, FFI };

struct Expr {
  ExprType T;
  string x;
  shared_ptr<Expr> e1, e2, e;
  vector<pair<string, shared_ptr<Expr> > > xes;
  vector<pair<vector<string>, shared_ptr<Expr> > > pes;
  string ffi;
  shared_ptr<Mono> type;
  shared_ptr<Poly> sig, gadt;
  Position pos;
  set<string> fv;

  string to_string(bool newline = false, size_t indent = 0,
                   const string& indents = "") {
    stringstream s;
    if (newline) {
      s << endl;
      for (size_t i = 0; i < indent; i++) {
        s << indents;
      }
    }
    switch (T) {
      case ExprType::VAR:
        s << x;
        break;
      case ExprType::APP:
        s << e1->to_string(false, indent, indents) << " ("
          << e2->to_string(false, indent, indents) << ")";
        break;
      case ExprType::ABS:
        s << "(\\" << x << "->" << e->to_string(true, indent + 1, indents)
          << ")";
        break;
      case ExprType::LET:
        s << "(let " << x << " = " << e1->to_string(false, indent + 1, indents)
          << " in" << endl
          << e2->to_string(false, indent, indents) << ")";
        break;
      case ExprType::REC: {
        s << "(rec";
        for (size_t i = 0; i < xes.size(); i++) {
          if (i) {
            s << " and ";
          } else {
            s << " ";
          }
          s << xes[i].first << " = "
            << xes[i].second->to_string(false, indent + 1, indents);
        }
        s << " in " << e->to_string(false, indent, indents) << ")";
      } break;
      case ExprType::CASE: {
        s << "case " << e->to_string(false, indent, indents) << " of {" << endl;
        for (size_t i = 0; i < pes.size(); i++) {
          for (size_t i = 0; i < indent + 1; i++) {
            s << indents;
          }
          for (size_t j = 0; j < pes[i].first.size(); j++) {
            s << pes[i].first[j] << (j + 1 == pes[i].first.size() ? "" : " ");
          }
          s << "->" << pes[i].second->to_string(true, indent + 2, indents)
            << (i + 1 == pes.size() ? "" : ";") << endl;
        }
        for (size_t i = 0; i < indent; i++) {
          s << indents;
        }
        s << "}";
      } break;
      case ExprType::FFI:
        s << "ffi `" << ffi << "`";
        break;
    }
    return s.str();
  }
};

#endif

#ifndef SU_BOLEYN_BSL_DS_EXPR_H
#define SU_BOLEYN_BSL_DS_EXPR_H

#include <map>
#include <memory>
#include <set>
#include <sstream>
#include <string>
#include <utility>
#include <vector>

#include "ffi.h"
#include "position.h"
#include "type.h"

using namespace std;

enum class ExprType { VAR, APP, ABS, LET, REC, CASE, FFI };

struct Expr {
  ExprType T;
  string x;
  shared_ptr<Expr> e1, e2, e;
  vector<pair<string, shared_ptr<Expr>>> xes;
  map<string, pair<vector<string>, shared_ptr<Expr>>> pes;
  shared_ptr<Ffi> ffi;
  shared_ptr<Poly> sig, gadt;
  Position pos;
};

string to_string(shared_ptr<Expr> e, size_t indent = 0,
                 const string &indents = "") {
  stringstream s;
  switch (e->T) {
    case ExprType::VAR:
      s << e->x;
      break;
    case ExprType::APP:
      s << to_string(e->e1, indent, indents) << " ("
        << to_string(e->e2, indent, indents) << ")";
      break;
    case ExprType::ABS:
      s << "(\\" << e->x << "->" << endl;
      for (size_t i = 0; i < indent + 1; i++) {
        s << indents;
      }
      s << to_string(e->e, indent + 1, indents) << ")";
      break;
    case ExprType::LET:
      s << "(" << endl;
      for (size_t i = 0; i < indent + 1; i++) {
        s << indents;
      }
      s << "let " << e->x;
      if (e->e1->sig != nullptr) {
        s << ":" << to_string(e->e1->sig);
      }
      s << " = " << to_string(e->e1, indent + 1, indents) << endl;
      for (size_t i = 0; i < indent + 1; i++) {
        s << indents;
      }
      s << "in " << to_string(e->e2, indent + 1, indents) << ")";
      break;
    case ExprType::REC: {
      s << "(" << endl;
      for (size_t i = 0; i < indent + 1; i++) {
        s << indents;
      }
      s << "rec ";
      bool first = true;
      for (auto &xe : e->xes) {
        if (!first) {
          for (size_t i = 0; i < indent + 1; i++) {
            s << indents;
          }
          s << "and ";
        }
        s << xe.first;
        if (xe.second->sig != nullptr) {
          s << ":" << to_string(xe.second->sig);
        }
        s << " = " << to_string(xe.second, indent + 1, indents) << endl;
        first = false;
      }
      for (size_t i = 0; i < indent + 1; i++) {
        s << indents;
      }
      s << "in " << to_string(e->e, indent + 1, indents) << ")";
    } break;
    case ExprType::CASE: {
      s << "case " << to_string(e->e, indent, indents) << " of";
      if (e->gadt != nullptr) {
        s << ":" << to_string(e->gadt);
      }
      s << " {" << endl;
      for (auto &pes_ : e->pes) {
        auto pes = pes_.second;
        for (size_t i = 0; i < indent + 1; i++) {
          s << indents;
        }
        s << pes_.first;
        for (size_t j = 0; j < pes.first.size(); j++) {
          s << " " << pes.first[j];
        }
        s << "->" << to_string(pes.second, indent + 1, indents) << ";" << endl;
      }
      for (size_t i = 0; i < indent; i++) {
        s << indents;
      }
      s << "}";
    } break;
    case ExprType::FFI:
      s << "ffi `" << e->ffi->source << "`";
      break;
  }
  return s.str();
}

#endif

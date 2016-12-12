#ifndef SU_BOLEYN_BSL_EXPR_H
#define SU_BOLEYN_BSL_EXPR_H

#include <cassert>
#include <cstdlib>
#include <memory>
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

  string to_string() {
    switch (T) {
      case ExprType::VAR:
        return x;
      case ExprType::APP:
        return e1->to_string() + " (" + e2->to_string() + ")";
      case ExprType::ABS:
        return "(\\" + x + "->" + e->to_string() + ")";
      case ExprType::LET:
        return "(let " + x + " = " + e1->to_string() + " in " +
               e2->to_string() + ")";
      case ExprType::REC: {
        stringstream s;
        s << "(rec";
        for (size_t i = 0; i < xes.size(); i++) {
          if (i) {
            s << " and ";
          } else {
            s << " ";
          }
          s << xes[i].first << " = " << xes[i].second->to_string();
        }
        s << " in " << e->to_string() << ")";
        return s.str();
      }
      case ExprType::CASE: {
        stringstream s;
        s << "(case " << e->to_string() << " of {";
        for (size_t i = 0; i < pes.size(); i++) {
          for (size_t j = 0; j < pes[i].first.size(); j++) {
            s << pes[i].first[j] << (j + 1 == pes[i].first.size() ? "" : " ");
          }
          s << "->" << pes[i].second->to_string()
            << (i + 1 == pes.size() ? "" : ";");
        }
        s << "})";
        return s.str();
      }
      case ExprType::FFI:
        return ffi;
      default:
        assert(false);
        exit(EXIT_FAILURE);
    }
  }
};

#endif

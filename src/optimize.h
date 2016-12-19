#ifndef SU_BOLEYN_BSL_OPTIMIZE_H
#define SU_BOLEYN_BSL_OPTIMIZE_H

#include <cstdlib>
#include <iostream>
#include <memory>
#include <set>
#include <sstream>
#include <string>
#include <utility>
#include <vector>

#include "ds/expr.h"

using namespace std;
struct Optimizer {
  void calculate_free_variable(shared_ptr<Expr> e) {
    auto &fv = e->fv;
    switch (e->T) {
      case ExprType::VAR: {
        fv.insert(e->x);
      } break;
      case ExprType::APP: {
        calculate_free_variable(e->e1);
        fv.insert(e->e1->fv.begin(), e->e1->fv.end());
        calculate_free_variable(e->e2);
        fv.insert(e->e2->fv.begin(), e->e2->fv.end());
      } break;
      case ExprType::ABS: {
        calculate_free_variable(e->e);
        fv.insert(e->e->fv.begin(), e->e->fv.end());
        fv.erase(e->x);
      } break;
      case ExprType::LET: {
        calculate_free_variable(e->e2);
        fv.insert(e->e2->fv.begin(), e->e2->fv.end());
        fv.erase(e->x);
        calculate_free_variable(e->e1);
        fv.insert(e->e1->fv.begin(), e->e1->fv.end());
      } break;
      case ExprType::REC: {
          for (auto &xe : e->xes) {
          calculate_free_variable(xe.second);
          fv.insert(xe.second->fv.begin(), xe.second->fv.end());
        }
        calculate_free_variable(e->e);
        fv.insert(e->e->fv.begin(), e->e->fv.end());
        for (auto &xe : e->xes) {
          fv.erase(xe.first);
        }
      } break;
      case ExprType::CASE: {
        calculate_free_variable(e->e);
        fv.insert(e->e->fv.begin(), e->e->fv.end());
        for (auto &pes_ : e->pes) {
          auto pes = pes_.second;
          set<string> fvi;
          calculate_free_variable(pes.second);
          fvi.insert(pes.second->fv.begin(), pes.second->fv.end());
          for (size_t i = 0; i < pes.first.size(); i++) {
            fvi.erase(pes.first[i]);
          }
          fv.insert(fvi.begin(), fvi.end());
        }
      } break;
      case ExprType::FFI: {
        size_t idx = 0;
        while (idx < e->ffi.length() &&
               (idx = e->ffi.find("$", idx)) != string::npos) {
          if (++idx < e->ffi.length()) {
            char c = e->ffi[idx];
            if (('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z') || c == '_') {
              string v;
              v.push_back(c);
              while (++idx < e->ffi.length()) {
                c = e->ffi[idx];
                if (!(('0' <= c && c <= '9') || ('A' <= c && c <= 'Z') ||
                      ('a' <= c && c <= 'z') || c == '_' || c == '\'')) {
                  break;
                }
                v.push_back(c);
              }
              fv.insert(v);
            }
          }
        }
      } break;
    }
  }

  shared_ptr<Expr> optimize(shared_ptr<Expr> e) {
    calculate_free_variable(e);
    if (!e->fv.empty()) {
      stringstream s;
      bool first = true;
      for (auto fv : e->fv) {
        if (!first) {
          s << " ,";
        }
        s << fv;
        first = false;
      }
      string data = s.str();
      if (data.length() > 80) {
        data = data.substr(0, 77) + "...";
      }
      cerr << "error: there are free variables at top level" << endl
           << data << endl;
      exit(EXIT_FAILURE);
    }
    return e;
  }
};

#endif

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

#include "data.h"
#include "expr.h"
#include "type.h"

using namespace std;

bool occ(shared_ptr<Mono_>, shared_ptr<Mono_>);

bool occ(shared_ptr<Mono_> a, shared_ptr<Poly_> b) {
  if (b->is_mono) {
    return occ(a, b->tau);
  } else {
    return occ(a, b->sigma);
  }
}

bool occ(shared_ptr<Mono_> a, shared_ptr<Mono_> b) {
  b = find(b);
  if (b->is_const) {
    for (size_t i = 0; i < b->tau.size(); i++) {
      if (occ(a, b->tau[i])) {
        return true;
      }
    }
    return false;
  } else {
    return a == b;
  }
}

bool unify(shared_ptr<Mono_> a, shared_ptr<Mono_> b, ostream *error,
           set<shared_ptr<Mono_>> *st) {
  a = find(a);
  b = find(b);
  if (st != nullptr) {
    st->insert(a);
  }
  if (a != b) {
    if (is_c(a)) {
      if (is_c(b)) {
        if (a->D == b->D && a->tau.size() == b->tau.size()) {
          for (size_t i = 0; i < a->tau.size(); i++) {
            auto at = inst(a->tau[i]);
            auto bt = inst(b->tau[i]);
            if (!unify(at, bt, error, st)) {
              return false;
            }
            shared_ptr<map<string, shared_ptr<Poly_>>> context =
                make_shared<map<string, shared_ptr<Poly_>>>();
            (*context)["a"] = a->tau[i];
            (*context)["b"] = b->tau[i];
            a->tau[i] = gen(context, at);
            b->tau[i] = gen(context, bt);
          }
          return true;
        } else {
          if (error != nullptr) {
            (*error) << "type error: " << to_string(a) << " /= " << to_string(b)
                     << endl;
          }
          return false;
        }
      } else if (is_f(b)) {
        if (occ(b, a)) {
          if (error != nullptr) {
            (*error) << "type error: " << to_string(a) << " ~ " << to_string(b)
                     << endl;
          }
          return false;
        } else {
          if (st != nullptr && st->count(b) && a != b) {
            if (error != nullptr) {
              (*error) << "type error: " << to_string(a) << " !< "
                       << to_string(b) << endl;
            }
            return false;
          } else {
            b->par = a;
            return true;
          }
        }
      } else {
        if (error != nullptr) {
          (*error) << "type error: " << to_string(a) << " /= " << to_string(b)
                   << endl;
        }
        return false;
      }
    } else if (is_f(a)) {
      if (is_c(b)) {
        if (occ(a, b)) {
          if (error != nullptr) {
            (*error) << "type error: " << to_string(a) << " ~ " << to_string(b)
                     << endl;
          }
          return false;
        } else {
          if (st != nullptr) {
            if (error != nullptr) {
              (*error) << "type error: " << to_string(a) << " !< "
                       << to_string(b) << endl;
            }
            return false;
          } else {
            a->par = b;
            return true;
          }
        }
      } else if (is_f(b)) {
        if (st != nullptr && st->count(b) && a != b) {
          if (error != nullptr) {
            (*error) << "type error: " << to_string(a) << " !< " << to_string(b)
                     << endl;
          }
          return false;
        } else {
          b->par = a;
          return true;
        }
      } else {
        if (st != nullptr) {
          if (error != nullptr) {
            (*error) << "type error: " << to_string(a) << " !< " << to_string(b)
                     << endl;
          }
          return false;
        } else {
          a->par = b;
          return true;
        }
      }
    } else {
      if (is_c(b)) {
        if (error != nullptr) {
          (*error) << "type error: " << to_string(a) << " /= " << to_string(b)
                   << endl;
        }
        return false;
      } else if (is_f(b)) {
        if (st != nullptr && st->count(b) && a != b) {
          if (error != nullptr) {
            (*error) << "type error: " << to_string(a) << " !< " << to_string(b)
                     << endl;
          }
          return false;
        } else {
          b->par = a;
          return true;
        }
      } else {
        if (error != nullptr) {
          (*error) << "type error: " << to_string(a) << " /= " << to_string(b)
                   << endl;
        }
        return false;
      }
    }
  } else {
    return true;
  }
}

void infer(shared_ptr<Expr> expr,
           shared_ptr<map<string, shared_ptr<Poly_>>> context,
           pair<shared_ptr<map<string, shared_ptr<Data>>>,
                shared_ptr<map<string, shared_ptr<Constructor>>>> &dnc) {
  switch (expr->T) {
    case ExprType::VAR:
      if (context->count(expr->x)) {
        expr->type = inst((*context)[expr->x]);
      } else {
        cerr << "type error: " << expr->x << " is not in context" << endl;
        exit(EXIT_FAILURE);
      }
      break;
    case ExprType::APP: {
      infer(expr->e1, context, dnc);
      infer(expr->e2, context, dnc);
      expr->type = new_forall_var();
      auto t = new_const_var("->");
      t->tau.push_back(to_poly(expr->e2->type));
      t->tau.push_back(to_poly(expr->type));
      if (!unify(expr->e1->type, t, &cerr, nullptr)) {
        exit(EXIT_FAILURE);
      }
      break;
    }
    case ExprType::ABS: {
      auto tau = new_forall_var();
      auto contextx = context->count(expr->x) ? (*context)[expr->x] : nullptr;
      (*context)[expr->x] = to_poly(tau);
      infer(expr->e, context, dnc);
      expr->type = new_const_var("->");
      expr->type->tau.push_back(to_poly(tau));
      expr->type->tau.push_back(to_poly(expr->e->type));
      if (contextx == nullptr) {
        context->erase(expr->x);
      } else {
        (*context)[expr->x] = contextx;
      }
      break;
    }
    case ExprType::LET: {
      infer(expr->e1, context, dnc);
      auto contextx = context->count(expr->x) ? (*context)[expr->x] : nullptr;
      if (expr->e1->sig != nullptr) {
        (*context)[expr->x] = expr->e1->sig;
      } else {
        (*context)[expr->x] = gen(context, expr->e1->type);
      }
      // cerr << "//" << expr->x << " : " << to_string((*context)[expr->x])
      //     << endl;
      infer(expr->e2, context, dnc);
      expr->type = expr->e2->type;
      if (contextx == nullptr) {
        context->erase(expr->x);
      } else {
        (*context)[expr->x] = contextx;
      }
      break;
    }
    case ExprType::REC: {
      vector<shared_ptr<Mono_>> taus_1;
      vector<shared_ptr<Poly_>> contextx_1;
      for (size_t i = 0; i < expr->xes.size(); i++) {
        if (context->count(expr->xes[i].first)) {
          contextx_1.push_back((*context)[expr->xes[i].first]);
        } else {
          contextx_1.push_back(nullptr);
        }
        taus_1.push_back(new_forall_var());
        if (expr->xes[i].second->sig != nullptr) {
          (*context)[expr->xes[i].first] = expr->xes[i].second->sig;
        } else {
          (*context)[expr->xes[i].first] = to_poly(taus_1[i]);
        }
      }
      for (size_t i = 0; i < expr->xes.size(); i++) {
        infer(expr->xes[i].second, context, dnc);
        if (!unify(expr->xes[i].second->type, taus_1[i], &cerr, nullptr)) {
          exit(EXIT_FAILURE);
        }
      }
      for (size_t i = 0; i < expr->xes.size(); i++) {
        if (contextx_1[i] == nullptr) {
          context->erase(expr->xes[i].first);
        } else {
          (*context)[expr->xes[i].first] = contextx_1[i];
        }
      }
      vector<shared_ptr<Poly_>> taus_2;
      vector<shared_ptr<Poly_>> contextx_2;
      for (size_t i = 0; i < expr->xes.size(); i++) {
        taus_2.push_back(gen(context, expr->xes[i].second->type));
      }
      for (size_t i = 0; i < expr->xes.size(); i++) {
        if (context->count(expr->xes[i].first)) {
          contextx_2.push_back((*context)[expr->xes[i].first]);
        } else {
          contextx_2.push_back(nullptr);
        }
        (*context)[expr->xes[i].first] = taus_2[i];
        // cerr << "//" << expr->xes[i].first << " : "
        //     << to_string((*context)[expr->xes[i].first]) << endl;
      }
      infer(expr->e, context, dnc);
      expr->type = expr->e->type;
      for (size_t i = 0; i < expr->xes.size(); i++) {
        if (contextx_2[i] == nullptr) {
          context->erase(expr->xes[i].first);
        } else {
          (*context)[expr->xes[i].first] = contextx_2[i];
        }
      }
      break;
    }
    case ExprType::CASE: {
      map<string, shared_ptr<Poly_>> fns;
      for (auto &pes_ : expr->pes) {
        auto pes = pes_.second;
        auto tau = inst((*context)[pes_.first]);
        vector<shared_ptr<Mono_>> taus_1;
        vector<shared_ptr<Poly_>> contextx_1;
        for (size_t i = 0; i < pes.first.size(); i++) {
          taus_1.push_back(new_forall_var());
          auto t1 = new_const_var("->"), t2 = new_forall_var();
          t1->tau.push_back(to_poly(taus_1[i]));
          t1->tau.push_back(to_poly(t2));
          if (!unify(t1, tau, &cerr, nullptr)) {
            exit(EXIT_FAILURE);
          }
          tau = t2;
        }
        shared_ptr<Mono_> fn = make_shared<Mono_>();
        fn->is_const = true;
        fn->D = "->";
        fn->tau.push_back(to_poly(tau));
        for (size_t i = 0; i < pes.first.size(); i++) {
          if (context->count(pes.first[i])) {
            contextx_1.push_back((*context)[pes.first[i]]);
          } else {
            contextx_1.push_back(nullptr);
          }
          (*context)[pes.first[i]] = to_poly(taus_1[i]);
        }
        infer(pes.second, context, dnc);
        for (size_t i = 0; i < pes.first.size(); i++) {
          if (contextx_1[i] == nullptr) {
            context->erase(pes.first[i]);
          } else {
            (*context)[pes.first[i]] = contextx_1[i];
          }
        }
        fn->tau.push_back(to_poly(pes.second->type));
        fns[pes_.first] = gen(context, fn);
      }

      if (expr->gadt == nullptr) {
        shared_ptr<Mono_> gadt = new_forall_var();
        for (auto &fn : fns) {
          if (!unify(gadt, inst(fn.second), &cerr, nullptr)) {
            exit(EXIT_FAILURE);
          }
        }
        expr->gadt = gen(context, gadt);
        auto t = get_mono(expr->gadt);
        assert(t->is_const && t->D == "->" && get_mono(t->tau[0])->is_const &&
               dnc.first->count(get_mono(t->tau[0])->D));
      } else {
        auto t = get_mono(expr->gadt);
        if (!(t->is_const && t->D == "->" && get_mono(t->tau[0])->is_const &&
              dnc.first->count(get_mono(t->tau[0])->D))) {
          cerr << "type error: invaliad signature for case expression" << endl
               << to_string(expr->gadt) << endl;
          exit(EXIT_FAILURE);
        }
      }
      // for (auto& fn : fns) {
      //  cerr << "//case " << fn.first << " : " << to_string(fn.second) <<
      //  endl;
      //}
      // cerr << "//: " << to_string(expr->gadt) << endl;
      auto c = (*dnc.first)[(*dnc.second)[fns.begin()->first]->data_name]
                   ->constructors;
      for (size_t i = 0; i < c.size(); i++) {
        auto tau = inst(c[i]->type);
        vector<shared_ptr<Mono_>> taus_1;
        vector<shared_ptr<Poly_>> contextx_1;
        for (size_t j = 0; j < c[i]->arg; j++) {
          taus_1.push_back(new_forall_var());
          auto t1 = new_const_var("->"), t2 = new_forall_var();
          t1->tau.push_back(to_poly(taus_1[j]));
          t1->tau.push_back(to_poly(t2));
          if (!unify(t1, tau, &cerr, nullptr)) {
            exit(EXIT_FAILURE);
          }
          tau = t2;
        }
        shared_ptr<Mono_> fn = new_const_var("->"), ret = new_forall_var();
        fn->tau.push_back(to_poly(tau));
        fn->tau.push_back(to_poly(ret));
        if (unify(fn, inst(expr->gadt), nullptr, nullptr)) {
          if (fns.count(c[i]->name)) {
            set<shared_ptr<Mono_>> st;
            if (!unify(fn, inst(fns[c[i]->name]), &cerr, &st)) {
              exit(EXIT_FAILURE);
            }
          } else {
            cerr << "type error: non-exhaustive patterns `" << c[i]->name << "`"
                 << endl;
            exit(EXIT_FAILURE);
          }
        } else {
          if (fns.count(c[i]->name)) {
            unify(fn, inst(expr->gadt), &cerr, nullptr);
          }
        }
      }
      infer(expr->e, context, dnc);
      expr->type = new_forall_var();
      shared_ptr<Mono_> fn = new_const_var("->");
      fn->tau.push_back(to_poly(expr->e->type));
      fn->tau.push_back(to_poly(expr->type));
      if (!unify(fn, inst(expr->gadt), &cerr, nullptr)) {
        exit(EXIT_FAILURE);
      }
      break;
    }
    case ExprType::FFI: {
      expr->type = new_forall_var();
      break;
    }
  }
  if (expr->sig != nullptr) {
    set<shared_ptr<Mono_>> st;
    if (!unify(inst(expr->sig), inst(gen(context, expr->type)), &cerr, &st)) {
      exit(EXIT_FAILURE);
    }
  }
}

#endif

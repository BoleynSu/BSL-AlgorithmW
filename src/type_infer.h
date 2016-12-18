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
  if (is_c(b)) {
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

bool check(shared_ptr<Poly_>, shared_ptr<Poly_>, size_t l, ostream *);
bool check(shared_ptr<Mono_> a, shared_ptr<Mono_> b, size_t l, ostream *cerr) {
  ::cerr << "check mono" << endl;
  ::cerr << to_string(a) << endl;
  ::cerr << to_string(b) << endl;
  ::cerr << is_c(a) << endl;
  ::cerr << is_c(b) << endl;
  a = find(a);
  b = find(b);
  if (a->level != b->level) {
    (*cerr) << "type error: " << to_string(a) << " /= " << to_string(b) << endl;
    return false;
  } else if (a != b) {
    if (is_c(a)) {
      if (is_c(b)) {
        if (a->D == b->D && a->tau.size() == b->tau.size()) {
          for (size_t i = 0; i < a->tau.size(); i++) {
            if (a->tau[i]->is_mono && b->tau[i]->is_mono) {
              if (!check(get_mono(a->tau[i]), get_mono(b->tau[i]), l, cerr)) {
                return false;
              }
            } else if (a->tau[i]->is_mono || b->tau[i]->is_mono) {
              if (cerr != nullptr) {  // FIXME
                (*cerr) << "type error: " << to_string(a)
                        << (a->tau[i]->is_mono ? " < " : " > ") << to_string(b)
                        << endl;
              }
              return false;
            } else {
              if (!check(a->tau[i], b->tau[i], l, cerr)) {
                return false;
              }
            }
          }
          return true;
        } else {
          if (cerr != nullptr) {
            (*cerr) << "type error: " << to_string(a) << " /= " << to_string(b)
                    << endl;
          }
          return false;
        }
      } else if (is_f(b)) {
        if (occ(b, a)) {
          if (cerr != nullptr) {
            (*cerr) << "type error: " << to_string(a) << " ~ " << to_string(b)
                    << endl;
          }
          return false;
        } else {
          b->par = a;
          return true;
        }
      } else {
        if (cerr != nullptr) {
          (*cerr) << "type error: " << to_string(a) << " /= " << to_string(b)
                  << endl;
        }
        return false;
      }
    } else if (is_f(a)) {
      if (is_c(b)) {
        if (cerr != nullptr) {
          (*cerr) << "type error: " << to_string(a) << " > " << to_string(b)
                  << endl;
        }
        return false;
      } else if (is_f(b)) {
        a->par = b;
        return true;
      } else {
        if (cerr != nullptr) {
          (*cerr) << "type error: " << to_string(a) << " > " << to_string(b)
                  << endl;
        }
        return false;
      }
    } else {
      if (is_c(b)) {
        if (cerr != nullptr) {
          (*cerr) << "type error: " << to_string(a) << " /= " << to_string(b)
                  << endl;
        }
        return false;
      } else if (is_f(b)) {
        b->par = a;
        return true;
      } else {
        if (cerr != nullptr) {
          (*cerr) << "type error: " << to_string(a) << " /= " << to_string(b)
                  << endl;
        }
        return false;
      }
    }
  } else {
    return true;
  }
}

bool check(shared_ptr<Poly_> a, shared_ptr<Poly_> b, size_t l, ostream *cerr) {
  ::cerr << "check" << endl;
  ::cerr << to_string(inst(a)) << endl;
  ::cerr << to_string(inst(b)) << endl;
  return check(inst(a, l + 1), inst(b, l + 1), l + 1, cerr);
}

bool unify(shared_ptr<Mono_> a, shared_ptr<Mono_> b, ostream *cerr) {
  a = find(a);
  b = find(b);
  if (a != b) {
    if (is_c(a)) {
      if (is_c(b)) {
        if (a->D == b->D && a->tau.size() == b->tau.size()) {
          for (size_t i = 0; i < a->tau.size(); i++) {
            if (a->tau[i]->is_mono && b->tau[i]->is_mono) {
              if (!unify(get_mono(a->tau[i]), get_mono(b->tau[i]), cerr)) {
                return false;
              }
            } else if (a->tau[i]->is_mono || b->tau[i]->is_mono) {
              // FIXME
              if (cerr != nullptr) {
                (*cerr) << "type error: " << to_string(a)
                        << " /= " << to_string(b) << endl;
              }
              return false;
            } else {
              if (!check(a->tau[i], b->tau[i], 0, cerr)) {
                return false;
              }
            }
          }
          return true;
        } else {
          if (cerr != nullptr) {
            (*cerr) << "type error: " << to_string(a) << " /= " << to_string(b)
                    << endl;
          }
          return false;
        }
      } else if (is_f(b)) {
        if (occ(b, a)) {
          if (cerr != nullptr) {
            (*cerr) << "type error: " << to_string(a) << " ~ " << to_string(b)
                    << endl;
          }
          return false;
        } else {
          b->par = a;
          return true;
        }
      } else {
        if (cerr != nullptr) {
          (*cerr) << "type error: " << to_string(a) << " /= " << to_string(b)
                  << endl;
        }
        return false;
      }
    } else if (is_f(a)) {
      if (is_c(b)) {
        if (occ(a, b)) {
          if (cerr != nullptr) {
            (*cerr) << "type error: " << to_string(a) << " ~ " << to_string(b)
                    << endl;
          }
          return false;
        } else {
          a->par = b;
          return true;
        }
      } else if (is_f(b)) {
        a->par = b;
        return true;
      } else {
        a->par = b;
        return true;
      }
    } else {
      if (is_c(b)) {
        if (cerr != nullptr) {
          (*cerr) << "type error: " << to_string(a) << " /= " << to_string(b)
                  << endl;
        }
        return false;
      } else if (is_f(b)) {
        b->par = a;
        return true;
      } else {
        if (cerr != nullptr) {
          (*cerr) << "type error: " << to_string(a) << " /= " << to_string(b)
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
           shared_ptr<map<string, shared_ptr<Mono_>>> context,
           pair<shared_ptr<map<string, shared_ptr<Data>>>,
                shared_ptr<map<string, shared_ptr<Constructor>>>> &dnc) {
  switch (expr->T) {
    case ExprType::VAR:
      if (context->count(expr->x)) {
        auto t = (*context)[expr->x];
        expr->type = inst(t);
      } else {
        cerr << "type error: " << expr->x << " is not in context" << endl;
        exit(EXIT_FAILURE);
      }
      break;
    case ExprType::APP: {
      infer(expr->e1, context, dnc);
      infer(expr->e2, context, dnc);
      expr->type = new_poly(new_forall_var());
      auto t = new_const_var("->");
      t->tau.push_back(expr->e2->type);
      t->tau.push_back(expr->type);
      cerr << to_string(expr->e1->type) << endl;
      cerr << to_string(inst(expr->e1->type)) << endl;
      cerr << to_string(t) << endl;
      cerr << to_string(expr->e2->type) << endl;
      if (!unify(inst(expr->e1->type), t, &cerr)) {
        exit(EXIT_FAILURE);
      }
      ::cerr << "end app" << endl;
      break;
    }
    case ExprType::ABS: {
      shared_ptr<Poly_> tau;
      if (expr->sig) {
        auto t = inst(expr->sig);
        if (is_c(t) && t->D == "->" && t->tau[0]->is_mono) {
          tau = t->tau[0];
        } else {
          cerr << "type error: invalid signature for lambda expression" << endl
               << to_string(expr->sig) << endl;
          exit(EXIT_FAILURE);
        }
      } else {
        tau = new_poly(new_forall_var());
      }
      auto contextx = context->count(expr->x) ? (*context)[expr->x] : nullptr;
      (*context)[expr->x] = tau;
      infer(expr->e, context, dnc);
      if (contextx == nullptr) {
        context->erase(expr->x);
      } else {
        (*context)[expr->x] = contextx;
      }
      expr->type = new_poly(new_const_var("->"));
      get_mono(expr->type)->tau.push_back(tau);
      get_mono(expr->type)->tau.push_back(expr->e->type);
      if (expr->sig) {
        check(expr->sig, expr->type, 0, &cerr);
      }
      //      cerr << expr->to_string() << " : " << to_string(expr->type) <<
      //      endl;
      break;
    }
    case ExprType::LET: {
      infer(expr->e1, context, dnc);
      ::cerr << "in let1" << endl;
      auto contextx = context->count(expr->x) ? (*context)[expr->x] : nullptr;
      if (expr->e1->sig != nullptr) {
        (*context)[expr->x] = expr->e1->sig;
      } else if (expr->e1->type->is_mono) {
        (*context)[expr->x] = gen(context, get_mono(expr->e1->type));
      } else {
        (*context)[expr->x] = expr->e1->type;
      }
      ::cerr << "in let2" << endl;
      cerr << "//" << expr->x << " : " << to_string((*context)[expr->x])
           << endl;
      infer(expr->e2, context, dnc);
      if (contextx == nullptr) {
        context->erase(expr->x);
      } else {
        (*context)[expr->x] = contextx;
      }
      if (expr->e2->type->is_mono) {
        expr->type = gen(context, get_mono(expr->e2->type));
      } else {
        expr->type = expr->e2->type;
      }
      break;
    }
    case ExprType::REC: {
      vector<shared_ptr<Poly_>> contextx_1;
      for (size_t i = 0; i < expr->xes.size(); i++) {
        if (context->count(expr->xes[i].first)) {
          contextx_1.push_back((*context)[expr->xes[i].first]);
        } else {
          contextx_1.push_back(nullptr);
        }
        if (expr->xes[i].second->sig != nullptr) {
          (*context)[expr->xes[i].first] = expr->xes[i].second->sig;
        } else {
          (*context)[expr->xes[i].first] = new_poly(new_forall_var());
        }
      }
      for (size_t i = 0; i < expr->xes.size(); i++) {
        infer(expr->xes[i].second, context, dnc);
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
        if (!expr->e1->type->is_mono) {
          taus_2.push_back(gen(context, get_mono(expr->xes[i].second->type)));
        } else {
          taus_2.push_back(expr->xes[i].second->type);
        }
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
      for (size_t i = 0; i < expr->xes.size(); i++) {
        if (contextx_2[i] == nullptr) {
          context->erase(expr->xes[i].first);
        } else {
          (*context)[expr->xes[i].first] = contextx_2[i];
        }
      }
      expr->type = expr->e->type;
      break;
    }
    case ExprType::CASE: {  // TODO FIXME
                            /*     map<string, shared_ptr<Poly_>> fns;
                                 for (auto &pes_ : expr->pes) {
                                   auto pes = pes_.second;
                                   auto tau = inst((*context)[pes_.first]);
                                   vector<shared_ptr<Mono_>> taus_1;
                                   vector<shared_ptr<Poly_>> contextx_1;
                                   for (size_t i = 0; i < pes.first.size(); i++) {
                                     taus_1.push_back(new_forall_var());
                                     auto t1 = new_const_var("->"), t2 = new_forall_var();
                                     t1->tau.push_back(new_poly(taus_1[i]));
                                     t1->tau.push_back(new_poly(t2));
                                     if (!unify(t1, tau, &cerr)) {
                                       exit(EXIT_FAILURE);
                                     }
                                     tau = t2;
                                   }
                                   shared_ptr<Mono_> fn = make_shared<Mono_>();
                                   fn->is_const = true;
                                   fn->D = "->";
                                   fn->tau.push_back(new_poly(tau));
                                   for (size_t i = 0; i < pes.first.size(); i++) {
                                     if (context->count(pes.first[i])) {
                                       contextx_1.push_back((*context)[pes.first[i]]);
                                     } else {
                                       contextx_1.push_back(nullptr);
                                     }
                                     (*context)[pes.first[i]] = new_poly(taus_1[i]);
                                   }
                                   infer(pes.second, context, dnc);
                                   for (size_t i = 0; i < pes.first.size(); i++) {
                                     if (contextx_1[i] == nullptr) {
                                       context->erase(pes.first[i]);
                                     } else {
                                       (*context)[pes.first[i]] = contextx_1[i];
                                     }
                                   }
                                   fn->tau.push_back(new_poly(pes.second->type));
                                   fns[pes_.first] = gen(context, fn);
                                 }

                                 if (expr->gadt == nullptr) {
                                   shared_ptr<Mono_> gadt = new_forall_var();
                                   for (auto &fn : fns) {
                                     if (!unify(gadt, inst(fn.second), &cerr)) {
                                       exit(EXIT_FAILURE);
                                     }
                                   }
                                   expr->gadt = gen(context, gadt);
                                   auto t = get_mono(expr->gadt);
                                   assert(t->is_const && t->D == "->" && get_mono(t->tau[0])->is_const
                               &&
                                          dnc.first->count(get_mono(t->tau[0])->D));
                                 } else {
                                   auto t = get_mono(expr->gadt);
                                   if (!(t->is_const && t->D == "->" && get_mono(t->tau[0])->is_const
                               &&
                                         dnc.first->count(get_mono(t->tau[0])->D))) {
                                     cerr << "type error: invalid signature for case expression" <<
                               endl
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
                                     t1->tau.push_back(new_poly(taus_1[j]));
                                     t1->tau.push_back(new_poly(t2));
                                     if (!unify(t1, tau, &cerr)) {
                                       exit(EXIT_FAILURE);
                                     }
                                     tau = t2;
                                   }
                                   shared_ptr<Mono_> fn = new_const_var("->"), ret = new_forall_var();
                                   fn->tau.push_back(new_poly(tau));
                                   fn->tau.push_back(new_poly(ret));
                                   if (unify(fn, inst(expr->gadt), nullptr)) {
                                     if (fns.count(c[i]->name)) {
                                       check(fn, inst(fns[c[i]->name]));
                                     } else {
                                       cerr << "type error: non-exhaustive patterns `" << c[i]->name
                               << "`"
                                            << endl;
                                       exit(EXIT_FAILURE);
                                     }
                                   } else {
                                     if (fns.count(c[i]->name)) {
                                       unify(fn, inst(expr->gadt), &cerr);
                                     }
                                   }
                                 }
                                 infer(expr->e, context, dnc);
                                 expr->type = new_forall_var();
                                 shared_ptr<Mono_> fn = new_const_var("->");
                                 fn->tau.push_back(new_poly(expr->e->type));
                                 fn->tau.push_back(new_poly(expr->type));
                                 if (!unify(fn, inst(expr->gadt), &cerr)) {
                                   exit(EXIT_FAILURE);
                                 }*/
      break;
    }
    case ExprType::FFI: {
      if (expr->sig != nullptr) {
        expr->type = expr->sig;
      } else {
        expr->type = new_poly(new_forall_var());
      }
      break;
    }
  }
  if (expr->sig != nullptr) {
    check(expr->sig, expr->type, 0, &cerr);
  }
}

#endif

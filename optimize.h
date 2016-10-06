#ifndef SU_BOLEYN_BSL_OPTIMIZE_H
#define SU_BOLEYN_BSL_OPTIMIZE_H

#include <memory>

#include "expr.h"

struct Optimizer {
  shared_ptr<Expr> optimize(shared_ptr<Expr> expr,
                            map<string, shared_ptr<Expr>>& context) {
    cerr << "expr " << expr->to_string() << " " << expr->T << endl;
    if (expr->T == 0) {
      cerr << context[expr->x]->T << endl;
    }
    if (expr->T == 0 && context[expr->x]->T != 0) {
      cerr << "context " << context[expr->x]->to_string() << endl;
    }
    shared_ptr<Expr> ret;
    switch (expr->T) {
      case 0:  // var
        if (context[expr->x]->T != 0) {
          shared_ptr<Expr> tmp = make_shared<Expr>();
          tmp->T = 0;
          tmp->x = expr->x;
          expr = context[expr->x];
          context[tmp->x] = tmp;
          ret = optimize(expr, context);
          context[tmp->x] = expr;
        } else {
          ret = expr;
        }
        break;
      case 1: {  // app
        ret = make_shared<Expr>();
        ret->T = 1;
        ret->e1 = optimize(expr->e1, context);
        if (ret->e1->T == 2) {
          // TODO FIXME
          // let y =1
          // let h = \x -> x in
          // let f = \x -> h x in
          // let h = \x -> x+1 in
          // f y

          // let f = \x -> E in f y == let f = \x -> E in let x = y in E
          shared_ptr<Expr> tmp = make_shared<Expr>();
          tmp->T = 3;
          tmp->x = ret->e1->x;
          tmp->e1 = expr->e2;
          tmp->e2 = ret->e1->e;
          ret = optimize(tmp, context);
        } else {
          ret->e2 = optimize(expr->e2, context);
        }
        break;
      }
      case 2: {  // abs
        ret = make_shared<Expr>();
        ret->T = 2;
        shared_ptr<Expr> contextx = context[expr->x];
        shared_ptr<Expr> tmp = make_shared<Expr>();
        tmp->T = 0;
        tmp->x = expr->x;
        context[expr->x] = tmp;
        ret->x = expr->x;
        ret->e = optimize(expr->e, context);
        if (contextx == nullptr) {
          context.erase(expr->x);
        } else {
          context[expr->x] = contextx;
        }
        break;
      }
      case 3: {  // let
        ret = make_shared<Expr>();
        ret->T = 3;
        ret->x = expr->x;
        ret->e1 = optimize(expr->e1, context);
        shared_ptr<Expr> contextx = context[expr->x];
        context[expr->x] = expr->e1;
        ret->e2 = optimize(expr->e2, context);
        if (contextx != nullptr) {
          context[expr->x] = contextx;
        } else {
          context.erase(expr->x);
        }
        break;
      }
      case 4: {  // rec
        ret = make_shared<Expr>();
        ret->T = 4;
        vector<shared_ptr<Expr>> contextx;
        for (int i = 0; i < expr->xes.size(); i++) {
          contextx.push_back(context[expr->xes[i].first]);
          shared_ptr<Expr> tmp = make_shared<Expr>();
          tmp->T = 0;
          tmp->x = expr->xes[i].first;
          context[expr->xes[i].first] = tmp;
        }
        for (int i = 0; i < expr->xes.size(); i++) {
          ret->xes.push_back(make_pair(expr->xes[i].first,
                                       optimize(expr->xes[i].second, context)));
        }
        for (int i = 0; i < expr->xes.size(); i++) {
          context[expr->xes[i].first] = ret->xes[i].second;
        }
        ret->e = optimize(expr->e, context);
        for (int i = 0; i < expr->xes.size(); i++) {
          if (contextx[i] == nullptr) {
            context.erase(expr->xes[i].first);
          } else {
            context[expr->xes[i].first] = contextx[i];
          }
        }
        break;
      }
      case 5: {  // case of
        ret = make_shared<Expr>();
        ret->T = 5;
        ret->e = optimize(expr->e, context);
        for (int i = 0; i < expr->pes.size(); i++) {
          vector<shared_ptr<Expr>> contextx;
          for (int j = 1; j < expr->pes[i].first.size(); j++) {
            contextx.push_back(context[expr->pes[i].first[j]]);
            shared_ptr<Expr> tmp = make_shared<Expr>();
            tmp->T = 0;
            tmp->x = expr->pes[i].first[j];
            context[expr->pes[i].first[j]] = tmp;
          }
          ret->pes.push_back(make_pair(expr->pes[i].first,
                                       optimize(expr->pes[i].second, context)));
          for (int j = 1; j < expr->pes[i].first.size(); j++) {
            if (contextx[j - 1] == nullptr) {
              context.erase(expr->pes[i].first[j]);
            } else {
              context[expr->pes[i].first[j]] = contextx[j - 1];
            }
          }
        }
        break;
      }
      case 6: {  // ffi
        ret = expr;
        break;
      }
    }
    ret->sig = expr->sig;
    return ret;
  }
};

#endif

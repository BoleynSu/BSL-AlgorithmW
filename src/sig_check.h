#ifndef SU_BOLEYN_BSL_SIG_CHECK_H
#define SU_BOLEYN_BSL_SIG_CHECK_H

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
#include "type.h"

void check(shared_ptr<Constructor> c, shared_ptr<Mono_> p,
           set<shared_ptr<Mono_>> &st, bool m, bool r,
           shared_ptr<map<string, shared_ptr<Data>>> data) {
  if (is_c(p)) {
    if (p->D == "->") {
      assert(p->tau.size() == 2);
      {
        auto tp = p->tau[0];
        while (!tp->is_mono) {
          st.insert(tp->alpha);
          tp = tp->sigma;
        }
        check(c, tp->tau, st, m && false, r, data);
      }
      {
        auto tp = p->tau[1];
        while (!tp->is_mono) {
          st.insert(tp->alpha);
          tp = tp->sigma;
        }
        check(c, tp->tau, st, m && true, r, data);
      }
    } else {
      if (data->count(p->D)) {
        if (m) {
          if (p->D != c->data_name) {
            cerr << "type error: in constructor " << c->name << ":"
                 << to_string(c->type) << endl
                 << "return type is `" << p->D << "` instead of `"
                 << c->data_name << "`" << endl;
            exit(EXIT_FAILURE);
          }
        }
        if (p->tau.size() != (*data)[p->D]->arg) {
          cerr << "type error: in constructor " << c->name << ":"
               << to_string(c->type) << endl
               << "type `" << p->D << "` expects " << (*data)[p->D]->arg
               << " arguments, but gets " << p->tau.size() << endl;
          exit(EXIT_FAILURE);
        }
        for (size_t i = 0; i < p->tau.size(); i++) {
          auto tp = p->tau[i];
          while (!tp->is_mono) {
            st.insert(tp->alpha);
            tp = tp->sigma;
          }
          check(c, tp->tau, st, false, m || r, data);
        }
        if (m) {
          for (auto t : st) {
            if (is_f(t)) {
              (*data)[c->data_name]->exists.push_back(t);
            }
          }
        }
      } else {
        cerr << "type error: in constructor " << c->name << ":"
             << to_string(c->type) << endl
             << "`" << p->D << "` is not a type" << endl;
        exit(EXIT_FAILURE);
      }
    }
  } else {
    if (m) {
      cerr << "type error: in constructor " << c->name << ":"
           << to_string(c->type) << endl
           << "return type is a type variable instead of " << c->data_name
           << endl;
      exit(EXIT_FAILURE);
    }
    if (r) {
      st.erase(p);
    }
  }
}

void check(shared_ptr<map<string, shared_ptr<Data>>> data, shared_ptr<Expr> e) {
  // TODO
}

void check(shared_ptr<map<string, shared_ptr<Data>>> data,
           shared_ptr<map<string, shared_ptr<Constructor>>> cons) {
  for (auto &c : *cons) {
    shared_ptr<Poly_> p = c.second->type;
    set<shared_ptr<Mono_>> st;
    while (!p->is_mono) {
      st.insert(p->alpha);
      p = p->sigma;
    }
    check(c.second, p->tau, st, true, false, data);
  }
}

#endif

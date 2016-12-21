#ifndef SU_BOLEYN_BSL_SIG_CHECK_H
#define SU_BOLEYN_BSL_SIG_CHECK_H

#include <cassert>
#include <cstdlib>
#include <iostream>
#include <memory>
#include <set>
#include <string>
#include <utility>
#include <vector>

#include "ds/data.h"
#include "ds/type.h"
#include "ds/unit.h"

using namespace std;

struct SigChecker {
  shared_ptr<Unit> unit;
  SigChecker(shared_ptr<Unit> unit) : unit(unit) {
    for (auto &c : unit->cons) {
      if (c.second->rank2sig != nullptr) {
        auto p = c.second->rank2sig;
        set<shared_ptr<Mono>> st;
        while (p->is_forall) {
          st.insert(p->alpha);
          p = p->sigma;
        }
        check(c.second, get_mono(p->poly), st, false, false);
        auto mo = p->mono;
        check(c.second, p->mono, st, true, false);
      } else {
        auto p = c.second->sig;
        set<shared_ptr<Mono>> st;
        while (!p->is_mono) {
          st.insert(p->alpha);
          p = p->sigma;
        }
        check(c.second, p->tau, st, true, false);
      }
    }
  }
  void check(shared_ptr<Constructor> c, shared_ptr<Mono> p,
             set<shared_ptr<Mono>> &st, bool m, bool r) {
    if (is_c(p)) {
      if (is_fun(p)) {
        assert(p->tau.size() == 2);
        check(c, p->tau[0], st, m && false, r);
        check(c, p->tau[1], st, m && true, r);
      } else {
        if (unit->has_data(p->D)) {
          if (m) {
            if (p->D != c->data_name) {
              cerr << "type error: in constructor " << c->name << ":"
                   << (c->rank2sig != nullptr ? to_string(c->rank2sig)
                                              : to_string(c->sig))
                   << endl
                   << "return type is `" << p->D << "` instead of `"
                   << c->data_name << "`" << endl;
              exit(EXIT_FAILURE);
            }
          }
          if (p->tau.size() != unit->get_data(p->D)->arg) {
            cerr << "type error: in constructor " << c->name << ":"
                 << (c->rank2sig != nullptr ? to_string(c->rank2sig)
                                            : to_string(c->sig))
                 << endl
                 << "type `" << p->D << "` expects "
                 << unit->get_data(p->D)->arg << " arguments, but gets "
                 << p->tau.size() << endl;
            exit(EXIT_FAILURE);
          }
          for (size_t i = 0; i < p->tau.size(); i++) {
            check(c, p->tau[i], st, false, m || r);
          }
          if (m) {
            for (auto t : st) {
              assert(is_f(t));
              unit->get_data(c->data_name)->exists.push_back(t);
            }
          }
        } else {
          cerr << "type error: in constructor " << c->name << ":"
               << (c->rank2sig != nullptr ? to_string(c->rank2sig)
                                          : to_string(c->sig))
               << endl
               << "`" << p->D << "` is not a type" << endl;
          exit(EXIT_FAILURE);
        }
      }
    } else {
      if (m) {
        cerr << "type error: in constructor " << c->name << ":"
             << (c->rank2sig != nullptr ? to_string(c->rank2sig)
                                        : to_string(c->sig))
             << endl
             << "return type is a type variable instead of " << c->data_name
             << endl;
        exit(EXIT_FAILURE);
      }
      if (r) {
        st.erase(p);
      }
    }
  }
};

#endif

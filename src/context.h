#ifndef SU_BOLEYN_BSL_CONTEXT_H
#define SU_BOLEYN_BSL_CONTEXT_H

#include <cassert>
#include <map>
#include <memory>
#include <string>
#include <utility>
#include <vector>

#include "type.h"

using namespace std;

struct Context : map<string, vector<pair<bool, shared_ptr<void>>>> {
  bool has_poly(const string& key) {
    auto it = find(key);
    if (it != end() && it->second.size() && it->second.back().first) {
      return true;
    } else {
      return false;
    }
  }
  bool has_rank2poly(const string& key) {
    auto it = find(key);
    if (it != end() && it->second.size() && !it->second.back().first) {
      return true;
    } else {
      return false;
    }
  }
  shared_ptr<Poly> get_poly(const string& key) {
    auto it = find(key);
    if (it != end() && it->second.size() && it->second.back().first) {
      return static_pointer_cast<Poly>(it->second.back().second);
    } else {
      return nullptr;
    }
  }
  shared_ptr<Rank2Poly> get_rank2poly(const string& key) {
    auto it = find(key);
    if (it != end() && it->second.size() && !it->second.back().first) {
      return static_pointer_cast<Rank2Poly>(it->second.back().second);
    } else {
      return nullptr;
    }
  }
  void set_poly(const string& key, shared_ptr<Poly> value) {
    (*this)[key].push_back(make_pair(true, static_pointer_cast<void>(value)));
  }
  void set_rank2poly(const string& key, shared_ptr<Rank2Poly> value) {
    (*this)[key].push_back(make_pair(true, static_pointer_cast<void>(value)));
  }
  void unset(const string& key) {
    auto& v = (*this)[key];
    v.pop_back();
    if (v.empty()) {
      erase(key);
    }
  }
};

#endif

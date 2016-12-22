#ifndef SU_BOLEYN_BSL_PARSE_H
#define SU_BOLEYN_BSL_PARSE_H

#include <algorithm>
#include <cstdlib>
#include <iostream>
#include <limits>
#include <map>
#include <memory>
#include <set>
#include <string>
#include <utility>
#include <vector>

#include "ds/data.h"
#include "ds/expr.h"
#include "ds/ffi.h"
#include "ds/position.h"
#include "ds/type.h"
#include "ds/unit.h"
#include "lex.h"

using namespace std;

struct Parser {
  Lexer &lexer;
  Token t;
  shared_ptr<Unit> unit;
  Parser(Lexer &lexer) : lexer(lexer) {}

  bool match(TokenType token_type) {
    while (lexer.look_at(0).token_type == TokenType::HASHBANG ||
           lexer.look_at(0).token_type == TokenType::SPACE ||
           lexer.look_at(0).token_type == TokenType::COMMENT) {
      lexer.next();
    }
    t = lexer.look_at(0);
    return lexer.look_at(0).token_type == token_type;
  }

  bool accept(TokenType token_type) {
    if (match(token_type)) {
      lexer.next();
      return true;
    }
    return false;
  }

  void expect(TokenType token_type) {
    if (!accept(token_type)) {
      string data = lexer.look_at(0).data;
      if (data.length() > 78) {
        data = data.substr(0, 75) + "...";
      }
      cerr << "parser: " << to_string(lexer.look_at(0).position) << " "
           << "expect " << token_type << " but get "
           << lexer.look_at(0).token_type << endl
           << "`" << data << "`" << endl;
      exit(EXIT_FAILURE);
    }
  }

  shared_ptr<Mono> parse_monotype(map<string, shared_ptr<Mono>> &m) {
    auto mo = parse_monotype_(m);
    if (accept(TokenType::RIGHTARROW)) {
      auto t = new_fun();
      t->tau.push_back(mo);
      t->tau.push_back(parse_monotype(m));
      mo = t;
    }
    return mo;
  }

  shared_ptr<Mono> parse_monotype_(map<string, shared_ptr<Mono>> &m) {
    shared_ptr<Mono> mo;
    if (accept(TokenType::IDENTIFIER)) {
      if (m.count(t.data)) {
        mo = m[t.data];
      } else {
        mo = new_const(t.data);
      }
    } else {
      expect(TokenType::LEFT_PARENTHESIS);
      auto po = parse_polytype(m);
      if (po->is_mono) {
        mo = po->tau;
      } else {
        mo = new_const(po);
      }
      expect(TokenType::RIGHT_PARENTHESIS);
    }
    if (is_c(mo) && !is_p(mo) && !is_fun(mo)) {
      auto t1 = mo;
      while (match(TokenType::IDENTIFIER) ||
             match(TokenType::LEFT_PARENTHESIS)) {
        if (accept(TokenType::IDENTIFIER)) {
          if (m.count(t.data)) {
            mo = m[t.data];
          } else {
            mo = new_const(t.data);
          }
        } else {
          expect(TokenType::LEFT_PARENTHESIS);
          mo = parse_monotype(m);
          expect(TokenType::RIGHT_PARENTHESIS);
        }
        t1->tau.push_back(mo);
      }
      mo = t1;
    }
    return mo;
  }

  shared_ptr<Poly> parse_polytype(map<string, shared_ptr<Mono>> &m) {
    if (accept(TokenType::FORALL)) {
      expect(TokenType::IDENTIFIER);
      if (m.count(t.data)) {
        string data = t.data;
        if (data.length() > 78) {
          data = data.substr(0, 75) + "...";
        }
        cerr << "parser: " << to_string(t.position)
             << " type variable names conflict" << endl
             << "`" << data << "`" << endl;
        exit(EXIT_FAILURE);
      }
      string tname = t.data;
      auto alpha = m[tname] = new_forall_var();
      expect(TokenType::DOT);
      auto ret = new_poly(alpha, parse_polytype(m));
      m.erase(tname);
      return ret;
    } else {
      return new_poly(parse_monotype(m));
    }
  }

  shared_ptr<Constructor> parse_constructor() {
    auto c = make_shared<Constructor>();
    expect(TokenType::IDENTIFIER);
    if (unit->data.count(t.data)) {
      string data = t.data;
      if (data.length() > 78) {
        data = data.substr(0, 75) + "...";
      }
      cerr << "parser: " << to_string(t.position)
           << " constructor names conflict" << endl
           << "`" << data << "`" << endl;
      exit(EXIT_FAILURE);
    }
    c->name = t.data;
    expect(TokenType::COLON);
    map<string, shared_ptr<Mono>> m;
    c->sig = parse_polytype(m);
    unit->cons[c->name] = c;
    auto tm = get_mono(c->sig);
    c->arg = 0;
    while (is_fun(tm)) {
      c->arg++;
      tm = tm->tau[1];
    }
    return c;
  }

  shared_ptr<Data> parse_data() {
    auto d = make_shared<Data>();
    expect(TokenType::IDENTIFIER);
    if (unit->data.count(t.data)) {
      string data = t.data;
      if (data.length() > 78) {
        data = data.substr(0, 75) + "...";
      }
      cerr << "parser: " << to_string(t.position) << " type names conflict"
           << endl
           << "`" << data << "`" << endl;
      exit(EXIT_FAILURE);
    }
    d->name = t.data;
    d->arg = 0;
    set<string> st;
    while (!match(TokenType::LEFT_BRACE)) {
      expect(TokenType::IDENTIFIER);
      if (st.count(t.data)) {
        string data = t.data;
        if (data.length() > 78) {
          data = data.substr(0, 75) + "...";
        }
        cerr << "parser: " << to_string(t.position)
             << " type variable names conflict" << endl
             << "`" << data << "`" << endl;
        exit(EXIT_FAILURE);
      }
      st.insert(t.data);
      d->arg++;
    }
    expect(TokenType::LEFT_BRACE);
    while (!accept(TokenType::RIGHT_BRACE)) {
      auto c = parse_constructor();
      c->data_name = d->name;
      d->constructors.push_back(c);
      if (!match(TokenType::RIGHT_BRACE)) {
        expect(TokenType::SEMICOLON);
      }
    }
    unit->data[d->name] = d;
    return d;
  }

  shared_ptr<Expr> parse_expr() {
    auto expr = make_shared<Expr>();
    if (accept(TokenType::LAMBDA)) {
      expr->T = ExprType::ABS;
      expect(TokenType::IDENTIFIER);
      expr->x = t.data;
      expect(TokenType::RIGHTARROW);
      expr->e = parse_expr();
    } else if (accept(TokenType::LET)) {
      shared_ptr<Poly> s;
      expr->T = ExprType::LET;
      expect(TokenType::IDENTIFIER);
      expr->x = t.data;
      if (accept(TokenType::COLON)) {
        map<string, shared_ptr<Mono>> m;
        s = parse_polytype(m);
      }
      expect(TokenType::EQUAL);
      expr->e1 = parse_expr();
      expr->e1->sig = s;
      expect(TokenType::IN);
      expr->e2 = parse_expr();
    } else if (accept(TokenType::REC)) {
      expr->T = ExprType::REC;
      do {
        shared_ptr<Poly> s;
        expect(TokenType::IDENTIFIER);
        if (expr->xes.count(t.data)) {
          string data = t.data;
          if (data.length() > 78) {
            data = data.substr(0, 75) + "...";
          }
          cerr << "parser: " << to_string(t.position)
               << " variable names conflict" << endl
               << "`" << data << "`" << endl;
          exit(EXIT_FAILURE);
        }
        string rname = t.data;
        if (accept(TokenType::COLON)) {
          map<string, shared_ptr<Mono>> m;
          s = parse_polytype(m);
        }
        expect(TokenType::EQUAL);
        expr->xes[rname] = parse_expr();
        expr->xes[rname]->sig = s;
      } while (accept(TokenType::AND));
      expect(TokenType::IN);
      expr->e = parse_expr();
    } else {
      expr = parse_expr_();
      if (match(TokenType::LAMBDA) || match(TokenType::LET) ||
          match(TokenType::REC)) {
        auto e1 = parse_expr();
        auto e2 = make_shared<Expr>();
        e2->T = ExprType::APP;
        e2->e1 = expr;
        e2->e2 = e1;
        expr = e2;
      }
    }
    return expr;
  }

  shared_ptr<Expr> parse_expr_() {
    auto expr = parse_expr__();
    while (match(TokenType::IDENTIFIER) || match(TokenType::LEFT_PARENTHESIS) ||
           match(TokenType::CASE) || match(TokenType::FFI)) {
      auto t1 = expr;
      expr = parse_expr__();
      auto t2 = make_shared<Expr>();
      t2->T = ExprType::APP;
      t2->e1 = t1;
      t2->e2 = expr;
      expr = t2;
    }
    return expr;
  }

  shared_ptr<Ffi> parse_ffi() {
    expect(TokenType::FFI);
    auto ffi = make_shared<Ffi>();
    stringstream s(t.data);
    s.get();
    s.get();
    s.get();
    string sep;
    s >> sep;
    size_t a = t.data.find(sep);
    ffi->source =
        t.data.substr(a + sep.size(), t.data.size() - (a + 2 * sep.size()));
    size_t idx = 0;
    while (idx < ffi->source.length() &&
           (idx = ffi->source.find("$", idx)) != string::npos) {
      if (++idx < ffi->source.length()) {
        char c = ffi->source[idx];
        if (('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z') || c == '_') {
          string v;
          v.push_back(c);
          idx++;
          while (idx < ffi->source.length()) {
            c = ffi->source[idx];
            if (!(('0' <= c && c <= '9') || ('A' <= c && c <= 'Z') ||
                  ('a' <= c && c <= 'z') || c == '_' || c == '\'')) {
              break;
            }
            v.push_back(c);
            idx++;
          }
        } else {
          string data = t.data;
          if (data.length() > 78) {
            data = data.substr(0, 75) + "...";
          }
          cerr << "parser: " << to_string(t.position) << " error in ffi" << endl
               << "`" << data << "`" << endl;
          exit(EXIT_FAILURE);
        }
      } else {
        string data = t.data;
        if (data.length() > 78) {
          data = data.substr(0, 75) + "...";
        }
        cerr << "parser: " << to_string(t.position) << " error in ffi" << endl
             << "`" << data << "`" << endl;
        exit(EXIT_FAILURE);
      }
    }
    return ffi;
  }
  shared_ptr<Expr> parse_expr__() {
    auto expr = make_shared<Expr>();
    if (accept(TokenType::IDENTIFIER)) {
      expr->T = ExprType::VAR;
      expr->x = t.data;
    } else if (accept(TokenType::LEFT_PARENTHESIS)) {
      expr = parse_expr();
      expect(TokenType::RIGHT_PARENTHESIS);
    } else if (accept(TokenType::CASE)) {
      expr->T = ExprType::CASE;
      expr->e = parse_expr();
      expect(TokenType::OF);
      shared_ptr<Poly> g;
      if (accept(TokenType::COLON)) {
        map<string, shared_ptr<Mono>> m;
        g = parse_polytype(m);
      }
      expr->gadt = g;
      expect(TokenType::LEFT_BRACE);
      string data_name;
      do {
        expect(TokenType::IDENTIFIER);
        if (!unit->cons.count(t.data)) {
          string data = t.data;
          if (data.length() > 78) {
            data = data.substr(0, 75) + "...";
          }
          cerr << "parser: " << to_string(t.position)
               << " constructor not found" << endl
               << "`" << data << "`" << endl;
          exit(EXIT_FAILURE);
        }
        if (expr->pes.count(t.data)) {
          string data = t.data;
          if (data.length() > 78) {
            data = data.substr(0, 75) + "...";
          }
          cerr << "parser: " << to_string(t.position)
               << " constructors conflict" << endl
               << "`" << data << "`" << endl;
          exit(EXIT_FAILURE);
        }
        string cname = t.data;
        expr->pes[cname] = make_pair(vector<string>{}, nullptr);
        auto &pes = expr->pes[cname];
        auto c = unit->cons[cname];
        if (data_name.empty()) {
          data_name = c->data_name;
        } else if (data_name != c->data_name) {
          string data = t.data;
          if (data.length() > 78) {
            data = data.substr(0, 75) + "...";
          }
          cerr << "parser: " << to_string(t.position) << " constructor of "
               << data_name << " expected, but found" << endl
               << "`" << data << "`" << endl;
          exit(EXIT_FAILURE);
        }
        set<string> st;
        for (size_t i = 0; i < c->arg; i++) {
          expect(TokenType::IDENTIFIER);
          if (st.count(t.data)) {
            string data = t.data;
            if (data.length() > 78) {
              data = data.substr(0, 75) + "...";
            }
            cerr << "parser: " << to_string(t.position)
                 << " variable names conflict" << endl
                 << "`" << data << "`" << endl;
            exit(EXIT_FAILURE);
          }
          st.insert(t.data);
          pes.first.push_back(t.data);
        }
        expect(TokenType::RIGHTARROW);
        pes.second = parse_expr();
        if (!match(TokenType::RIGHT_BRACE)) {
          expect(TokenType::SEMICOLON);
        }
      } while (!match(TokenType::RIGHT_BRACE));
      expect(TokenType::RIGHT_BRACE);
    } else if (match(TokenType::FFI)) {
      expr->T = ExprType::FFI;
      expr->ffi = parse_ffi();
    }
    return expr;
  }
  shared_ptr<Unit> parse() {
    unit = make_shared<Unit>();
    while (accept(TokenType::DATA)) {
      parse_data();
    }
    unit->expr = parse_expr();
    expect(TokenType::END);
    return unit;
  }
};

#endif

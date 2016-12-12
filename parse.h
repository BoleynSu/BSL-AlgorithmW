#ifndef SU_BOLEYN_BSL_PARSE_H
#define SU_BOLEYN_BSL_PARSE_H

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
#include "lex.h"
#include "type.h"

using namespace std;

struct Parser {
  Lexer& lexer;
  Token t;
  shared_ptr<map<string, shared_ptr<Data>>> data_decl;

  Parser(Lexer& lexer) : lexer(lexer) {}

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
      cerr << "parser: " << lexer.look_at(0).position << " "
           << "expect " << token_type << " but get "
           << lexer.look_at(0).token_type << endl
           << "`" << data << "`" << endl;
      exit(EXIT_FAILURE);
    }
  }

  shared_ptr<Mono> parse_monotype(map<string, shared_ptr<Mono>>& m) {
    shared_ptr<Mono> mo = parse_monotype_(m);
    if (accept(TokenType::RIGHTARROW)) {
      auto t = make_shared<Mono>();
      t->is_const = true;
      t->D = "->";
      t->tau.push_back(mo);
      t->tau.push_back(parse_monotype(m));
      mo = t;
    }
    return mo;
  }

  shared_ptr<Mono> parse_monotype_(map<string, shared_ptr<Mono>>& m) {  // FIXME
    shared_ptr<Mono> mo;
    if (accept(TokenType::IDENTIFIER)) {
      if (m.count(t.data)) {
        mo = m[t.data];
      } else {
        mo = make_shared<Mono>();
        mo->is_const = true;
        mo->D = t.data;
      }
    } else {
      expect(TokenType::LEFT_PARENTHESIS);
      mo = parse_monotype(m);
      expect(TokenType::RIGHT_PARENTHESIS);
    }
    if (mo->is_const && mo->D != "->") {
      auto t1 = mo;
      while (match(TokenType::IDENTIFIER) ||
             match(TokenType::LEFT_PARENTHESIS)) {
        if (accept(TokenType::IDENTIFIER)) {
          if (m.count(t.data)) {
            mo = m[t.data];
          } else {
            mo = make_shared<Mono>();
            mo->is_const = true;
            mo->D = t.data;
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

  shared_ptr<Poly> parse_polytype(map<string, shared_ptr<Mono>>& m) {
    if (accept(TokenType::FORALL)) {
      expect(TokenType::IDENTIFIER);
      if (m.count(t.data)) {
        string data = t.data;
        if (data.length() > 78) {
          data = data.substr(0, 75) + "...";
        }
        cerr << "parser: " << t.position << " type variable names conflict"
             << endl
             << "`" << data << "`" << endl;
        exit(EXIT_FAILURE);
      }
      auto alpha = m[t.data] = make_shared<Mono>(Mono{false});
      expect(TokenType::DOT);
      return make_shared<Poly>(Poly{true, nullptr, alpha, parse_polytype(m)});
    } else {
      return make_shared<Poly>(Poly{false, parse_monotype(m)});
    }
  }

  pair<string, shared_ptr<Poly>> parse_constructor() {
    pair<string, shared_ptr<Poly>> c;
    expect(TokenType::IDENTIFIER);
    c.first = t.data;
    expect(TokenType::COLON);
    map<string, shared_ptr<Mono>> m;
    c.second = parse_polytype(m);
    return c;
  }

  shared_ptr<Data> parse_data() {
    auto d = make_shared<Data>();
    expect(TokenType::IDENTIFIER);
    d->name = t.data;
    d->arg = 0;
    set<string> st;
    while (!accept(TokenType::WHERE)) {
      expect(TokenType::IDENTIFIER);
      if (st.count(t.data)) {
        string data = t.data;
        if (data.length() > 78) {
          data = data.substr(0, 75) + "...";
        }
        cerr << "parser: " << t.position << " type variable names conflict"
             << endl
             << "`" << data << "`" << endl;
        exit(EXIT_FAILURE);
      }
      st.insert(t.data);
      d->arg++;
    }
    if (accept(TokenType::FFI)) {
      d->is_ffi = true;
      stringstream s(t.data);
      s.get();
      s.get();
      s.get();
      string sep;
      s >> sep;
      int a = t.data.find(sep);
      d->ffi =
          t.data.substr(a + sep.size(), t.data.size() - (a + 2 * sep.size()));
    } else {
      expect(TokenType::LEFT_BRACE);
      d->is_ffi = false;
      while (!accept(TokenType::RIGHT_BRACE)) {
        d->constructors.push_back(parse_constructor());
        if (!match(TokenType::RIGHT_BRACE)) {
          expect(TokenType::SEMICOLON);
        }
      }
    }
    return d;
  }

  shared_ptr<Expr> parse_expr() {
    auto expr = make_shared<Expr>();
    if (accept(TokenType::LAMBDA)) {
      expr->T = 2;
      expect(TokenType::IDENTIFIER);
      expr->x = t.data;
      expect(TokenType::RIGHTARROW);
      expr->e = parse_expr();
    } else if (accept(TokenType::LET)) {
      shared_ptr<Poly> s;
      expr->T = 3;
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
      expr->T = 4;
      do {
        shared_ptr<Poly> s;
        expect(TokenType::IDENTIFIER);
        expr->xes.push_back(make_pair(t.data, nullptr));
        if (accept(TokenType::COLON)) {
          map<string, shared_ptr<Mono>> m;
          s = parse_polytype(m);
        }
        expect(TokenType::EQUAL);
        expr->xes.back().second = parse_expr();
        expr->xes.back().second->sig = s;
      } while (accept(TokenType::AND));
      expect(TokenType::IN);
      expr->e = parse_expr();
    } else {
      expr = parse_expr_();
      if (match(TokenType::LAMBDA) || match(TokenType::LET) ||
          match(TokenType::REC)) {
        auto e1 = parse_expr();
        auto e2 = make_shared<Expr>();
        e2->T = 1;
        e2->e1 = expr;
        e2->e2 = e1;
        expr = e2;
      }
    }
    return expr;
  }

  shared_ptr<Expr> parse_expr_() {
    auto expr = make_shared<Expr>();
    if (accept(TokenType::IDENTIFIER)) {
      expr->T = 0;
      expr->x = t.data;
    } else if (accept(TokenType::LEFT_PARENTHESIS)) {
      expr = parse_expr();
      expect(TokenType::RIGHT_PARENTHESIS);
    } else if (accept(TokenType::CASE)) {
      expr->T = 5;
      expr->e = parse_expr();
      expect(TokenType::OF);
      shared_ptr<Poly> g;
      if (accept(TokenType::COLON)) {
        map<string, shared_ptr<Mono>> m;
        g = parse_polytype(m);
      }
      expr->gadt = g;
      expect(TokenType::LEFT_BRACE);
      while (!match(TokenType::RIGHT_BRACE)) {
        expr->pes.push_back(make_pair(vector<string>{}, nullptr));
        do {
          expect(TokenType::IDENTIFIER);
          expr->pes.back().first.push_back(t.data);
        } while (!accept(TokenType::RIGHTARROW));
        expr->pes.back().second = parse_expr();
        accept(TokenType::SEMICOLON);
      }
      expect(TokenType::RIGHT_BRACE);
    } else if (accept(TokenType::FFI)) {
      expr->T = 6;
      stringstream s(t.data);
      s.get();
      s.get();
      s.get();
      string sep;
      s >> sep;
      int a = t.data.find(sep);
      expr->ffi =
          t.data.substr(a + sep.size(), t.data.size() - (a + 2 * sep.size()));
    }
    while (match(TokenType::IDENTIFIER) || match(TokenType::LEFT_PARENTHESIS) ||
           match(TokenType::CASE) || match(TokenType::FFI)) {
      auto t1 = expr;
      expr = make_shared<Expr>();
      if (accept(TokenType::IDENTIFIER)) {
        expr->T = 0;
        expr->x = t.data;
      } else if (accept(TokenType::LEFT_PARENTHESIS)) {
        expr = parse_expr();
        expect(TokenType::RIGHT_PARENTHESIS);
      } else if (accept(TokenType::CASE)) {
        expr->T = 5;
        expr->e = parse_expr();
        expect(TokenType::OF);
        expect(TokenType::LEFT_BRACE);
        while (!match(TokenType::RIGHT_BRACE)) {
          expr->pes.push_back(make_pair(vector<string>{}, nullptr));
          do {
            expect(TokenType::IDENTIFIER);
            expr->pes.back().first.push_back(t.data);
          } while (!accept(TokenType::RIGHTARROW));
          expr->pes.back().second = parse_expr();
          accept(TokenType::SEMICOLON);
        }
        expect(TokenType::RIGHT_BRACE);
      } else if (accept(TokenType::FFI)) {
        expr->T = 6;
        stringstream s(t.data);
        s.get();
        s.get();
        s.get();
        string sep;
        s >> sep;
        int a = t.data.find(sep);
        expr->ffi =
            t.data.substr(a + sep.size(), t.data.size() - (a + 2 * sep.size()));
      }
      auto t2 = make_shared<Expr>();
      t2->T = 1;
      t2->e1 = t1;
      t2->e2 = expr;
      expr = t2;
    }
    return expr;
  }

  pair<shared_ptr<map<string, shared_ptr<Data>>>, shared_ptr<Expr>> parse() {
    data_decl = make_shared<map<string, shared_ptr<Data>>>();
    while (accept(TokenType::DATA)) {
      auto d = parse_data();
      (*data_decl)[d->name] = d;
    }
    auto expr = parse_expr();
    expect(TokenType::END);
    return make_pair(data_decl, expr);
  }
};

#endif

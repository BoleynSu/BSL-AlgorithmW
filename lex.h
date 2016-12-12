#ifndef SU_BOLEYN_BSL_LEX_H
#define SU_BOLEYN_BSL_LEX_H

#include <cstdio>
#include <deque>
#include <fstream>
#include <iostream>
#include <string>

using namespace std;

struct Position {
  string filename;
  int beginRow, beginColumn, endRow, endColumn;
};
ostream& operator<<(ostream& out, Position& p) {
  out << p.filename << ":[" << p.beginRow << "," << p.beginColumn << "-"
      << p.endRow << "," << p.endColumn << ")";
}

enum TokenType {
  HASHBANG,

  DATA,
  WHERE,
  FORALL,
  DOT,
  COLON,
  RIGHTARROW,
  SEMICOLON,

  LAMBDA,
  LET,
  EQUAL,
  IN,
  REC,
  AND,
  CASE,
  OF,
  FFI,

  LEFT_PARENTHESIS,
  RIGHT_PARENTHESIS,
  LEFT_BRACE,
  RIGHT_BRACE,

  IDENTIFIER,

  SPACE,
  COMMENT,
  END,

  ERROR
};

struct Token {
  TokenType token_type;
  string data;
  Position position;
};

struct Lexer {
  string filename;
  ifstream in;
  deque<Token> tokens;
  Lexer(const string& filename) : filename(filename), in(filename) {
    Position position;
    position.filename = filename;
    position.endRow = 1;
    position.endColumn = 1;
    for (;;) {
      position.beginRow = position.endRow;
      position.beginColumn = position.endColumn;
      TokenType token_type;
      string data;
      char c = in.get();
      if (c == EOF) {
        break;
      } else if (c == ' ' || c == '\t') {
        data.push_back(c);
        position.endColumn++;
      } else if (c == '\n') {
        data.push_back(c);
        position.endColumn++;
        position.endRow++;
        position.endColumn = 1;
      } else if (c == '\r') {
        data.push_back(c);
        position.endColumn++;
        c = in.get();
        if (c != EOF) {
          if (c == '\n') {
            data.push_back(c);
            position.endColumn++;
          } else {
            in.putback(c);
          }
        }
        position.endRow++;
        position.endColumn = 1;
      } else if (('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z') || c == '_' ||
                 c == '\'') {
        for (;;) {
          data.push_back(c);
          position.endColumn++;
          c = in.get();
          if (c == EOF) {
            break;
          }
          if (!(('0' <= c && c <= '9') || ('A' <= c && c <= 'Z') ||
                ('a' <= c && c <= 'z') || c == '_' || c == '\'')) {
            in.putback(c);
            break;
          }
        }
      } else {
        data.push_back(c);
        position.endColumn++;
        if (c == '-') {
          c = in.get();
          if (c != EOF) {
            if (c == '>') {  //"->"
              data.push_back(c);
              position.endColumn++;
            } else if (c == '-') {  //"--"
              data.push_back(c);
              position.endColumn++;
            } else {
              in.putback(c);
            }
          }
        } else if (c == '#') {
          if (position.beginRow == 1 && position.beginColumn == 1) {
            c = in.get();
            if (c != EOF) {
              if (c == '!') {  //"#!"
                data.push_back(c);
                position.endColumn++;
              } else {
                in.putback(c);
              }
            }
          }
        } else if (c == '{') {
          c = in.get();
          if (c != EOF) {
            if (c == '-') {  //"{-"
              data.push_back(c);
              position.endColumn++;
            } else {
              in.putback(c);
            }
          }
        }
      }
      if (data == "#!") {
        for (;;) {
          c = in.get();
          if (c != EOF) {
            if (c == '\n' || c == '\r') {
              in.putback(c);
              break;
            } else {
              data.push_back(c);
              position.endColumn++;
            }
          } else {
            break;
          }
        }
        token_type = HASHBANG;
      } else if (data == "data") {
        token_type = DATA;
      } else if (data == "where") {
        token_type = WHERE;
      } else if (data == "forall") {
        token_type = FORALL;
      } else if (data == ".") {
        token_type = DOT;
      } else if (data == ":") {
        token_type = COLON;
      } else if (data == "where") {
        token_type = WHERE;
      } else if (data == "->") {
        token_type = RIGHTARROW;
      } else if (data == ";") {
        token_type = SEMICOLON;
      } else if (data == "\\") {
        token_type = LAMBDA;
      } else if (data == "let") {
        token_type = LET;
      } else if (data == "=") {
        token_type = EQUAL;
      } else if (data == "in") {
        token_type = IN;
      } else if (data == "rec") {
        token_type = REC;
      } else if (data == "and") {
        token_type = AND;
      } else if (data == "case") {
        token_type = CASE;
      } else if (data == "of") {
        token_type = OF;
      } else if (data == "ffi") {
        token_type = FFI;
        do {
          data.push_back(c);
          c = in.get();
          if (c == EOF) {
            break;
          } else if (c == '\n') {
            position.endRow++;
            position.endColumn = 1;
          } else if (c == '\r') {
            c = in.get();
            if (c != EOF) {
              if (c == '\n') {
                data.push_back(c);
              } else {
                in.putback(c);
              }
            }
            position.endRow++;
            position.endColumn = 1;
          } else {
            position.endColumn++;
          }
        } while (c == ' ' || c == '\t' || c == '\n' || c == '\r');
        if (c == EOF) {
          token_type = ERROR;
        } else {
          string sep;
          do {
            data.push_back(c);
            position.endColumn++;
            sep.push_back(c);
            c = in.get();
            if (c == EOF) {
              break;
            }
          } while (!(c == ' ' || c == '\t' || c == '\n' || c == '\r'));
          if (c == EOF) {
            token_type = ERROR;
          } else {
            for (;;) {
              data.push_back(c);
              if (data.substr(data.size() - sep.size(), sep.size()) == sep) {
                break;
              }
              c = in.get();
              if (c == EOF) {
                break;
              } else if (c == '\n') {
                position.endRow++;
                position.endColumn = 1;
              } else if (c == '\r') {
                c = in.get();
                if (c != EOF) {
                  if (c == '\n') {
                    data.push_back(c);
                  } else {
                    in.putback(c);
                  }
                }
                position.endRow++;
                position.endColumn = 1;
              } else {
                position.endColumn++;
              }
            }
            if (c == EOF) {
              token_type = ERROR;
            }
          }
        }
      } else if (data == "(") {
        token_type = LEFT_PARENTHESIS;
      } else if (data == ")") {
        token_type = RIGHT_PARENTHESIS;
      } else if (data == "{") {
        token_type = LEFT_BRACE;
      } else if (data == "}") {
        token_type = RIGHT_BRACE;
      } else if (data == "--") {
        for (;;) {
          c = in.get();
          if (c != EOF) {
            if (c == '\n' || c == '\r') {
              in.putback(c);
              break;
            } else {
              data.push_back(c);
              position.endColumn++;
            }
          } else {
            break;
          }
        }
        token_type = COMMENT;
      } else if (data == "{-") {
        token_type = COMMENT;
        for (;;) {
          c = in.get();
          if (c != EOF) {
            if (c == '-') {
              data.push_back(c);
              position.endColumn++;
              c = in.get();
              if (c != EOF) {
                if (c == '}') {  //"-}"
                  data.push_back(c);
                  position.endColumn++;
                  break;
                } else {
                  in.putback(c);
                }
              } else {
                break;
              }
            } else {
              data.push_back(c);
              position.endColumn++;
            }
          } else {
            break;
          }
        }
        if (c == EOF) {
          token_type = ERROR;
        }
      } else {
        c = data[0];
        if (('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z') || c == '_' ||
            c == '\'') {
          token_type = IDENTIFIER;
        } else if (c == ' ' || c == '\t' || c == '\n' || c == '\r') {
          token_type = SPACE;
        } else {
          token_type = ERROR;
        }
      }
      if (token_type == ERROR) {
        if (data.length() > 78) {
          data = data.substr(0, 75) + "...";
        }
        cerr << "lexer: " << position << " token not recognized:" << endl
             << "`" << data << "`" << endl;
        exit(EXIT_FAILURE);
      }
      tokens.push_back({token_type, data, position});
    }
    tokens.push_back({END, "", position});
  }
  Token look_at(int i) { return tokens[i]; }
  Token next() {
    Token token = tokens.front();
    tokens.pop_front();
    return token;
  }
};

#endif

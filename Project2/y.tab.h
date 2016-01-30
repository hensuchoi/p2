/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton interface for Bison's Yacc-like parsers in C

   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     T_Void = 258,
     T_Bool = 259,
     T_Int = 260,
     T_Float = 261,
     T_LessEqual = 262,
     T_GreaterEqual = 263,
     T_EqualOp = 264,
     T_NotEqual = 265,
     T_Dims = 266,
     T_And = 267,
     T_Or = 268,
     T_Inc = 269,
     T_Dec = 270,
     T_Plus = 271,
     T_Dash = 272,
     T_Star = 273,
     T_Slash = 274,
     T_AddAssign = 275,
     T_SubAssign = 276,
     T_MulAssign = 277,
     T_DivAssign = 278,
     T_Equal = 279,
     T_LeftAngle = 280,
     T_RightAngle = 281,
     T_While = 282,
     T_For = 283,
     T_If = 284,
     T_Else = 285,
     T_Return = 286,
     T_Break = 287,
     T_Vec2 = 288,
     T_Vec3 = 289,
     T_Vec4 = 290,
     T_Switch = 291,
     T_Case = 292,
     T_Default = 293,
     T_Do = 294,
     T_Continue = 295,
     T_Mat2 = 296,
     T_Mat3 = 297,
     T_Mat4 = 298,
     T_LeftParen = 299,
     T = 300,
     _RightParen = 301,
     T_Colon = 302,
     T_Semicolon = 303,
     T_LeftBrace = 304,
     T_RightBrace = 305,
     T_Dot = 306,
     T_LeftBracket = 307,
     T_RightBracket = 308,
     T_Identifier = 309,
     T_IntConstant = 310,
     T_FloatConstant = 311,
     T_BoolConstant = 312,
     T_FieldSelection = 313
   };
#endif
/* Tokens.  */
#define T_Void 258
#define T_Bool 259
#define T_Int 260
#define T_Float 261
#define T_LessEqual 262
#define T_GreaterEqual 263
#define T_EqualOp 264
#define T_NotEqual 265
#define T_Dims 266
#define T_And 267
#define T_Or 268
#define T_Inc 269
#define T_Dec 270
#define T_Plus 271
#define T_Dash 272
#define T_Star 273
#define T_Slash 274
#define T_AddAssign 275
#define T_SubAssign 276
#define T_MulAssign 277
#define T_DivAssign 278
#define T_Equal 279
#define T_LeftAngle 280
#define T_RightAngle 281
#define T_While 282
#define T_For 283
#define T_If 284
#define T_Else 285
#define T_Return 286
#define T_Break 287
#define T_Vec2 288
#define T_Vec3 289
#define T_Vec4 290
#define T_Switch 291
#define T_Case 292
#define T_Default 293
#define T_Do 294
#define T_Continue 295
#define T_Mat2 296
#define T_Mat3 297
#define T_Mat4 298
#define T_LeftParen 299
#define T 300
#define _RightParen 301
#define T_Colon 302
#define T_Semicolon 303
#define T_LeftBrace 304
#define T_RightBrace 305
#define T_Dot 306
#define T_LeftBracket 307
#define T_RightBracket 308
#define T_Identifier 309
#define T_IntConstant 310
#define T_FloatConstant 311
#define T_BoolConstant 312
#define T_FieldSelection 313




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
#line 41 "parser.y"
{
    int integerConstant;
    bool boolConstant;
    char *stringConstant;
    double doubleConstant;
    char identifier[MaxIdentLen+1]; // +1 for terminating null
    Decl *decl;
    List<Decl*> *declList;
    IntConstant *intConst;
    FloatConstant *floatConst;
    BoolConstant *boolConst;
    Node *node;
    Identifier *ident;
    Error *error;
    VarDecl *varDecl;
    List<VarDecl*> *varDeclList;
    CompoundExpr *compoundExpr;
    ArithmeticExpr *arithmeticExpr;
    RelationalExpr *relationalExpr;
    EqualityExpr *equalityExpr;
    LogicalExpr *logicalExpr;
    AssignExpr *assignExpr;
    PostfixExpr *postfixExpr;
    VarDeclError *varDeclError;
    FnDecl *fnDecl;
    FormalsError *formalsError;
    Expr *expr;
    ExprError *exprError;
    EmptyExpr *emptyExpr;
    Operator *opt;
    LValue *lValue;
    ArrayAccess *arrayAccess;
    FieldAccess *fieldAccess;
    Call *call;
    ActualsError *actualsError;
    ConditionalStmt *conditionalStmt;
    LoopStmt *loopStmt;
    ForStmt *forStmt;
    WhileStmt *whileStmt;
    IfStmt *ifStmt;
    IfStmtExprError *ifStmtExprError;
    Stmt *stmt;
    StmtBlock *stmtBlock;
    Type *type;
    ArrayType *arrayType;
    BreakStmt *breakStmt;
    ReturnStmt *returnStmt;
    SwitchLabel *switchLabel;
    Case *_case;
    Default *_default;
    SwitchStmt *switchStmt;
    SwitchStmtError *switchStmtError;
    void *none;
    List<Case*> *caseList;
    struct FnHdr fnHdr;
    struct FnHdrParam fnHdrParam;
    struct FnCallHdrParam fnCallHdrParam;
    struct StmtList stmtList;
    struct SelectRest selectRest;
    struct ForRest forRest;
    struct SwitchBody switchBody;
    Program *program;
}
/* Line 1529 of yacc.c.  */
#line 229 "y.tab.h"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif

extern YYSTYPE yylval;

#if ! defined YYLTYPE && ! defined YYLTYPE_IS_DECLARED
typedef struct YYLTYPE
{
  int first_line;
  int first_column;
  int last_line;
  int last_column;
} YYLTYPE;
# define yyltype YYLTYPE /* obsolescent; will be withdrawn */
# define YYLTYPE_IS_DECLARED 1
# define YYLTYPE_IS_TRIVIAL 1
#endif

extern YYLTYPE yylloc;

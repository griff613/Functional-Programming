package edu.colorado.csci3155.project1

/* Inductive Definition for abstract syntax */

sealed trait CalcProgram
case class TopLevel(listOfCmds: List[Cmd]) extends CalcProgram /* List of commands at the "top level" */

sealed trait Expr
case class Const(v: Double) extends Expr
case class Ident(x: String) extends Expr
case class Plus(e1: Expr, e2: Expr) extends Expr
case class Minus(e1: Expr, e2: Expr) extends Expr
case class Mult(e1: Expr, e2: Expr) extends Expr
case class Div(e1: Expr, e2: Expr) extends Expr
case class Geq(e1: Expr, e2: Expr) extends Expr
case class Gt(e1: Expr, e2: Expr) extends Expr
case class Eq(e1: Expr, e2: Expr) extends Expr
case class And(e1: Expr, e2: Expr) extends Expr
case class Or(e1: Expr, e2: Expr) extends Expr
case class Not(e1: Expr) extends Expr
case class IfThenElse(e1: Expr, e2: Expr, e3: Expr) extends Expr

sealed trait Cmd
case class Define(s: String, expr: Expr) extends Cmd /* Bind an identifier to an expression */
case class Display(e: Expr) extends Cmd /* Eval an expression and print result */






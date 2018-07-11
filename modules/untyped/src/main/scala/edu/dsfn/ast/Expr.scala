package edu.dsfn.ast

sealed trait Expr
object Expr {
  final case object NEZero                                       extends Expr
  final case object NETrue                                       extends Expr
  final case object NEFalse                                      extends Expr
  final case class NESucc(t: Expr)                               extends Expr
  final case class NEPred(t: Expr)                               extends Expr
  final case class NEVar(n: String)                              extends Expr
  final case class NEIsZero(t: Expr)                             extends Expr
  final case class NEString(s: String)                           extends Expr
  final case class NEAbs(n: String, expr: Expr)                  extends Expr
  final case class NEApp(expr1: Expr, expr2: Expr)               extends Expr
  final case class NEProj(t: Expr, name: String)                 extends Expr
  final case class NERec(fields: Map[String, Expr])              extends Expr
  final case class NESimpleLet(name: String, e: Expr)            extends Expr
  final case class NELet(name: String, e1: Expr, e2: Expr)       extends Expr
  final case class NEIf(cond: Expr, ifTrue: Expr, ifFalse: Expr) extends Expr
}

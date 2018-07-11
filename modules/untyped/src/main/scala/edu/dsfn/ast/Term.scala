package edu.dsfn.ast

sealed trait Term
object Term {
  final case object TZero                                       extends Term
  final case object TTrue                                       extends Term
  final case object TFalse                                      extends Term
  final case class TSucc(t: Term)                               extends Term
  final case class TPred(t: Term)                               extends Term
  final case class TIsZero(t: Term)                             extends Term
  final case class TString(s: String)                           extends Term
  final case class TApp(t1: Term, t2: Term)                     extends Term
  final case class TProj(t: Term, name: String)                 extends Term
  final case class TVar(index: Int, length: Int)                extends Term
  final case class TAbs(oldName: String, t: Term)               extends Term
  final case class TRec(fields: Map[String, Term])              extends Term
  final case class TSimpleLet(name: String, e: Term)            extends Term
  final case class TLet(name: String, e1: Term, e2: Term)       extends Term
  final case class TIf(cond: Term, ifTrue: Term, ifFalse: Term) extends Term

  implicit class TOps(t: Term) {
    def mapV[A](c: Int)(onVar: (Int, TVar) => Term): Term = {
      def walk(c: Int, t: Term): Term = t match {
        case v: TVar                    => onVar(c, v)
        case TAbs(x, t2)                => TAbs(x, walk(c + 1, t2))
        case TApp(t1, t2)               => TApp(walk(c, t1), walk(c, t2))
        case TSucc(t1)                  => TSucc(walk(c, t1))
        case TPred(t1)                  => TPred(walk(c, t1))
        case TIsZero(t1)                => TIsZero(walk(c, t1))
        case TProj(t1, name)            => TProj(walk(c, t1), name)
        case TRec(fields)               => TRec(fields.map { case (n, e) => n -> walk(c, e) })
        case TSimpleLet(name, e)        => TSimpleLet(name, walk(c + 1, e))
        case TLet(name, e1, e2)         => TLet(name, walk(c, e1), walk(c + 1, e2))
        case TIf(cond, ifTrue, ifFalse) => TIf(walk(c, cond), walk(c, ifTrue), walk(c, ifFalse))
        case term                       => term
      }
      walk(c, t)
    }
  }
}

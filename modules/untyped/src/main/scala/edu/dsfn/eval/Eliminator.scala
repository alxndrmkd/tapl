package edu.dsfn.eval

import cats.MonadError
import cats.implicits._
import cats.mtl.ApplicativeLocal
import edu.dsfn.ast.Expr._
import edu.dsfn.ast.Term._
import edu.dsfn.ast.{Expr, Term}
import edu.dsfn.utils.Context

trait Eliminator[F[_]] {
  implicit def L: ApplicativeLocal[F, Context]
  implicit def E: MonadError[F, String]

  def eliminate(e: Expr): F[Term] = e match {
    case NEZero          => (TZero: Term).pure[F]
    case NETrue          => (TTrue: Term).pure[F]
    case NEFalse         => (TFalse: Term).pure[F]
    case NESucc(e)       => eliminate(e).map(t => TSucc(t): Term)
    case NEPred(e)       => eliminate(e).map(t => TPred(t): Term)
    case NEIsZero(e)     => eliminate(e).map(TIsZero)
    case NEString(s)     => (TString(s): Term).pure[F]
    case NEProj(e, name) => eliminate(e).map(t => TProj(t, name): Term)
    case NERec(fields) =>
      fields.toList
        .traverse[F, (String, Term)]({
          case (k, v) =>
            eliminate(v).map(t => k -> t)
        })
        .map(fs => TRec(fs.toMap))
    case NESimpleLet(name, e1) =>
      eliminate(e1).map(ee => TSimpleLet(name, ee))

    case NELet(name, e1, e2) =>
      (eliminate(e1), eliminate(e2))
        .mapN { case (t1, t2) => TLet(name, t1, t2) }
    case NEIf(cond, ifTrue, ifFalse) =>
      (eliminate(cond), eliminate(ifTrue), eliminate(ifFalse))
        .mapN({
          case (c, itr, ifa) => TIf(c, itr, ifa)
        })

    case NEVar(n) =>
      L.ask.reader { ctx =>
        ctx
          .findIndex(n)
          .fold(E.raiseError[Int](s"Undefined variable $n"))(_.pure[F])
          .map(ind => TVar(ind, ctx.length): Term)
      }.flatten
    case NEAbs(n, expr) =>
      L.local(_.addName(n))(eliminate(expr))
        .map(inner => TAbs(n, inner))

    case NEApp(expr1, expr2) =>
      (eliminate(expr1), eliminate(expr2))
        .mapN(TApp)
  }
}

object Eliminator {
  def apply[F[_]](implicit e: Eliminator[F]): Eliminator[F] = e
  implicit def instance[F[_]](implicit al: ApplicativeLocal[F, Context], me: MonadError[F, String]): Eliminator[F] =
    new Eliminator[F] {
      override implicit val L: ApplicativeLocal[F, Context] = al
      override implicit val E: MonadError[F, String]        = me
    }
}

package edu.dsfn.eval

import cats.MonadError
import cats.implicits._
import cats.mtl.ApplicativeLocal
import edu.dsfn.ast.Term
import edu.dsfn.ast.Term.{TAbs, TApp, TFalse, TIf, TIsZero, TLet, TPred, TProj, TRec, TSimpleLet, TString, TSucc, TTrue, TVar, TZero}
import edu.dsfn.utils.Context

trait Printer[F[_]] {

  implicit def L: ApplicativeLocal[F, Context]
  implicit def E: MonadError[F, String]

  def printTerm(t: Term): F[String] = t match {
    case TZero          => "0".pure[F]
    case TTrue          => "true".pure[F]
    case TFalse         => "false".pure[F]
    case TSucc(t)       => printTerm(t).map(pt => s"succ $pt")
    case TPred(t)       => printTerm(t).map(pt => s"pred $pt")
    case TIsZero(t)     => printTerm(t).map(pt => s"iszero $pt")
    case TString(s)     => s.pure[F]
    case TProj(t, name) => printTerm(t).map(pt => s"$pt.$name")
    case TRec(fields) =>
      fields.toList
        .traverse { case (k, v) => printTerm(v).map(pt => s"$k = $v") }
        .map { fs =>
          s"{ ${fs mkString ", "} }"
        }
    case TSimpleLet(name, e) => printTerm(e).map(pt => s"let $name = $pt")
    case TLet(name, e1, e2) =>
      (printTerm(e1), printTerm(e2))
        .mapN {
          case (pt1, pt2) =>
            s"let $name = $pt1 in $pt2"
        }
    case TIf(cond, ifTrue, ifFalse) =>
      (printTerm(cond), printTerm(ifTrue), printTerm(ifFalse))
        .mapN {
          case (pc, pit, pif) =>
            s"""
               |if $pc
               |then $pit
               |else $pif
             """.stripMargin
        }
    case TVar(index, length) =>
      for {
        ctx <- L.ask.ask
          .ensureOr(ctx => {
            val bindingsStrings =
              ctx.bindings
                .map({
                  case (n, binds) => s"$n -> $binds"
                })
            s"[bad index: $index/$length in { ${bindingsStrings.mkString(", ")} }]"
          })(_.length == length)
        name <- ctx
          .findName(index)
          .fold(E.raiseError[String](s"[bad index: $index $ctx]"))(_.pure[F])
      } yield name

    case TAbs(oldName, inner) =>
      for {
        ctx <- L.ask.ask
        (nctx, name) = ctx.pickFreshName(oldName)
        pt <- L.scope(nctx)(printTerm(inner))
      } yield s"λ$name. $pt"

    case TApp(t1, t2) =>
      (printTerm(t1), printTerm(t2))
        .mapN({ case (pt1, pt2) => s"$pt1 $pt2" })
  }

}

object Printer {
  def apply[F[_]](implicit p: Printer[F]): Printer[F] = p
  implicit def instance[F[_]](implicit al: ApplicativeLocal[F, Context], me: MonadError[F, String]): Printer[F] = new Printer[F] {
    override implicit val L: ApplicativeLocal[F, Context] = al
    override implicit val E: MonadError[F, String]        = me
  }
}

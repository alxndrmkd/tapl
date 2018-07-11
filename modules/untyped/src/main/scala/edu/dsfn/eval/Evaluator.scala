package edu.dsfn.eval

import cats.MonadError
import cats.implicits._
import edu.dsfn.ast.Term
import edu.dsfn.ast.Term._

trait Evaluator[F[_]] {
  implicit def E: MonadError[F, String]
  import Evaluator._

  def evaluate(t: Term): F[Term] = {
    def step(e: Term): F[Term] = {
      e match {
        case TSucc(t1)                              => evaluate(t1)
        case TIf(TTrue, ifTrue, _)                  => (ifTrue: Term).pure[F]
        case TIf(TFalse, _, ifFalse)                => (ifFalse: Term).pure[F]
        case TIf(cond, ifTrue, ifFalse)             => evaluate(cond).map(TIf(_, ifTrue, ifFalse))
        case TApp(TAbs(_, t1), v2) if isVal(v2)     => termSubstTop(v2, t1).pure[F]
        case TApp(v1, t2) if isVal(v1)              => step(t2).map(TApp(v1, _))
        case TApp(t1, t2)                           => step(t1).map(TApp(_, t2))
        case TSimpleLet(name, e1)                   => e1.pure[F]
        case TLet(name, e1, e2) if isVal(e1)        => (TSimpleLet(name, TApp(e2, e1)): Term).pure[F]
        case TLet(name, e1, e2)                     => evaluate(e1).map(t1 => TLet(name, t1, e2))
        case TPred(TZero)                           => (TZero: Term).pure[F]
        case TPred(TSucc(t1)) if isNumericVal(t1)   => t1.pure[F]
        case TPred(t1)                              => evaluate(t1).map(TPred)
        case TIsZero(TZero)                         => (TTrue: Term).pure[F]
        case TIsZero(TSucc(t1)) if isNumericVal(t1) => (TFalse: Term).pure[F]
        case TIsZero(t1)                            => evaluate(t1).map(TIsZero)
        case TProj(v @ TRec(fields), name) if isVal(v) =>
          fields
            .get(name)
            .fold(E.raiseError[Term](s"No field: $name"))(_.pure[F])
        case TProj(t1, name) => evaluate(t1).map(TProj(_, name))
        case TRec(fields) =>
          fields.toList
            .traverse {
              case (k, v) =>
                evaluate(v).map(t => k -> t)
            }
            .map(kvs => TRec(kvs.toMap))
        case _ => E.raiseError[Term]("No rule applies")
      }
    }

    step(t)
      .flatMap(evaluate)
      .recoverWith({
        case "No rule applies" => t.pure[F]
      })
  }
}

object Evaluator {

  def apply[F[_]](implicit e: Evaluator[F]): Evaluator[F] = e
  implicit def instance[F[_]](implicit me: MonadError[F, String]): Evaluator[F] = new Evaluator[F] {
    override implicit val E: MonadError[F, String] = me
  }

  private def isNumericVal(t: Term): Boolean = t match {
    case TZero    => true
    case TSucc(i) => isNumericVal(i)
    case TPred(i) => isNumericVal(i)
    case _        => false
  }

  private def isVal(t: Term): Boolean =
    (t match {
      case _: TAbs | _: TString | TTrue | TFalse => true
      case TRec(fields)                          => fields.forall(f => isVal(f._2))
      case _                                     => false
    }) || isNumericVal(t)

  private def shift(d: Int, t: Term): Term = {
    t.mapV(0) {
      case (c, TVar(index, length)) =>
        if (index >= c) TVar(index + d, length + d)
        else TVar(index, length + d)
    }
  }

  private def substitute(j: Int, s: Term, t: Term): Term = {
    t.mapV(0) {
      case (c, TVar(index, length)) =>
        if (index == j + c) shift(c, s)
        else TVar(index, length)
    }
  }

  private def termSubstTop(s: Term, t: Term): Term =
    shift(-1, substitute(0, shift(1, s), t))
}

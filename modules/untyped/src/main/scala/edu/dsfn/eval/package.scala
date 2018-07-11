package edu.dsfn

import cats.implicits._
import cats.{Monad, MonadError}
import edu.dsfn.ast.Expr
import edu.dsfn.parsing.Parser

package object eval {

  def runExpr[F[_]: Monad](expr: Expr)(
      implicit
      el: Eliminator[F],
      ev: Evaluator[F],
      pr: Printer[F],
  ): F[String] = {
    for {
      tm <- el.eliminate(expr)
      ev <- ev.evaluate(tm)
      pr <- pr.printTerm(ev)
    } yield pr
  }

  def runScript[F[_]](src: String)(
      implicit
      EL: Eliminator[F],
      EV: Evaluator[F],
      PR: Printer[F],
      ME: MonadError[F, String]
  ): F[String] = {
    Parser(src)
      .fold(
        ME.raiseError,
        _.traverse(runExpr[F]).map(_ mkString "\n")
      )
  }

}

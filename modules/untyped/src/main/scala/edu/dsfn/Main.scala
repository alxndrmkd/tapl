package edu.dsfn

import cats.data.Kleisli
import cats.implicits._
import cats.mtl.implicits._
import edu.dsfn.eval._
import edu.dsfn.parsing.Parser
import edu.dsfn.utils.Context

object Main extends App {

  val src0 =
    """
      |λa. λb. a b;
      |λa.λb. b a;
      |(λa.λb. b a);
      |(λy. y) (λz. z);
      |(λl.λm.λn. l m n)(λt.λf. t)(λx. x)(λy. y);
      |(\a.\b.b)(\x.x)(\y.y);
    """.stripMargin


  val src1 =
    """
      |let zz = (λ x. x);
      |
      |true;
      |if false then true else false;
      |
      |let x = true;
      |
      |λ x. x;
      |(λ x. x) (λ x. x);
      |
      |{ x = true, y = (\x .x) }.x;
      |"hello";
      |
      |0;
      |succ (pred 0);
      |iszero (pred (succ (succ 0)));
      |
    """.stripMargin

  type M[A] = Kleisli[Either[String, ?], Context, A]


  println(runScript[M](src0).run(Context.empty))
  println(runScript[M](src1).run(Context.empty))
}

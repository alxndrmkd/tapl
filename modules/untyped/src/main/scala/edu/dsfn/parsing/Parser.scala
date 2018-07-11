package edu.dsfn.parsing

import edu.dsfn.ast.Expr
import edu.dsfn.ast.Expr._

import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.combinator.{ImplicitConversions, PackratParsers}

object Parser extends StdTokenParsers with PackratParsers with ImplicitConversions {

  /***
    *
    * [*] NEZero
    * [*] NETrue
    * [*] NEFalse
    * [*] NESucc
    * [*] NEPred
    * [*] NEVar
    * [*] NEIsZero
    * [*] NEString
    * [*] NEAbs
    * [*] NEApp
    * [ ] NEProj
    * [ ] NERec
    * [*] NESimpleLet
    * [*] NELet
    * [ ] NEIf
    *
    */
  protected class LL extends StdLexical {
    override def letter: Parser[Char] = elem("letter", c => c.isLetter && c != 'λ')
  }

  type Tokens = LL
  val lexical = new LL
  lexical.reserved ++= Seq(
    "true",
    "false",
    "if",
    "then",
    "else",
    "let",
    "in",
    "succ",
    "pred",
    "iszero",
  )
  lexical.delimiters ++= Seq(
    "λ",
    "(",
    ")",
    ";",
    "/",
    ".",
    ":",
    "->",
    "=",
    "<",
    ">",
    "{",
    "}",
    "=>",
    "==>",
    ",",
    "|",
    "\\"
  )

  type P[+A] = PackratParser[A]

  lazy val expr: P[Expr] = proj | let | eif | rec | evar | isZero | eapp | eabs | epar | atom

  lazy val eapp: P[NEApp] = expression ~ expression ^^ NEApp
  lazy val eabs: P[NEAbs] = ("\\" | "λ") ~ ident ~ "." ~ expression ^^ { case _ ~ v ~ _ ~ e => NEAbs(v, e) }
  lazy val evar: P[NEVar] = ident ^^ NEVar
  lazy val epar: P[Expr]  = "(" ~> expression <~ ")" ^^ identity

  lazy val eif: P[Expr] =
    ("if" ~> expression) ~
      ("then" ~> expression) ~
      ("else" ~> expression) ^^ NEIf

  lazy val let: P[Expr] =
    ("let" ~ ident ~ "=" ~ expression ~ "in" ~ expression ^^ { case _ ~ n ~ _ ~ e1 ~ _ ~ e2 => NELet(n, e1, e2) }) |
      ("let" ~ ident ~ "=" ~ expression ^^ { case _ ~ n ~ _ ~ e                             => NESimpleLet(n, e) })

  lazy val atom: P[Expr] =
    ("succ" ~> expression ^^ NESucc) |
      ("pred" ~> expression ^^ NEPred) |
      ("true" ^^ { _ =>
        NETrue
      }) |
      ("false" ^^ { _ =>
        NEFalse
      }) |
      (numericLit ^^ { nl =>
        toNat(nl.toInt)
      }) |
      (stringLit ^^ NEString)

  lazy val isZero: P[Expr] = "iszero" ~> expression ^^ NEIsZero

  lazy val fields: P[Map[String, Expr]] =
    repsep(ident ~ ("=" ~> expression) ^^ { case k ~ v => k -> v }, ",")
      .map { pairs =>
        val m = pairs.toMap
        if(m.size == pairs.size) m
        else throw new Exception(s"Fields in rec must have unique names: ${m.keysIterator.toList.diff(pairs.toList)}")
      }

  lazy val rec: P[Expr] = {
    "{" ~> fields <~ "}" ^^ NERec
  }

  lazy val proj: P[Expr] = {
    (expression <~ ".") ~ ident ^^ NEProj
  }

  def toNat(i: Int): Expr =
    if (i > 0) NESucc(toNat(i - 1))
    else NEZero

  lazy val expression: P[Expr] = rep1(expr) >> {
    case x :: xs => success((xs foldLeft x)(NEApp))
    case _       => failure("No expressions  found :(")
  }

  lazy val expressions: P[List[Expr]] =
    ((expression <~ ";") ~ expressions) ^^ { case e ~ es => e :: es } | success { List.empty }

  def apply(input: String): Either[String, List[Expr]] =
    phrase(expressions)(new lexical.Scanner(input)) match {
      case Success(result, _) => Right(result)
      case Failure(err, n)    => Left(s"Failure: $err at ${n.pos.line} : ${n.pos.column}")
      case Error(err, n)      => Left(s"Error: $err at ${n.pos.line} : ${n.pos.column}")
    }
}

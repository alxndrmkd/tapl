package edu.dsfn.utils

import edu.dsfn.utils.Binding.NameBinding

final case class Context(bindings: List[(String, Binding)])

object Context {

  val empty = Context(List.empty)

  implicit class ContextOps(c: Context) {

    import c._

    val length: Int = bindings.length

    def addBinding(s: String, bind: Binding): Context =
      Context((s, bind) :: bindings)

    def addName(s: String): Context =
      addBinding(s, NameBinding)

    def bound(s: String): Boolean =
      bindings.exists(_._1 == s)

    def pickFreshName(n: String): (Context, String) =
      if (bound(n)) pickFreshName("$" + n)
      else (Context((n, NameBinding) :: bindings), n)

    def findName(i: Int): Option[String] =
      bindings.lift(i).map(_._1)

    def findIndex(s: String): Option[Int] =
      bindings.indexWhere {
        _._1 == s
      } match {
        case -1 => None
        case i  => Some(i)
      }

    def findBinding(i: Int): Option[Binding] =
      bindings.lift(i).map(_._2)
  }

}

package edu.dsfn.utils

import edu.dsfn.ast.Term

sealed trait Binding
object Binding {
  final case object NameBinding        extends Binding
  final case class AbbBinding(t: Term) extends Binding
}

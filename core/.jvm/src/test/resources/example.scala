package example

import simulacrum._

@typeclass trait Semigroup[T] {
  @op("|+|", alias = true)
  def ap@def1@pend(x: T, y: T): T
  def appendCurried(x: T)(y: T): T = append(x, y)
}

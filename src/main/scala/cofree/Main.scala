package cofree

import cats.Eval
import cats.free.Cofree

object Main extends App {
  import cofree.Expr._

  class Parse[A](a: A)(implicit fa: Algebra[Expr, A]) {
    type ExprF = Cofree[Expr, A]

    val expr3: ExprF = Cofree(a, Eval.now(IntValue(10)))
    val expr2: ExprF = Cofree(a, Eval.now(DecValue(2)))
    val expr1: ExprF = Cofree(a, Eval.now(Multiply(expr2, expr3)))
    val expr: ExprF  = Cofree(a, Eval.now(Square(expr1)))

    val run: Eval[A] = Cofree.cata[Expr, A, A](expr)((_, fb) => Eval.later(fa(fb)))
  }

  println(new Parse[String]("").run.value)
  println(new Parse[Double](0.0).run.value)
}

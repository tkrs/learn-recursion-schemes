package cofree

import cats.{Applicative, Eval, Traverse}

final case class Fix[F[_]](fix: F[Fix[F]])

sealed trait Expr[A]

final case class Square[A](expr: A)              extends Expr[A]
final case class Sum[A](expr1: A, expr2: A)      extends Expr[A]
final case class Multiply[A](expr1: A, expr2: A) extends Expr[A]
final case class Divide[A](expr1: A, expr2: A)   extends Expr[A]
final case class IntValue[A](x: Int)             extends Expr[A]
final case class DecValue[A](x: Double)          extends Expr[A]

object Expr {
  type Algebra[F[_], A] = F[A] => A

  implicit val evaluate: Algebra[Expr, Double] = {
    case IntValue(v)      => v.toDouble
    case DecValue(v)      => v
    case Sum(e1, e2)      => e1 + e2
    case Multiply(e1, e2) => e1 * e2
    case Divide(e1, e2)   => e1 / e2
    case Square(e)        => e * e
  }

  implicit val mkString: Algebra[Expr, String] = {
    case IntValue(v)      => v.toString
    case DecValue(v)      => v.toString
    case Sum(e1, e2)      => s"(+ $e1 $e2)"
    case Multiply(e1, e2) => s"(* $e1 $e2)"
    case Divide(e1, e2)   => s"(/ $e1 $e2)"
    case Square(e)        => s"(^ $e)"
  }

  implicit val traverse: Traverse[Expr] = new Traverse[Expr] {
    def traverse[G[_], A, B](fa: Expr[A])(f: A => G[B])(implicit G: Applicative[G]): G[Expr[B]] =
      fa match {
        case IntValue(v)                  => G.pure(IntValue(v))
        case DecValue(v)                  => G.pure(DecValue(v))
        case Sum(e1, e2)                  => G.map2(f(e1), f(e2))(Sum.apply)
        case Multiply(e1, e2) if e1 == e2 => G.map(f(e1))(Square.apply)
        case Multiply(e1, e2)             => G.map2(f(e1), f(e2))(Multiply.apply)
        case Divide(e1, e2)               => G.map2(f(e1), f(e2))(Divide.apply)
        case Square(e)                    => G.map(f(e))(Square.apply)
      }

    def foldLeft[A, B](fa: Expr[A], b: B)(f: (B, A) => B): B = {
      ???
    }

    def foldRight[A, B](fa: Expr[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
      ???
    }
  }

}

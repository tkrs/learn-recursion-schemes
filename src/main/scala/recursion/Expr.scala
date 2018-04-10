package recursion

sealed trait Expr

final case class Square(expr: Expr)                 extends Expr
final case class Sum(expr1: Expr, expr2: Expr)      extends Expr
final case class Multiply(expr1: Expr, expr2: Expr) extends Expr
final case class Divide(expr1: Expr, expr2: Expr)   extends Expr
final case class IntValue(x: Long)                  extends Expr
final case class DecValue(x: Double)                extends Expr

object Expr {
  val evaluate: Expr => Double = {
    case IntValue(v)      => v.toDouble
    case DecValue(v)      => v
    case Sum(e1, e2)      => evaluate(e1) + evaluate(e2)
    case Multiply(e1, e2) => evaluate(e1) * evaluate(e2)
    case Divide(e1, e2)   => evaluate(e1) / evaluate(e2)
    case Square(e)        => val v = evaluate(e); v * v
  }

  val mkString: Expr => String = {
    case IntValue(v)      => v.toString
    case DecValue(v)      => v.toString
    case Sum(e1, e2)      => s"( ${mkString(e1)} ) + ( ${mkString(e2)} )"
    case Multiply(e1, e2) => s"( ${mkString(e1)} ) * ( ${mkString(e2)} )"
    case Divide(e1, e2)   => s"( ${mkString(e1)} ) / ( ${mkString(e2)} )"
    case Square(e)        => s"( ${mkString(e)} )^2"
  }

  val optimize: Expr => Expr = {
    case v @ IntValue(_)              => v
    case v @ DecValue(_)              => v
    case Sum(e1, e2)                  => Sum(optimize(e1), optimize(e2))
    case Multiply(e1, e2) if e1 == e2 => Square(optimize(e1))
    case Multiply(e1, e2)             => Multiply(optimize(e1), optimize(e2))
    case Divide(e1, e2)               => Divide(optimize(e1), optimize(e2))
    case Square(e)                    => Square(optimize(e))
  }
}

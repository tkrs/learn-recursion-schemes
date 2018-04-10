package recursion

object Main extends App {
  val expr =
    Square(
      Sum(Square(IntValue(10)),
          Multiply(DecValue(20.5), Multiply(Square(DecValue(10.5)), Square(DecValue(10.5))))))

  println(s"evaluate: ${Expr.evaluate(expr)}")
  println(s"mkString: ${Expr.mkString(expr)}")
  println("")

  val optimized = Expr.optimize(expr)
  println(s"evaluate: ${Expr.evaluate(optimized)}")
  println(s"mkString: ${Expr.mkString(optimized)}")
}

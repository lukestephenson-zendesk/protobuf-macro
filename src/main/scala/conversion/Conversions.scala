package conversion

import scala.quoted.{Expr, Quotes}

object Conversions {
  def errorMessage(expr: Expr[Any])(using quotes: Quotes): Expr[String] = {
    import quotes.reflect._

    Expr(expr.show)
    //    '{ val error = "Value is missing " + ${Expr(expr.show)}  }
  }
}

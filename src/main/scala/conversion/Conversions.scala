package conversion

import scala.quoted.{Expr, Quotes}

object Conversions {
  def sourceCode(expr: Expr[Any])(using quotes: Quotes): Expr[String] = {
    import quotes.reflect._

    Expr(expr.show)
  }
}

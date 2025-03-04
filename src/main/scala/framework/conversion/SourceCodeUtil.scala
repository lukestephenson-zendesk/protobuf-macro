package framework.conversion

import scala.quoted.{Expr, Quotes}

object SourceCodeUtil {
  def sourceCode(expr: Expr[Any])(using quotes: Quotes): Expr[String] = {
    import quotes.reflect._

    Expr(expr.show)
  }
}

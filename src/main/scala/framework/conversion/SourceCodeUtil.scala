package framework.conversion

import scala.quoted.{Expr, Quotes}

object SourceCodeUtil {
  def sourceCode(expr: Expr[Any])(using quotes: Quotes): Expr[String] = {
    Expr(expr.show)
  }
}

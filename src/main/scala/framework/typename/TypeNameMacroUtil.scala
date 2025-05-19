package framework.typename

import quoted.*

object TypeNameMacroUtil {
  def show[T](using quotes: Quotes)(using Type[T]): Expr[String] = {
    val typeName = Type.show[T]
    Expr(typeName)
  }
}

package framework.typename

object TypeNameMacro {
  inline def apply[A]: String =
    ${ TypeNameMacroUtil.show[A] }
  
}

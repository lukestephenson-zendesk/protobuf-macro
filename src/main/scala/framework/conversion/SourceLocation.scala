package framework.conversion

opaque type SourceLocation = String

object SourceLocation {
  inline def apply(inline value: Any): SourceLocation =
    ${ SourceCodeUtil.sourceCode('value) }
    
  inline def explicit(label: String): SourceLocation = label
}

package framework.conversion

opaque type SourceLocation = String

object SourceLocation {
  // The ${} syntax is known as splicing.  The ' sytnax is known as quoting. Quoting converts the source code into an AST,
  // and splicing injects the modified AST back into the code.
  inline def apply(inline value: Any): SourceLocation =
    ${ SourceCodeUtil.sourceCode('value) }
    
  inline def explicit(label: String): SourceLocation = label
  
  extension (sourceLocation: SourceLocation) {
    def withIndex(index:Int): SourceLocation = sourceLocation + "[" + index + "]"
  }
}

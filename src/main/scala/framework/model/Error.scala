package framework.model

import framework.conversion.SourceLocation

case class Error(message: String, path: List[SourceLocation]) // NonEmptyList[String] ideally

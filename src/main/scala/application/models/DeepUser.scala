package application.models

import scala.compiletime.{constValue, erasedValue, error, summonInline}

case class NiceAddress(street: String, city: String)

case class DeepUser(name: String, age: Int, address: NiceAddress)

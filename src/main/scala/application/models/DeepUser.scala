package application.models

import scala.compiletime.{constValue, erasedValue, error, summonInline}

case class Address(street: String, city: String)

case class DeepUser(name: String, age: Int, address: Address)

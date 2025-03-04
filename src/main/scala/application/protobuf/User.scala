package application.protobuf

case class Address(street: Option[String], city: Option[String]) 
case class User(name: Option[String], age: Option[Int], address: Option[Address])
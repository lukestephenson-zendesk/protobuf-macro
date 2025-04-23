package application.protobuf

case class ProtoAddress(street: Option[String], city: Option[String])

case class ProtoUser(name: Option[String], age: Option[Int], address: Option[ProtoAddress])

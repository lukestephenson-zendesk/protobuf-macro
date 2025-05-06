package application.conversions

import application.models.NiceUser
import application.protobuf.ProtoUser
import framework.conversion.SourceLocation
import framework.model.Error

object UserMappings1 {
  def fromProto(source: ProtoUser): Either[Error, NiceUser] = {
    for {
      // name <- source.name.toRight(Error("Unable to find name.", Nil))
      name <- expected(source.name)
      age <- expected(source.age)
    } yield NiceUser(name, age)
  }

  inline def expected[T](value: Option[T]): Either[Error, T] = {
    val path = SourceLocation(value)

    value.toRight(Error("Unable to find value.", List(path)))
  }
}

package application.conversions

import application.models.NiceUser
import application.protobuf.ProtoUser
import framework.conversion.SourceLocation
import framework.model.Error

extension [T](value: Option[T]) {
  inline def expected: Either[Error, T] = UserMappings1.expected(value)
}

object UserMappings2 {

  def fromProto(source: ProtoUser): Either[Error, NiceUser] = {
    for {
      // name <- expected(source.name)
      name <- source.name.expected
      age <- source.age.expected
    } yield NiceUser(name, age)
  }
}

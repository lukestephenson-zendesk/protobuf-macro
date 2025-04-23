package application.conversions

import application.models.{Address, DeepUser, NiceUser}
import application.protobuf.ProtoUser
import framework.conversion.SourceLocation
import framework.model.Error

extension[T] (value: Option[T]) {
  inline def expected: Either[Error, T] = UserMappings1.expected(value)
}

object UserMappings2 {

  def fromProto(source2: ProtoUser): Either[Error, NiceUser] = {
    for {
      name <- source2.name.expected
      age <- source2.age.expected
    } yield NiceUser(name, age)
  }
}

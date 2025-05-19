package application.conversions

import application.models.NiceUser
import application.protobuf.ProtoUser
import framework.model.Error

object TraditionalUserMapping {
  def fromProto(source: ProtoUser): Either[Error, NiceUser] = {
    for {
      name <- source.name.toRight(Error("Unable to find name.", Nil))
      age <- source.age.toRight(Error("Unable to find age.", Nil))
    } yield NiceUser(name, age)
  }
}

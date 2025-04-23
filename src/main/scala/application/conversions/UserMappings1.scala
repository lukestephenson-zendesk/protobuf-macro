package application.conversions

import application.models.NiceUser
import application.protobuf.ProtoUser
import framework.conversion.SourceLocation
import framework.model.Error

object UserMappings1 {
    def fromProto(source1: ProtoUser): Either[Error, NiceUser] = {
      for {
        name <- expected(source1.name) // source.name.toRight(Error("Unable to find name.", Nil))
        age <- expected(source1.age)
      } yield NiceUser(name, age)
    }

    inline def expected[T](inline value: Option[T]): Either[Error, T] = {
      val path = SourceLocation(value)

      value match {
        case Some(v) => Right(v)
        case None => Left(Error("Unable to find value.", List(path)))
      }
    }
}

package application.conversions

import application.models.{DeepUser, NiceAddress}
import application.protobuf.{ProtoAddress, ProtoUser}
import framework.conversion.SourceLocation
import framework.model.Error

object DeepUserMappings {

  extension [T](inline value: Option[T]) {
    inline def expected: Either[Error, T] = UserMappings1.expected(value)
  }

  extension [T, S](inline value: Option[T]) {
    inline def expectedWith(fn: T => Either[Error, S]): Either[Error, S] = {
      val path = SourceLocation(value)
      val errorOrProto: Either[Error, T] = UserMappings1.expected(value)

      errorOrProto.flatMap(proto => fn(proto).left.map(error => error.copy(path = path :: error.path)))
    }
  }

  def fromAddressProto(source: ProtoAddress): Either[Error, NiceAddress] = {
    for {
      street <- source.street.expected
      city <- source.city.expected
    } yield NiceAddress(street, city)
  }

  def fromProto(source: ProtoUser): Either[Error, DeepUser] = {
    for {
      name <- source.name.expected
      age <- source.age.expected
      address <- source.address.expectedWith(fromAddressProto)
    } yield DeepUser(name, age, address)
  }
}

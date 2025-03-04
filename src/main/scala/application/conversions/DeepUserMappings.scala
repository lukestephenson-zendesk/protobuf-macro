package application.conversions

import application.models.{Address, DeepUser, Mappings}
import application.protobuf.{Address as ProtoAddress, User as ProtoUser}
import framework.conversion.SourceLocation
import framework.model.Error
import Mappings.expected

object DeepUserMappings {

  extension [T](inline value: Option[T]) {
    inline def expected: Either[Error, T] = Mappings.expected(value)
  }
  
  extension [T, S](value: Option[T]) {
    inline def expectedWith(fn: T => Either[Error, S]): Either[Error, S] = {
      val path = SourceLocation(value)
      val errorOrProto: Either[Error, T] = Mappings.expected(value)

      errorOrProto.flatMap { proto =>
        fn(proto).left.map(error => error.copy(path = path :: error.path))
      }
    }
  }

  def fromAddressProto(source: ProtoAddress): Either[Error, Address] = {
    for {
      street <- source.street.expected
      city <- source.city.expected
    } yield Address(street, city)
  }

  def fromProto(source2: ProtoUser): Either[Error, DeepUser] = {
    for {
      name <- source2.name.expected
      age <- source2.age.expected
      address <- source2.address.expectedWith(fromAddressProto)
    } yield DeepUser(name, age, address)
  }
}

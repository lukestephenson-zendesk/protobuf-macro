package application.conversions

import application.models.{DeepUser, NiceAddress}
import application.protobuf.{ProtoAddress, ProtoUser}
import framework.conversion.SourceLocation
import framework.model.Error

import scala.compiletime.{constValue, erasedValue, error, summonInline}

trait Mapper[T, S] {
  def map(value: T)(using sourceLocation: SourceLocation): Either[Error, S]
}

object Mapper {
  given idMapper[A]: Mapper[A, A] with {
    def map(value: A)(using sourceLocation: SourceLocation): Either[Error, A] = Right(value)
  }

  given optionMapper[T, S](using mapper: Mapper[T, S]): Mapper[Option[T], S] with {
    def map(value: Option[T])(using sourceLocation: SourceLocation): Either[Error, S] = value match {
      case Some(v) => mapper.map(v).left.map(error => error.copy(path = sourceLocation :: error.path))
      case None => Left(Error("Unable to find value.", List(sourceLocation)))
    }
  }

  extension [T](inline value: T) {
    inline def as[S](using mapper: Mapper[T, S]): Either[Error, S] = {
      given SourceLocation = SourceLocation(value)

      mapper.map(value)
    }
  }
}

object DeepUserMappings3 {
  import application.conversions.Mapper.as

  given addressMapper: Mapper[ProtoAddress, NiceAddress] with {
    def map(source: ProtoAddress)(using sourceLocation: SourceLocation): Either[Error, NiceAddress] =
      for {
        street <- source.street.as[String]
        city <- source.city.as[String]
      } yield NiceAddress(street, city)
  }

  def fromProto(source: ProtoUser): Either[Error, DeepUser] = {
    for {
      name <- source.name.as[String]
      age <- source.age.as[Int]
      address <- source.address.as[NiceAddress]
    } yield DeepUser(name, age, address)
  }
}

package models

import conversion.Conversions
import models.Mappings.expected
import source.User as ProtoUser
import source.Address as ProtoAddress

import scala.quoted.{Expr, Quotes}

case class Address(street: String, city: String)

case class DeepUser(name: String, age: Int, address: Address)

object DeepUserMappings {

  extension [T, S](value: Option[T]) {
    inline def expectedWith(fn: T => Either[Error, S]): Either[Error, S] = {
      val path = Mappings.small(value)
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

trait Mapper[T, S] {
  def map(value: T)(using sourceLocation: SourceLocation): Either[Error, S]
}

trait SourceLocation {
  def sourceLocation: String
}

object DeepUserMappings3 {

  given idMapper[A]: Mapper[A, A] with {
    def map(value: A)(using sourceLocation: SourceLocation): Either[Error, A] = Right(value)
  }

  given optionMapper[T, S](using mapper: Mapper[T, S]): Mapper[Option[T], S] with {
    def map(value: Option[T])(using sourceLocation: SourceLocation): Either[Error, S] = value match {
      case Some(v) => mapper.map(v).left.map(error => error.copy(path = sourceLocation.sourceLocation :: error.path))
      case None => Left(Error("Unable to find value.", List(sourceLocation.sourceLocation)))
    }
  }

  extension [T](inline value: T) {
    inline def as[S](using mapper: Mapper[T, S]): Either[Error, S] = {
      given SourceLocation with {
        override val sourceLocation: String = Mappings.small(value)
      }
      mapper.map(value)
    }
  }

  extension [T](inline value: Option[T]) {
    inline def expected(using Mapper[Option[T],T]): Either[Error, T] = value.as[T]
  }

  given addressMapper: Mapper[ProtoAddress, Address] with {
    def map(source: ProtoAddress)(using sourceLocation: SourceLocation): Either[Error, Address] =
      for {
        street <- source.street.expected
        city <- source.city.expected
      } yield Address(street, city)
  }

  def fromProto(source2: ProtoUser): Either[Error, DeepUser] = {
    for {
      name <- source2.name.as[String]
      age <- source2.age.as[Int]
      address <- source2.address.as[Address]
    } yield DeepUser(name, age, address)
  }
}
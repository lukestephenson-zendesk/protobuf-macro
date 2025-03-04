package models

import conversion.SourceLocation
import models.Mappings.expected
import protobuf.User as ProtoUser

import scala.quoted.{Expr, Quotes}

case class Error(message: String, path: List[SourceLocation]) // NonEmptyList[String] ideally

case class User(name: String, age: Int)

object Mappings {
  def fromProto(source1: ProtoUser): Either[Error, User] = {
    for {
      name <- expected(source1.name)
      age <- expected(source1.age)
    } yield User(name, age)
  }

  inline def expected[T](inline value: Option[T]): Either[Error, T] = {
    val path = SourceLocation(value)

    value match {
      case Some(v) => Right(v)
      case None => Left(Error("Unable to find value.", List(path)))
    }
  }
  
}

extension[T] (value: Option[T]) {
  inline def expected: Either[Error, T] = Mappings.expected(value)
}

object Mappings2 {

  def fromProto(source2: ProtoUser): Either[Error, User] = {
    for {
      name <- source2.name.expected
      age <- source2.age.expected
    } yield User(name, age)
  }
}
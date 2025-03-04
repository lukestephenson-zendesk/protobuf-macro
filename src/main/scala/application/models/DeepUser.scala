package application.models

import framework.conversion.SourceLocation
import scala.compiletime.{constValue, erasedValue, error, summonInline}
import scala.deriving.Mirror
import scala.quoted.*
import application.protobuf.{User => ProtoUser, Address => ProtoAddress}

case class Address(street: String, city: String)

case class DeepUser(name: String, age: Int, address: Address)

object DeepUserMappings {

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

trait Mapper[T, S] {
  def map(value: T)(using sourceLocation: SourceLocation): Either[Error, S]
}

sealed trait Field[Label <: String, Type]

object Field:
  type FromLabelsAndTypes[Labels <: Tuple, Types <: Tuple] <: Tuple =
    (Labels, Types) match
      case (EmptyTuple, EmptyTuple) => EmptyTuple
      case (labelHead *: labelTail, typeHead *: typeTail) => Field[labelHead, typeHead] *: FromLabelsAndTypes[labelTail, typeTail]

  type TypeForLabel[Label <: String, Fields <: Tuple] =
    Fields match
      case Field[Label, tpe] *: _ => tpe
      case _ *: tail => TypeForLabel[Label, tail]
      case EmptyTuple => Nothing

  type DropByLabel[Label <: String, Fields <: Tuple] <: Tuple =
    Fields match
      case EmptyTuple => EmptyTuple
      case Field[Label, _] *: tail => tail
      case head *: tail => head *: DropByLabel[Label, tail]


opaque type FieldName = String

object FieldName:

  inline def fromLiteralLabel[Label <: String]: FieldName =
    constValue[Label]

  def wrapAll[K, V](map: Map[K, V]): Map[String, V] =
    map.asInstanceOf[Map[String, V]]

object Mapper {

  inline def labels[Labels <: Tuple]: List[FieldName] =
    inline erasedValue[Labels] match
      case _: EmptyTuple => List.empty
      case _: (h *: t) =>
        constValue[h].asInstanceOf[FieldName] :: labels[t]


  inline def transformerForField[
    ToLabel <: String,
    ToType,
    FromFields <: Tuple
  ]: (FieldName, Mapper[?, ?]) =
    inline erasedValue[FromFields] match {
      case _: EmptyTuple =>
        error("Mapper not found for field '" + constValue[ToLabel] + "'")
      case _: (Field[ToLabel, fromType] *: _) =>
        FieldName.fromLiteralLabel[ToLabel] -> summonInline[Mapper[fromType, ToType]]
      case _: (_ *: tail) =>
        transformerForField[ToLabel, ToType, tail]
    }

  inline def transformersForAllFields[FromFields <: Tuple, ToFields <: Tuple]: Map[FieldName, Mapper[?, ?]] =
    inline erasedValue[ToFields] match
      case _: EmptyTuple =>
        Map.empty
      case _: (Field[label, tpe] *: tail) =>
        transformersForAllFields[FromFields, tail] + transformerForField[label, tpe, FromFields]

  inline def unsafeConstructInstance[To](from: Product)(unsafeMapper: (Map[String, ?], FieldName) => Either[Error, ?])(using To: Mirror.ProductOf[To]): Either[Error, To] =
    val labelsToValuesOfFrom: Map[String, Any] = FieldName.wrapAll(from.productElementNames.zip(from.productIterator).toMap)
    val labelIndicesOfTo: Map[FieldName, Int] = labels[To.MirroredElemLabels].zipWithIndex.toMap
    val valueArrayOfTo: Array[Any] = new Array[Any](labelIndicesOfTo.size)

    var failed: Left[Error, ?] = null
    var idx = 0
    while idx < labelIndicesOfTo.size && (failed eq null) do
      val label = labels[To.MirroredElemLabels](idx)
        unsafeMapper(labelsToValuesOfFrom, label) match
          case Left(error) => failed = Left(error)
          case Right(value) => valueArrayOfTo.update(idx, value)
      idx += 1
    end while

    if failed eq null then Right(To.fromProduct(Tuple.fromArray(valueArrayOfTo)))
    else failed.asInstanceOf[Either[Error, To]]

//  inline given [From <: Product, To <: Product](using A: Mirror.ProductOf[From], B: Mirror.ProductOf[To]): Mapper[From, To] =
//    new Mapper[From, To]:
//      override def map(from: From)(using sourceLocation: SourceLocation): Either[Error,To] =
//        val transformers = transformersForAllFields[
//          Field.FromLabelsAndTypes[A.MirroredElemLabels, A.MirroredElemTypes],
//          Field.FromLabelsAndTypes[B.MirroredElemLabels, B.MirroredElemTypes]
//        ]
//        unsafeConstructInstance(from) { (labelsToValuesOfA, label) =>
//          transformers(label)
//            .asInstanceOf[Mapper[Any, Any]]
//            .map(labelsToValuesOfA(label.toString)) // TODO create a new SourceLocation based on the label
//        }

  inline def dervied[From <: Product, To <: Product](using A: Mirror.ProductOf[From], B: Mirror.ProductOf[To]): Mapper[From, To] =
    new Mapper[From, To]:
      override def map(from: From)(using sourceLocation: SourceLocation): Either[Error,To] =
        val transformers = transformersForAllFields[
          Field.FromLabelsAndTypes[A.MirroredElemLabels, A.MirroredElemTypes],
          Field.FromLabelsAndTypes[B.MirroredElemLabels, B.MirroredElemTypes]
        ]
        unsafeConstructInstance(from) { (labelsToValuesOfA, label) =>
          given SourceLocation = SourceLocation.explicit(label.toString)
          
          transformers(label)
            .asInstanceOf[Mapper[Any, Any]]
            .map(labelsToValuesOfA(label.toString)) // TODO create a new SourceLocation based on the label
        }
}



object DeepUserMappings3 {

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

object DeepUserMappings4 {

  given idMapper[A]: Mapper[A, A] with {
    def map(value: A)(using sourceLocation: SourceLocation): Either[Error, A] = Right(value)
  }

  given optionMapper[T, S](using mapper: Mapper[T, S]): Mapper[Option[T], S] with {
    def map(value: Option[T])(using sourceLocation: SourceLocation): Either[Error, S] = value match {
      case Some(v) => mapper.map(v).left.map(error => error.copy(path = sourceLocation :: error.path))
      case None => Left(Error("Unable to find value.", List(sourceLocation)))
    }
  }

  given Mapper[ProtoAddress, Address] = Mapper.dervied

  given Mapper[ProtoUser, DeepUser] = Mapper.dervied
  
  extension [T](inline value: T) {
    inline def as[S](using mapper: Mapper[T, S]): Either[Error, S] = {
      given SourceLocation = SourceLocation(value)
      mapper.map(value)
    }
  }

  def fromProto(source2: ProtoUser): Either[Error, DeepUser] = {
    source2.as[DeepUser]
  }
}
package application.conversions

import application.models.{DeepUser, NiceAddress}
import application.protobuf.{ProtoAddress, ProtoUser}
import cats.data.NonEmptyList
import cats.syntax.all.*
import framework.conversion.SourceLocation
import framework.model.Error

import scala.annotation.nowarn
import scala.compiletime.{constValue, erasedValue, error, summonInline}
import scala.deriving.Mirror

// Credit for this work goes to
// https://medium.com/scalac/inline-your-boilerplate-harnessing-scala-3-metaprogramming-without-macros-c106ef8d6dfb
// Mostly modified to propagate SourceLocation and Error explicitly

// Two examples to focus on
// 1. Extracting the field names and types from a case class
// 2. Using the above information to find the appropriate mapper for each field
sealed trait Field[Label <: String, Type]

object Field:
  type FromLabelsAndTypes[Labels <: Tuple, Types <: Tuple] <: Tuple =
    (Labels, Types) match
      case (EmptyTuple, EmptyTuple) => EmptyTuple
      case (labelHead *: labelTail, typeHead *: typeTail) => Field[labelHead, typeHead] *: FromLabelsAndTypes[labelTail, typeTail]

opaque type FieldName = String

object FieldName:

  inline def fromLiteralLabel[Label <: String]: FieldName =
    constValue[Label]

//noinspection NoTailRecursionAnnotation
object DerivedMapper {

  private inline def labels[Labels <: Tuple]: List[FieldName] =
    inline erasedValue[Labels] match
      case _: EmptyTuple => List.empty
      case _: (h *: t) =>
        constValue[h].asInstanceOf[FieldName] :: labels[t]

  private inline def transformerForField[
      ToLabel <: String,
      ToType,
      FromFields <: Tuple]: (FieldName, Mapper[?, ?]) =
    inline erasedValue[FromFields] match {
      case _: EmptyTuple =>
        error("Field on target model '" + constValue[ToLabel] + "' was not found on source model")
      case _: (Field[ToLabel, fromType] *: _) =>
        FieldName.fromLiteralLabel[ToLabel] -> summonInline[Mapper[fromType, ToType]]
      case _: (_ *: tail) =>
        transformerForField[ToLabel, ToType, tail]
    }

  private inline def transformersForAllFields[FromFields <: Tuple, ToFields <: Tuple]: Map[FieldName, Mapper[?, ?]] =
    inline erasedValue[ToFields] match
      case _: EmptyTuple =>
        Map.empty
      case _: (Field[label, tpe] *: tail) =>
        transformersForAllFields[FromFields, tail] + transformerForField[label, tpe, FromFields]

  private inline def unsafeConstructInstance[To](from: Product)(unsafeMapper: (Map[String, ?], FieldName) => Either[Error, ?])(using
      To: Mirror.ProductOf[To]): Either[Error, To] =

    val labelsToValuesOfFrom: Map[String, Any] = from.productElementNames.zip(from.productIterator).toMap
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

  // TODO fix compiler warning here
  @nowarn
  inline def derived[From <: Product, To <: Product](using A: Mirror.ProductOf[From], B: Mirror.ProductOf[To]): Mapper[From, To] = {
    new Mapper[From, To]:
      override def map(from: From)(using sourceLocation: SourceLocation): Either[Error, To] =
        val transformers: Map[FieldName, Mapper[?, ?]] = transformersForAllFields[
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

  //  inline def invalid[To <: Product](using A: Mirror.ProductOf[To]): String = {
//    type toTypes = A.MirroredElemTypes
//    type toTypesPlus1 = String *: toTypes
//
////    type fields = Field.FromLabelsAndTypes[A.MirroredElemLabels, toTypesPlus1]
//
//    TypeName[Field.FromLabelsAndTypes[A.MirroredElemLabels, toTypesPlus1]].value
//  }
//
//  inline def showLabelsAndTypes[To <: Product](using A: Mirror.ProductOf[To]): String = {
//    TypeName[Field.FromLabelsAndTypes[A.MirroredElemLabels, A.MirroredElemTypes]].value
//  }

}

//object ShowTheTypes {
//  def main(args: Array[FieldName]): Unit = {
//    val myTuple: String *: Int *: EmptyTuple = ("hello", 42)
//
//    val tail: Int *: EmptyTuple = myTuple.tail
//
//
////    val foo = myTuple.tail.tail
////    val bar = myTuple.tail.tail.tail.head
//
//    val aTuple = if (true) EmptyTuple else myTuple
//    val aTuple2 = if (true) EmptyTuple else myTuple.tail
//    val foo = (aTuple, aTuple2) match
//      case (labelHead *: labelTail, labelHead2 *: labelTail2) => "something $labelHead2"
//      case (EmptyTuple, EmptyTuple) => "empty"
//
//    println(s"$foo")
//
//    println(DerivedMapper.invalid[NiceUser])
//
//    println(DerivedMapper.showLabelsAndTypes[NiceUser])
//  }
//}

object DeepUserMappings4 {

  given Mapper[ProtoAddress, NiceAddress] = DerivedMapper.derived

  given Mapper[ProtoUser, DeepUser] = DerivedMapper.derived

  import Mapper.as

  def fromProto(source: ProtoUser): Either[Error, DeepUser] = {
    source.as[DeepUser]
  }
}

object ErrorMappingExample {
  case class ProtoResponse(error: Option[ProtoErrors])
  case class ProtoErrors(messages: List[String])

  case class NiceResponse(error: NiceErrors)
  case class NiceErrors(messages: NonEmptyList[Int])

  given listMapper[T, S](using mapper: Mapper[T, S]): Mapper[List[T], List[S]] with {
    def map(value: List[T])(using sourceLocation: SourceLocation): Either[Error, List[S]] = {
      value.zipWithIndex.traverse { (item, index) =>
        given SourceLocation = sourceLocation.withIndex(index)
        mapper.map(item)
      }
    }
  }

  given nonEmptyListMapper[T, S](using mapper: Mapper[T, S]): Mapper[List[T], NonEmptyList[S]] with {
    def map(value: List[T])(using sourceLocation: SourceLocation): Either[Error, NonEmptyList[S]] = {
      val mappedList = listMapper[T, S].map(value)

      mappedList.flatMap { list =>
        NonEmptyList.fromList(list) match {
          case Some(nonEmptyList) => Right(nonEmptyList)
          case None => Left(Error("List must have 1 or more elements", List(sourceLocation)))
        }
      }
    }
  }

  given Mapper[String, Int] with {
    def map(value: String)(using sourceLocation: SourceLocation): Either[Error, Int] = {
      value.toIntOption match {
        case Some(intValue) => Right(intValue)
        case None => Left(Error(s"Unable to parse string '$value' to int", List(sourceLocation)))
      }
    }
  }

  given Mapper[ProtoResponse, NiceResponse] = DerivedMapper.derived
  given Mapper[ProtoErrors, NiceErrors] = DerivedMapper.derived

  import Mapper.as

  def main(args: Array[FieldName]): Unit = {
    val protoResponse = ProtoResponse(Some(ProtoErrors(List("1", "hello"))))
//    val protoResponse = ProtoResponse(Some(ProtoErrors(List("1"))))
    val response = protoResponse.as[NiceResponse]

    println(response)
  }
}

object FailuresExample {
  //  // Demo additional fields failure at compilation time
  //  case class DeepUserAdditionalFields(name: String, age: Int, address: Address, email: String)
  //
  //  given Mapper[ProtoUser, DeepUserAdditionalFields] = DerivedMapper.derived
  //
  //  // Demo unhandled field type failure at compilation time
  //  case class DeepUserUnhandledConversion(name: String, age: Long)
  //
  //  given Mapper[ProtoUser, DeepUserUnhandledConversion] = DerivedMapper.derived
}

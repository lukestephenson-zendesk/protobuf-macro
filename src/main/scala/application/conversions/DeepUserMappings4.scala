package application.conversions

import application.models.{Address, DeepUser}
import application.protobuf.{Address as ProtoAddress, User as ProtoUser}
import framework.conversion.SourceLocation
import framework.model.Error

import scala.compiletime.{constValue, erasedValue, error, summonInline}
import scala.deriving.Mirror

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

  def wrapAll[K, V](map: Map[K, V]): Map[String, V] =
    map.asInstanceOf[Map[String, V]]

object DerivedMapper {

  private inline def labels[Labels <: Tuple]: List[FieldName] =
    inline erasedValue[Labels] match
      case _: EmptyTuple => List.empty
      case _: (h *: t) =>
        constValue[h].asInstanceOf[FieldName] :: labels[t]


  private inline def transformerForField[
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

  private inline def transformersForAllFields[FromFields <: Tuple, ToFields <: Tuple]: Map[FieldName, Mapper[?, ?]] =
    inline erasedValue[ToFields] match
      case _: EmptyTuple =>
        Map.empty
      case _: (Field[label, tpe] *: tail) =>
        transformersForAllFields[FromFields, tail] + transformerForField[label, tpe, FromFields]

  private inline def unsafeConstructInstance[To](from: Product)(unsafeMapper: (Map[String, ?], FieldName) => Either[Error, ?])(using To: Mirror.ProductOf[To]): Either[Error, To] =
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

  // TODO fix compiler warning here
  inline def derived[From <: Product, To <: Product](using A: Mirror.ProductOf[From], B: Mirror.ProductOf[To]): Mapper[From, To] =
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

object DeepUserMappings4 {

  given Mapper[ProtoAddress, Address] = DerivedMapper.derived

  given Mapper[ProtoUser, DeepUser] = DerivedMapper.derived

  import Mapper.as

  def fromProto(source2: ProtoUser): Either[Error, DeepUser] = {
    source2.as[DeepUser]
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
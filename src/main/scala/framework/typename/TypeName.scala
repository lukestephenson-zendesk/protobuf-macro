package framework.typename

import application.protobuf.ProtoUser

case class TypeName[A](value: String)

object TypeName {
  inline def apply[A]: TypeName[A] = TypeName.apply(TypeNameMacro[A])
}

object TypeNameRunner {
  def main(args: Array[String]): Unit = {
    val typeName = TypeName[List[Option[ProtoUser]]]
    println(typeName)
  }
}
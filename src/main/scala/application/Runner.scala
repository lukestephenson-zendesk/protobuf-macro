package application

import application.conversions.*
import application.protobuf.*
import framework.conversion.SourceLocation

object Runner {
  def main(args: Array[String]): Unit = {
    val protoUser = ProtoUser(name = Some("John"), age = Some(5), address = None)
    val errorOrUser = TraditionalUserMapping.fromProto(protoUser)

    println("User1 " + errorOrUser)

    val protoUser2 = ProtoUser(name = None, age = Some(5), address = None)
    val errorOrUser2 = UserMappings1.fromProto(protoUser2)
    println("User2 " + errorOrUser2)

    val protoUser3 = ProtoUser(name = Some("Luke"), age = None, address = None)
    val errorOrUser3 = UserMappings2.fromProto(protoUser3)
    println("User3 " + errorOrUser3)

    val protoUser4 = ProtoUser(name = Some("Luke"), age = Some(35), address = Some(ProtoAddress(street = Some("Main St"), city = None)))
    val errorOrUser4 = DeepUserMappings.fromProto(protoUser4)
    println("User4 " + errorOrUser4)

    val errorOrUser5 = DeepUserMappings3.fromProto(protoUser4)
    println("User5 " + errorOrUser5)

    val errorOrUser6 = DeepUserMappings4.fromProto(protoUser4)
    println("User6 " + errorOrUser6)

    val helloVal = "Hello"
    println(SourceLocation(helloVal))
  }
}

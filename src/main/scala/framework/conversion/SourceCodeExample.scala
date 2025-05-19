package framework.conversion

import application.models.NiceUser

object SourceCodeExample {
  def main(args: Array[String]): Unit = {
    val myUser = NiceUser("John Doe", 30)
    val sourceLocation: SourceLocation = SourceLocation(myUser.age.toLong)
    println(sourceLocation)
  }
}

import sbt._

object Dependencies {
 val compile = Seq(
  )


    val test = Seq(
      "org.scalatest" %% "scalatest" % "3.1.0"
    ).map( _ % "test")

  val all = compile ++ test
}

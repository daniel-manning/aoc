import sbt._

object Dependencies {
 val compile = Seq(
   "org.scala-lang.modules" %% "scala-swing" % "2.1.1",
   "io.monix" %% "monix" % "3.1.0",
   "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
 )


    val test = Seq(
      "org.scalatest" %% "scalatest" % "3.1.0"
    ).map( _ % "test")

  val all = compile ++ test
}

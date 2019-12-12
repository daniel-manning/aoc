package Year2019

import Year2019.ProgrammeOperations.vectorProgrammeToMap

object Day02 extends App {

  val startingProgramme = IntCodeProgramme(programme =
    vectorProgrammeToMap(Vector(1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,6,1,19,1,5,19,23,2,9,23,27,1,6,27,31,1,31,9,35,2,35,10,
      39,1,5,39,43,2,43,9,47,1,5,47,51,1,51,5,55,1,55,9,59,2,59,13,63,1,63,9,67,1,9,67,71,2,71,10,75,
      1,75,6,79,2,10,79,83,1,5,83,87,2,87,10,91,1,91,5,95,1,6,95,99,2,99,13,103,1,103,6,107,1,107,5,
      111,2,6,111,115,1,115,13,119,1,119,2,123,1,5,123,0,99,2,0,14,0)))

  val endProgramme = startingProgramme.runProgramme()
  println(s"At the end of the programme at position 0 the value is ${endProgramme.programme(0)}")

  def startingProgrammeParameterised(a:Int, b:Int) = IntCodeProgramme(programme =
    vectorProgrammeToMap(Vector(1,a,b,3,1,1,2,3,1,3,4,3,1,5,0,3,2,6,1,19,1,5,19,23,2,9,23,27,1,6,27,31,1,31,9,35,2,35,10,
      39,1,5,39,43,2,43,9,47,1,5,47,51,1,51,5,55,1,55,9,59,2,59,13,63,1,63,9,67,1,9,67,71,2,71,10,75,
      1,75,6,79,2,10,79,83,1,5,83,87,2,87,10,91,1,91,5,95,1,6,95,99,2,99,13,103,1,103,6,107,1,107,5,
      111,2,6,111,115,1,115,13,119,1,119,2,123,1,5,123,0,99,2,0,14,0)))

  val params = for{
    a <- (0 to 100)
    b <- (0 to 100)
    if(startingProgrammeParameterised(a,b).runProgramme().programme(0) == 19690720)
  } yield (a, b)

  println(s"The combination leading to the moon landing is: $params")
  params.map(a => println(s"Noun: ${a._1} Verb: ${a._2}  So value is ${100*a._1 + a._2}"))

}




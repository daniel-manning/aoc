package Year2019

object Day02 extends App {


  def nextOperation(programme: IntCodeProgramme) =
    programme.programme(programme.pointer) match {
      case 1 => Some(Operation(4, programme.programme(programme.pointer + 1), programme.programme(programme.pointer + 2), programme.programme(programme.pointer + 3), _ + _))
      case 2 => Some(Operation(4, programme.programme(programme.pointer + 1), programme.programme(programme.pointer + 2), programme.programme(programme.pointer + 3), _ * _))
      case 99 => None
    }

  def performOperation(op: Option[Operation], programme: IntCodeProgramme): IntCodeProgramme =
    op.map{ operation =>
      IntCodeProgramme(programme.pointer + operation.codelength,
        programme.programme.updated(operation.address3,
                                    operation.operator(programme.programme(operation.address1),
                                                       programme.programme(operation.address2))))
    }.getOrElse(programme)


  def runProgramme(programme: IntCodeProgramme): IntCodeProgramme =
  LazyList.unfold(programme){
    p => val op = nextOperation(p)
      val nextProgramme = performOperation(op, p)

      op.map{ _ =>
        (nextProgramme, nextProgramme)
      }
  }.last


/*  val startingProgramme = IntCodeProgramme(Vector(1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,6,1,19,1,5,19,23,2,9,23,27,1,6,27,31,1,31,9,35,2,35,10,39,1,5,39,43,2,43,9,47,1,5,47,51,1,51,5,55,1,55,9,59,2,59,13,63,1,63,9,67,1,9,67,71,2,71,10,75,1,75,6,79,2,10,79,83,1,5,83,87,2,87,10,91,1,91,5,95,1,6,95,99,2,99,13,103,1,103,6,107,1,107,5,111,2,6,111,115,1,115,13,119,1,119,2,123,1,5,123,0,99,2,0,14,0))

  val endProgramme = runProgramme(startingProgramme)
  println(s"At the end of the programme at position 0 the value is ${endProgramme.programme(0)}")*/

  def startingProgramme(a:Int, b:Int) = IntCodeProgramme(programme = Vector(1,a,b,3,1,1,2,3,1,3,4,3,1,5,0,3,2,6,1,19,1,5,19,23,2,9,23,27,1,6,27,31,1,31,9,35,2,35,10,39,1,5,39,43,2,43,9,47,1,5,47,51,1,51,5,55,1,55,9,59,2,59,13,63,1,63,9,67,1,9,67,71,2,71,10,75,1,75,6,79,2,10,79,83,1,5,83,87,2,87,10,91,1,91,5,95,1,6,95,99,2,99,13,103,1,103,6,107,1,107,5,111,2,6,111,115,1,115,13,119,1,119,2,123,1,5,123,0,99,2,0,14,0))

  val params = for{
    a <- (0 to 100)
    b <- (0 to 100)
    if(runProgramme(startingProgramme(a,b)).programme(0) == 19690720)
  } yield (a, b)

  println(s"The combination leading to the moon landing is: $params")
  params.map(a => println(s"Noun: ${a._1} Verb: ${a._2}  So value is ${100*a._1 + a._2}"))

}


case class Operation(codelength:Int, address1: Int, address2:Int, address3: Int, operator: (Int, Int) => Int)
case class IntCodeProgramme(pointer: Int = 0, programme: Vector[Int])

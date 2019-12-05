package Year2019

sealed trait Operation {
  val codeLength:Int

  def run(programme: IntCodeProgramme):IntCodeProgramme
}

case class SumOperation(address1: Int, address2:Int, address3: Int) extends Operation {
  val codeLength = 4
  def run(programme: IntCodeProgramme):IntCodeProgramme = {
    IntCodeProgramme(programme.pointer + codeLength,
      programme.programme.updated(address3,
        programme.programme(address1) + programme.programme(address2)))
  }
}

case class MultiplyOperation(address1: Int, address2:Int, address3: Int) extends Operation {
  val codeLength = 4
  def run(programme: IntCodeProgramme):IntCodeProgramme = {
    IntCodeProgramme(programme.pointer + codeLength,
      programme.programme.updated(address3,
        programme.programme(address1) * programme.programme(address2)))
  }
}

case object ExitOperation extends Operation {
  val codeLength = 1
  def run(programme: IntCodeProgramme):IntCodeProgramme = programme
}

case class IntCodeProgramme(pointer: Int = 0, programme: Vector[Int]){
  def nextOperation(): Operation =
    programme(pointer) match {
      case 1 => SumOperation(programme(pointer + 1), programme(pointer + 2), programme(pointer + 3))
      case 2 => MultiplyOperation(programme(pointer + 1), programme(pointer + 2), programme(pointer + 3))
      case 99 => ExitOperation
    }


  def runProgramme(): IntCodeProgramme =
    LazyList.unfold(this){
      p => val op = p.nextOperation()
            val nextProgramme = op.run(p)

        op match {
          case ExitOperation => None
          case _ => Some((nextProgramme, nextProgramme))
        }
    }.last
}

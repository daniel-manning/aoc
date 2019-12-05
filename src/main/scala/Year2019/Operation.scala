package Year2019

sealed trait Operation {
  val codeLength:Int

  def run(programme: IntCodeProgramme):IntCodeProgramme

  def lookup(address: Int, mode: Mode, programme: IntCodeProgramme): Int =
    mode match {
      case PositionMode => programme.programme(address)
      case ImmediateMode => address
    }

}

case class SumOperation(address1: Int, address2:Int, address3: Int, mask:Seq[Mode]) extends Operation {
  val codeLength = 4


  def run(programme: IntCodeProgramme):IntCodeProgramme = {
    IntCodeProgramme(programme.pointer + codeLength,
      programme.programme.updated(address3,
        lookup(address1, mask(0), programme) + lookup(address2, mask(1), programme)))
  }
}

case class MultiplyOperation(address1: Int, address2:Int, address3: Int, mask:Seq[Mode]) extends Operation {
  val codeLength = 4
  def run(programme: IntCodeProgramme):IntCodeProgramme = {
    IntCodeProgramme(programme.pointer + codeLength,
      programme.programme.updated(address3,
        lookup(address1, mask(0), programme) * lookup(address2, mask(1), programme)),
      programme.inputStack,
      programme.outputStack)
  }
}

case class InputOperation(address: Int, mask:Seq[Mode]) extends Operation {
  val codeLength = 2

  def run(programme: IntCodeProgramme):IntCodeProgramme = {

    val input: Int = programme.inputStack.pop()

    IntCodeProgramme(programme.pointer + codeLength,
      programme.programme.updated(address, input)
    )
  }
}

case class OutputOperation(address: Int, mask:Seq[Mode]) extends Operation {
  val codeLength = 2


  def run(programme: IntCodeProgramme):IntCodeProgramme = {

    val output: Int = programme.programme(address)
    println(s"output: $output")

    IntCodeProgramme(programme.pointer + codeLength,
      programme.programme,
      programme.inputStack,
      programme.outputStack.push(output))
  }
}

case object ExitOperation extends Operation {
  val codeLength = 1
  def run(programme: IntCodeProgramme):IntCodeProgramme = programme
}

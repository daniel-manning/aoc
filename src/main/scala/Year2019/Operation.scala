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
        lookup(address1, mask(0), programme) + lookup(address2, mask(1), programme)),
      programme.inputStack,
      programme.outputStack)
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

    println("Waiting for Input!")
    while(programme.inputStack.length < 1){

    }

    println("Getting Input!")
    val input: Int = programme.inputStack.pop()

    IntCodeProgramme(programme.pointer + codeLength,
      programme.programme.updated(address, input),
      programme.inputStack,
      programme.outputStack
    )
  }
}

case class OutputOperation(address: Int, mask:Seq[Mode]) extends Operation {
  val codeLength = 2


  def run(programme: IntCodeProgramme):IntCodeProgramme = {

    val output: Int = lookup(address, mask(0), programme)

    IntCodeProgramme(programme.pointer + codeLength,
      programme.programme,
      programme.inputStack,
      programme.outputStack.push(output))
  }
}

case class JumpIfTrueOperation(address1: Int, address2:Int, mask:Seq[Mode]) extends Operation {
  val codeLength = 3

  def run(programme: IntCodeProgramme):IntCodeProgramme ={
    if(lookup(address1, mask(0), programme) > 0){
      programme.copy(pointer = lookup(address2, mask(1), programme))
    } else {
      programme.copy(pointer = programme.pointer + codeLength)
    }
  }
}

case class JumpIfFalseOperation(address1: Int, address2:Int, mask:Seq[Mode]) extends Operation {
  val codeLength = 3

  def run(programme: IntCodeProgramme):IntCodeProgramme ={
    if(lookup(address1, mask(0), programme) == 0){
      programme.copy(pointer = lookup(address2, mask(1), programme))
    } else {
      programme.copy(pointer = programme.pointer + codeLength)
    }
  }
}

case class LessThanOperation(address1: Int, address2:Int, address3:Int, mask:Seq[Mode]) extends Operation {
  val codeLength = 4

  def run(programme: IntCodeProgramme):IntCodeProgramme ={
    val updateValue = if(lookup(address1, mask(0), programme) < lookup(address2, mask(1), programme)){
      1
    } else {
      0
    }

    IntCodeProgramme(programme.pointer + codeLength,
      programme.programme.updated(address3, updateValue),
      programme.inputStack,
      programme.outputStack)
  }

}

case class EqualOperation(address1: Int, address2:Int, address3:Int, mask:Seq[Mode]) extends Operation {
  val codeLength = 4

  def run(programme: IntCodeProgramme):IntCodeProgramme ={
    val updateValue = if(lookup(address1, mask(0), programme) == lookup(address2, mask(1), programme)){
      1
    } else {
      0
    }

    IntCodeProgramme(programme.pointer + codeLength,
      programme.programme.updated(address3, updateValue),
      programme.inputStack,
      programme.outputStack)
  }

}

case object ExitOperation extends Operation {
  val codeLength = 1
  def run(programme: IntCodeProgramme):IntCodeProgramme = programme
}

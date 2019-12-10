package Year2019

sealed trait Operation {
  val codeLength:Int

  def run(programme: IntCodeProgramme)(implicit label:String):IntCodeProgramme

  def lookup(address: Int, mode: Mode, programme: IntCodeProgramme): Int =
    mode match {
      case PositionMode => programme.programme(address)
      case ImmediateMode => address
    }

}

case class SumOperation(address1: Int, address2:Int, address3: Int, mask:Seq[Mode]) extends Operation {
  val codeLength = 4


  def run(programme: IntCodeProgramme)(implicit label:String):IntCodeProgramme = {
    IntCodeProgramme(programme.pointer + codeLength,
      programme.programme.updated(address3,
        lookup(address1, mask(0), programme) + lookup(address2, mask(1), programme)),
      programme.inputQueue,
      programme.outputQueue)
  }
}

case class MultiplyOperation(address1: Int, address2:Int, address3: Int, mask:Seq[Mode]) extends Operation {
  val codeLength = 4
  def run(programme: IntCodeProgramme)(implicit label:String):IntCodeProgramme = {
    IntCodeProgramme(programme.pointer + codeLength,
      programme.programme.updated(address3,
        lookup(address1, mask(0), programme) * lookup(address2, mask(1), programme)),
      programme.inputQueue,
      programme.outputQueue)
  }
}

case class InputOperation(address: Int, mask:Seq[Mode]) extends Operation {
  val codeLength = 2

  def run(programme: IntCodeProgramme)(implicit label:String):IntCodeProgramme = {

    println(s"$label - Waiting for Input!")
    while(programme.inputQueue.length < 1){

    }

    val input: Int = programme.inputQueue.dequeue()
    println(s"$label - Getting Input! - $input")

    IntCodeProgramme(programme.pointer + codeLength,
      programme.programme.updated(address, input),
      programme.inputQueue,
      programme.outputQueue
    )
  }
}

case class OutputOperation(address: Int, mask:Seq[Mode]) extends Operation {
  val codeLength = 2


  def run(programme: IntCodeProgramme)(implicit label:String):IntCodeProgramme = {

    val output: Int = lookup(address, mask(0), programme)
    println(s"$label - writing $output to the output channel")

    IntCodeProgramme(programme.pointer + codeLength,
      programme.programme,
      programme.inputQueue,
      programme.outputQueue.enqueue(output))
  }
}

case class JumpIfTrueOperation(address1: Int, address2:Int, mask:Seq[Mode]) extends Operation {
  val codeLength = 3

  def run(programme: IntCodeProgramme)(implicit label:String):IntCodeProgramme ={
    if(lookup(address1, mask(0), programme) > 0){
      programme.copy(pointer = lookup(address2, mask(1), programme))
    } else {
      programme.copy(pointer = programme.pointer + codeLength)
    }
  }
}

case class JumpIfFalseOperation(address1: Int, address2:Int, mask:Seq[Mode]) extends Operation {
  val codeLength = 3

  def run(programme: IntCodeProgramme)(implicit label:String):IntCodeProgramme ={
    if(lookup(address1, mask(0), programme) == 0){
      programme.copy(pointer = lookup(address2, mask(1), programme))
    } else {
      programme.copy(pointer = programme.pointer + codeLength)
    }
  }
}

case class LessThanOperation(address1: Int, address2:Int, address3:Int, mask:Seq[Mode]) extends Operation {
  val codeLength = 4

  def run(programme: IntCodeProgramme)(implicit label:String):IntCodeProgramme ={
    val updateValue = if(lookup(address1, mask(0), programme) < lookup(address2, mask(1), programme)){
      1
    } else {
      0
    }

    IntCodeProgramme(programme.pointer + codeLength,
      programme.programme.updated(address3, updateValue),
      programme.inputQueue,
      programme.outputQueue)
  }

}

case class EqualOperation(address1: Int, address2:Int, address3:Int, mask:Seq[Mode]) extends Operation {
  val codeLength = 4

  def run(programme: IntCodeProgramme)(implicit label:String):IntCodeProgramme ={
    val updateValue = if(lookup(address1, mask(0), programme) == lookup(address2, mask(1), programme)){
      1
    } else {
      0
    }

    IntCodeProgramme(programme.pointer + codeLength,
      programme.programme.updated(address3, updateValue),
      programme.inputQueue,
      programme.outputQueue)
  }

}

case object ExitOperation extends Operation {
  val codeLength = 1
  def run(programme: IntCodeProgramme)(implicit label:String):IntCodeProgramme = programme
}

package Year2019

sealed trait Operation {
  val codeLength:Int

  def run(programme: IntCodeProgramme)(implicit settings: RunningSettings): IntCodeProgramme

  def lookup(address: Int, mode: Mode, programme: IntCodeProgramme): Int =
    mode match {
      case PositionMode => programme.programme(address)
      case ImmediateMode => address
    }

  def debugLog(message: String)(implicit settings: RunningSettings): Unit =
    if(settings.debugOutput) println(s"${settings.label} - $message")

}

case class SumOperation(address1: Int, address2:Int, address3: Int, mask:Seq[Mode]) extends Operation {
  val codeLength = 4


  def run(programme: IntCodeProgramme)(implicit settings: RunningSettings):IntCodeProgramme = {
    val sumValues = lookup(address1, mask(0), programme) + lookup(address2, mask(1), programme)
    debugLog(s"Writing sum value $sumValues to location $address3")

    IntCodeProgramme(programme.pointer + codeLength,
      programme.programme.updated(address3, sumValues),
      programme.inputQueue,
      programme.outputQueue)
  }
}

case class MultiplyOperation(address1: Int, address2:Int, address3: Int, mask:Seq[Mode]) extends Operation {
  val codeLength = 4

  def run(programme: IntCodeProgramme)(implicit settings: RunningSettings): IntCodeProgramme = {
    val multiplyValues = lookup(address1, mask(0), programme) * lookup(address2, mask(1), programme)
    debugLog(s"Writing sum value $multiplyValues to location $address3")

    IntCodeProgramme(programme.pointer + codeLength,
      programme.programme.updated(address3, multiplyValues),
      programme.inputQueue,
      programme.outputQueue)
  }
}

case class InputOperation(address: Int, mask:Seq[Mode]) extends Operation {
  val codeLength = 2

  def run(programme: IntCodeProgramme)(implicit settings: RunningSettings): IntCodeProgramme = {

    debugLog(s"Waiting for Input!")
    while(programme.inputQueue.length < 1){
      Thread.sleep(5)
    }

    val input: Int = programme.inputQueue.dequeue()
    debugLog(s"Getting Input! - $input")

    IntCodeProgramme(programme.pointer + codeLength,
      programme.programme.updated(address, input),
      programme.inputQueue,
      programme.outputQueue
    )
  }
}

case class OutputOperation(address: Int, mask:Seq[Mode]) extends Operation {
  val codeLength = 2


  def run(programme: IntCodeProgramme)(implicit settings: RunningSettings): IntCodeProgramme = {

    val output: Int = lookup(address, mask(0), programme)
    debugLog(s"writing $output to the output channel")

    IntCodeProgramme(programme.pointer + codeLength,
      programme.programme,
      programme.inputQueue,
      programme.outputQueue.enqueue(output))
  }
}

case class JumpIfTrueOperation(address1: Int, address2:Int, mask:Seq[Mode]) extends Operation {
  val codeLength = 3

  def run(programme: IntCodeProgramme)(implicit settings: RunningSettings): IntCodeProgramme ={
    val addressValue = lookup(address1, mask(0), programme)
    val locationValue = lookup(address2, mask(1), programme)

    if(addressValue != 0){
      debugLog(s"jumpiftrue: $addressValue is not zero so jumping to address $locationValue")
      programme.copy(pointer = locationValue)
    } else {
      debugLog(s"jumpiftrue: $addressValue is zero so moving on")
      programme.copy(pointer = programme.pointer + codeLength)
    }
  }
}

case class JumpIfFalseOperation(address1: Int, address2:Int, mask:Seq[Mode]) extends Operation {
  val codeLength = 3

  def run(programme: IntCodeProgramme)(implicit settings: RunningSettings):IntCodeProgramme ={
    val addressValue = lookup(address1, mask(0), programme)
    val locationValue = lookup(address2, mask(1), programme)

    if(addressValue == 0){
      debugLog(s"jumpiffalse: $addressValue is zero so jumping to address $locationValue")
      programme.copy(pointer = locationValue)
    } else {
      debugLog(s"jumpiffalse: $addressValue is not zero so moving on")
      programme.copy(pointer = programme.pointer + codeLength)
    }
  }
}

case class LessThanOperation(address1: Int, address2:Int, address3:Int, mask:Seq[Mode]) extends Operation {
  val codeLength = 4

  def run(programme: IntCodeProgramme)(implicit settings: RunningSettings):IntCodeProgramme ={
    val addressOneValue = lookup(address1, mask(0), programme)
    val addressTwoValue = lookup(address2, mask(1), programme)

    val updateValue = if(addressOneValue < addressTwoValue){
      1
    } else {
      0
    }

    debugLog(s"lto: updating location $address3 with value $updateValue")
    IntCodeProgramme(programme.pointer + codeLength,
      programme.programme.updated(address3, updateValue),
      programme.inputQueue,
      programme.outputQueue)
  }

}

case class EqualOperation(address1: Int, address2:Int, address3:Int, mask:Seq[Mode]) extends Operation {
  val codeLength = 4

  def run(programme: IntCodeProgramme)(implicit settings: RunningSettings):IntCodeProgramme = {
    val addressOneValue = lookup(address1, mask(0), programme)
    val addressTwoValue = lookup(address2, mask(1), programme)

    val updateValue = if(addressOneValue == addressTwoValue){
      1
    } else {
      0
    }

    debugLog(s"eo: updating location $address3 with value $updateValue")
    IntCodeProgramme(programme.pointer + codeLength,
      programme.programme.updated(address3, updateValue),
      programme.inputQueue,
      programme.outputQueue)
  }

}

case object ExitOperation extends Operation {
  val codeLength = 1
  def run(programme: IntCodeProgramme)(implicit settings: RunningSettings):IntCodeProgramme = {
    debugLog(s"Exit Operation!")
    programme
  }
}

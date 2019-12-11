package Year2019

sealed trait Operation {
  val codeLength:Int

  def run(programme: IntCodeProgramme)(implicit settings: RunningSettings): IntCodeProgramme

  def lookup(address: BigInt, mode: Mode, programme: IntCodeProgramme): BigInt =
    mode match {
      case PositionMode => programme.programme.getOrElse(address, 0)
      case ImmediateMode => address
      case RelativeMode => programme.programme.getOrElse(address + programme.relativeBase, 0)
    }

  def debugLog(message: String)(implicit settings: RunningSettings): Unit =
    if(settings.debugOutput) println(s"${settings.label} - $message")

}

case class SumOperation(address1: BigInt, address2:BigInt, address3: BigInt, mask:Seq[Mode]) extends Operation {
  val codeLength = 4


  def run(programme: IntCodeProgramme)(implicit settings: RunningSettings):IntCodeProgramme = {
    val sumValues = lookup(address1, mask(0), programme) + lookup(address2, mask(1), programme)
    debugLog(s"Writing sum value $sumValues to location $address3")

    programme.copy(
      pointer = programme.pointer + codeLength,
      programme = programme.programme.updated(address3, sumValues)
    )
  }
}

case class MultiplyOperation(address1: BigInt, address2:BigInt, address3: BigInt, mask:Seq[Mode]) extends Operation {
  val codeLength = 4

  def run(programme: IntCodeProgramme)(implicit settings: RunningSettings): IntCodeProgramme = {
    val multiplyValues = lookup(address1, mask(0), programme) * lookup(address2, mask(1), programme)
    debugLog(s"Writing sum value $multiplyValues to location $address3")

    programme.copy(
      pointer = programme.pointer + codeLength,
      programme =  programme.programme.updated(address3, multiplyValues)
    )
  }
}

case class InputOperation(address: BigInt, mask:Seq[Mode]) extends Operation {
  val codeLength = 2

  def run(programme: IntCodeProgramme)(implicit settings: RunningSettings): IntCodeProgramme = {

    debugLog(s"Waiting for Input!")
    while(programme.inputQueue.length < 1){
      Thread.sleep(5)
    }

    val input: BigInt = programme.inputQueue.dequeue()
    debugLog(s"Getting Input! - $input")

    programme.copy(
      pointer = programme.pointer + codeLength,
      programme = programme.programme.updated(address, input)
    )
  }
}

case class OutputOperation(address: BigInt, mask:Seq[Mode]) extends Operation {
  val codeLength = 2


  def run(programme: IntCodeProgramme)(implicit settings: RunningSettings): IntCodeProgramme = {

    val output: BigInt = lookup(address, mask(0), programme)
    debugLog(s"writing $output to the output channel")

    programme.copy(
      pointer = programme.pointer + codeLength,
      outputQueue = programme.outputQueue.enqueue(output)
    )
  }
}

case class JumpIfTrueOperation(address1: BigInt, address2:BigInt, mask:Seq[Mode]) extends Operation {
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

case class JumpIfFalseOperation(address1: BigInt, address2:BigInt, mask:Seq[Mode]) extends Operation {
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

case class LessThanOperation(address1: BigInt, address2:BigInt, address3:BigInt, mask:Seq[Mode]) extends Operation {
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

    programme.copy(
      pointer = programme.pointer + codeLength,
      programme = programme.programme.updated(address3, updateValue)
    )
  }
}

case class EqualOperation(address1: BigInt, address2:BigInt, address3:BigInt, mask:Seq[Mode]) extends Operation {
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

    programme.copy(
      pointer = programme.pointer + codeLength,
      programme = programme.programme.updated(address3, updateValue)
    )
  }
}

case class AdjustRelativeBaseOperation(address1: BigInt, mask:Seq[Mode]) extends Operation {
  val codeLength = 2

  def run(programme: IntCodeProgramme)(implicit settings: RunningSettings):IntCodeProgramme = {
    val addressValue = lookup(address1, mask(0), programme)

    debugLog(s"adjust relative base: changing relative base by $addressValue")

    programme.copy(
      pointer = programme.pointer + codeLength,
      relativeBase = programme.relativeBase + addressValue
    )
  }
}

case object ExitOperation extends Operation {
  val codeLength = 1
  def run(programme: IntCodeProgramme)(implicit settings: RunningSettings):IntCodeProgramme = {
    debugLog(s"Exit Operation!")
    programme
  }
}

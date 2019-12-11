package Year2019

import scala.collection.mutable

case class IntCodeProgramme(pointer: Int = 0,
                            programme: Vector[Int],
                            inputQueue:mutable.Queue[Int] = new mutable.Queue[Int](),
                            outputQueue:mutable.Queue[Int] = new mutable.Queue[Int]()
                           ){
  def nextOperation(): Operation = {
    val OpCodeWithMask = IntCodeProgramme.parseOpCode(programme(pointer))
    OpCodeWithMask match {
      case ("01", mask) => SumOperation(programme(pointer + 1), programme(pointer + 2), programme(pointer + 3), mask)
      case ("02", mask) => MultiplyOperation(programme(pointer + 1), programme(pointer + 2), programme(pointer + 3), mask)
      case ("03", mask) => InputOperation(programme(pointer + 1), mask)
      case ("04", mask) => OutputOperation(programme(pointer + 1), mask)
      case ("05", mask) => JumpIfTrueOperation(programme(pointer + 1), programme(pointer + 2), mask)
      case ("06", mask) => JumpIfFalseOperation(programme(pointer + 1), programme(pointer + 2), mask)
      case ("07", mask) => LessThanOperation(programme(pointer + 1), programme(pointer + 2), programme(pointer + 3), mask)
      case ("08", mask) => EqualOperation(programme(pointer + 1), programme(pointer + 2), programme(pointer + 3), mask)
      case ("99", _) => ExitOperation
      case (a, _) => {println(s"Oh NO: $a"); throw new RuntimeException("Out of Cheese Error. Redo from Start")}
    }
  }

  def runProgramme()(implicit settings: RunningSettings = RunningSettings("", debugOutput = false)): IntCodeProgramme = {
    if(settings.debugOutput) println(s"${settings.label} - Starting programme!")

    LazyList.unfold(this) {
      p =>
        val op = p.nextOperation()
        val nextProgramme = op.run(p)

        op match {
          case ExitOperation => None
          case _ => Some((nextProgramme, nextProgramme))
        }
    }.last
  }
}

object IntCodeProgramme {
  def parseOpCode(value: Int): (String, Seq[Mode]) = {
    val numString = value.toString
    val bufferedValue = "0" * (5 - numString.length) ++ numString.toString
    val thirdMode = Mode(bufferedValue(0))
    val secondMode = Mode(bufferedValue(1))
    val firstMode = Mode(bufferedValue(2))
    val opCode = bufferedValue.slice(3,5)

    (opCode, Seq(firstMode, secondMode, thirdMode))
  }
}

case class RunningSettings(label: String, debugOutput: Boolean)

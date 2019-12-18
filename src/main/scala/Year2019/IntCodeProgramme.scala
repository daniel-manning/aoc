package Year2019

import scala.collection.mutable

case class IntCodeProgramme(pointer: BigInt = 0,
                            relativeBase: BigInt = 0,
                            programme: Map[BigInt, BigInt],
                            inputQueue:mutable.Queue[BigInt] = new mutable.Queue[BigInt](),
                            outputQueue:mutable.Queue[BigInt] = new mutable.Queue[BigInt](),
                            waitingForInputQueue: mutable.Queue[BigInt] = new mutable.Queue[BigInt]()
                           ){
  def nextOperation(): Operation = {
    val OpCodeWithMask = IntCodeProgramme.parseOpCode(programme.getOrElse(pointer, 0))
    OpCodeWithMask match {
      case ("01", mask) => SumOperation(programme.getOrElse(pointer + 1, 0), programme.getOrElse(pointer + 2, 0), programme.getOrElse(pointer + 3, 0), mask)
      case ("02", mask) => MultiplyOperation(programme.getOrElse(pointer + 1, 0), programme.getOrElse(pointer + 2, 0), programme.getOrElse(pointer + 3, 0), mask)
      case ("03", mask) => InputOperation(programme.getOrElse(pointer + 1, 0), mask)
      case ("04", mask) => OutputOperation(programme.getOrElse(pointer + 1, 0), mask)
      case ("05", mask) => JumpIfTrueOperation(programme.getOrElse(pointer + 1, 0), programme.getOrElse(pointer + 2, 0), mask)
      case ("06", mask) => JumpIfFalseOperation(programme.getOrElse(pointer + 1, 0), programme.getOrElse(pointer + 2, 0), mask)
      case ("07", mask) => LessThanOperation(programme.getOrElse(pointer + 1, 0), programme.getOrElse(pointer + 2, 0), programme.getOrElse(pointer + 3, 0), mask)
      case ("08", mask) => EqualOperation(programme.getOrElse(pointer + 1, 0), programme.getOrElse(pointer + 2, 0), programme.getOrElse(pointer + 3, 0), mask)
      case ("09", mask) => AdjustRelativeBaseOperation(programme.getOrElse(pointer + 1, 0), mask)
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
  def parseOpCode(value: BigInt): (String, Seq[Mode]) = {
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


object ProgrammeOperations {
  def vectorProgrammeToMap(programmeCode: Vector[BigInt]):Map[BigInt, BigInt] =
    programmeCode.zipWithIndex.map(x => (BigInt(x._2), x._1)).toMap
}

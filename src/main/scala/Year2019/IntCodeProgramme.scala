package Year2019

import scala.collection.mutable

case class IntCodeProgramme(pointer: Int = 0,
                            programme: Vector[Int],
                            inputStack:mutable.Stack[Int] = new mutable.Stack[Int](),
                            outputStack:mutable.Stack[Int] = new mutable.Stack[Int]()
                           ){
  def nextOperation(): Operation = {
    val OpCodeWithMask = IntCodeProgramme.parseOpCode(programme(pointer))
    OpCodeWithMask match {
      case ("01", mask) => SumOperation(programme(pointer + 1), programme(pointer + 2), programme(pointer + 3), mask)
      case ("02", mask) => MultiplyOperation(programme(pointer + 1), programme(pointer + 2), programme(pointer + 3), mask)
      case ("03", mask) => InputOperation(programme(pointer + 1), mask)
      case ("04", mask) => OutputOperation(programme(pointer + 1), mask)
      case ("99", _) => ExitOperation
      case (_, _) => throw new RuntimeException("Out of Cheese Error. Redo from Start")
    }
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

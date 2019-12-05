package Year2019

case class Operation(codelength:Int, address1: Int, address2:Int, address3: Int, operator: (Int, Int) => Int)

case class IntCodeProgramme(pointer: Int = 0, programme: Vector[Int]){
  def nextOperation(): Option[Operation] =
    programme(pointer) match {
      case 1 => Some(Operation(4, programme(pointer + 1), programme(pointer + 2), programme(pointer + 3), _ + _))
      case 2 => Some(Operation(4, programme(pointer + 1), programme(pointer + 2), programme(pointer + 3), _ * _))
      case 99 => None
    }

  def performOperation(op: Option[Operation]): IntCodeProgramme =
    op.map{ operation =>
      IntCodeProgramme(pointer + operation.codelength,
        programme.updated(operation.address3,
          operation.operator(programme(operation.address1),
            programme(operation.address2))))
    }.getOrElse(this)


  def runProgramme(): IntCodeProgramme =
    LazyList.unfold(this){
      p => val op = p.nextOperation()
        val nextProgramme = p.performOperation(op)

        op.map{ _ =>
          (nextProgramme, nextProgramme)
        }
    }.last
}

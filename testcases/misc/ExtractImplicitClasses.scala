
object ExtractImplicitClasses {

  implicit class BoolOps(value: Boolean) {
    def negate: Boolean = !value
  }

  def testNegate(x: Boolean) = {
    x.negate
  } ensuring { x == !_ }

  case class Box(value: Int)

  implicit class BoxOps(box: Box) {
    def double = Box(box.value * 2)
  }

  def testDouble(x: Box) = {
    x.double
  } ensuring { _.value == x.value * 2 }

}


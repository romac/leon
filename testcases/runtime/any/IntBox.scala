
object IntBox {

  case class A()
  case class B()

  // def fails = A().isInstanceOf[B]

  // case class IntBox(value: Int)

  // case class CastingBox(value: IntBox) {
  //   def casted: IntBox = value.asInstanceOf[IntBox]

  //   def safeCasted: IntBox =
  //     if (value.isInstanceOf[IntBox]) casted
  //     else IntBox(-1)
  // }

  // case class CastingIntBox(value: Int) {
  //   def casted: Int = value.asInstanceOf[Int]

  //   def safeCasted: Int =
  //     if (value.isInstanceOf[Int]) casted
  //     else -1
  // }

  case class CastingBox(value: Any) {
    def casted: Int = value.asInstanceOf[Int]

    def safeCasted: Int =
      if (value.isInstanceOf[Int]) casted
      else -1
  }

}

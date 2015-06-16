
object Id {

  def identity[T](x: T): T = {
    x
  } ensuring { res => res == x }

  def id(x: Any): Any = {
    x
  } ensuring { res => res == x }

}

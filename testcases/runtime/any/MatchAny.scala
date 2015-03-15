
object MatchAny {

  case class Foo(i: Int)
  case class Bar()
  case class Error()

  def trueIfFoo(x: Any): Boolean = x match {
    case x: Foo => true
    case _ => false
  }

  def intIfFoo(x: Any): Int = x match {
    case x: Foo => x.i
    case _ => -1
  }

  def onlyFooBar(x: Any): Any = x match {
    case foo: Foo => foo
    case bar: Bar => bar
    case _        => Error()
  }

  // Could not extract refined type as PureScala: Product with Serializable
  // (class scala.reflect.internal.Types$RefinedType0)
  //             object MatchAny {
  //             ^^^^^^^^^^^^^^^^^
  // def fooBar(x: Any) = x match {
  //   case _: Foo => Foo(42)
  //   case _ => Bar()
  // }

}

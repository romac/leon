import leon.lang._

object AnyDef {

  //
  // working
  //

  def ret42a(x: ?!?): Int = 42

  def ret42b(x: Any): Int = 42

  def retXa(x: ?!?): ?!? = x

  def retXb(x: Any): Any = x

  //
  // almost working
  //

  // [Warning] Resorting to uninterpreted type for : Tree? (class leon.purescala.TypeTrees$AnyType$)
  // def retXEnsuring(x: Any): Any = {
  //   x
  // } ensuring (out => out == x)

  //
  // not working (yet)
  //

  // [error] Exception in thread "main" java.lang.Exception:
  //         I can't choose simplest value for type Tree? (class leon.purescala.TypeTrees$AnyType$)
  //         at leon.purescala.TreeOps$.simplestValue(TreeOps.scala:1038)
  // def retXWrong(x: Any): Any = {
  //   x
  // } ensuring (out => !(out == x))

}

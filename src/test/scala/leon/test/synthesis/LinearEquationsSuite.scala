package leon.test.synthesis

import org.scalatest.FunSuite

import leon.Evaluator
import leon.purescala.Trees._
import leon.purescala.Common._

import leon.synthesis.LinearEquations._
import leon.synthesis.LikelyEq

class LinearEquationsSuite extends FunSuite {

  def i(x: Int) = IntLiteral(x)

  val xId = FreshIdentifier("x")
  val x = Variable(xId)
  val yId = FreshIdentifier("y")
  val y = Variable(yId)
  val zId = FreshIdentifier("z")
  val z = Variable(zId)

  val aId = FreshIdentifier("a")
  val a = Variable(aId)
  val bId = FreshIdentifier("b")
  val b = Variable(bId)

  def toSum(es: Seq[Expr]) = es.reduceLeft(Plus(_, _))
  
  def checkSameExpr(e1: Expr, e2: Expr, vs: Set[Identifier], prec: Expr, defaultMap: Map[Identifier, Expr] = Map()) {
    assert( //this outer assert should not be needed because of the nested one
      LikelyEq(e1, e2, vs, prec, (e1, e2) => {assert(e1 === e2); true}, defaultMap)
    )
  }


  //use some random values to check that any vector in the basis is a valid solution to
  //the equation
  def checkVectorSpace(basis: Array[Array[Int]], equation: Array[Int]) {
    require(basis.size == basis(0).size + 1 && basis.size == equation.size)
    val n = basis(0).size
    val min = -5
    val max = 5
    val components = Array.fill(n)(min)
    var counter = 0

    while(counter < n) {
      val sol = mult(basis, components) //one linear combination of the basis
      assert(eval(sol, equation) === 0)

      //next components
      if(components(counter) < max)
        components(counter) += 1
      else {
        while(counter < n && components(counter) >= max) {
          components(counter) = min
          counter += 1
        }
        if(counter < n) {
          components(counter) += 1
          counter = 0
        }
      }
    }
  }

  //val that the sol vector with the term in the equation
  def eval(sol: Array[Int], equation: Array[Int]): Int = {
    require(sol.size == equation.size)
    sol.zip(equation).foldLeft(0)((acc, p) => acc + p._1 * p._2)
  }

  //multiply the matrix by the vector: [M1 M2 .. Mn] * [v1 .. vn] = v1*M1 + ... + vn*Mn]
  def mult(matrix: Array[Array[Int]], vector: Array[Int]): Array[Int] = {
    require(vector.size == matrix(0).size && vector.size > 0)
    val tmat = matrix.transpose
    var tmp: Array[Int] = null
    tmp = mult(vector(0), tmat(0))
    var i = 1
    while(i < vector.size) {
      tmp = add(tmp, mult(vector(i), tmat(i)))
      i += 1
    }
    tmp
  }

  def mult(c: Int, v: Array[Int]): Array[Int] = v.map(_ * c)
  def add(v1: Array[Int], v2: Array[Int]): Array[Int] = {
    require(v1.size == v2.size)
    v1.zip(v2).map(p => p._1 + p._2)
  }

  test("checkVectorSpace") {
    checkVectorSpace(Array(Array(1), Array(2)), Array(-2, 1))
    checkVectorSpace(Array(Array(4, 0), Array(-3, 2), Array(0, -1)), Array(3, 4, 8))
  }

  
  test("particularSolution basecase") {
    def toExpr(es: Array[Expr]): Expr = {
      val coef: Array[Expr] = es
      val vars: Array[Expr] = Array[Expr](IntLiteral(1)) ++ Array[Expr](x, y)
      es.zip(vars).foldLeft[Expr](IntLiteral(0))( (acc: Expr, p: (Expr, Expr)) => Plus(acc, Times(p._1, p._2)) )
    }

    val t1: Expr = Plus(a, b)
    val c1: Expr = IntLiteral(4)
    val d1: Expr = IntLiteral(22)
    val e1: Array[Expr] = Array(t1, c1, d1)
    val (pre1, (w1, w2)) = particularSolution(Set(aId, bId), t1, c1, d1)
    checkSameExpr(toExpr(e1), IntLiteral(0), Set(aId, bId), pre1, Map(xId -> w1, yId -> w2))
  }

  test("particularSolution preprocess") {
    def toExpr(es: Array[Expr], vs: Array[Expr]): Expr = {
      val coef: Array[Expr] = es
      val vars: Array[Expr] = Array[Expr](IntLiteral(1)) ++ vs
      es.zip(vars).foldLeft[Expr](IntLiteral(0))( (acc: Expr, p: (Expr, Expr)) => Plus(acc, Times(p._1, p._2)) )
    }

    val t1: Expr = Plus(a, b)
    val c1: Expr = IntLiteral(4)
    val d1: Expr = IntLiteral(22)
    val e1: Array[Expr] = Array(t1, c1, d1)
    val (pre1, s1) = particularSolution(Set(aId, bId), e1.toList)
    checkSameExpr(toExpr(e1, Array(x, y)), IntLiteral(0), Set(aId, bId), pre1, Array(xId, yId).zip(s1).toMap)

    val t2: Expr = Plus(a, b)
    val c2: Expr = IntLiteral(4)
    val d2: Expr = IntLiteral(22)
    val f2: Expr = IntLiteral(10)
    val e2: Array[Expr] = Array(t2, c2, d2, f2)
    val (pre2, s2) = particularSolution(Set(aId, bId), e2.toList)
    checkSameExpr(toExpr(e2, Array(x, y, z)), IntLiteral(0), Set(aId, bId), pre2, Array(xId, yId, zId).zip(s2).toMap)

    val t3: Expr = Plus(a, Times(IntLiteral(2), b))
    val c3: Expr = IntLiteral(6)
    val d3: Expr = IntLiteral(24)
    val f3: Expr = IntLiteral(9)
    val e3: Array[Expr] = Array(t3, c3, d3, f3)
    val (pre3, s3) = particularSolution(Set(aId, bId), e3.toList)
    checkSameExpr(toExpr(e3, Array(x, y, z)), IntLiteral(0), Set(aId, bId), pre3, Array(xId, yId, zId).zip(s3).toMap)
  }

}

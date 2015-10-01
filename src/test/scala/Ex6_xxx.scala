import org.scalatest.{Matchers, FlatSpec}

import scala.annotation.tailrec

/**
 * Created by administrator on 30/09/15.
 */
class Ex6_xxx extends FlatSpec {

  "flip" should "do stuff" in {

    val flip = (f: ((Char, Char) => String)) => (x: Char, y: Char) => f(y,x)

    val cat = (a: Char, b: Char) => "" + a + b

    assert( cat('a','b') == "ab")

    val flipcat = flip(cat)

    assert( flipcat('a','b') == "ba")
  }

}


class Ex6_yyy extends FlatSpec with Matchers {

  val double = (x: Int) => { x * 2 }
  val sum : (Int, Int) => Int = (x,y) => {x + y}
  val checkLength : (String, Int) => Boolean = (s, len) => { s.length == len }
  val isOdd = (num:Int) => ( (num % 2) == 1)
  val invertLong : (Boolean) => Boolean = (b) => {!b}
  val invert = (b:Boolean) => {!b}
  val isEven = isOdd andThen invert

  def mapper(min:Int, max:Int, f:(Int)=>Int) : Seq[Int] = {
    for(i <- min to max) yield f(i)
  }

  def sum3(x:Int)(y:Int)(z:Int) : Int = {x + y + z}

  @tailrec
  private def sum(seq:Seq[Int], acc:Int=0) : Int = {

    if(!seq.isEmpty) sum(seq.tail, acc+seq.head)
    else acc
  }

  @tailrec
  private def sumP(seq:Seq[Int], acc:Int=0) : Int = seq match {
    case( Nil ) => acc
    case( head :: tail ) => sumP(tail, acc+head)
  }

  "Double" should "double *duh*!" in {

    assert( double(4) == 8)
    assert( double(2) == 4)
    assert( double(0) == 0)
    assert( double(-7) == -14)
  }

  "Sum" should "errr.... sum" in {

    sum(1,2) shouldBe 3
    sum(3,-4) shouldBe -1
  }

  "Check length" should "check the length of a string" in {

    assert( checkLength( "", 0 ) )
    assert( checkLength( "123", 3 ) )
    assert( checkLength( "111", 2 ) == false )
    assert( checkLength( "123123", 6 ) )
  }

  "Check Odd" should "check numbers are odd!" in {
    assert( isOdd(1) )
    assert( isOdd(3) )
    assert( isOdd(2) == false )
  }

  "Check Even" should "check numbers are even using !isOdd" in {
    assert( isEven(4) )
    assert( isEven(2) )
    assert( isEven(3) == false )
  }

  "Check mapper" should "double 4 to 8" in {
    mapper(4, 8, (x:Int)=>x*2) shouldBe Seq(8, 10, 12, 14, 16)
  }

  it should "increment 5 to 7" in {
    mapper(5, 7, (x:Int)=>x+1) shouldBe Seq(6, 7, 8)
  }

  "Check curried sum3" should "allow simple summation" in {
    val sumPlus4and5 = sum3(4)(5)_

    sumPlus4and5(6) shouldBe 15
  }



  "sum" should "sum a sequence" in {
    sum( Seq(1,2,3) ) shouldBe 6
  }
  it should "sum another" in {
    sum( Seq(1,2,5) ) shouldBe 8
  }
  it should "return 0 for an empty sequence" in {
    sum( Seq() ) shouldBe 0
  }

  "sumP" should "sum a sequence" in {
    sumP( Seq(1,2,3) ) shouldBe 6
  }
  it should "sum another" in {
    sumP( Seq(1,2,5) ) shouldBe 8
  }
  it should "return 0 for an empty sequence" in {
    sumP( Seq() ) shouldBe 0
  }
}
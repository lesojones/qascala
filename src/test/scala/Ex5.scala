
import org.scalatest.{FlatSpec, Matchers}

//class Ex5 extends FlatSpec with Matchers {
//
//  "Luhn reverse and remove" should "return ((4,3,2,1), 5) for (1,2,3,4,5)" in {
//    Luhn2.reverseAndRemove("12345") should be (List(4,3,2,1), 5)
//  }
//
//}

class Ex5_Old extends FlatSpec with Matchers {

  "Luhn remove last number" should "have 5432 for 54321" in {

    Luhn.removeLastDigit("54321") should be ("5432",1)
  }

  it should "have 2345 for 23456" in {

    Luhn.removeLastDigit("23456") should be ("2345",6)
  }

  "Luhn then reversing the ccn" should "have 12345 for 54321" in {
    Luhn.reverse("54321") should be (Seq(1,2,3,4,5))
  }
  it should "have 987 for 789" in {
    Luhn.reverse("987") should be (Seq(7,8,9))
  }

  "Luhn then reversing & extract" should "have (List(2345),1) for List(5,4,3,2,1)" in {
    Luhn.reverseExtract("54321") should be (List(2,3,4,5),1)
  }
  it should "have (List(8,7),9) for List(7,8,9)" in {
    Luhn.reverseExtract("987") should be (List(8,9), 7)
  }

  "Luhn doDoubles" should "have (2,2,6,4,10) for (1,2,3,4,5)" in {
    Luhn.doDoubles( Seq(1,2,3,4,5) ) should be (Seq(2,2,6,4,10))
  }

  it should "have (10,4,6,2,2) for (5,4,3,2,1)" in {
    Luhn.doDoubles( Seq(5,4,3,2,1) ) should be (Seq(10,4,6,2,2))
  }

  "Luhn sub 9's" should "have (1,1,2,2) for (1,10,2,11)" in {
    Luhn.subtract9fromGT9(Seq(1,10,2,11)) should be (Seq(1,1,2,2))
  }

  it should "have (3,4,5,6) for (12,4,5,15)" in {
    Luhn.subtract9fromGT9(Seq(12,4,5,15)) should be (Seq(3,4,5,6))
  }

  "Luhn sum" should " return 5 for (1,2,2)" in {
    Luhn.sum(Seq(1,2,2)) should be (5)
  }

  it should " return 15 for (1,2,3,4,5)" in {
    Luhn.sum(Seq(1,2,3,4,5)) should be (15)
  }

  "Luhn" should "be valid for 4539783139842390" in {
    Luhn.checkValid("4539783139842390") should be (true)
  }

  it should "not be valid for 4539783139842391" in {
    Luhn.checkValid("4539783139842391") should be (false)
  }

  it should "be valid for 4485362699926580" in {
    Luhn.checkValid("4485362699926580") should be (true)
  }

  it should "be valid for 4556737586899855" in {
    Luhn.checkValid("4556737586899855") should be (true)
  }

  it should "be valid for 3096240644450216" in {
    Luhn.checkValid("3096240644450216") should be (true)
  }


}

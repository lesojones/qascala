// test

import org.scalatest.FlatSpec

/**
 * Created by administrator on 29/09/15.
 */
class Ex2_For extends FlatSpec {

  "Leap year" should "have 2012 as a leap year" in {

    assert( LeapYear.isLeapYear(2012) )
  }

  it should "have 2000 as a leap year" in {

    assert( LeapYear.isLeapYear(2000) )
  }

  it should "have 2004 as a leap year" in {

    assert( LeapYear.isLeapYear(2004) )
  }

  it should "not have 1999 as a leap year" in {

    assert( LeapYear.isLeapYear(1999) == false )
  }

  it should "pick out 2000 & 2004 as leap years between 1999 & 2005" in {

    val years = LeapYear.years(1999, 2005)
    assert( years.length == 2 )
    assert( years == Vector(2000, 2004))
  }

  it should "pick out 13 years as leap years between 1960 & 2010" in {

    val years = LeapYear.years(1960, 2010)
    assert( years.length == 13 )
    assert( years == Vector(1960, 1964, 1968, 1972, 1976, 1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008))
  }
}

class Ex2_Recursion extends FlatSpec {

  import XpowN._

  "power" should "be 1 for x^0" in {
    assert( xpown(1, 0) == 1)
    assert( xpown(4, 0) == 1)
    assert( xpown(6, 0) == 1)
  }

  it should "be 4 for 2^2" in {
    assert( xpown(2, 2) == 4)
  }

  it should "be 81 for 3^4" in {
    assert( xpown(3, 4) == 81)
  }

  it should "be 1/64 for 64^-1" in {
    assert( xpown(64, -1) == (1.0/64))
  }

  "factorial" should "be 1 for 1!" in {
    assert(factorial(1) == 1)
  }

  it should "be 6 for 3!" in {
    assert(factorial(3) == 6)
  }

  it should "be 120 for 5!" in {
    assert(factorial(5) == 120)
  }
}

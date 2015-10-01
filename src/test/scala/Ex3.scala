// test

import org.scalatest.FlatSpec

/**
 * Created by administrator on 29/09/15.
 */
class Ex3_Person extends FlatSpec {

  "A person" should "have a name age and email address" in {

    val stuff = ("bob", 18, "x@x.com")
    val p = new Person(stuff._1, stuff._2, stuff._3)

    assert( (p.name, p.age, p.email) == stuff )
  }

  it should "allow the age to be increased" in {

    val stuff = ("bob", 18, "x@x.com")
    val p = new Person(stuff._1, stuff._2, stuff._3)

    p.age = 20

    assert( (p.name, p.age, p.email) == (stuff._1, 20, stuff._3) )
  }

  it should "not allow the age to be decreased" in {

    val stuff = ("bob", 18, "x@x.com")
    val p = new Person(stuff._1, stuff._2, stuff._3)

    p.age = 16

    assert( (p.name, p.age, p.email) == (stuff._1, 18, stuff._3) )
  }
}

class Ex3_BankAcct extends FlatSpec {

  "A bank account" should "allow creation with acctnum & opening balance" in {

    val acct = new BankAccount("12345678", 123.45)
  }

  it should "allow creation with just the acctnum, setting balance to zero" in {

    val acct = new BankAccount("23456789")

    assert(acct.balance == 0)
  }

  it should "output wibble if set to a string" in {
    val acct = new BankAccount("12345678")

    assert( "" + acct == "Bank(12345678,0.0)")
  }

  it should "allow withdrawal" in {
    val acct = new BankAccount("12345678", 100.0)

    acct.withdraw(15)

    assert(acct.balance == 85)
  }

  it should "allow withdrawal of more than the balance" in {
    val acct = new BankAccount("12345678", 100.0)

    intercept[OverDrawnException] {
      acct.withdraw(150)
    }
  }

  it should "allow simple construction" in {
    val acct = BankAccount("87654321", 321.98)

    assert( (acct.acctnum, acct.balance) == ("87654321", 321.98) )
  }

  it should "allow sequential creation accounts with unique numbers" in {

    val acct1 = BankAccount()
    val acct2 = BankAccount()

    assert(acct1.acctnum != acct2.acctnum)
    assert(acct1.acctnum == "00000000")
    assert(acct2.acctnum == "00000001")
  }
}

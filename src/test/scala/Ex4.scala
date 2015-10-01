// test

import org.scalatest.FlatSpec

/**
 * Created by administrator on 29/09/15.
 */
class Ex4_Inheritance extends FlatSpec {

  "Animals" should "meow if a cat" in {
    val v = new Cat( "x", 1 )
    assert( v.move == "meow" )
  }

  it should "bark if a dog" in {
    val v = new Dog( "x", 1 )
    assert( v.move == "woof" )
  }

  it should "hop if a rabbit" in {
    val v = new Rabbit( "x", 1 )
    assert( v.move == "hop" )
  }

  "Savings account" should "add interest to the balance" in {

    val s = new SavingsAccount("12345678", 100, 0.10)

    s.addInterest()
    assert(s.balance == 110)

    s.addInterest()
    assert(s.balance == 121)
  }

  "Current account" should "allow an overdraft" in {

    val c = new CurrentAccount("12345678", 100, 150)

    c.withdraw(200)
    assert(c.balance == -100)
  }

  it should "not allow withdrawal over the limit" in {

    val c = new CurrentAccount("12345678", 100, 50)

    intercept[OverDrawnException] {
      c.withdraw(200)
    }

    assert(c.balance == 100)
  }
}

class Ex4_Traits extends FlatSpec {

  "vocal" should "allow the making of noise" in {

    val v : vocal = new vocal { def makeNoise = "foo" }

    assert(v.makeNoise == "foo")
  }

  // it should "allow a cat to meow" ignore { assert(new Cat("x",1).makeNoise == "meow") }
  it should "allow a dog to bark" in { assert(new Dog("x",1).makeNoise == "woof") }
  // it should "allow a rabbit to sniff" ignore { assert(new Rabbit("x",1).makeNoise == "*sniff*") }

  it should "allow a cat to be vocal via object extension" in {
    val c : vocal = new Cat("x",1) with vocal {override def makeNoise = "meow!"}
    assert(c.makeNoise == "meow!")
  }

  it should "allow a cat to be defaulted vocally" in {
    val c : Vocal2 = new Cat("x",1)
    assert(c.makeNoise == "WOOF!")
  }

  "logger" should "allow one to log" in {
    val l : Logger = new Logger {}
  }

}

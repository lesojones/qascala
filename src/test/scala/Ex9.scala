import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by administrator on 01/10/15.
 */
class Ex9 extends FlatSpec with Matchers {

  def removeFromList[A](list:List[A], removal:List[A]) : List[A] = {
    // list.filter(!removal.contains(_))

    def removeRule[A]( hl:List[A], tl:List[A]) : List[A] = tl match {
      case(Nil) => hl.reverse
      case(h :: t) if removal.contains(h) => removeRule(hl, t)
      case(h :: t) => removeRule(h :: hl, t)
    }
    removeRule(List[A](), list)
  }

  "2." should "remove foo and bar from a list" in {
    val res = removeFromList(List("foo","hoober","bar","foo","wibble"),List("foo","bar"))
    res shouldBe List("hoober","wibble")
  }
  it should "remove 2 and 3 from an int list" in {
    val res = removeFromList(1 to 5 toList, List(2,3))
    res shouldBe List(1,4,5)
  }

  class Address(val city:String, val country: String)
  class Person(val name: String, val address: Address)
  object Address {
    def apply(city: String, country: String) = new Address(city, country)
    def unapply(a: Address) = Some((a.city, a.country))
  }
  object Person {
    def apply(name:String, address:Address) = new Person(name, address)
    def unapply(p: Person) = Some((p.name, p.address))
  }

  def livesIn( p : Person ) = p match {
    case Person(_, Address("London", _)) => "LONDON"
    case Person(_, Address("New York", _)) => "NY"
    case Person(_, Address(city, _)) => city
  }

  "3." should "allow a person with an address to be created" in {
    Person("bob", Address("London", "UK"))
  }
  it should "return LONDON for a London resident" in {
    livesIn( Person("bob", Address("London", "UK")) ) shouldBe "LONDON"
  }
  it should "return NY for a New York resident" in {
    livesIn( Person("bert", Address("New York", "USA")) ) shouldBe "NY"
  }
  it should "return the city for elsewhere" in {
    livesIn( Person("brian", Address("Toronto", "Canada")) ) shouldBe "Toronto"
  }

  trait Expression { }
  case class Const(v: Int) extends Expression
  case class Neg(e: Expression) extends Expression
  case class Add(l: Expression, r: Expression) extends Expression

  "5." should "allow simple expression syntax" in {
    Add(Const(10), Neg(Add(Const(3), Const(4))))
  }

  def eval(e: Expression) : Int = e match {
    case Const(c) => c
    case Neg(i) => -(eval(i))
    case Add(l, r) => eval(l) + eval(r)
  }

  "6. eval" should "evaluate 10 + -(3 + 4)" in {
    val ex = Add(Const(10), Neg(Add(Const(3), Const(4))))
    eval(ex) shouldBe 3
  }

}

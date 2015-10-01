import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by administrator on 01/10/15.
 */
class Ex8 extends FlatSpec with Matchers {

  val list_2 = List("12", "34", "wibble", "54")
  val map_4 = Map("London" -> "UK", "Paris" -> "France", "Madrid" -> "Spain")
  val list_5 = List(1,2,3,4,5)

  def toInt(numString:String) : Option[Int] = {
    try {
      Some(numString.toInt)
    } catch {
      case _ : NumberFormatException => None
    }
  }

  def swapKVwithMap[K,V]( inMap: Map[K,V]) : Map[V,K] = {
    inMap.map( _ match { case(a,b) => (b,a) } )
//    inMap.map( {case(a,b) => (b,a)} )
  }

  def swapKV[K,V]( inMap: Map[K,V]) : Map[V,K] = {
    for( (k, v) <- inMap ) yield (v, k)
  }

  class Employee(val name:String, val reports: Employee* ) {
    val isManager : Boolean = reports.nonEmpty
  }
  object Employee {
    def apply(name: String) = new Employee(name)
    def apply(name: String, reports: Employee*) = new Employee(name, reports : _*)
  }
  val emp1 = Employee("bob")
  val emp2 = Employee("bert")
  val emp3 = Employee("frank", emp1, emp2)
  val empList = List(emp1, emp2, emp3)
  val managers = empList.filter( _.isManager )

  "1. toInt" should "return 1 for \"1\"" in { toInt("1") shouldBe Some(1) }
  it should "return 4 for \"4\"" in { toInt("4") shouldBe Some(4) }
  it should "return None for \"wibble\"" in { toInt("wibble") shouldBe None }

  "3." should "show map with toInt being used on a string list" in {
    list_2 map toInt shouldBe List(Some(12), Some(34), None, Some(54))
  }
  it should "show flatMap with toInt being used on a string list" in {
    list_2 flatMap toInt shouldBe List(12, 34, 54)
  }

  "4." should "cities in countries to countries with cities" in {
    swapKV(map_4) shouldBe Map("France" -> "Paris", "Spain" -> "Madrid", "UK" -> "London")
  }

  "4.other" should "cities in countries to countries with cities" in {
    swapKVwithMap( map_4 ) shouldBe Map("UK" -> "London", "France" -> "Paris", "Spain" -> "Madrid")
  }

  "5." should "sum using foldLeft" in { list_5.foldLeft(0)(_ + _) }

  "6." should "... already done ..." in { }

  "7." should "allow employees to be created" in {
    empList.length shouldBe 3
    empList map (_.name) shouldBe List("bob", "bert", "frank")
    empList map (_.reports.length) shouldBe List(0,0,2)
  }

  "8." should "filter managers" in {
    managers.length shouldBe 1
    managers map (_.name) shouldBe List("frank")
  }

  "9." should "create manager,employee tuple" in {
    managers.flatMap(
      m => m.reports.map( e => (m.name, e.name))
    ) shouldBe List( ("frank","bob"), ("frank","bert") )
  }

}

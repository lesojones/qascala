import org.scalatest.{Matchers, FlatSpec}

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

// Test

/**
 * Created by administrator on 01/10/15.
 */
class Ex7 extends FlatSpec with Matchers {

  val list_1 = List(1,2,3)
  val buff_2 = ArrayBuffer[Int](1,2,3,4,5,6,7,8,9,10)
  val list_3 = "a" :: "b" :: "c" :: Nil
  val list_5i = List(1,2,3,4,5,6,7)
  val list_5s = List("1","2","3","4","5","6","7")
  val list_6i = list_5i
  val list_6s = list_5s
  val list_7i = list_5i
  val list_7s = list_5s

  // not tail-recursive :-(
  def bufferToList( buff:ArrayBuffer[Int] ) : List[Int] = {
    if (buff.nonEmpty) (buff(0) :: bufferToList(buff.drop(1))) else List()
  }

  @tailrec
  private def getItemFromList[A](list:List[A], index: Int) : A = {
    if( index == 1 ) list.head else getItemFromList(list.tail, index -1)
  }

  // not tail-recursive :-(
  private def getIndexOld(list:List[Int], item:Int, index:Int=1) : Int = list match {
    case (head :: _) if head == item => index
    case (_ :: tail) => getIndexOld(tail, item, index+1)
    case _ => -1
  }

  def getIndex[A](list2:List[A], item2:A) : Int = {
    @tailrec
    def getIndexRule(l:List[A], elem:A, index:Int) : Int = l match {
      case (Nil) => -1
      case (head :: _) if head == elem => index
      case (_ :: tail) => getIndexRule(tail, elem, index + 1)
    }
    getIndexRule(list2, item2, 1)
  }

  def remove(list:List[Int], index:Int) : List[Int] = {
    def removeRule(headList:List[Int],index:Int,tailList:List[Int]) : List[Int] = tailList match {
      case( Nil ) => headList.reverse
      case( head :: tail ) if index == 1 => headList.reverse ::: tail
      case( head :: tail ) => removeRule(head :: headList, index - 1, tail)
    }
    removeRule(List[Int](),index,list)
  }

  def values( f: (Int)=>Int, min:Int, max:Int ) : Seq[(Int,Int)] = {
    (min to max).map( (i:Int) => (i, f(i)) ).toSeq
  }

  "2." should "use an ArrayBuffer to create a List of 1 to 10" in {
    bufferToList(buff_2) shouldBe List(1,2,3,4,5,6,7,8,9,10)
  }

  "3." should "create a list of strings using ::" in {
    list_3 shouldBe List("a", "b", "c")
  }

  "4." should "use 1 & 2 to concatenate" in {
    (list_1 ::: bufferToList(buff_2)) shouldBe List(1,2,3,1,2,3,4,5,6,7,8,9,10)
  }

  "5." should "get the 4th Int item manually" in { getItemFromList(list_5i, 4) shouldBe 4  }
  it should "get the 6th Int item manually" in { getItemFromList(list_5i, 6) shouldBe 6 }
  it should "get the 4th String item manually" in { getItemFromList(list_5s, 4) shouldBe "4" }
  it should "get the 6th String item manually" in { getItemFromList(list_5s, 6) shouldBe "6" }

  "6." should "get index pos of 3 (1 based) manually!" in { getIndex(list_6i, 3) shouldBe 3 }
  it should "get index pos of 7 (1 based) manually!" in { getIndex(list_6i, 7) shouldBe 7 }
  it should "return -1 if not in list" in { getIndex(list_6i, 9) shouldBe -1 }
  it should "get index pos of \"3\" (1 based) manually!" in { getIndex(list_6s, "3") shouldBe 3 }
  it should "get index pos of \"7\" (1 based) manually!" in { getIndex(list_6s, "7") shouldBe 7 }
  it should "return -1 if not in list of strings" in { getIndex(list_6s, "9") shouldBe -1 }

  "7." should "remove the 5th int item" in {remove(list_7i, 5) shouldBe List(1,2,3,4,6,7)}
  it should "remove the 7th int item" in {remove(list_7i, 7) shouldBe List(1,2,3,4,5,6)}
  it should "removing off the end should return the original list" in {
    remove(list_7i, 9) shouldBe list_7i
  }

  "8." should "double numbers for _*2" in {
    values( _ * 2 , 1, 5 ) shouldBe Seq((1,2), (2,4), (3,6), (4,8), (5,10))
  }
  it should "square numbers" in {
    values((i:Int) => (i * i), 1, 5) shouldBe Seq((1, 1), (2, 4), (3, 9), (4, 16), (5, 25))
  }

}


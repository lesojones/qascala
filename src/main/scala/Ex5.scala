/**
 * Created by administrator on 30/09/15.
 */
class Ex5 {

}

//object Luhn2 {
//  import Luhn.charToInt
//
//  def reverseAndRemove(ccn:List[Char]) : (List[Int], Int) = {
//
//    val x = ccn.foldLeft((List[Char](),None))((t, ch) => {
//      t => t match {
//        case (l, None) => (l,ch)
//        case (l : List[Char], Some(c)) => (c :: l, ch)
//      }
//    })
//
////    val x = ccn.fol[(List[Char],Char)]()
//
////    val x = ccn.foldLeft[Tuple2[List[Char],Option[Char]]]((Nil,None))( (ch, t) => t match {
////      case (l, None) => (l, Some(charToInt(ch)))
////      case (l:List[Int], sd)   => (charToInt(ch) :: l, sd)
////    })
//
//    val y : Tuple2[List[Int], Int] = x match {
//      case(l, Some(d)) => (l, d)
//    }
//
//    y
//  }
//}

object Luhn {

  def charToInt(ch: Char) : Int = { ch - 48 }

  def reverseExtract( ccn: String ) : (List[Int], Int) = {

    ccn.map( charToInt ).reverse.toList match {
      case( head :: tail ) => (tail, head)
    }
  }

  def isEven(i:Int) : Boolean = { ! isOdd(i) }
  def isOdd(i:Int)  : Boolean = { (i % 2) == 1 }

  def doSum( digits: Seq[Int] ) : Int = {
    val index = 1 to digits.size

    index.zip(digits).map( e => e match {
        case ( i, j ) if isOdd(i) => j*2
        case ( _, j ) => j
      }
    ).map( _ match {
        case x if x > 9 => x-9
        case y => y
      }
    ).foldLeft(0)(_ + _)
  }

  def checkValid(ccn:String) : Boolean = {
    reverseExtract(ccn) match {
      case ( digits, last ) => {
        //        val total : Int = sum( subtract9fromGT9( doDoubles( digits ) ) )
        val total : Int = doSum( digits )

        (( total + last ) % 10 == 0)
      }
    }
  }

  // --------

  def removeLastDigit(ccn: String) : (String,Int) = {
    (ccn.take(ccn.size-1), charToInt(ccn.last))
  }

  def reverse( num: String) : Seq[Int] = {
    num.reverse.map( charToInt )
  }

  def doDoubles( digits: Seq[Int] )= {
    val index = 1 to digits.size

    index.zip(digits).map( e => e match {
        case ( i, j ) if isOdd(i) => j*2
        case ( _, j ) => j
      }
    )
  }

  def subtract9fromGT9( digits: Seq[Int]) : Seq[Int] = {
    digits.map( _ match {
      case x if x > 9 => x-9
      case y => y
    })
  }

  def sum( digits: Seq[Int] ) : Int = {
    digits.foldLeft(0)(_ + _)
  }


}

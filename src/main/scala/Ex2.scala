// Copyright (C) 2011-2012 the original author or authors.
// See the LICENCE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

import scala.annotation.tailrec

/**
 * Created by administrator on 29/09/15.
 */

object LeapYear {

  def isLeapYear( year: Int ) : Boolean = { (year %4 == 0) && (year % 100 != 0) || (year %400 ==0) }

  def years( fromYear: Int, toYear: Int) : Seq[Int] = {
    for( year <- fromYear to toYear if isLeapYear(year)) yield year
  }
}

object XpowN {

  def xpown( x: Double, n: Int ) : Double = {

    if( n == 0 ) { 1 }
    else if( n > 0 ) {
      if(n % 2 == 0) { val y = xpown(x, n/2 ); y*y }
      else { x * xpown( x, n-1) }
    } else {
        ( 1 / xpown(x, -n) )
    }
  }

  def factorialOld( n : Int) :Int = n match {

    case 1 => 1
    case _ => n * factorialOld(n-1)
  }

  def factorial( n : Int) :Int = {
    @tailrec
    def factWithAct( acc: Int, num : Int ) : Int = num match {
      case 1 => acc
      case _ => factWithAct( acc * num, num - 1 )
    }

    factWithAct(1, n)
  }

}

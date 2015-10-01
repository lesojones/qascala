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

//import java.io.{PrintWriter, Writer}

abstract class Animal(val name: String, val age: Int) { //extends vocal {

  override def toString() : String = s"Animal($name,$age)"
  def move : String
}

class Cat(private val _name: String, private val _age: Int) extends Animal(_name, _age) with Vocal2 {

  def move : String = "meow"
  // def makeNoise: String = "meow"

}

class Dog(private val _name: String, private val _age: Int) extends Animal(_name, _age) with vocal {

  def move : String = "woof"
  def makeNoise: String = "woof"
}

class Rabbit(private val _name: String, private val _age: Int) extends Animal(_name, _age) {

  def move : String = "hop"
  // def makeNoise: String = "*sniff*"
}


class SavingsAccount(private val _acctnum : String, private var _bal : Double, val intRate: Double)
  extends BankAccount(_acctnum, _bal) {

  def addInterest() { balance += (balance * intRate) }
}

class CurrentAccount(private val _acctnum : String, private var _bal : Double, val overdraft: Double)
  extends BankAccount(_acctnum, _bal) {

  override def withdraw(withdrawal:Double) {
    if(withdrawal <= (balance + overdraft) ) { balance -= withdrawal }
    else { throw new OverDrawnException }
  }
}

trait vocal {
  def makeNoise : String
}

trait Vocal2 extends vocal {
  def makeNoise : String = "WOOF!"
}

trait Logger {
  def log(message: String) {}
}

//implicit def w2p(w: Writer) : PrintWriter = { new PrintWriter(w) }
//trait ConsoleLogger(val w: Writer) extends Logger {
//  override def log(message: String) { w.print ln(message) }
//}

// trait TimestampLogger

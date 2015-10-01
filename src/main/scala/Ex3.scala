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

class Person(private var _name: String, private var _age: Int, private var _email: String) {

  def name: String = _name

  def age: Int = _age

  def age_=(age:Int) { if( age > _age) _age = age}

  def email: String = _email
}

class OverDrawnException extends Exception

class BankAccount(val acctnum : String, var _balance : Double) {

  def this(acctnum: String) = this(acctnum, 0)

  def balance: Double = _balance
  protected def balance_=(newBalance:Double) = _balance = newBalance

  override def toString() : String = s"Bank(${acctnum},${balance})"

  def withdraw(withdrawal:Double) {if(withdrawal <= balance) balance -= withdrawal else throw new OverDrawnException}
}

object BankAccount {

  private var _nextAcctNum : Int = 0
  private def nextAcctNum = {
    val num = _nextAcctNum ; _nextAcctNum += 1
    f"${num}%08d"
  }

  def apply(num: String, bal: Double) : BankAccount = new BankAccount(num, bal)
  def apply() : BankAccount = new BankAccount(nextAcctNum, 0)
}

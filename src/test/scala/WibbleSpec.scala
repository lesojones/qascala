// test

import org.scalatest.FlatSpec

/**
 * Created by administrator on 29/09/15.
 */
class WibbleSpec extends FlatSpec {

  "wibble" should "have a nermal" in {
    val wibble = new Wibble()
    assert( wibble.nermal == "nermal")
  }

  it should "have a suffix to prefix" in {
    val wibble = new Wibble("xxx")
    assert( wibble.suffix("yyy") == "xxxyyy")
  }

  it should "implicitly have a suffix to prefix" in {

    import Wibble.convertStringToWibbleX

    assert( ("xxx" suffix  "yyy") == "Xxxxyyy")
  }

}

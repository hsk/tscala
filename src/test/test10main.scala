package test

import org.junit._
import Assert._
import java.io.FileInputStream
import reader.parse
import compiler._

class test10main {

  @Test
  def test_string2 {
    main.main(Array[String]("src/lib/test.dia"))
  }

}

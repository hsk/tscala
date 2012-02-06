package test.compiler

import org.junit.Assert._;
import org.junit.Test;
import compiler._

class test03genid {

  @Test
  def 数字が連番で出力される確認 {
    assertEquals("a1", genid("a"))
    assertEquals("b2", genid("b"))
    assertEquals("a3", genid("a"))
  }

}
package test

import org.junit._
import Assert._
import java.io.FileInputStream
import reader.parse
import compiler._
import script._

class test12eval {
  @Test def test_add {
    assertEquals(3,eval("1+2"))
  }
  @Test def test_add_float {
    assertEquals(3.1,eval("1.1+2"))
  }
  @Test def test_add_string {
    assertEquals("1.12",eval("""1.1+"2" """))
    assertEquals("1.12",eval(""" "1.1"+2 """))
  }
  @Test def test_while {
    val prg = "a = 0; while(a<10){a++} a"
    assertEquals(10,eval(prg))
  }
  @Test def test_while_break {
    val prg = "a = 0; while(a<10){a++;break} a"
    assertEquals(1,eval(prg))
  }
  @Test def test_while_break2 {
    val prg = "a = 0; while(true){a++;if(a>10)break;} a"
    assertEquals(11,eval(prg))
  }
  @Test def test_while_continue {
    val prg = "b = 0; a = 0; while(a<10){a++; if(a%2==0)continue; b++;} b"
    assertEquals(5,eval(prg))
  }
  // TODO switch
  @Test def test_switch {
    val prg = "a=2; switch(a){case(1){a=11}case(2){a=22}} a"
    assertEquals(22, eval(prg))
  }
}

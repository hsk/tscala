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
  @Test def test_while_break_name {
    val prg = "a = 0; lp:while(true){while(true){a++;if(a>10)break lp;}} a"
    assertEquals(11,eval(prg))
  }
  @Test def test_while_continue {
    val prg = "b = 0; a = 0; while(a<10){a++; if(a%2==0)continue; b++;} b"
    assertEquals(5,eval(prg))
  }
  @Test def test_while_continue_name {
    val prg = "b = 0; a = 0; lp:while(a<10){while(true){a++; if(a%2==0)continue lp; b++;}} b"
    assertEquals(5,eval(prg))
  }
  @Test def test_switch {
    val prg = "a=2; switch(a){case(1){a=11}case(2){a=22}} a"
    assertEquals(22, eval(prg))
  }
  @Test def test_for {
    val prg = "a=1;for(i=0;i<10;i++){a++;} a"
    assertEquals(11, eval(prg))
  }
  @Test def test_for_break {
    val prg = "a=0;for(i=0;i<10;i++){if(i>5)break;a++;} a"
    assertEquals(6, eval(prg))
  }
  @Test def test_for_continue {
    val prg = "a=1;for(i=0;i<10;i++){if(i %2==0)continue;a++;} a"
    assertEquals(6, eval(prg))
  }
  @Test def test_for_break_name {
    val prg = "a=0;lp:for(j=0;j<10;j++){for(i=0;i<10;i++){if(i>5)break lp;a++;}} a"
    assertEquals(6, eval(prg))
  }
  @Test def test_for_continue_name {
    val prg = "a=1;lp:for(i=0;i<10;i++){for(j=0;j<1;j++){if(i %2==0)continue lp;a++;}} a"
    assertEquals(6, eval(prg))
  }
}

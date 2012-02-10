package test

import org.junit._
import Assert._
import java.io.FileInputStream
import reader.parse
import compiler._

class test07setmem {

  @Test
  def test_setmem {
    genid.counter = 0
    val prg = List(
      EFundef("_main", TUnit(), List(), List(
        ECall("_printInt", List(ECall("_add", List(EInt(1), EInt(2), EInt(30))))))),
      EFundef("_add", TUnit(), List("a", "b", "c"), List(
        ERet(EAdd(EStr("a"), EAdd(EStr("b"), EStr("c")))))))
    val s = setmem(prg)
    println("s=" + s)
    assertEquals(
        "List(EFundef(_main,TUnit(),List(),List(EMov(EInt(30),EStr(s_3)), EMov(EInt(2),EStr(s_2)), EMov(EInt(1),EStr(s_1)), ECall(_printInt,List(ECall(_add,List(EStr(s_1), EStr(s_2), EStr(s_3))))))), EFundef(_add,TUnit(),List(a, b, c),List(ERet(EAdd(EStr(a),EAdd(EStr(b),EStr(c)))))))",
        s+""
    )
    val e = expand(s)
    val m = memAlloc(e)
    emit("e.s", m)
    exec("gcc -m64 -o e e.s src/lib/lib.c")
  }
}

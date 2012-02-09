package test

import org.junit._
import Assert._
import java.io.FileInputStream
import reader.parse
import compiler._

class test09parse {

  @Test
  def test_string {
    val prg = """def main(){ a="abc"; printInt(1)} """
    val st = parse(prg)
    val ast = st2ast(st)
    println("ast=" + ast)
    val s = setmem(ast)
    println("s=" + s)
    val e = expand(s)
    println("e=" + e)
    val m = memAlloc(e)
    println("m=" + m)
    emit("e_string.s", m)

  }
}

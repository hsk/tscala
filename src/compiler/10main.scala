package compiler

import java.io._
import reader._
import script._

object main {
  def main(argv:Array[String]) {
    val prg = exec.readAll(new FileInputStream(argv(0)))// string
    val st = parse(prg)// any
    val st2 = macro(st)// any
    val ast = st2ast(st2) // any
    val s = setmem(ast) // syntax.t
    val e = expand(s) // knormal.t
    val m = memAlloc(e) // x86.t
    emit("e.s", m)
    exec("gcc -m64 -o e e.s src/lib/lib.c")
    exec("./e")
  }
}

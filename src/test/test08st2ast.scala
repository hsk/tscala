package test

import org.junit._
import Assert._
import java.io.FileInputStream
import reader.parse
import compiler._

class test08st2ast {

  @Test
  def test_st2ast {
    val st =
      (
        (
          'def,
          (
            ('main, Symbol("("), 'void, Symbol(")")),
            Symbol("{"),
            (
              'printInt,
              Symbol("("),
              ('add, Symbol("("), (1, Symbol(","), (2, Symbol(","), 3)), Symbol(")")),
              Symbol(")")),
              Symbol("}"))),
              '@,
              (
                'def,
                (
                  ('add, Symbol("("), ('a, Symbol(","), ('b, Symbol(","), 'c)), Symbol(")")),
                  Symbol("{"),
                  ('a, '+, ('b, '+, 'c)),
                  Symbol("}"))))
    val ast = st2ast(st)
    println("ast=" + ast)
    val s = setmem(ast)
    val e = expand(s)
    val m = memAlloc(e)
    emit("e.s", m)
    exec("gcc -m64 -o e e.s src/lib/lib.c")
  }
}

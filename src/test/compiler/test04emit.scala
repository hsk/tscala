package test.compiler

import org.junit._
import Assert._
import java.io.FileInputStream
import reader.parse
import compiler._

class test04emit {


  @Test
  def test_emit {
    // 1を出力するプログラム
    val prg = List(
      Fundef("_main", List(
        Movl("$1", "%edi"), // スタックに1をつむ
        Call("_printInt", List()) // printInt関数を呼び出す
        )))
    // コード出力
    emit("e.s", prg)

    // コンパイル
    exec("gcc -m64 -o e e.s src/lib/lib.c") match {
      // 成功時
      case (0, a, b) =>
        // 実行
        val (a, b, c) = exec("./e")
        // 結果
        assertEquals(b, "1\n")
      case _ => fail("test_emit fail")
    }
  }
}

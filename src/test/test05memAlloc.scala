package test

import org.junit._
import Assert._
import java.io.FileInputStream
import reader.parse
import compiler._

class test05memAlloc {

  @Test
  def test_memAlloc {

    // プログラム
    val prgs = List(
      MFundef("_main", List(
        MMovl("$5", "a"),
        MCall("_printInt", List("a"))))
    )

    // memAlloc
    val l = memAlloc(prgs)

    // 結果
    assertEquals(l,
      List(
        Fundef("_main",
          List(
              Subq("$16", "%rsp"),
              Movl("$5", "-4(%rbp)"),
              Call("_printInt", List("-4(%rbp)")))))
    )

    // コード出力
    emit("e.s", l)
    val (rc, a, b) = exec("gcc -m64 -o e e.s src/lib/lib.c")
    // 結果
    assertEquals(rc, 0)

    // 実行
    val (r2, a2, c2) = exec("./e")

    // 結果 5
    assertEquals(a2, "5\n")
  }
}

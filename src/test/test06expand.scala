package test

import org.junit._
import Assert._
import java.io.FileInputStream
import reader.parse
import compiler._

class test06expand {

  @Test
  def test_expand {
    genid.counter = 0
    
    // 展開前プログラム
    val prg = List(
      EFundef("_main", TUnit(), List(), List(
        EMov(EInt(10), EStr("a")),
        EMov(EInt(1), EStr("b")),
        EMov(EInt(2), EStr("c")),
        EMov(ECall("_printInt", List(EAdd(EStr("a"), EAdd(EStr("b"), EStr("c"))))), EStr("d")),
        ERet(EStr("d")))),
      EFundef("_add", TUnit(), List("a", "b"), List(
        ERet(EAdd(EStr("a"), EStr("b"))))
        )
    )
    // 展開
    val p = expand(prg)

    // 結果
    assertEquals(
      List(
        MFundef("_main",TUnit(), 
          List(
            MMovl("$10", "a"),
            MMovl("$1", "b"),
            MMovl("$2", "c"),
            MAddl("b", "c", "ex_2"),
            MAddl("a", "ex_2", "ex_1"),
            MCall("_printInt", List("ex_1")),
            MMovl("%eax", "d"),
            MRet("d"))),
            MFundef("_add",TUnit(), 
          List(
            MMovl("%esi", "b"),
            MMovl("%edi", "a"),
            MAddl("a", "b", "ex_3"),
            MRet("ex_3")))),
      p
    )

    // メモリ配置
    val m = memAlloc(p)

    // メモリ配置結果
    assert(m + "" == "List(Fundef(_main,TUnit(),List(Subq($32,%rsp), Movl($10,-4(%rbp)), Movl($1,-8(%rbp)), Movl($2,-12(%rbp)), Addl(-8(%rbp),-12(%rbp),-16(%rbp)), Addl(-4(%rbp),-16(%rbp),-20(%rbp)), Call(_printInt,List(-20(%rbp))), Movl(%eax,-24(%rbp)), Ret(-24(%rbp)))), Fundef(_add,TUnit(),List(Subq($16,%rsp), Movl(%esi,-4(%rbp)), Movl(%edi,-8(%rbp)), Addl(-8(%rbp),-4(%rbp),-12(%rbp)), Ret(-12(%rbp)))))")

    // コード出力
    emit("e.s", m)

    // コンパイル
    exec("gcc -m64 -o e e.s src/lib/lib.c")

  }

}

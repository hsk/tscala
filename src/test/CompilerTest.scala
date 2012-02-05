package test

import org.junit._
import Assert._
import java.io.FileInputStream
import reader.parse
import compiler._

class CompilerTest {

  @Test
  def test_asm {
    asm.open("a.txt")
    asm("test")
    asm.close()
    Assert.assertEquals("test\n", exec.readAll(new FileInputStream("a.txt")))
  }

  @Test
  def test_genid {
    assertEquals("a1", genid("a"))
    assertEquals("b2", genid("b"))
    assertEquals("a3", genid("a"))
  }

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

  @Test
  def test_expand {
  	// 展開前プログラム
  	val prg = List(
      EFundef("_main", List(), List(
        EMov(EInt(10), EStr("a")),
        EMov(EInt(1), EStr("b")),
        EMov(EInt(2), EStr("c")),
        EMov(ECall("_printInt", List(EAdd(EStr("a"), EAdd(EStr("b"), EStr("c"))))), EStr("d")),
        ERet(EStr("d")))),
      EFundef("_add", List("a", "b"), List(
        ERet(EAdd(EStr("a"), EStr("b"))))
        )
  	)
    // 展開
    val p = expand(prg)

    // 結果
    assertEquals(
      List(
        MFundef("_main",
          List(
            MMovl("$10", "a"),
            MMovl("$1", "b"),
            MMovl("$2", "c"),
            MAddl("b", "c", "ex_2"),
            MAddl("a", "ex_2", "ex_1"),
            MCall("_printInt", List("ex_1")),
            MMovl("%eax", "d"),
            MRet("d"))),
            MFundef("_add",
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
    assert(m + "" == "List(Fundef(_main,List(Subq($32,%rsp), Movl($10,-4(%rbp)), Movl($1,-8(%rbp)), Movl($2,-12(%rbp)), Addl(-8(%rbp),-12(%rbp),-16(%rbp)), Addl(-4(%rbp),-16(%rbp),-20(%rbp)), Call(_printInt,List(-20(%rbp))), Movl(%eax,-24(%rbp)), Ret(-24(%rbp)))), Fundef(_add,List(Subq($16,%rsp), Movl(%esi,-4(%rbp)), Movl(%edi,-8(%rbp)), Addl(-8(%rbp),-4(%rbp),-12(%rbp)), Ret(-12(%rbp)))))")

    // コード出力
    emit("e.s", m)

    // コンパイル
    exec("gcc -m64 -o e e.s src/lib/lib.c")

  }
  
  @Test
  def test_setmem {
    val prg = List(
      EFundef("_main", List(), List(
        ECall("_printInt", List(ECall("_add", List(EInt(1), EInt(2), EInt(30))))))),
      EFundef("_add", List("a", "b", "c"), List(
        ERet(EAdd(EStr("a"), EAdd(EStr("b"), EStr("c")))))))
    val s = setmem(prg)
    println("s=" + s)
    assert(s + "" == "List(EFundef(_main,List(),List(EMov(EInt(30),EStr(s_3)), EMov(EInt(2),EStr(s_2)), EMov(EInt(1),EStr(s_1)), ECall(_printInt,List(ECall(_add,List(EStr(s_1), EStr(s_2), EStr(s_3))))))), EFundef(_add,List(a, b, c),List(ERet(EAdd(EStr(a),EAdd(EStr(b),EStr(c)))))))")
    val e = expand(s)
    val m = memAlloc(e)
    emit("e.s", m)
    exec("gcc -m64 -o e e.s src/lib/lib.c")
  }

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

  @Test
  def test_emit_string {
    // 1を出力するプログラム
    emit("test_emit.s", List(
      Fundef("_main", List(
        Movl("$1", "%edi"), // スタックに1をつむ
        Ascii("lb1", "test"), // スタックに1をつむ
        Call("_printInt", List()) // printInt関数を呼び出す
        ))))
  }

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

  @Test
  def test_string2 {
    main.main(Array[String]("src/lib/test.dia"))
  }
}

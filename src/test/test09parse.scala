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

    // パース
    val st = parse(prg)
    // 結果
    assertEquals(
      "('def,(('main,'(,'void,')),'{,((('a,'=,abc),';),'@,('printInt,'(,1,'))),'}))",
      st + "")

    // 抽象構文木へ変換
    val ast = st2ast(st)
    // 結果
    assertEquals(
      List(EFundef("_main", List(), List(EMov(EAscii("abc", "str_1"), EStr("a")), ECall("_printInt", List(EInt(1)))))),
      ast)

    // 定数をメモリに割り当ててしまう
    val s = setmem(ast)
    // 結果
    assertEquals(
      "List(EFundef(_main,List(),List(EMov(EInt(1),EStr(s_2)), EMov(EAscii(abc,str_1),EStr(a)), ECall(_printInt,List(EStr(s_2))))))",
      s + "")

    // 複雑な式をフラットに展開
    val e = expand(s)
    // 結果
    assertEquals(
      "List(MFundef(_main,List(MMovl($1,s_2), MAscii(abc,str_1), MLeaq(str_1,a), MCall(_printInt,List(s_2)))))",
      e + "")

    // メモリ割当て
    val m = memAlloc(e)
    // 結果
    assertEquals(
      "List(Fundef(_main,List(Subq($16,%rsp), Movl($1,-4(%rbp)), Ascii(abc,str_1), Leaq(str_1,-12(%rbp)), Call(_printInt,List(-4(%rbp))))))",
      m + "")

    // コード出力
    emit("e_string.s", m)
    // 結果

  }

  @Test
  def test_1add2 {
    val prg = "1+2"
    // 足し算
    val st = parse(prg)
    // 結果
    assertEquals(
      st,
      (1, '+, 2))
  }
  
  @Test
  def test_1add2add3 {
    val prg = "1+2+3"
    // 足し算２つ
    val st = parse(prg)
    // 結果
    assertEquals(
      st,
      ((1, '+, 2), '+, 3))
  }

  @Test
  def test_crlf {
    val prg = "1\r\n+2+3"
    // 足し算２つ
    val st = parse(prg)
    // 結果
    assertEquals(
      st,
      ((1, '+, 2), '+, 3))
  }

}

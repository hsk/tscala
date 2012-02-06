/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package compiler

abstract class X86_64
case class Movl(a: String, b: String) extends X86_64
case class Subq(a: String, b: String) extends X86_64
case class Addl(a: String, b: String, c: String) extends X86_64
case class Subl(a: String, b: String, c: String) extends X86_64
case class Call(n: String, b: List[Any]) extends X86_64
case class Ret(a: String) extends X86_64
case class Ifeq(a: String, b: String, c: List[X86_64], d: List[X86_64]) extends X86_64
case class Ascii(a: String, b: String) extends X86_64
case class Leaq(a: String, b: String) extends X86_64
case class Fundef(name: String, body: List[X86_64])

object emit {

  /**
   * コード出力処理
   * 
   * @param filename アセンブラ出力ファイル名
   * @param ls 関数定義リスト
   */
  def apply(filename: String, ls: List[Fundef]) {
    // ファイルオープン
    asm.open(filename)

    // リストをループして処理
    ls.foreach {
      case Fundef(name: String, body: List[X86_64]) =>
        // 文字列出力
        asm(".cstring")
        // 文字列出力関数
        def s(e: X86_64): Any = e match {
          // asciiだけ出力
          case Ascii(name, a) =>
            asm(name + ":")
            asm("\t.ascii \"" + a + "\\0\"")
          case Ifeq(a, b, c, d) =>
            c.foreach(s)
            d.foreach(s)
          // ascii以外は無視
          case _ =>
        }
        // body内の文字列出力
        body.foreach(s)

        // プログラム出力
        asm(".text")

        // グローバルなシンボル
        asm(".globl " + name)
        asm(name + ":")

        // 関数のエンター処理
        asm("\tpushq\t%rbp")
        asm("\tmovq\t%rsp, %rbp")

        // 各議事命令の展開
        def f(e: X86_64) {
          e match {
            case Movl(a, b) if(a==b) =>
            // movl は %eaxに一度転送してから bへ転送する
            case Movl(a, b) =>
              asm("movl " + a + ", %eax")
              asm("movl %eax, " + b)
            // 引き算
            case Subq(a, b) => asm("subq " + a + ", " + b)
            // 足し算
            case Addl(a, b, c) =>
              asm("movl " + a + ", %eax")
              asm("addl " + b + ", %eax")
              asm("movl %eax, " + c)
            // 引き算
            case Subl(a, b, c) =>
              asm("movl " + a + ", %eax")
              asm("subl " + b + ", %eax")
              asm("movl %eax, " + c)
            // 関数呼び出し
            case Call(n, b) =>
              prms(b, regs)
              asm("call " + n)
            // リターン
            case Ret(a) =>
              asm("movl " + a + ", %eax")
              asm("leave")
              asm("ret")
            // If文
            case Ifeq(a, b, c, d) =>
              val id_else = genid("id_else")
              val id_cont = genid("id_cont")
              asm("movl " + a + ", %eax")
              asm("cmpl " + b + ", %eax")
              asm("jne " + id_else)
              c.foreach(f)
              asm(id_else + ":")
              asm("jmp " + id_cont)
              c.foreach(f)
              asm(id_cont + ":")
            // 文字列は関数内には出力しない
            case Ascii(a, b) =>
            // Leaq
            case Leaq(a, b) =>
              asm("leaq " + a + "(%rip), %rax")
              asm("movq %rax, " + b)
          }
        }
        // ボディに対してアセンブラ出力
        body.foreach(f)
        // 関数終了処理
        asm("\tleave")
        asm("\tret")
    }
    // ファイルクローズ
    asm.close()
  }

  val regs = List("%edi", "%esi", "%edx")
  def prms(ps: List[Any], rs: List[Any]) {
    (ps, rs) match {
      case (List(), _) =>
      case (p :: ps, r :: rs) =>
        asm("movl " + p + ", " + r)
        prms(ps, rs)
      case (_, List()) =>
    }
  }
}

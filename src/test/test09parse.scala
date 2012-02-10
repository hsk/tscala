package test

import org.junit._
import Assert._
import java.io.FileInputStream
import reader.parse
import compiler._

class test09parse {

  @Test def test_string {
    genid.counter = 0
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

  @Test def test_1add2 {
    val prg = "1+2"
    // 足し算
    val st = parse(prg)
    // 結果
    assertEquals(
      st,
      (1, '+, 2))
  }
  
  @Test def test_1add2add3 {
    val prg = "1+2+3"
    // 足し算２つ
    val st = parse(prg)
    // 結果
    assertEquals(
      st,
      ((1, '+, 2), '+, 3))
  }

  @Test def test_crlf {
    val prg = "1\r\n+2+3"
    // 足し算２つ
    val st = parse(prg)
    // 結果
    assertEquals(
      st,
      ((1, '+, 2), '+, 3))
  }

  @Test def test_1add2mul3 {
    
  }

  @Test def test_1mul2add3 {
  }

  // infixs
  @Test def test_paren_message() {
    assertEquals(
        ('a,Symbol("("),'b,Symbol(")")),
        parse("a(b)")
    )
  }
  @Test def test_array_message() {
    assertEquals(
        ('a,Symbol("["),'b,Symbol("]")),
        parse("a[b]")
    )
  }
  @Test def test_brace_message() {
    assertEquals(
        (('a,Symbol("{"),'b,Symbol("}")),Symbol("{"),'d,Symbol("}")),
        parse("a{b}{d}")
    )
  }

  @Test def test_dot() {
    assertEquals(
        (('a,Symbol("."),'b),Symbol("."),'c),
        parse("a.b.c")
    )
  }
  @Test def test_post_plpl() {
    //'++ -> ("e", 160),
    assertEquals(
        ('e,'++),
        parse("e++")
    )
  }
  @Test def test_post_mnmn() {
    // '-- -> ("e", 160),
    assertEquals(
        ('e,'--),
        parse("e--")
    )
  }

  @Test def test_mul() {
    // '* -> ("l", 150),
    assertEquals(
        ('e,'*,'f),
        parse("e*f")
    )
  }

  @Test def test_div() {
    // '/ -> ("l", 150),
    assertEquals(
        ('e,'/,'f),
        parse("e/f")
    )
  }
  @Test def test_mod() {
    // '% -> ("l", 150),
    assertEquals(
        ('e,'%,'f),
        parse("e%f")
    )
  }
  @Test def test_add() {
    // '+ -> ("l", 140),
    assertEquals(
        ('e,'+,'f),
        parse("e+f")
    )
  }
  @Test def test_sub() {
    // '- -> ("l", 140),
    assertEquals(
        ('e,'-,'f),
        parse("e-f")
    )
  }
  @Test def test_ltlt() {
    // '<< -> ("l", 130),
    assertEquals(
        ('e,'<<,'f),
        parse("e<<f")
    )
  }
  @Test def test_gtgt() {
    // '>> -> ("l", 130),
    assertEquals(
        ('e,'>>,'f),
        parse("e>>f")
    )
  }
  @Test def test_gtgtgt() {
    // '>>> -> ("l", 130),
    assertEquals(
        ('e,'>>>,'f),
        parse("e>>>f")
    )
  }
  @Test def test_lt() {
    // '< -> ("l", 120),
    assertEquals(
        ('e,'<,'f),
        parse("e<f")
    )
  }
  @Test def test_lteq() {
    // '<= -> ("l", 120),
    assertEquals(
        ('e,'<=,'f),
        parse("e<=f")
    )
  }
  @Test def test_gt() {
    // '> -> ("l", 120),
    assertEquals(
        ('e,'>,'f),
        parse("e>f")
    )
  }
  @Test def test_gteq() {
    // '>= -> ("l", 120),
    assertEquals(
        ('e,'>=,'f),
        parse("e>=f")
    )
  }
  @Test def test_instanceof() {
    // 'instanceof -> ("l", 120),
    assertEquals(
        ('e,'instanceof,'f),
        parse("e instanceof f")
    )
  }
  @Test def test_in() {
    // 'in -> ("l", 120),
    assertEquals(
        ('e,'in,'f),
        parse("e in f")
    )
  }
  @Test def test_eqeq() {
    // '== -> ("l", 110),
    assertEquals(
        ('e,'==,'f),
        parse("e==f")
    )
  }
  @Test def test_noteq() {
    // '!= -> ("l", 110),
    assertEquals(
        ('e,'!=,'f),
        parse("e!=f")
    )
  }
  @Test def test_eqeqeq() {
    // '=== -> ("l", 100),
    assertEquals(
        ('e,'===,'f),
        parse("e===f")
    )
  }
  @Test def test_noteqeq() {
    // '!== -> ("l", 100),
    assertEquals(
        ('e,'!==,'f),
        parse("e!==f")
    )
  }
  @Test def test_and() {
    // '& -> ("l", 90),
    assertEquals(
        ('e,'&,'f),
        parse("e&f")
    )
  }
  @Test def test_xor() {
    // '^ -> ("l", 80),
    assertEquals(
        ('e,'^,'f),
        parse("e^f")
    )
  }
  
  @Test def test_or() {
    // '| -> ("l", 70),
    assertEquals(
        ('e,'|,'f),
        parse("e|f")
    )
  }
  @Test def test_andand() {
    // '&& -> ("l", 60),
    assertEquals(
        ('e,'&&,'f),
        parse("e&&f")
    )
  }
  @Test def test_oror() {
    // '|| -> ("l", 50),
    assertEquals(
        ('e,'||,'f),
        parse("e||f")
    )
  }
  @Test def test_cron() {
    // Symbol(":") -> ("l", 50),
    assertEquals(
        ('e,':,'f),
        parse("e:f")
    )
  }
  @Test def test_quest() {
    // '? -> ("r", 40),
    assertEquals(
        ('e,'?,'f),
        parse("e?f")
    )
  }
  @Test def test_eq() {
    // '= -> ("r", 40),
    assertEquals(
        ('e,'=,'f),
        parse("e=f")
    )
  }
  @Test def test_addeq() {
    // '+= -> ("r", 40),
    assertEquals(
        ('e,'+=,'f),
        parse("e+=f")
    )
  }
  @Test def test_subeq() {
    // '-= -> ("r", 40),
    assertEquals(
        ('e,'-=,'f),
        parse("e-=f")
    )
  }
  @Test def test_muleq() {
    //'*= -> ("r", 40),
    assertEquals(
        ('e,'*=,'f),
        parse("e*=f")
    )
  }
  @Test def test_diveq() {
    // '/= -> ("r", 40),
    assertEquals(
        ('e,'/=,'f),
        parse("e/=f")
    )
  }
  @Test def test_parsenteq() {
    // '%= -> ("r", 40),
    assertEquals(
        ('e,'%=,'f),
        parse("e%=f")
    )
  }
  @Test def test_andeq() {
    // '&= -> ("r", 40),
    assertEquals(
        ('e,'&=,'f),
        parse("e&=f")
    )
  }
  @Test def test_oreq() {
    // '|= -> ("r", 40),
    assertEquals(
        ('e,'|=,'f),
        parse("e|=f")
    )
  }
  @Test def test_xoreq() {
    // '^= -> ("r", 40),
    assertEquals(
        ('e,'^=,'f),
        parse("e^=f")
    )
  }
  @Test def test_gtgteq() {
    // '<<= -> ("r", 40),
    assertEquals(
        ('e,'<<=,'f),
        parse("e<<=f")
    )
  }
  @Test def test_ltlteq() {
    // '>>= -> ("r", 40),
    assertEquals(
        ('e,'>>=,'f),
        parse("e>>=f")
    )
  }
  @Test def test_ltltlteq() {
    // '>>>= -> ("r", 40),
    assertEquals(
        ('e,'>>>=,'f),
        parse("e>>>=f")
    )
  }
  @Test def test_semicoron() {
    // Symbol(";") -> ("e2",30),
    assertEquals(
      (('e, Symbol(";")), '@, 'f),
        parse("e;f")
    )
  }
  @Test def test_camma() {
    // Symbol(",") -> ("r", 20),
    assertEquals(
        ('e,Symbol(","),'f),
        parse("e,f")
    )
  }
  @Test def test_else() {
    // 'else ->("l2",10)
    assertEquals(
        ('e,'else,'f),
        parse("e else f")
    )
  }
  @Test def test_paren() {
    // Symbol("(") -> ("p", Symbol(")")),
    assertEquals(
        (Symbol("("),'a,Symbol(")")),
        parse("(a)")
    )
  }
  @Test def test_array() {
    // Symbol("[") -> ("p", Symbol("]")),
    assertEquals(
        (Symbol("["),'a,Symbol("]")),
        parse("[a]")
    )
  }
  @Test def test_brace() {
    // Symbol("{") -> ("p", Symbol("}")),
    assertEquals(
        (Symbol("{"),'a,Symbol("}")),
        parse("{a}")
    )
  }
  @Test def test_new() {
    // 'new -> ("l", 160),
    assertEquals(
        ('new, ('new,'a)),
        parse("new new a")
    )
  }
  @Test def test_preadd() {
    // '++ -> ("l", 160),
    assertEquals(
        ('++, ('++,'a)),
        parse("++ ++ a")
    )
  }
  @Test def test_predec() {
    // '-- -> ("l", 160),
    assertEquals(
        ('--, ('--,'a)),
        parse("-- --a")
    )
  }
  @Test def test_plus() {
    // '+ -> ("l", 160),
    assertEquals(
        ('+, ('+,'a)),
        parse("+ + a")
    )
  }
  @Test def test_pointer() {
    // '* -> ("l", 160),
    assertEquals(
        ('*, ('*,'a)),
        parse("**a")
    )
  }
  @Test def test_address() {
    // '& -> ("l", 160),
    assertEquals(
        ('&, ('&,'a)),
        parse("& &a")
    )
  }
  @Test def test_minus() {
    // '- -> ("l", 160),
    assertEquals(
        ('-, ('-,'a)),
        parse("- - a")
    )
  }
  @Test def test_xnot() {
    // '~ -> ("l", 160),
    assertEquals(
        ('~, ('~,'a)),
        parse("~ ~a")
    )
  }
  @Test def test_not() {
    // '! -> ("l", 160),
    assertEquals(
        ('!, ('!,'a)),
        parse("! !a")
    )
  }
  @Test def test_delete() {
    // 'delete -> ("l", 160),
    assertEquals(
        ('delete, ('delete,'a)),
        parse("delete delete a")
    )
  }
  @Test def test_typeof() {
    // 'typeof -> ("l", 160),
    assertEquals(
        ('typeof, ('typeof,'a)),
        parse("typeof typeof a")
    )
  }
  @Test def test_break() {
    // 'break ->("l",160),
    assertEquals(
        ('break, ('break,'a)),
        parse("break break a")
    )
  }
  @Test def test_continue() {
    // 'continue ->("l",160),
    assertEquals(
        ('continue, ('continue,'a)),
        parse("continue continue a")
    )
  }
  @Test def test_import() {
    // 'import ->("l",160),
    assertEquals(
        ('import, ('import,'a)),
        parse("import import a")
    )
  }
  @Test def test_def() {
    // 'def -> ("l", 20),
    assertEquals(
        ('def, ('def,'a)),
        parse("def def a")
    )
  }
  @Test def test_return() {
    // 'return ->("l",0),
    assertEquals(
        ('return, ('return,'a)),
        parse("return return a")
    )
  }
  @Test def test_mac() {
    // 'mac->("st",Symbol("("),Symbol(")"),0),
    assertEquals(
        ('mac, Symbol("("),'a,Symbol(")"), 'b),
        parse("mac(a)b")
    )
  }
  @Test def test_if() {
    // 'if ->("st",Symbol("("),Symbol(")"),0),
    assertEquals(
        ('if, Symbol("("),'a,Symbol(")"), 'b),
        parse("if(a)b")
    )
  }
  @Test def test_for() {
    // 'for ->("st",Symbol("("),Symbol(")"),0),
    assertEquals(
        ('for, Symbol("("),'a,Symbol(")"), 'b),
        parse("for(a)b")
    )
  }
  @Test def test_while() {
    // 'while ->("st",Symbol("("),Symbol(")"),0),
    assertEquals(
        ('while, Symbol("("),'a,Symbol(")"), 'b),
        parse("while(a)b")
    )
  }
  @Test def test_do() {
    // 'do ->("st",Symbol("{"),Symbol("}"),0)
    assertEquals(
        ('do, Symbol("{"),'a,Symbol("}"), ('while,Symbol("("),'b,Symbol(")"), Symbol(";"))),
        parse("do{a}while(b);")
    )
  }

  // TODO if文周りのテスト
  // TODO 優先順位テスト
  // TODO import周りのテスト
  // TODO セミコロン周りのテスト
  // TODO 改行とカッコのテスト
  // TODO 前置演算子のテスト
  
  // TODO 後置演算子のテストは済
}

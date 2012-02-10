package compiler

object setmem {
  var ls:List[EAst] = List()

  def apply(e:List[EFundef]):List[EFundef] = e.map {
    case EFundef(n:String, t:TType, a:List[String], b:List[EAst]) =>
      ls = List()
      val b2 = b.map(f)
      EFundef(n, t, a, ls:::b2)
  }

  def f(e:EAst):EAst = e match {
    case EMov(a, b) => EMov(f(a), f(b))
    case ERet(a) => ERet(f(a))
    case EAdd(a, b) => EAdd(f(a), f(b))
    case ESub(a, b) => ESub(f(a), f(b))
    case ECall(a, b:List[EAst]) => ECall(a, b.map(f))
    case EInt(a) =>
      val id = genid("s_")
      ls = EMov(EInt(a), EStr(id))::ls
      EStr(id)
    case a => a
  }

}

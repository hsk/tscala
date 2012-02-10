package compiler

sealed abstract class TType
case class TUnit extends TType
case class TBool extends TType
case class TInt extends TType
case class TFloat extends TType
case class TFun(a:List[TType], b:TType) extends TType // arguments are uncurried
case class TTuple(a:List[TType]) extends TType
case class TArray(a:TType) extends TType
case class TVar(var a:Option[TType]) extends TType

abstract class EAst
case class EMov(a:EAst, b:EAst) extends EAst
case class EAdd(a:EAst, b:EAst) extends EAst
case class ESub(a:EAst, b:EAst) extends EAst
case class EAscii(a:String, b:String) extends EAst
case class ECall(a:String, b:List[EAst]) extends EAst
case class EIf(a:EAst, b:List[EAst], c:List[EAst]) extends EAst
case class EStr(a:String) extends EAst
case class EInt(a:Int) extends EAst
case class ERet(a:EAst) extends EAst
case class EEq(a:EAst,b:EAst) extends EAst
case class EBlock(a:List[EAst]) extends EAst

case class EFundef(a:String, t:TType, b:List[String], c:List[EAst])

object expand {

  def apply(p:List[EFundef]):List[MFundef] = {
    p.map {
      case EFundef(n, t, a, bs:List[EAst]) =>
        var l = argv(a, regs)
        var (l2, id) = bs.foldLeft(l, "") { case ((l, id), b) => f(l, b) }
        MFundef(n, t, l2.reverse)
    }
  }

  def argv(as:List[String], rs:List[String]):List[MX86_64] = {
    (as, rs) match {
      case (List(), rs) => List[MX86_64]()
      case (a::as, r::rs) => MMovl(r.asInstanceOf[String], a)::argv(as, rs)
    }
  }

  val regs = List("%edi", "%esi", "%edx", "%ecx", "%r8d", "%r9d")
  def f(l:List[MX86_64],e:EAst):(List[MX86_64],String) = e match {
    case EAdd(a, b) =>
      val id = genid("ex_")
      val (la, a1) = f(l, a)
      val (lb, b1) = f(la, b)
      (MAddl(a1, b1, id)::lb,id)
    case ESub(a, b) =>
      val id = genid("ex_")
      val (la, a1) = f(l, a)
      val (lb, b1) = f(la, b)
      (MSubl(a1, b1, id)::lb,id)
    case EMov(EStr(a), EStr(id)) => (MMovl(a, id)::l, id)
    case EAscii(a, id) => (MAscii(a, id)::l, id)
    case EMov(EInt(a), EStr(id)) => (MMovl(("$"+a),id)::l, id)
    case EMov(a@EAscii(_,_), EStr(id)) =>
      val (l2, id1) = f(l, a)
      (MLeaq(id1, id)::l2, id)
    case EMov(a, EStr(id)) =>
      val (l2, id1) = f(l, a)
      (MMovl(id1, id)::l2, id)
    case ECall(a, bs:List[EAst]) =>
      var (la,ids) = bs.foldLeft(l,List[String]()) {
        case ((l, ids), b) =>
        	val (la, id) = f(l, b)
        	(la, id::ids)
      }
      (MCall(a.asInstanceOf[String], ids)::la, "%eax")
    case EIf(a, bs:List[EAst], cs:List[EAst]) =>
      var (bl, bid) = bs.foldLeft(List[MX86_64](),"") { case ((l, id), b) => f(l, b) }
      var (cl, cid) = cs.foldLeft(List[MX86_64](),"") { case ((l, id), c) => f(l, c) }
      val (al, aid) = f(l, a)
      (MIfeq(bid, "$0", bl, cl)::al, null)
    case ERet(e) =>
      val (l2, id) = f(l, e)
      (MRet(id)::l2, id)
    case EStr(id) => (l, id)
  }
}

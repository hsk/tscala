package compiler

abstract class MX86_64
case class MMovl(a:String, b:String) extends MX86_64
case class MLeaq(a:String, b:String) extends MX86_64
case class MAddl(a:String, b:String, c:String) extends MX86_64
case class MSubl(a:String, b:String, c:String) extends MX86_64
case class MCall(a:String, b:List[Any]) extends MX86_64
case class MRet(a:String) extends MX86_64
case class MIfeq(a:String,b:String, c:List[Any], d:List[Any]) extends MX86_64
case class MAscii(a:String,b:String) extends MX86_64

case class MFundef(a:String, typ:TType, b:List[MX86_64])

object memAlloc {
  var m:Map[String, String] = null
  def apply(ls:List[MFundef]):List[Fundef] = ls.map {
    case MFundef(n, t, ls)=>
      counter = 0
      m = Map()
      val ll = ls.map(g)
      val size = ((15 - counter) / 16) * 16
      Fundef(n, t, Subq("$" + size, "%rsp")::ll)
  }

  def g(l:MX86_64):X86_64 = l match {
    case MMovl(a, b) => Movl(adr(a).asInstanceOf[String], adr(b).asInstanceOf[String])
    case MLeaq(a, b) => Leaq(a, adrn(b,8).asInstanceOf[String])
    case MAddl(a, b, c) => Addl(adr(a).asInstanceOf[String], adr(b).asInstanceOf[String], adr(c).asInstanceOf[String])
    case MSubl(a, b, c) => Subl(adr(a).asInstanceOf[String], adr(b).asInstanceOf[String], adr(c).asInstanceOf[String])
    case MCall(a, b) => Call(a, b.map(adr))
    case MRet(a) => Ret(adr(a) + "")
    case MIfeq(a, b, c, d) =>
      Ifeq(adr(a) + "", adr(b) + "", c.map(adr).asInstanceOf[List[X86_64]], d.map(adr).asInstanceOf[List[X86_64]])
    case MAscii(a, b) => Ascii(a+"", b+"")
  }

  var counter = 0 
  def adr(a:Any):Any = adrn(a, 4)
  def adrn(a:Any,size:Int):Any = a match {
    case a:String if(m.contains(a))=> m(a)
    case a:String if(a.substring(0,1)=="%" || a.substring(0,1)=="$") => a 
    case a:String => counter -= size; val n = counter + "(%rbp)"; m = m + (a -> n); n
    case a => a
  }
}


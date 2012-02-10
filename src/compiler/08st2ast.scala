package compiler

object st2ast {

  def apply(st:Any):List[EFundef] = st match {
    case (a,'@,b) => f(a)::apply(b)
    case a => List(f(a))
  }

  def f(fn:Any):EFundef = fn match {
    case ('def,((Symbol(n),Symbol("("),a,Symbol(")")),Symbol("{"),b, Symbol("}"))) =>
      EFundef("_"+n, TUnit(), params(a), bodys(b))
  }

  def params(e:Any):List[String] = e match {
    case (a,Symbol(","),b) => params(a):::params(b)
    case 'void=>List()
    case Symbol(a) => List(a)
  }
  def fargs(e:Any):List[EAst] = e match {
    case (a,Symbol(","),b) => fargs(a):::fargs(b)
    case a => List(exp(a))
  }

  def exp(e:Any):EAst = e match {
    case (Symbol("{"),b,Symbol("}")) => EBlock(bodys(b))
    case (Symbol("("),b,Symbol(")")) => exp(b)
    case (Symbol(a),Symbol("("),b,Symbol(")")) => ECall("_"+a,fargs(b))
    case (a,'=,b) => EMov(exp(b), exp(a))
    case (a,'+,b) => EAdd(exp(a), exp(b))
    case (a,'-,b) => ESub(exp(a), exp(b))
    case ('return, a) => ERet(exp(a))
    case (a,Symbol(";")) => exp(a)
    case a:Int => EInt(a)
    case Symbol(a) => EStr(a)
    case a:String => EAscii(a,genid("str_"))
  }
  def bodys(e:Any):List[EAst] = e match {
    case (a,'@,b) => bodys(a):::bodys(b)
    case a =>
      exp(a) match {
        case EBlock(e) => e
        case a => List(a)
      }
  }
}

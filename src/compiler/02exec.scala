package compiler

import java.io._

object exec {
  def apply(cmd:String):(Int, String, String) = {
    val p = Runtime.getRuntime().exec(cmd)
    val stdin = (readAll(p.getInputStream()))
    val stderr = (readAll(p.getErrorStream()))
    (p.waitFor(), stdin, stderr)
  }

  def readAll(p:InputStream):String = {
    def f(s:String, i:BufferedReader):String = {
      i.readLine() match {
        case null => s
        case a =>  f(s+a+"\n", i)
      }
    }
    f("",new BufferedReader(new InputStreamReader(p)))
  }
}

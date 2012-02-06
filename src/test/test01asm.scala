package test

import org.junit.Assert._;
import org.junit.Test;
import java.io.FileInputStream
import compiler._

class test01asm {

	@Test
	def テキストファイルが正しく出力される確認 {
    asm.open("a.txt")
    asm("test")
    asm.close()
    assertEquals("test\n", exec.readAll(new FileInputStream("a.txt")))
	}

}

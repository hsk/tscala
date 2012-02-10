package test

import org.junit.Assert._;
import org.junit.Test;
import java.io.FileInputStream
import compiler.asm
import compiler.exec

class test01asm {

	@Test def test_asm {
    asm.open("a.txt")
    asm("test")
    asm.close()
    assertEquals("test\n", exec.readAll(new FileInputStream("a.txt")))
	}
}

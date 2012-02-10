.cstring
.text
.globl _main
_main:
	pushq	%rbp
	movq	%rsp, %rbp
movl $1, %eax
movl %eax, %edi
call _printInt
	leave
	ret

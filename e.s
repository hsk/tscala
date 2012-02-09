.cstring
.text
.globl _main
_main:
	pushq	%rbp
	movq	%rsp, %rbp
subq $16, %rsp
movl $5, %eax
movl %eax, -4(%rbp)
movl -4(%rbp), %edi
call _printInt
	leave
	ret

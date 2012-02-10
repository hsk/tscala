.cstring
abc:
	.ascii "str_1\0"
.text
.globl _main
_main:
	pushq	%rbp
	movq	%rsp, %rbp
subq $16, %rsp
movl $1, %eax
movl %eax, -4(%rbp)
leaq str_1(%rip), %rax
movq %rax, -12(%rbp)
movl -4(%rbp), %edi
call _printInt
	leave
	ret

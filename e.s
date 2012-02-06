.cstring
.text
.globl _main
_main:
	pushq	%rbp
	movq	%rsp, %rbp
subq $32, %rsp
movl $3, %eax
movl %eax, -4(%rbp)
movl $20, %eax
movl %eax, -8(%rbp)
movl $100, %eax
movl %eax, -12(%rbp)
movl -12(%rbp), %eax
addl -8(%rbp), %eax
movl %eax, -16(%rbp)
movl -16(%rbp), %eax
addl -4(%rbp), %eax
movl %eax, -20(%rbp)
movl -20(%rbp), %edi
call _printInt
	leave
	ret

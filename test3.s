.cstring
LC0:
	.ascii "%d\12\0"
.text
.globl _printInt
_printInt:
	pushq	%rbp
	movq	%rsp, %rbp
subq $16, %rsp
movl %edi, %eax
movl %eax, -4(%rbp)
movl -4(%rbp), %eax
movl %eax, %esi
leaq LC0(%rip), %rax
movq %rax, %rdi
movl $0, %eax
movl %eax, %eax
call _printf
	leave
	ret
.cstring
.text
.globl _main
_main:
	pushq	%rbp
	movq	%rsp, %rbp
subq $16, %rsp
movl $333, %eax
movl %eax, -4(%rbp)
movl -4(%rbp), %edi
call _printInt
	leave
	ret

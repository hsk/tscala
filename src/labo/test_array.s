	.cstring
LC0:
	.ascii "%d\12\0"
	.text
.globl _printInt
_printInt:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %esi
	leaq	LC0(%rip), %rdi
	movl	$0, %eax
	call	_printf
	leave
	ret

.globl _test
_test:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	movl	-12(%rbp), %eax
	cltq
	salq	$2, %rax
	addq	-8(%rbp), %rax
	movl	(%rax), %edi
	call	_printInt
	leave
	ret

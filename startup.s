	.section	__TEXT,__text,regular,pure_instructions
	.macosx_version_min 10, 13
	.intel_syntax noprefix
	.globl	_main
	.p2align	4, 0x90
_main:                                  ## @main
	.cfi_startproc
## BB#0:
	push	rbp
Lcfi0:
	.cfi_def_cfa_offset 16
Lcfi1:
	.cfi_offset rbp, -16
	mov	rbp, rsp
Lcfi2:
	.cfi_def_cfa_register rbp
	sub	rsp, 48
	mov	dword ptr [rbp - 4], 0
	mov	dword ptr [rbp - 8], edi
	mov	qword ptr [rbp - 16], rsi
	mov	dword ptr [rbp - 20], 65536
	mov	edi, dword ptr [rbp - 20]
	call	_allocate_protected_space
	mov	qword ptr [rbp - 32], rax
	mov	rax, qword ptr [rbp - 32]
	movsxd	rsi, dword ptr [rbp - 20]
	add	rax, rsi
	mov	qword ptr [rbp - 40], rax
	mov	rdi, qword ptr [rbp - 40]
	mov	al, 0
	call	_scheme_entry
	mov	edi, eax
	call	_prn
	mov	rdi, qword ptr [rbp - 32]
	mov	esi, dword ptr [rbp - 20]
	call	_deallocate_protected_space
	xor	eax, eax
	add	rsp, 48
	pop	rbp
	ret
	.cfi_endproc

	.p2align	4, 0x90
_allocate_protected_space:              ## @allocate_protected_space
	.cfi_startproc
## BB#0:
	push	rbp
Lcfi3:
	.cfi_def_cfa_offset 16
Lcfi4:
	.cfi_offset rbp, -16
	mov	rbp, rsp
Lcfi5:
	.cfi_def_cfa_register rbp
	sub	rsp, 48
	mov	dword ptr [rbp - 4], edi
	call	_getpagesize
	xor	edi, edi
	mov	ecx, edi
	mov	edx, 3
	mov	edi, 4098
	xor	r8d, r8d
	mov	dword ptr [rbp - 8], eax
	mov	eax, dword ptr [rbp - 4]
	add	eax, dword ptr [rbp - 8]
	sub	eax, 1
	mov	dword ptr [rbp - 28], edx ## 4-byte Spill
	cdq
	idiv	dword ptr [rbp - 8]
	imul	eax, dword ptr [rbp - 8]
	mov	dword ptr [rbp - 16], eax
	mov	eax, dword ptr [rbp - 16]
	mov	esi, dword ptr [rbp - 8]
	shl	esi, 1
	add	eax, esi
	movsxd	rsi, eax
	mov	dword ptr [rbp - 32], edi ## 4-byte Spill
	mov	rdi, rcx
	mov	eax, dword ptr [rbp - 28] ## 4-byte Reload
	mov	edx, eax
	mov	r9d, dword ptr [rbp - 32] ## 4-byte Reload
	mov	qword ptr [rbp - 40], rcx ## 8-byte Spill
	mov	ecx, r9d
	mov	r9, qword ptr [rbp - 40] ## 8-byte Reload
	call	_mmap
	mov	rsi, -1
	mov	qword ptr [rbp - 24], rax
	cmp	qword ptr [rbp - 24], rsi
	jne	LBB1_2
## BB#1:
	lea	rdi, [rip + L_.str.33]
	call	_perror
LBB1_2:
	xor	edx, edx
	mov	rdi, qword ptr [rbp - 24]
	movsxd	rsi, dword ptr [rbp - 8]
	call	_mprotect
	mov	dword ptr [rbp - 12], eax
	cmp	dword ptr [rbp - 12], 0
	je	LBB1_4
## BB#3:
	lea	rdi, [rip + L_.str.34]
	call	_perror
LBB1_4:
	xor	edx, edx
	mov	rax, qword ptr [rbp - 24]
	movsxd	rcx, dword ptr [rbp - 8]
	add	rax, rcx
	movsxd	rcx, dword ptr [rbp - 16]
	add	rax, rcx
	movsxd	rsi, dword ptr [rbp - 8]
	mov	rdi, rax
	call	_mprotect
	mov	dword ptr [rbp - 12], eax
	cmp	dword ptr [rbp - 12], 0
	je	LBB1_6
## BB#5:
	lea	rdi, [rip + L_.str.35]
	call	_perror
LBB1_6:
	mov	rax, qword ptr [rbp - 24]
	movsxd	rcx, dword ptr [rbp - 8]
	add	rax, rcx
	add	rsp, 48
	pop	rbp
	ret
	.cfi_endproc

	.p2align	4, 0x90
_prn:                                   ## @prn
	.cfi_startproc
## BB#0:
	push	rbp
Lcfi6:
	.cfi_def_cfa_offset 16
Lcfi7:
	.cfi_offset rbp, -16
	mov	rbp, rsp
Lcfi8:
	.cfi_def_cfa_register rbp
	sub	rsp, 48
	mov	dword ptr [rbp - 4], edi
	mov	edi, dword ptr [rbp - 4]
	and	edi, 3
	cmp	edi, 0
	jne	LBB2_2
## BB#1:
	lea	rdi, [rip + L_.str.36]
	mov	eax, dword ptr [rbp - 4]
	sar	eax, 2
	mov	esi, eax
	mov	al, 0
	call	_printf
	mov	dword ptr [rbp - 12], eax ## 4-byte Spill
	jmp	LBB2_25
LBB2_2:
	cmp	dword ptr [rbp - 4], 159
	jne	LBB2_4
## BB#3:
	lea	rdi, [rip + L_.str.37]
	mov	al, 0
	call	_printf
	mov	dword ptr [rbp - 16], eax ## 4-byte Spill
	jmp	LBB2_24
LBB2_4:
	cmp	dword ptr [rbp - 4], 31
	jne	LBB2_6
## BB#5:
	lea	rdi, [rip + L_.str.38]
	mov	al, 0
	call	_printf
	mov	dword ptr [rbp - 20], eax ## 4-byte Spill
	jmp	LBB2_23
LBB2_6:
	cmp	dword ptr [rbp - 4], 47
	jne	LBB2_8
## BB#7:
	lea	rdi, [rip + L_.str.39]
	mov	al, 0
	call	_printf
	mov	dword ptr [rbp - 24], eax ## 4-byte Spill
	jmp	LBB2_22
LBB2_8:
	mov	eax, dword ptr [rbp - 4]
	and	eax, 255
	cmp	eax, 15
	jne	LBB2_20
## BB#9:
	mov	eax, dword ptr [rbp - 4]
	sar	eax, 8
	mov	cl, al
	mov	byte ptr [rbp - 5], cl
	movsx	edi, byte ptr [rbp - 5]
	call	_iscntrl
	cmp	eax, 0
	jne	LBB2_11
## BB#10:
	movsx	edi, byte ptr [rbp - 5]
	call	_isspace
	cmp	eax, 0
	je	LBB2_15
LBB2_11:
	movsx	eax, byte ptr [rbp - 5]
	cmp	eax, 127
	jne	LBB2_13
## BB#12:
	lea	rdi, [rip + L_.str.40]
	mov	al, 0
	call	_printf
	mov	dword ptr [rbp - 28], eax ## 4-byte Spill
	jmp	LBB2_14
LBB2_13:
	lea	rdi, [rip + L_.str.41]
	lea	rax, [rip + _ascii_table]
	movzx	ecx, byte ptr [rbp - 5]
	mov	edx, ecx
	mov	rsi, qword ptr [rax + 8*rdx]
	mov	al, 0
	call	_printf
	mov	dword ptr [rbp - 32], eax ## 4-byte Spill
LBB2_14:
	jmp	LBB2_19
LBB2_15:
	lea	rsi, [rip + L_.str.42]
	lea	rdi, [rbp - 5]
	call	_strcmp
	cmp	eax, 0
	jne	LBB2_17
## BB#16:
	lea	rdi, [rip + L_.str.43]
	mov	al, 0
	call	_printf
	mov	dword ptr [rbp - 36], eax ## 4-byte Spill
	jmp	LBB2_18
LBB2_17:
	lea	rdi, [rip + L_.str.44]
	movsx	esi, byte ptr [rbp - 5]
	mov	al, 0
	call	_printf
	mov	dword ptr [rbp - 40], eax ## 4-byte Spill
LBB2_18:
	jmp	LBB2_19
LBB2_19:
	jmp	LBB2_21
LBB2_20:
	lea	rdi, [rip + L_.str.45]
	mov	al, 0
	call	_printf
	mov	dword ptr [rbp - 44], eax ## 4-byte Spill
LBB2_21:
	jmp	LBB2_22
LBB2_22:
	jmp	LBB2_23
LBB2_23:
	jmp	LBB2_24
LBB2_24:
	jmp	LBB2_25
LBB2_25:
	lea	rdi, [rip + L_.str.46]
	mov	al, 0
	call	_printf
	mov	dword ptr [rbp - 48], eax ## 4-byte Spill
	add	rsp, 48
	pop	rbp
	ret
	.cfi_endproc

	.p2align	4, 0x90
_deallocate_protected_space:            ## @deallocate_protected_space
	.cfi_startproc
## BB#0:
	push	rbp
Lcfi9:
	.cfi_def_cfa_offset 16
Lcfi10:
	.cfi_offset rbp, -16
	mov	rbp, rsp
Lcfi11:
	.cfi_def_cfa_register rbp
	sub	rsp, 32
	mov	qword ptr [rbp - 8], rdi
	mov	dword ptr [rbp - 12], esi
	call	_getpagesize
	xor	esi, esi
	mov	edi, esi
	mov	dword ptr [rbp - 16], eax
	mov	eax, dword ptr [rbp - 12]
	add	eax, dword ptr [rbp - 16]
	sub	eax, 1
	cdq
	idiv	dword ptr [rbp - 16]
	imul	eax, dword ptr [rbp - 16]
	mov	dword ptr [rbp - 24], eax
	mov	rcx, qword ptr [rbp - 8]
	movsxd	r8, dword ptr [rbp - 16]
	sub	rdi, r8
	add	rcx, rdi
	mov	eax, dword ptr [rbp - 24]
	mov	esi, dword ptr [rbp - 16]
	shl	esi, 1
	add	eax, esi
	movsxd	rsi, eax
	mov	rdi, rcx
	call	_munmap
	mov	dword ptr [rbp - 20], eax
	cmp	dword ptr [rbp - 20], 0
	je	LBB3_2
## BB#1:
	lea	rdi, [rip + L_.str.47]
	call	_perror
LBB3_2:
	add	rsp, 32
	pop	rbp
	ret
	.cfi_endproc

	.section	__TEXT,__cstring,cstring_literals
L_.str:                                 ## @.str
	.asciz	"nul"

L_.str.1:                               ## @.str.1
	.asciz	"soh"

L_.str.2:                               ## @.str.2
	.asciz	"stx"

L_.str.3:                               ## @.str.3
	.asciz	"etx"

L_.str.4:                               ## @.str.4
	.asciz	"eot"

L_.str.5:                               ## @.str.5
	.asciz	"enq"

L_.str.6:                               ## @.str.6
	.asciz	"ack"

L_.str.7:                               ## @.str.7
	.asciz	"bel"

L_.str.8:                               ## @.str.8
	.asciz	"bs"

L_.str.9:                               ## @.str.9
	.asciz	"tab"

L_.str.10:                              ## @.str.10
	.asciz	"newline"

L_.str.11:                              ## @.str.11
	.asciz	"vt"

L_.str.12:                              ## @.str.12
	.asciz	"ff"

L_.str.13:                              ## @.str.13
	.asciz	"return"

L_.str.14:                              ## @.str.14
	.asciz	"so"

L_.str.15:                              ## @.str.15
	.asciz	"si"

L_.str.16:                              ## @.str.16
	.asciz	"dle"

L_.str.17:                              ## @.str.17
	.asciz	"dc1"

L_.str.18:                              ## @.str.18
	.asciz	"dc2"

L_.str.19:                              ## @.str.19
	.asciz	"dc3"

L_.str.20:                              ## @.str.20
	.asciz	"dc4"

L_.str.21:                              ## @.str.21
	.asciz	"nak"

L_.str.22:                              ## @.str.22
	.asciz	"syn"

L_.str.23:                              ## @.str.23
	.asciz	"etb"

L_.str.24:                              ## @.str.24
	.asciz	"can"

L_.str.25:                              ## @.str.25
	.asciz	"em"

L_.str.26:                              ## @.str.26
	.asciz	"sub"

L_.str.27:                              ## @.str.27
	.asciz	"esc"

L_.str.28:                              ## @.str.28
	.asciz	"fs"

L_.str.29:                              ## @.str.29
	.asciz	"gs"

L_.str.30:                              ## @.str.30
	.asciz	"rs"

L_.str.31:                              ## @.str.31
	.asciz	"us"

L_.str.32:                              ## @.str.32
	.asciz	"space"

	.section	__DATA,__data
	.globl	_ascii_table            ## @ascii_table
	.p2align	4
_ascii_table:
	.quad	L_.str
	.quad	L_.str.1
	.quad	L_.str.2
	.quad	L_.str.3
	.quad	L_.str.4
	.quad	L_.str.5
	.quad	L_.str.6
	.quad	L_.str.7
	.quad	L_.str.8
	.quad	L_.str.9
	.quad	L_.str.10
	.quad	L_.str.11
	.quad	L_.str.12
	.quad	L_.str.13
	.quad	L_.str.14
	.quad	L_.str.15
	.quad	L_.str.16
	.quad	L_.str.17
	.quad	L_.str.18
	.quad	L_.str.19
	.quad	L_.str.20
	.quad	L_.str.21
	.quad	L_.str.22
	.quad	L_.str.23
	.quad	L_.str.24
	.quad	L_.str.25
	.quad	L_.str.26
	.quad	L_.str.27
	.quad	L_.str.28
	.quad	L_.str.29
	.quad	L_.str.30
	.quad	L_.str.31
	.quad	L_.str.32
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0

	.section	__TEXT,__cstring,cstring_literals
L_.str.33:                              ## @.str.33
	.asciz	"mmap failed :'("

L_.str.34:                              ## @.str.34
	.asciz	"failed to protect mmapped space"

L_.str.35:                              ## @.str.35
	.asciz	"failed to protect mmapped space (2)"

L_.str.36:                              ## @.str.36
	.asciz	"%d"

L_.str.37:                              ## @.str.37
	.asciz	"#t"

L_.str.38:                              ## @.str.38
	.asciz	"#f"

L_.str.39:                              ## @.str.39
	.asciz	"()"

L_.str.40:                              ## @.str.40
	.asciz	"#\\del"

L_.str.41:                              ## @.str.41
	.asciz	"#\\%s"

L_.str.42:                              ## @.str.42
	.asciz	"\\\\"

L_.str.43:                              ## @.str.43
	.asciz	"#\\\\"

L_.str.44:                              ## @.str.44
	.asciz	"#\\%c"

L_.str.45:                              ## @.str.45
	.asciz	"FAIL!"

L_.str.46:                              ## @.str.46
	.asciz	"\n"

L_.str.47:                              ## @.str.47
	.asciz	"failed to deallocate protected space :'("


.subsections_via_symbols

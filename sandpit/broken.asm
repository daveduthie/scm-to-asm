global L_scheme_entry
L_scheme_entry:
  ; si is 0
  ; -------- emitting args before tail call <<*>>
  mov rax, 8
  mov [rsp - 8], rax ; passing arg to sum
  mov rax, 0
  mov [rsp - 16], rax ; passing arg to sum
  jmp L_0_lambda ; app call
global scheme_entry
scheme_entry:
  mov rcx, rsp
  mov rsp, rdi
  call L_scheme_entry
  mov rsp, rcx
  ret
global L_0_lambda
L_0_lambda:
  mov rax, [rsp - 8]
  cmp rax, 0
  setz al
  movzx rax, al
  shl rax, 7
  or rax, 31 ; end predicate
  cmp rax, 31 ; cmp #f
  je L_2_alt
  cmp rax, 47 ; cmp ()
  je L_2_alt
  mov rax, [rsp - 16]
  ret
  jmp L_1_end
L_2_alt: ; alt
  ; si is 24
  ; -------- emitting args before tail call <<*>>
  mov rax, [rsp - 8]
  sub rax, 4
  mov [rsp - 8], rax ; passing arg to sum
  ; emitting build on stack (si: 16)
  mov rax, [rsp - 16]
  mov [rsp - 24], rax
  mov rax, [rsp - 8]
  add rax, [rsp - 24]
  ; end of reduce-stack
  mov [rsp - 16], rax ; passing arg to sum
  jmp L_0_lambda ; app call
L_1_end: ; end

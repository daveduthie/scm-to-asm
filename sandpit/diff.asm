global L_scheme_entry                                              global L_scheme_entry
L_scheme_entry:                                                    L_scheme_entry:
  ; si is 0                                                          ; si is 0
  ; -------- emitting args before tail call <<*>>                    ; -------- emitting args before tail call <<*>>
  mov rax, 8                                                         mov rax, 8
  mov [rsp - 16], rax ; passing arg to sum                      |    mov [rsp - 8], rax ; passing arg to sum
  mov rax, 0                                                         mov rax, 0
  mov [rsp - 24], rax ; passing arg to sum                      |    mov [rsp - 16], rax ; passing arg to sum
  ; -------- shifting stack frame of 2 args down by 8 <<*>>     <
  mov rax, [rsp - 16]                                           <
  mov [rsp - 8], rax                                            <
  mov rax, [rsp - 24]                                           <
  mov [rsp - 16], rax                                           <
  jmp L_0_lambda ; app call                                          jmp L_0_lambda ; app call
global scheme_entry                                                global scheme_entry
scheme_entry:                                                      scheme_entry:
  mov rcx, rsp                                                       mov rcx, rsp
  mov rsp, rdi                                                       mov rsp, rdi
  call L_scheme_entry                                                call L_scheme_entry
  mov rsp, rcx                                                       mov rsp, rcx
  ret                                                                ret
global L_0_lambda                                                  global L_0_lambda
L_0_lambda:                                                        L_0_lambda:
  mov rax, [rsp - 8]                                                 mov rax, [rsp - 8]
  cmp rax, 0                                                         cmp rax, 0
  setz al                                                            setz al
  movzx rax, al                                                      movzx rax, al
  shl rax, 7                                                         shl rax, 7
  or rax, 31 ; end predicate                                         or rax, 31 ; end predicate
  cmp rax, 31 ; cmp #f                                               cmp rax, 31 ; cmp #f
  je L_2_alt                                                         je L_2_alt
  cmp rax, 47 ; cmp ()                                               cmp rax, 47 ; cmp ()
  je L_2_alt                                                         je L_2_alt
  mov rax, [rsp - 16]                                                mov rax, [rsp - 16]
  ret                                                                ret
  jmp L_1_end                                                        jmp L_1_end
L_2_alt: ; alt                                                     L_2_alt: ; alt
  ; si is 24                                                         ; si is 24
  ; -------- emitting args before tail call <<*>>                    ; -------- emitting args before tail call <<*>>
  mov rax, [rsp - 8]                                                 mov rax, [rsp - 8]
  sub rax, 4                                                         sub rax, 4
  mov [rsp - 40], rax ; passing arg to sum                      |    mov [rsp - 8], rax ; passing arg to sum
  ; emitting build on stack (si: 48)                            |    ; emitting build on stack (si: 16)
  mov rax, [rsp - 16]                                                mov rax, [rsp - 16]
  mov [rsp - 56], rax                                           |    mov [rsp - 24], rax
  mov rax, [rsp - 8]                                                 mov rax, [rsp - 8]
  add rax, [rsp - 56]                                           |    add rax, [rsp - 24]
  ; end of reduce-stack                                              ; end of reduce-stack
  mov [rsp - 48], rax ; passing arg to sum                      |    mov [rsp - 16], rax ; passing arg to sum
  ; -------- shifting stack frame of 2 args down by 32 <<*>>    <
  mov rax, [rsp - 40]                                           <
  mov [rsp - 8], rax                                            <
  mov rax, [rsp - 48]                                           <
  mov [rsp - 16], rax                                           <
  jmp L_0_lambda ; app call                                          jmp L_0_lambda ; app call
L_1_end: ; end                                                     L_1_end: ; end

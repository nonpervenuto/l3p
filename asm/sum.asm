format ELF64
section '.text' executable
public main
main:                
  extrn putchar      
  sub rsp, 64 ; alloc stack  
  
  ; assignment
  mov [rsp + 0], DWORD 67 
  mov [rsp + 32], DWORD 1
  
  ; add 
  mov rax, [rsp + 0] 
  add rax, [rsp + 32]
  mov [rsp], rax
  
  mov rdi, [rsp]
  call putchar       
  add rsp, 64         
  mov rax, 0         
  ret                

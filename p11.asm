;Mountains

[org 0x0100]

jmp start

clr_scr:
                  push es
                  push ax
                  push di
                  
                  mov ax,0xb800
                  mov es,ax
                  mov di,0

nextloc:
             ;     call delay
                  mov word[es:di],0x0720
                  add di,2
                  cmp di,4000
                  jne nextloc

                  pop di
                  pop ax
                  pop es
                  mov di,0
                  ret

                  
function_mountain:
                  push es
                  push ax
                  push bx
                  push di
               ;   mov bx,di
               ;   sub bx,160
;loop1:
            ;      add bx,160
            ;      mov di,bx
                  mov ax,0xb800
                  mov es,ax
                  mov word[es:di],0x675F
                  mov si,di
mountain:
                  call delay
                  add di,158
                  add si,162
                  mov word[es:di],0x672F
                  mov word[es:si],0x675C
                  cmp di,1280
                  jna mountain
         ;         cmp bx,1280
         ;         jna loop1

                  pop di 
                  pop bx                
                  pop ax
                  pop es
                  
                  ret
             
sky:                
                  push es
                  push ax
                  push di
                  
                  mov ax,0xb800
                  mov es,ax
                  mov di,0

nextloc1:
        ;          call delay
                  mov word[es:di],0x7720
                  add di,2
                  cmp di,1280
                  jne nextloc1

                  pop di
                  pop ax
                  pop es
                  mov di,0
                  ret
     
delay:
                  push cx
                  mov cx,0xFFFF
loop1:
                  loop loop1
                  mov cx,0xFFFF
loop2:
                  loop loop2
                  pop cx

                  ret

start:

call clr_scr

call sky

mov di,240
call function_mountain

mov di,200
call function_mountain

mov di,120
call function_mountain

mov di,0
call function_mountain

mov di,140
call function_mountain



















                  
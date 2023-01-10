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
              
loopmount:
          ;        call delay
                  mov di,bx
                  add bx,160
                  mov ax,0xb800
                  mov es,ax
                  mov word[es:di],0x6720
                  mov si,di
mountain:

                  add di,158
                  add si,162
                  mov word[es:di],0x6720
                  mov word[es:si],0x6720
                  cmp di,1280
                  jna mountain
                  cmp bx,1280
                  jna loopmount
                  mov di,bx
                  mov word[es:di],0x6720

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
                  mov word[es:di],0x7F20
                  add di,2
                  cmp di,1280
                  jne nextloc1

                  pop di
                  pop ax
                  pop es
                  mov di,0
                  call sun1
                 ; mov di,200
                 ; call sun2

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

sea:
                  push es
                  push ax
                  push di
                  
                  mov ax,0xb800
                  mov es,ax
                  mov di,1280

nextloc2:
                 ; call delay
                  mov word[es:di],0x1100
                  add di,2
                  cmp di,2880
                  jne nextloc2

                  pop di
                  pop ax
                  pop es
                  mov di,0
                  ret
     
sun1:
                  push es
                  push ax
                  push di
                  
                  mov ax,0xb800
                  mov es,ax

                  mov word[es:di],0xFF20
                  add di,2

mov word[es:di],0x0120
                  add di,2
mov word[es:di],0x1220
                  add di,2
mov word[es:di],0x2320
                  add di,2
mov word[es:di],0x3420
                  add di,2
mov word[es:di],0x4520
                  add di,2
mov word[es:di],0x5620
                  add di,2
mov word[es:di],0x6720
                  add di,2
mov word[es:di],0x7820
                  add di,2
mov word[es:di],0x8920
                  add di,2
mov word[es:di],0x9A20
                  add di,2
mov word[es:di],0xAB20
                  add di,2
mov word[es:di],0xBC20
                  add di,2
mov word[es:di],0xCD20
                  add di,2
mov word[es:di],0xDE20
                  add di,2
mov word[es:di],0xEF20
                  add di,2
mov word[es:di],0xF120
                  add di,2


mov word[es:di],0x0020
                  add di,2
mov word[es:di],0x1120
                  add di,2
mov word[es:di],0x2220
                  add di,2
mov word[es:di],0x3320
                  add di,2
mov word[es:di],0x4420
                  add di,2
mov word[es:di],0x5520
                  add di,2
mov word[es:di],0x6620
                  add di,2
mov word[es:di],0x7720
                  add di,2
mov word[es:di],0x8820
                  add di,2
mov word[es:di],0x9920
                  add di,2
mov word[es:di],0xAA20
                  add di,2
mov word[es:di],0xBB20
                  add di,2
mov word[es:di],0xCC20
                  add di,2
mov word[es:di],0xDD20
                  add di,2
mov word[es:di],0xEE20
                  add di,2
mov word[es:di],0xFF20
                  add di,2





                  mov word[es:di],0x3300
                  add di,2
                  mov word[es:di],0x3300
                  add di,158
                  mov word[es:di],0x3300
                 
                  pop di
                  pop ax
                  pop es

                  ret
     

     
sun2:
                  push es
                  push ax
                  push di
                  
                  mov ax,0xb800
                  mov es,ax

                  mov word[es:di],0x3300
                  add di,2
                  mov word[es:di],0x1100
                  add di,158
                  mov word[es:di],0x1100
                  add di,2
                  mov word[es:di],0x1100
                 
                  pop di
                  pop ax
                  pop es

                  ret
     

start:

call clr_scr

call sky
;40
mov bx,40
call function_mountain
;70
mov bx,230
call function_mountain
;94
mov bx,94
call function_mountain
;130
mov bx,130
call function_mountain

;158
mov bx,318
call function_mountain

call sea
jmp $

















                  
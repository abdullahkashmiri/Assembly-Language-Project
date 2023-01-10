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
                  cmp bx,320
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
                  mov di,216
                  call sun1
                  ;mov di,180
                  ;call sun2

                  mov di,20
                  call sun3
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


nextloc23:
                 ; call delay
                  mov word[es:di],0x3420
                  add di,2
                  cmp di,4000
                  jne nextloc23

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

                  mov word[es:di],0x4520
                  add di,158
                  mov word[es:di],0x4520
                  add di,2
                  mov word[es:di],0x4520
                  add di,2
                  mov word[es:di],0x4520
                  add di,158
                  mov word[es:di],0x4520
                 
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
                   
                  mov word[es:di],0x4520
                  add di,158
                  mov word[es:di],0x4520
                  add di,2
                  mov word[es:di],0x4520
                  add di,2
                  mov word[es:di],0x4520
                  add di,156
                  mov word[es:di],0x4520
                  add di,2
                  mov word[es:di],0x4520
                  add di,2
                  mov word[es:di],0x4520
                  add di,158
                  mov word[es:di],0x4520
                  
                  pop di
                  pop ax
                  pop es

                  ret
     

     
sun3:
                  push es
                  push ax
                  push di
                  
                  mov ax,0xb800
                  mov es,ax
                   
                  mov word[es:di],0x0edb
                  add di,2
                  mov word[es:di],0x4520
                  add di,2
                  mov word[es:di],0x4520
                  add di,154
                  mov word[es:di],0x4520
                  add di,2
                  mov word[es:di],0x4520
                  add di,2
                  mov word[es:di],0x4520
                  add di,2
                  mov word[es:di],0x4520
                  add di,2
                  mov word[es:di],0x4520
                  add di,154
                  mov word[es:di],0x4520
                  add di,2
                  mov word[es:di],0x4520
                  add di,2
                  mov word[es:di],0x4520


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

















                  
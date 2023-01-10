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
           ;       add di,2
           ;       add si,2
           ;       mov word[es:di],0x6720
            ;      mov word[es:si],0x6720
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
 
ground:

    
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
     

calculateshipsize:
                  jmp calculate
                                    
                  updateless:
                                    mov si,2  ;si lower                                                    
                                    sub ax,5  ;-5
                                    
                                    jnae r1

                  updatemore:
                                    mov si,3  
                                    sub ax,7  ;-7
                       
                                    jae r1
 
                  calculate:
                                    mov cx,bx
                                    mov ax,cx
                           ;         add ax,di
                           ;         add si,di

                                    mov dx,0

                                    cmp cx,50
                                    jnae updateless
                                    jae updatemore

                                    r1:
                                    add cx,di
                  ret
ship:
                  push es
                  push ax
                  push bx
                  push cx
                  push dx
                  
                  push si
                  push di
                  push bp

          

                  mov ax,0xb800
                  mov es,ax
             ;     mov di,0

                  call calculateshipsize

shipright:
                  call delay
                  mov word[es:di],0x0edb
                  add di,2
                  cmp di,cx
                  jne shipright
                  mov dx,0

shipdown:
                  call delay
                  mov word[es:di],0x0edb
                  add di,158
                  add dx,1
                  cmp dx,si
                  jne shipdown
                  mov dx,0

shipleft:
                  call delay
                  mov word[es:di],0x0edb
                  sub di,2
                  add dx,2
                  cmp dx,ax
                  jne shipleft
                  mov dx,0

shipup:
                  call delay
                  mov word[es:di],0x0edb
                  sub di,162
                  add dx,1
                  cmp dx,si
                  jne shipup
                  
                  pop bp
                  pop di
                  pop si
                  pop dx
                  pop cx
                  pop bx
                  pop ax
                  pop es
                  add di,162
                  sub bx,4
                  sub bp,1
                  jnz ship
                  mov di,0
                  ret



start:

call clr_scr

call sky
;40
mov bx,200
call function_mountain
;70
mov bx,390
call function_mountain
;94
mov bx,254
call function_mountain
;130
mov bx,290
call function_mountain

;158
mov bx,478
call function_mountain

call sea

mov di,1500
mov bx,40
mov bp,2
call ship



jmp $

















                  
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
                  push cx
                  push di

                  mov cx,0x0Adb
loopmount:
     ;             call delay
                  mov di,bx
                  add bx,160
                  mov ax,0xb800
                  mov es,ax
                  mov word[es:di],cx
                  mov word[es:di-2],cx
                  mov si,di
                  add si,2
                  mov word[es:si],cx
                  mov word[es:si+2],cx
                  
mountain:

                  add di,156
                  add si,164
                  mov word[es:di],cx
                  mov word[es:si],cx
                  
                  mov word[es:di-2],cx
                  mov word[es:si+2],cx

           ;       add di,2
           ;       add si,2
           ;       mov word[es:di],0x6720
            ;      mov word[es:si],0x6720
                  cmp di,1280
                  jna mountain
                  mov cx,0x06db
                  cmp bx,1280
                  jna loopmount
                  mov di,bx
                  mov word[es:di],0x6720

                  pop di 
                  pop cx
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
                  mov word[es:di],0x0bdb
                  add di,2
                  cmp di,1280
                  jne nextloc1

                  pop di
                  pop ax
                  pop es
                
                  mov di,194
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
                  mov word[es:di],0x09db
                  add di,2
                  cmp di,2880
                  jne nextloc2


nextloc23:
                 ; call delay
                  mov word[es:di],0x01db
                  add di,2
                  cmp di,4000
                  jne nextloc23

                  pop di
                  pop ax
                  pop es
                  mov di,0
                  ret
 
ground:

                  push es
                  push ax
                  push di
                  
                  mov ax,0xb800
                  mov es,ax
                  mov di,1280

nextlo2:
                 ; call delay
                  mov word[es:di],0x02db
                  add di,2
                  cmp di,1440
                  jne nextlo2

                  pop di
                  pop ax
                  pop es
                  mov di,0
                  ret
 
sun3:
                  push es
                  push ax
                  push di
                  
                  mov ax,0xb800
                  mov es,ax
                   
                  mov word[es:di],0x0edb
                  add di,2
                  mov word[es:di],0x0edb
                  add di,2
                  mov word[es:di],0x0edb
                  add di,154
                  mov word[es:di],0x0edb
                  add di,2
                  mov word[es:di],0x0edb
                  add di,2
                  mov word[es:di],0x0edb
                  add di,2
                  mov word[es:di],0x0edb
                  add di,2
                  mov word[es:di],0x0edb
                  add di,154
                  mov word[es:di],0x0edb
                  add di,2
                  mov word[es:di],0x0edb
                  add di,2
                  mov word[es:di],0x0edb


                  pop di
                  pop ax
                  pop es

                  ret
     
shiphead:
                  push es
                  push ax
                  push bx
                  push di
                  
                  sub di,160
                  mov ax,0xb800
                  mov es,ax
                  jmp r3
u1:
                  shr bx,1
                  add di,bx
                  sub di,2
                  mov bx,di
                  add bx,4
                  jmp r2
                  r3:     
                  cmp bx,30
                  jnae u1   

                  shr bx,1
                  add di,bx
                  sub di,6
                  mov bx,di
                  add bx,12
                  r2:
shiptop:
                  ;call delay
                  mov word[es:di],0x08db
                  add di,2
                  cmp di,bx
                  jne shiptop

                  pop di
                  pop bx
                  pop ax
                  pop es
                  
                  ret


calculatesize:
                  jmp start1

updateless:
           mov cx,3
           mov si,3
           jnae r1
updatemore:
           mov cx,4
           mov si,4
           jae r1
 
start1:
                  cmp bx,50
                  jnae updateless
                  jae updatemore
                
                  r1:
              ;    add bx,di
                  call shiphead
                  mov dx,0
                  ret
ship:

                  push es
                  push ax        ;ax color 
                  push bx        ;bx size
                  push cx        ;cx up/low
                  push dx        ;dx size check
                  push di
                  push si             
     
                  mov ax,0xb800
                  mov es,ax
                  mov ax,0x4520
                  call calculatesize

loops1:
               ;   call delay
                  mov word[es:di],ax
                  add dx,2
                  add di,2
                  cmp dx,bx

                  jne loops1
                  sub di,bx
                  add di,162
                  mov dx,0
                  sub bx,4
                  sub si,1
                  jz end
                  mov ax,0x7F20
loops2:
       ;           call delay
                  mov word[es:di],ax
                  add dx,2
                  add di,2
                  cmp dx,bx
                  jne loops2
                  sub di,bx
                  add di,162
                  mov dx,0
                  sub bx,4
                  sub si,1
                  jz end
                  jnz loops1
end:

                  pop si
                  pop di
                  pop dx
                  pop cx
                  pop bx
                  pop ax
                  pop es
                  mov di,0
                  ret

cloud:

                  push es
                  push ax
                  push di
                  
                  mov ax,0xb800
                  mov es,ax
                  mov ax,3
               ;  call delay
                  mov word[es:di],0x7F20
                  add di,158
nextcloud:
                  mov word[es:di],0x7F20
                  add di,2
                  sub ax,1
                  jnz nextcloud

                  pop di
                  pop ax
                  pop es
                  mov di,0
                  ret



movesky:
       
                  push es
                  push ax
                  push bx
                  push cx
                  push di
                  push si
                  
                  mov cx,0
                  mov ax,0xb800
                  mov es,ax
                  mov di,-2
                  mov bx,-2
                   
                  

outernextms:
                  add di,2
                  mov ax,word[es:di]
                  push ax

                  add bx,160
                  mov si,bx                 
            
nextms:
              ;    call delay
                  
                  mov ax,word[es:di+2]
                  mov word[es:di],ax
                  
                  add di,2
                  cmp di,bx
                  jne nextms
                  pop ax
                  mov word[es:si],ax
                  add cx,1
                  cmp cx,8
        

                  jne outernextms

                
                  pop si
                  pop di
                  pop cx
                  pop bx
                  pop ax
                  pop es
                  mov di,0
                  ret




movesea:
       
                  push es
                  push ax
                  push bx
                  push cx
                  push di
                  push si
                  
                  mov cx,0
                  mov ax,0xb800
                  mov es,ax
                  mov di,2878
                  mov bx,2880
                   
                  

outernextmsea:
                  sub di,2
                  mov ax,word[es:di]
                  push ax

                  sub bx,160
                  mov si,bx                 
            
nextmsea:
              ;    call delay
                  mov ax,word[es:di-2]
                  mov word[es:di],ax
                
                  
                  sub di,2
                  cmp di,bx
                  jne nextmsea
                  pop ax
                  mov word[es:si],ax
                  add cx,1
                  cmp cx,9
       

                  jne outernextmsea

               
                  pop si
                  pop di
                  pop cx
                  pop bx
                  pop ax
                  pop es
                  mov di,0
                  ret




live:
                  push cx
                  mov cx,1
infinite:
                  call delay
                  call movesky
                  call delay
                  call movesea
                  call delay
                  cmp cx,0
                  jne infinite
                
                  ret


start:

call clr_scr

call sky


;40
mov bx,498
call function_mountain
;80
mov bx,226
call function_mountain
;94
;mov bx,416
;call function_mountain
;130
mov bx,592
call function_mountain

;150
mov bx,784
call function_mountain


mov di,10
call cloud

mov di,260
call cloud

mov di,290
call cloud

mov di,312
call cloud


mov di,210
call cloud


call sea

call ground


mov di,1820
mov bx,40

call ship


mov di,1864
mov bx,52

call ship


mov di,2260
mov bx,20

call ship

call live


jmp $

















                  

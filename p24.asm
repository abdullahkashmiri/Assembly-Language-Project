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
     
shiphead:
                  push es
                  push ax
                  push bx
                  push di
                  
                  sub di,160
                  mov ax,0xb800
                  mov es,ax
                  
                  shr bx,1
                  add di,bx
                  sub di,6
                  mov bx,di
                  add bx,12

shiptop:
                  call delay
                  mov word[es:di],0x0720
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
                  call delay
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
                  call delay
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

call ship


mov di,1870
mov bx,52

call ship


mov di,2240
mov bx,52

call ship


jmp $

















                  
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
                  push dx
                  push di

                
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
                 ; mov cx,0x06db
                   mov cx,dx
                  cmp bx,1280
                  jna loopmount
                  mov di,bx
                  mov word[es:di],0x6720

                  pop di 
                  pop dx
                  pop cx
                  pop bx                
                  pop ax
                  pop es
                  
                  ret
             
sky:                
                  push es
                  push ax
                  push bx
                  push di
                  
                  mov ax,0xb800
                  mov es,ax
                  mov di,0

nextloc1:
        ;          call delay
                  mov word[es:di],bx
                  add di,2
                  cmp di,1280
                  jne nextloc1

                  pop di
                  pop bx
                  pop ax
                  pop es
                   
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
                  push bx
                  push di
                  
                  mov ax,0xb800
                  mov es,ax
                  mov di,1280

nextloc2:
                 ; call delay
                  mov word[es:di],bx
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
                  pop bx
                  pop ax
                  pop es
                  mov di,0
                  ret
 
ground:

                  push es
                  push ax
                  push bx
                  push di
                  
                  mov ax,0xb800
                  mov es,ax
               

                  mov ax,di
                  add ax,160
nextlo2:
              
                  mov word[es:di],bx           ;0x02db
                  add di,2
                  cmp di,ax
                  jne nextlo2

                  pop di
                  pop bx
                  pop ax
                  pop es
                  mov di,0
                  ret
 
planet:
                  push es
                  push ax
                  push bx
                  push cx
                  push di

                  mov cx,3
                  mov ax,0xb800
                  mov es,ax
pl1:    
                  mov word[es:di],bx
                  add di,2
                  sub cx,1
                  jnz pl1
                  add di,152
                  mov cx,5
                  
pl2:    
                  mov word[es:di],bx
                  add di,2
                  sub cx,1
                  jnz pl2
                  add di,152
                  mov cx,3
pl3:    
                  mov word[es:di],bx
                  add di,2
                  sub cx,1
                  jnz pl3

                  pop di
                  pop cx
                  pop bx
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
                 ; mov ax,0x4520
                  mov ax,0x04db
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
                  mov ax,0x07db
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


movedeepsea:
       
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
                  mov bx,2878
                   
                  

outernextmsdeep:
                  add di,2
                  mov ax,word[es:di]
                  push ax

                  add bx,160
                  mov si,bx                 
            
nextmsdeep:
              ;    call delay
                  
                  mov ax,word[es:di+2]
                  mov word[es:di],ax
                  
                  add di,2
                  cmp di,bx
                  jne nextmsdeep
                  pop ax
                  mov word[es:si],ax
                  add cx,1
                  cmp cx,7
        

                  jne outernextmsdeep

                
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


clouds:

                  
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

                  ret
mountainss:
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

                  ret
ships:
                  mov di,1760
                  mov bx,40
                  call ship
                  mov di,2016
                  mov bx,52
                  call ship
                  mov di,2300
                  mov bx,24
                  call ship

                  ret

deepsea:
                  push bx
                  push cx 
                  push di

                  mov bx,2
                  mov di,3850
                  call weed

                  mov bx,1
                  mov di,3870
                  call weed

                  mov bx,2
                  mov di,3890
                  call weed

                  mov bx,1
                  mov di,3920
                  call weed

                  mov bx,2
                  mov di,3950
                  call weed

                  mov bx,1
                  mov di,3970
                  call weed

                  mov bx,2
                  mov di,3990
                  call weed


                  pop di
                  pop cx
                  pop bx

                  ret
weed:
                  push es
                  push ax  ; gpr
                  push bx  ; size
                  push cx  ; color
                  push di
                  
                  mov ax,0xb800
                  mov es,ax
                  
                  mov word[es:di],cx
nextweed:
              ;    call delay
                  sub di,162
                  mov word[es:di],cx
                  sub di,158
                  mov word[es:di],cx
                  sub bx,1
                  jnz nextweed

                  pop di 
                  pop cx
                  pop bx
                  pop ax
                  pop es
       
                  ret

fishes:
                  push cx
                  push di

                  mov cx,0x0cdb     ; just color
                  mov di,3230
                  call fish

                  mov cx,0x03db     ; just color
                  mov di,3440
                  call fish

                  mov cx,0x04db     ; just color
                  mov di,3306
                  call fish

                  mov cx,0x05db     ; just color
                  mov di,3650
                  call fish

                  pop di
                  pop cx

                  ret

fish:
                  push es
                  push ax
              
                  push cx
              
                  push di
                  
                  mov ax,0xb800
                  mov es,ax

                  
                
                  call delay
                  
                  mov word[es:di],cx
                  mov bx,2
flp1:
                  add di,2
                  mov word[es:di],cx
                  mov word[es:di+160],cx
                  mov word[es:di-160],cx
                  sub bx,1
                  jnz flp1
                  add di,2
                  mov word[es:di],cx
                  add di,2
                  mov word[es:di],cx
                  mov word[es:di+160],cx
                  mov word[es:di-160],cx
                  


                  pop di 
             
                  pop cx
              
                  pop ax
                  pop es
                  mov di,0
                  ret
                






live:

                  push ax
                  push bx
                  push cx 
                  push dx
                  mov ax,0
                  mov cx,1
                  mov bx,80
                  mov dx,160
          
                 
day:
                  call clr_scr
                  push cx
                  push bx
                  push dx
                  mov bx,0x0bdb
                  call sky
                  mov bx,0x0edb
                  mov di,194
                  call planet 
                  mov cx,0x0adb
                  mov dx,0x06db
                  call mountainss

                  call clouds
                  mov bx,0x09db
                  call sea
                  mov bx,0x0adb
                  mov di,1280
                  call ground
                  mov cx,0x0adb
                  call deepsea
                  call fishes

        ;          mov di,3840
         ;         call ground
                  call ships
                  mov ax,0
                  pop dx
                  pop bx
                  pop cx
                  jmp infinite 

night:
                  call clr_scr 
                  push cx
                  push bx
                  push ax
                  push dx
                  push di
                  mov bx,0x08db
                  call sky
                  mov bx,0xFFdb
                  mov di,194
                  call planet
                  mov cx,0x02db
                  mov dx,0x04db
                  call mountainss

                  call clouds
                  mov bx,0x01db
                  call sea
                  mov bx,0x02db
                  mov di,1280
                  call ground
                  mov cx,0x2220
                  call deepsea

       ;           mov di,3840
        ;          call ground
                  call ships  

                  pop di
                  pop dx
                  pop ax
                  pop bx
                  pop cx
                  jmp infinite


infinite:
                  call delay
                  call movedeepsea
                  call delay
                  call movesky
              ;    call delay
                  call movesea
                  
                  add ax,1
                  cmp ax,bx
                  jnae infinite
                  je night
                  cmp ax,dx
                  jnae infinite
                  je day
                  mov cx,1
                  cmp cx,0
                  jne infinite

                  pop cx
                  pop bx
                  pop ax

                  ret


start:
call clr_scr

call live


;jmp $

















                  

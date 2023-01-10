;PROJECT 
;-------------------------------------------------------------------------------------------
;GROUP MEMBERS 
;1-> ABDULLAH DAR BSCS 3G 21-L-7512
;2-> SAMI KHOKHER BSCS 3G 21-L-1868
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
[org 0x0100]
jmp start
fishloc: dw 3240		; fish location 
red: dw 3110,0
green: dw 3480,0
score: dw 0
random: dw 2720
clock: dw 0,0
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR CLEARING SCREEN
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
clrscr: 			
		push es 		; pushing relevant registers
 		push ax 
 		push cx 
		push di 

 		mov ax, 0xb800 
 		mov es, ax 		; point es to video base 
 		xor di, di 		; point di to top left column 
 		mov ax, 0x0720 		; space char in normal attribute 
 		mov cx, 2000 		; number of screen locations 
 		cld 			; auto increment mode 
 		rep stosw 		; clear the whole screen 

 		pop di			; popping relevant used registers
 		pop cx 
 		pop ax 
 		pop es 
 		ret 
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR PRINTING SKY
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------

sky
                push bp			; pushing relevant registers
                mov bp,sp
		push es 
 		push ax 
		push bx
 		push cx 
		push di  

		mov bx,[bp+4]		; assigning color code for sky by parameter

 		mov ax, 0xb800 
 		mov es, ax 		; point es to video base 
 		xor di, di 		; point di to top left column 
 		mov ax, bx		; printing color for sky
 		mov cx, 640 		; number of screen locations 
 		cld 			; auto increment mode 
 		rep stosw 		; print to geiven size

 		pop di			; popping relevant used registers
 		pop cx 
		pop bx
 		pop ax 
 		pop es 
   		pop bp
 		ret 2			; one parameter send i.e sky size
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR PRINTING A MOUNTAIN
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------

function_mountain:
		  push bp			; pushing relevant registers
	  	  mov bp,sp
                  push es
                  push ax
						; following registers are used for
                  push bx  			; bx size of mountain
                  push cx 			; cx grass color
                  push dx  			; dx soil color
                  push di
						; fetching data from stack
		  mov bx, [bp+4]  		; size
		  mov dx, [bp+8]		; soil
                  mov cx, [bp+10]		; grass
      
         
loopmountain:					; loop used for increment in size for printing mountains
                  mov di,bx			; giving di the starting point of mountain
                  add bx,160			; bx used for comparing di for each line
                  mov ax,0xb800
                  mov es,ax
                  mov word[es:di],cx		; pushing color to top of mountain
                  mov word[es:di-2],cx		; pushing color to top left of mountain
                  mov si,di			; assigning values for right side of mountain
                  add si,2
                  mov word[es:si],cx		; pushing color to top of mountain
                  mov word[es:si+2],cx 		; pushing color to top right of mountain
                  
mountain:					; loop used for printing single diagonal for a mountain

                  add di,156			; adding 156 so it will stay 2 blocks before the original point above
                  add si,164			; adding 164 so it will stay 2 blocks after  the original point above
                  mov word[es:di],cx		; placing color
                  mov word[es:si],cx            ; placing color
                  mov word[es:di-2],cx		; placing color
                  mov word[es:si+2],cx 		; placing color

                  cmp di,1280			; comparing till the ground reached
                  jna mountain
                  mov cx,dx			; chaning color to soil color
                  cmp bx,1280			; comparing till it reaches ground i,e last diagonal row
                  jna loopmountain
                  mov di,bx			
                  mov word[es:di],0x6720	; printing on bottom

                  pop di 			; popping relevant used registers
                  pop dx
                  pop cx
                  pop bx                
                  pop ax
                  pop es
                  pop bp
                  ret 2				; one parameter send i.e starting index

;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR PRINTING ALL THE MOUNTAINS
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------

mountainss:
                  ;40
                  mov bx,498          		;starting index of mountain
                  push bx
                  call function_mountain
                  ;80
                  mov bx,226          		;starting index of mountain
                  push bx
                  call function_mountain
                  ;130
                  mov bx,592          		;starting index of mountain
                  push bx
                  call function_mountain
                  ;150
                  mov bx,784          		;starting index of mountain
                  push bx
                  call function_mountain

                  ret 4				; two parameters passed i.e colors of grass and soil

;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR PRINTING A STAR
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
stars:
	push ax
	push es
	push di
	mov ax,0xb800
	mov es,ax
	mov di,180
	mov word[es:di],0x0f2a
	mov di,218
	mov word[es:di],0x0f2a
	mov di,140
	mov word[es:di],0x0f2a
	pop di
	pop es
	pop ax
ret
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR PRINTING A CLOUD
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------

cloud:
		  push bp			; pushing relevant registers
		  mov bp,sp
                  push es
                  push ax
	  push cx
                  push di
                  mov cx,[bp+8]
	  mov di,[bp+4]			; starting index of cloud
                  mov ax,0xb800
                  mov es,ax
                  mov ax,3			; size of cloud
               
                  mov word[es:di], cx	; cloud color printing at top
                  add di,158
nextcloud:
                  mov word[es:di], cx	; printing cloud
                  add di,2
                  sub ax,1
                  jnz nextcloud

                  pop di 			; popping relevant used registers
	  pop cx
                  pop ax
                  pop es
	  pop bp
                  
                  ret 2				; one parameter sent i.e starting index

;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR PRINTING PLANET I.E EITHER SUN OR MOON FOR DAY NIGHT FUNCTIONALITY
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------

planet:
		  push bp		; pushing relevant registers
		  mov bp,sp
                  push es
                  push ax
                  push bx
                  push cx
                  push di

		  mov di,[bp+4]		; starting index of planet
		  mov bx,[bp+6]		; color of planet
                  mov cx,3		; size of planet
                  mov ax,0xb800
                  mov es,ax
pl1:    
                  mov word[es:di],bx	; printing first row of planet
                  add di,2
                  sub cx,1
                  jnz pl1
                  add di,152
                  mov cx,5            
pl2:    
                  mov word[es:di],bx	; printing second row of planet
                  add di,2
                  sub cx,1
                  jnz pl2
                  add di,152
                  mov cx,3
pl3:    
                  mov word[es:di],bx	; printing third row of planet
                  add di,2
                  sub cx,1
                  jnz pl3

                  pop di 		; popping relevant used registers
                  pop cx
                  pop bx
                  pop ax
                  pop es
		  pop bp

                  ret 4			; two parameters passed i.e color and starting index

;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
; SUBROUTINE FOR PRINTING ALL COLOUDS
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------

clouds:         
                  mov bx,10		; starting index
	 	  push bx
                  call cloud
                  mov bx,260		; starting index
	 	  push bx
                  call cloud
                  mov bx,290		; starting index
	 	  push bx
                  call cloud
                  mov bx,210		; starting index
	 	  push bx
                  call cloud

                  ret 2

;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
; SUBROUTINE FOR PRINTING SEA
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------

sea: 
		push bp			; pushing relevant registers
		mov bp,sp
		push es 
 		push ax 
		push bx
 		push cx 
		push di 

		mov bx,[bp+4]		; sea color
 		mov ax, 0xb800 
 		mov es, ax 		; point es to video base 
 		mov di,1280		; point di to top left column 
 		mov ax, bx 		; printing sea 
 		mov cx, 720		; number of screen locations 
 		cld 			; auto increment mode 
 		rep stosw 		; print to given size
	
 		mov di,2720
 		mov ax, 0x01db 		; printing deep sea
 		mov cx, 640		; number of screen locations 
 		cld 			; auto increment mode 
 		rep stosw 		; print to end of screen as of givem size 

 		pop di 			; popping relevant used registers	
 		pop cx 
		pop bx
 		pop ax 
 		pop es 
		pop bp
 		ret 2			; one parameter send i.e color


;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR PRINTING GROUND
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------

ground:
		push bp			; pushing relevant registers
		mov bp,sp

	    	push es 
 		push ax 
		push bx
 		push cx 
		push di 

		mov di,[bp+4]		; starting index of ground
		mov bx,[bp+6]		; color of ground
 		mov ax, 0xb800 
 		mov es, ax 		; point es to video base 
 		
		mov cx,80
 		mov ax, bx		; printing ground
 					
 		cld 			; auto increment mode 
 		rep stosw 		; print to whole line 

 		pop di 			; popping relevant used registers
 		pop cx 
		pop bx
 		pop ax 
 		pop es 
		pop bp
 		ret 4			; two parametrs send i.e starting index and color

;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR PRINTING SHIP HEAD/TOP
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
     
shiphead:
                  push es			; pushing relevant registers
                  push ax
                  push bx
                  push di
                  
                  sub di,160			; move di to top for printing head
                  mov ax,0xb800
                  mov es,ax
                  jmp r3
u1:
                  shr bx,1 			; multiplying size
                  add di,bx			
                  sub di,2			; getting half index from size
                  mov bx,di
                  add bx,4			; second end of top of ship i.e total 4 block to print
                  jmp r2

                  r3:     			; comparing ship size to make head of desired accurate size
                  cmp bx,30
                  jnae u1   

                  shr bx,1			; multiplying by two
                  add di,bx			; adding size to starting point
                  sub di,6			; decreasing by 6
                  mov bx,di			
                  add bx,12			; adding 6x2 for printing head of 6 blocks for bigger ships

                  r2:
shiptop:					; printing ship top
                  
                  mov word[es:di],0x08db	; printing color
                  add di,2
                  cmp di,bx
                  jne shiptop

                  pop di 			; popping relevant used registers
                  pop bx
                  pop ax
                  pop es
                  
                  ret

;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR CALCULATING  SHIP SIZE
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------


calculatesize:			; calculating size of ship 
                  jmp start1	; jump to start

updateless:
           mov cx,3		; print ship of 3 levels
           mov si,3
           jnae r1
updatemore:
           mov cx,4		; print ship of 4 levels
           mov si,4
           jae r1
 
start1:
                  cmp bx,50
                  jnae updateless		; if size less than 50 
                  jae updatemore		; if size greater than 50 or above
                
                  r1:				; first printing ship head
                  call shiphead			; printing ship head
                  mov dx,0
                  ret

;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
; SUBROUTINE FOR PRINTING SHIP
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------

ship:
		push bp			; pushing relevant registers
		mov bp,sp
                  push es
                  push ax        	; ax color 
                  push bx        	; bx size
                  push cx        	; cx up/low
                  push dx        	
                  push di
                  push si    
		mov bx,[bp+4] 		; size of ship
		mov di,[bp+6]           ; index of ship
     
                  mov ax,0xb800
                  mov es,ax
                  mov ax,0x04db
                  call calculatesize	; calculting size function called

loops1:					; loop for forward prointing
               
                  mov word[es:di],ax 	; print ship
                  add dx,2		; size check
                  add di,2
                  cmp dx,bx

                  jne loops1

                  sub di,bx		; moving to second row of ship
                  add di,162
                  mov dx,0		; size count =0
                  sub bx,4		; comparing register lowered for next row
                  sub si,1
                  jz end		; if levels become zero end
                  mov ax,0x07db

loops2:					; loop for forward backward printing
       
                  mov word[es:di],ax	; printing ship
                  add dx,2
                  add di,2
                  cmp dx,bx		; comparing with bounds
                  jne loops2
                  sub di,bx
                  add di,162		; moving to next row
                  mov dx,0
                  sub bx,4
                  sub si,1
                  jz end		; if levels become zero end 
                ;  sub ax,0x0100
                  jnz loops1
end:

                  pop si 			; popping relevant used registers
                  pop di
                  pop dx
                  pop cx
                  pop bx
                  pop ax
                  pop es
                  pop bp
                  ret 4				; two parametrs size and starting index


;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR PRINTING ALL SHIPS
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------

ships:
                  mov bx,1760		; staring index
                  push bx
                  mov bx,40		; size
                  push bx
                  call ship
                  mov bx,2016		; staring index
                  push bx
                  mov bx,52		; size
                  push bx
                  call ship
                  mov bx,2140		; staring index
                  push bx
                  mov bx,24		; size
                  push bx
                  call ship

                  ret

;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR MOVING SCREEN OF GIVEN SIZE TO LEFT
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------

movleft:
		push bp			; pushing relevant registers
		mov bp,sp
		  push ds
                  push es
                  push ax
                  push bx
                  push cx
                  push di
                  push si



			mov ax,0xb800			; video mode set
			mov es,ax
			mov ds,ax           

			mov ax,80      			; ax to 80   
			mul byte[bp+4]			; multiplying with row sent via parameter
			shl ax,1			; multiplying ax with two to get exact bytes

			mov di,ax			; moving to di
			add ax,2
			mov si,ax			; moving si 1 next to di

			mov cx,80			; comparing for single line
			push word[es:di]		; pushing first box
			cld                  		; auto inc
			rep movsw			; repeat

			sub di,2			; mov to last block of line
			pop word[es:di]			; print/pop to last block



       		  pop si 			; popping relevant used registers
                  pop di
                  pop cx
                  pop bx
                  pop ax
                  pop es
                  pop ds
		pop bp
                 
                  ret 2

;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
; SUBROUTINE FON MOVING SKY
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------

movesky:
        push ax			; pushing relevant registers
	mov ax,0
skyloop:

        push ax
        call movleft
        inc ax			; ax ++
        cmp ax,8 		; moving till 8 line
	jne skyloop
	pop ax 			; popping relevant used registers
	ret

;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR MOVING SCREEN TO RIGHT
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------

movright:
		push bp			; pushing relevant registers
		mov bp,sp
		  push ds
                  push es
                  push ax
                  push bx
                  push cx
                  push di
                  push si



			mov ax,0xb800			; display mode
			mov es,ax
			mov ds,ax          

			mov ax,80   			; ax = 80        
			mul byte[bp+4]			; multiplying with row sent
			shl ax,1			; ax  x 2
			sub ax,2     	 		; moving ax to last block

			mov di,ax			; moving ax to di
			mov si,ax
			sub si,2			; placing si 1 before di

			mov cx,80			; count for single line
			push word[es:di]		; pushing in stack
			std                 		; decrementing 
			rep movsw			; repeat

			add si,4			; moving si to first block
			pop word[es:si]			; poping value



       		  pop si 			; popping relevant used registers
                  pop di
                  pop cx
                  pop bx
                  pop ax
                  pop es
                  pop ds
		pop bp
                 
                  ret 2
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR MOVING SEA
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------


movesea:
        push ax			; pushing relevant registers
	mov ax,10		; starting of sea
sealoop:

        push ax
        call movright
        inc ax			; inc
        cmp ax,18		; move to right till line 18 
	jne sealoop
	pop ax 			; popping relevant used registers
	ret
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
; SUBROUTINE FOR  MOVING DEEP SEA
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------

movedeepsea:
        push ax			; pushing relevant registers
	mov ax,17	 	; ax =19 line

deepsealoop:

        push ax
        call movleft
        inc ax			; inc
        cmp ax,25 		; moving to left till last line
	jne deepsealoop
	pop ax 			; popping relevant used registers
	ret

;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR DELAY
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------

delay:
                  push cx			; pushing relevant registers
                  mov cx,0xFFFF
loop1:
                  loop loop1
                  mov cx,0xFFFF
loop2:
                  loop loop2
                  pop cx 			; popping relevant used registers
                  
                  ret

;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBRPUTINE FOR WEED 
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------

weed:
		push bp			; pushing relevant registers
		mov bp,sp
                  push es
                  push ax  		; gpr
                  push bx  		; size
                  push cx  		; color
                  push di

                  mov cx,[bp+10]		; placing color
		mov di,[bp+4]			; placing starting point
		mov bx,[bp+6]			; placing size of weed
                  mov ax,0xb800
                  mov es,ax
                  mov dx,cx
	mov dl,0xdd
	mov cl,0xde
                  mov word[es:di],cx		; printing at bottom
nextweed:
            
                  sub di,162			; printing till size user sent
                  mov word[es:di],cx
                  sub di,158
                  mov word[es:di],dx
                  sub bx,1
                  jnz nextweed

                  pop di  			; popping relevant used registers
                  pop cx
                  pop bx
                  pop ax
                  pop es
		pop bp
       
                  ret 4				; two parameters sent i.e size and starting index

;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR PRINTING IN DEEP SEA
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------

deepsea: 
		push es 
 		push ax 
		push di 

 		mov ax, 0xb800 
 		mov es, ax 		; point es to video base 
	
 		mov di,2720
 		mov ax, 0x01db 		; printing deep sea
 		mov cx, 640		; number of screen locations 
 		cld 			; auto increment mode 
 		rep stosw 		; print to end of screen as of givem size 

     	        	pop di 			; popping relevant used registers	
 		pop ax 
 		pop es 	

	
                  mov bx,2			; size
                  push bx
                  mov bx,3850			; starting index
                  push bx
                  call weed

                  mov bx,1			; size
                  push bx
                  mov bx,3856			; starting index
                  push bx
                  call weed

                  mov bx,2			; size
                  push bx
                  mov bx,3862			; starting index
                  push bx
                  call weed

                  mov bx,1			; size
                  push bx
                  mov bx,3920			; starting index
                  push bx
                  call weed

                  mov bx,2			; size
                  push bx
                  mov bx,3926			; starting index
                  push bx
                  call weed

                  mov bx,1			; size
                  push bx
                  mov bx,3980			; starting index
                  push bx
                  call weed

                  mov bx,2			; size
                  push bx
                  mov bx,3988			; starting index
                  push bx
                  call weed

                  ret 2				; one parameter color



;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR FISH 
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------



fish: 			
		push es 		; pushing relevant registers
 		push ax 
		push di 
		xor di,di
 		mov ax, 0xb800 		
 		mov es, ax 		; point es to video base 
		mov di,[cs:fishloc]
	
		mov word[es:di-2],0x17fe
		mov word[es:di  ],0x17db
		mov word[es:di+2],0x77de
		mov word[es:di+4],0x17fe

 		pop di			; popping relevant used registers
 		pop ax 
 		pop es 
 		ret 			; return


;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR FISH MOVE
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
checkright:
		push bx
		loopr:
			sub bx,160
			cmp bx,160
			jnc loopr
		cmp bx,0
   		pop bx
		jne rright
		sub bx,160
		jmp rright
		
checkleft:
		push bx
		loopl:
			sub bx,160
			cmp bx,160
			jnc loopl
		cmp bx,0
   		pop bx
		jne rleft
		add bx,160
		jmp rleft
		
kbisr:
		push ax
		push bx
		push es

		mov ax,0xb800
		mov es,ax
		in al,0x60		; getting keyborad interupt
		cmp al,0x4d		; compare right key
		jne nextcompare1	; jump if not equal
		mov bx,[cs:fishloc]	; if equal add 2
		add bx,2
		jmp checkright		; check if on right col
		rright:			; return 
		mov [cs:fishloc],bx	; placing fish back
		jmp exit		; jump to exit
nextcompare1:
		cmp al,0x4b		; compare if left key is pressed
		jne nextcompare2	; jmp not equal
		mov bx,[cs:fishloc]	; get fish loc
		jmp checkleft		; check if left col
		rleft:			; return
		sub bx,2		; move left
		mov [cs:fishloc],bx	; place fish back
		jmp exit		; exit
nextcompare2:
		cmp al,0x48		; compare if up key
		jne nextcompare3	; jump if not equal
		mov bx,[cs:fishloc]	; get fish loc
                cmp bx,2880		; cmp with top of deep sea
		jc noup			; jump to skip 
		sub bx,160		; else go up one row
		mov [cs:fishloc],bx	; place fish back
		jmp exit		; exit
		noup:			; label
		call sound		; sound if hits top
		jmp exit		; exit
nextcompare3:
		cmp al,0x50		; check if down
		jne nextcompare4	; jump if not eqaul
		mov bx,[cs:fishloc]	; get fish loc
		cmp bx,3840		; check if bottom
		jnc nodown		; jump if not at bottom
		add bx,160		; move to bottom one row
		mov [cs:fishloc],bx	; place fish back
		jmp exit		; exit
		nodown:			; label
		call sound		; call sound if hits bottom
		jmp exit		; exit
nextcompare4:
		cmp al,0x01		; check if escape key to terminate
		jne nomatch		; jump no match
mov al,0x20		; ready to take next interrupt
		out 0x20,al
		pop es			; pop registers
                pop bx
		pop ax
                mov ax,10		; exit val
		iret
nomatch:				; no match key found
		pop es			; pop registers
		pop bx
		pop ax
		jmp far [cs:oldisr]	; call original isr if no key found
exit:					; label
		call collosioncheck
		mov al,0x20		; ready to take next interrupt
		out 0x20,al
		pop es			; pop registers
		pop bx
		pop ax
		iret			; return cs and ip
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR HOOKING
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
hook:
		pusha			; pushing all registers 
		xor ax,ax		; ax=0
		mov es,ax		; point es to 0
		mov ax, [es:9*4]	; placing keyboard isr function in old isr
		mov [oldisr], ax		
		mov ax, [es:9*4+2]	; placing cs too
		mov [oldisr + 2], ax

		mov ax,[es:8*4]
		mov [oldtimer],ax
		mov ax,[es:8*4+2]
		mov [oldtimer+2],ax
		cli			; stopping all interrupts
		mov word[es:9*4], kbisr	; placing new keyborad function
		mov [es:9*4+2], cs	; cs too
		mov word[es:8*4], timer	; placing new keyborad function
		mov [es:8*4+2], cs	; cs too
		
		sti			; allowing all interrupts

		popa			; pop all registers
		ret			; return
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR UN HOOKING
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
unhook:
		pusha			; pushing all registers
		xor ax,ax		; ax=0
		mov es,ax		; es=0
		mov ax,[oldisr]		; getting old isr function value
		mov bx,[oldisr+2]	; getting cs too

		mov cx,[oldtimer]		; getting old isr function value
		mov dx,[oldtimer+2]	; getting cs too
		cli			; disabling all registers
		mov [es:9*4],ax		; placing all register in table
		mov [es:9*4+2],bx	; cs too
		mov [es:8*4],cx		; placing all register in table
		mov [es:8*4+2],dx	; cs too
		sti			; enabelling all interrupts
		popa			; popping all registers
		ret			; return
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR SOUND
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------


delay1:
                  push cx			; pushing relevant registers
                  mov cx,0x0FFF
loop123:
                  loop loop123
		  pop cx
                  ret
sound:
		pusha
 		mov cx, 5
		loopsound1:  	
	       	mov al, 0b6h
		out 43h, al

		;load the counter 2 value for d3
		mov ax, 1fb4h
		out 42h, al
		mov al, ah
		out 42h, al

		;turn the speaker on
		in al, 61h
		mov ah,al
		or al, 3h
		out 61h, al
		call delay1
		mov al, ah
		out 61h, al

		call delay1

		;load the counter 2 value for a3
		mov ax, 152fh
		out 42h, al
		mov al, ah
		out 42h, al

		;turn the speaker on
		in al, 61h
		mov ah,al
		or al, 3h
		out 61h, al
		call delay1
		mov al, ah
		out 61h, al

		call delay1
	
		;load the counter 2 value for a4
		mov ax, 0A97h
		out 42h, al
		mov al, ah
		out 42h, al
	
		;turn the speaker on
		in al, 61h
		mov ah,al
		or al, 3h
		out 61h, al
		call delay1
		mov al, ah
		out 61h, al

		call delay1
 
 		loop loopsound1
		popa
		ret
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR TIMER 
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
update1:
	call updateday
	mov word[cs:tickcount+2],0
	jmp returntimer
update2:
	call updatenight
	jmp returntimer
update_random:
	mov word[cs:random],2720
	jmp returnrandom
updatesecond
	add word[cs:clock],1
	mov word[cs:clock+2],0
	add word[cs:red+2],1
	cmp word[cs:red+2],5
	je resetred
	returnresetred:
	add word[cs:green+2],1
	cmp word[cs:green+2],10
	je resetgreen
	returnresetgreen:
	call displaypoints
	jmp secondreturn
resetred:
	call updatered
	mov word[cs:red+2], 0
	jmp returnresetred
resetgreen:
	call updategreen
	mov word[cs:green+2], 0
	jmp returnresetgreen

timer:
	pusha
	add word[cs:random],10
	cmp word[cs:random],3900
	jnc update_random
	returnrandom:
	inc word[cs:clock+2]
	cmp word[cs:clock+2],18
	je updatesecond
	secondreturn:
	inc word[cs:tickcount]
	cmp word[cs:tickcount],2
	jne exittimer
               ; inc word [cs:score]			; increment score count
	mov word[cs:tickcount],0
	inc word[cs:tickcount+2]
	cmp word[cs:tickcount+2],160
	je update1
	cmp word[cs:tickcount+2],80
	je update2
returntimer:
	cli
	call movesky
	call movesea
	sti
exittimer:
	mov al,0x20
	out 0x20,al
	popa
	iret

;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
; SUBROUTINE FOR CHECKING COLLOSION
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
redcollide:
	add word[cs:score],50
	call updatered
	jmp redcollidereturn
greencollide:
	add word[cs:score],10
	call updategreen
	jmp greencollidereturn

collosioncheck:
	pusha
	mov ax,[cs:fishloc]
	cmp ax,[cs:red]
	je redcollide
	redcollidereturn:
	cmp ax,[cs:green]
	je greencollide
	greencollidereturn:
	call displaypoints
	popa
ret
	
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
; SUBROUTINE FOR DISPLAY POINTS
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
displaypoints:
	pusha
	mov ax,0xb800
	mov es,ax
	mov di,[cs:red]
	mov word[es:di],0x14fe
	mov di,[cs:green]
	mov word[es:di],0x12fe
	popa
ret
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
; SUBROUTINE FOR UPDATE RED
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
; SUBROUTINE FOR UPDATE RED
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
updatered:
	pusha
	mov word[cs:red+2],0
	mov ax,[cs:random]
	mov [cs:red],ax
	popa
ret

;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
; SUBROUTINE FOR UPDATE GREEN
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
updategreen:
	pusha
	mov word[cs:green+2],0
	mov ax,[cs:random]
	mov [cs:green],ax
	popa
ret
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR SCORE
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
printnum: 
	push bp
	mov bp, sp
	push es
	push ax
	push bx
	push cx
	push dx
	push di
	mov ax, 0xb800
	mov es, ax		 ; point es to video base
	mov ax, [bp+4]		 ; load number in ax
	mov bx, 10		 ; use base 10 for division
	mov cx, 0 			 ; initialize count of digits
	nextdigit: mov dx, 0		 ; zero upper half of dividend
	div bx ; divide by 10
	add dl, 0x30		 ; convert digit into ascii value
	push dx			 ; save ascii value on stack
	inc cx			 ; increment count of values
	cmp ax, 0			 ; is the quotient zero
	jnz nextdigit		 ; if no divide it again
	mov di, 3992		 ; point di to 70th column
	nextpos: pop dx		 ; remove a digit from the stack
	mov dh, 0x70		 ; use normal attribute
	mov [es:di], dx		 ; print char on screen
	add di, 2			 ; move to next screen location
	loop nextpos		 ; repeat for all digits on stack
	pop di
	pop dx
	pop cx
	pop bx
	pop ax 
	pop es
	pop bp
ret 2
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR LIVE 
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
daynow1:
mov bx,0x1adb
push bx
jmp return_live
nightnow1:
mov bx,0x12db
push bx
jmp return_live

live:	
	cmp word[cs:tickcount+2],79
	jna daynow1
	cmp word[cs:tickcount+2],79
	ja nightnow1
return_live:
      	call deepsea
     	call fish
	push word [cs:score]
	call printnum ; print score count
	;push word [cs:random]
	;call printnum ; print random count
	;call collosioncheck
	call displaypoints
	call delay
        	cmp ax,10
        	je endlive
       	 jne live
endlive:
         	ret
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
; SUBROUTINE FOR RANDOM NUMBER GENERATION
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------

RANDGEN:        		 	; generate a rand no using the system time
	RANDSTART:
	push bp
	mov bp,sp
	pusha
   	mov ah, 00h 		; interrupts to get system time        
   	int 1ah      		; CX:DX now hold number of clock ticks since midnight      
   	mov  ax, dx
   	xor  dx, dx
   	mov  cx, 10    
   	div  cx       		; here dx contains the remainder of the division - from 0 to 9
   	add  dl, '0'  		; to ascii from '0' to '9'
	mov bx,0x0f00
	mov bl,dl
	mov [bp+4],bx
	popa
	pop bp
ret   
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
; SUBROUTINE FOR UPDATION
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
updateday:
	pusha
                call clrscr
        	mov bx, 0x0bdb		; parameter sky color 
	push bx
	call sky			; sky printing function
        	mov bx,0x0edb		; parameter planet color
	push bx
        	mov bx,152		; parmeter starting index of planet
	push bx
        	call planet 		; calling planet fun
        	mov bx, 0x0adb		; parameter for  mountaings grass color
        	push bx
        	mov bx,0x06db		; parameter for mountains soil color
        	push bx
	call mountainss		; calling mountains fun
	mov bx,0x0fdb
	push bx
        	call clouds		; calling clouds fun
       	mov bx,0x09db		; parameter for sea color
	push bx
	call sea			; calling sea fun
        	mov bx,0x0adb		; parameter for ground color
	push bx
        	mov bx,1280		; parameter for ground starting index
	push bx
        	call ground		; calling ground fun
	call ships			; calling ship fun
	mov bx,0x1adb		; parameter for deep sea weed color
	push bx
	call deepsea		; calling deep sea fun
        	call fish
	popa
	ret

updatenight:
           pusha
                call clrscr
        	mov bx, 0x00db		; parameter sky color 
	push bx
	call sky			; sky printing function
        	mov bx,0xffdb		; parameter planet color
	push bx
        	mov bx,152		; parmeter starting index of planet
	push bx
        	call planet 		; calling planet fun
        	mov bx, 0x02db		; parameter for  mountaings grass color
        	push bx
        	mov bx,0x04db		; parameter for mountains soil color
        	push bx
	call mountainss		; calling mountains fun
	mov bx,0x07db
	push bx
        	call clouds		; calling clouds fun
	call stars
       	mov bx,0x01db		; parameter for sea color
	push bx
	call sea			; calling sea fun
        	mov bx,0x02db		; parameter for ground color
	push bx
        	mov bx,1280		; parameter for ground starting index
	push bx
        	call ground		; calling ground fun
	call ships			; calling ship fun
	mov bx,0x12db		; parameter for deep sea weed color
	push bx
	call deepsea		; calling deep sea fun
        	call fish
	popa
	ret

;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;MAIN PROGRAM START HERE
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
start:
	call clrscr		; clear screeen fun
        	mov bx, 0x0bdb		; parameter sky color 
	push bx
	call sky		; sky printing function
        
        	mov bx,0x0edb		; parameter planet color
	push bx
        	mov bx,152		; parmeter starting index of planet
	push bx
        	call planet 		; calling planet fun

        	mov bx, 0x0adb		; parameter for  mountaings grass color
        	push bx
        	mov bx,0x06db		; parameter for mountains soil color
        	push bx
	call mountainss		; calling mountains fun
	mov bx,0x0fdb
	push bx
        	call clouds		; calling clouds fun
        	mov bx,0x09db		; parameter for sea color
	push bx
	call sea			; calling sea fun
        	mov bx,0x0adb		; parameter for ground color
	push bx
        	mov bx,1280		; parameter for ground starting index
	push bx
        	call ground		; calling ground fun
	call ships			; calling ship fun
	mov bx,0x1adb		; parameter for deep sea weed color
	push bx
	call deepsea		; calling deep sea fun
        	call fish			; printing fish
	call hook			; hooking fish move keyborad interrupt
       	call live			; calling infinite live fun
	call unhook		; un hooking keyboard interrupt
	call clrscr			; clearing screen
        	mov ax,0x4c00		; termination
	int 0x21
; Global Variables
oldisr:	dw 0,0			; space for saving old isr
tickcount: dw 0,0			; tickcount timer value
oldtimer:  dw 0,0 		; space for old timer function

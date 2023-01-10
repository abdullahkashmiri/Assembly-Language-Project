;PROJECT 
;-------------------------------------------------------------------------------------------
;GROUP MEMBERS 
;1-> ABDULLAH DAR BSCS 3G 21-L-7512
;2-> SAMI KHOKHER BSCS 3G 21-L-1868
	
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------

[org 0x0100]
jmp start
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
;SUBROUTINE FOR PRINTING A CLOUD
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------

cloud:
		  push bp			; pushing relevant registers
		  mov bp,sp
                  push es
                  push ax
                  push di
                  
		  mov di,[bp+4]			; starting index of cloud
                  mov ax,0xb800
                  mov es,ax
                  mov ax,3			; size of cloud
               
                  mov word[es:di],0x7F20	; cloud color printing at top
                  add di,158
nextcloud:
                  mov word[es:di],0x7F20	; printing cloud
                  add di,2
                  sub ax,1
                  jnz nextcloud

                  pop di 			; popping relevant used registers
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
                  mov bx,144		; starting index
	 	  push bx
                  call cloud
                  mov bx,210		; starting index
	 	  push bx
                  call cloud

                  ret

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
                  
                  mov word[es:di],cx		; printing at bottom
nextweed:
            
                  sub di,162			; printing till size user sent
                  mov word[es:di],cx
                  sub di,158
                  mov word[es:di],cx
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
                  mov bx,3860			; starting index
                  push bx
                  call weed

                  mov bx,2			; size
                  push bx
                  mov bx,3870			; starting index
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
                  mov bx,3990			; starting index
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
 		push cx 
		push di 

 		mov ax, 0xb800 
 		mov es, ax 		; point es to video base 
		mov di,[fishloc]

 		mov word[es:di-2],  0x1f3c
 		mov word[es:di],    0x1f3d
		mov word[es:di-160],0x1f5f
		mov word[es:di+160],0x1f60
		mov word[es:di+2],  0x1f7b
                
 		pop di			; popping relevant used registers
 		pop cx 
 		pop ax 
 		pop es 
 		ret 


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

		in al,0x60

		cmp al,0x4d		; right
		jne nextcompare1

		mov bx,[fishloc]
		add bx,2
		jmp checkright
		rright:
		
		mov [fishloc],bx

		jmp exit

nextcompare1:
		cmp al,0x4b		; left
		jne nextcompare2

		mov bx,[fishloc]
		
		jmp checkleft
      
		rleft:
		sub bx,2
		mov [fishloc],bx

		jmp exit

nextcompare2:
		cmp al,0x48		; up
		jne nextcompare3

		mov bx,[fishloc]
         ;       cmp bx,2880
                cmp bx,3040
		jc noup
		sub bx,160
		mov [fishloc],bx
 
		jmp exit
		noup:
		call sound
		jmp exit

nextcompare3:
		cmp al,0x50		; down
		jne nextcompare4


		mov bx,[fishloc]
	;	cmp bx,3840
		cmp bx,3680
		jnc nodown
		add bx,160
		mov [fishloc],bx
		jmp exit
		nodown:
		call sound
		jmp exit

nextcompare4:
		cmp al,0x01		; terminate
		jne nomatch

		pop es
                pop bx
		pop ax
         
                mov ax,10
		iret


nomatch:
		pop es
		pop bx
		pop ax
		jmp far [cs:oldisr]	; call original isr

exit:
		mov al,0x20
		out 0x20,al

		pop es
		pop bx
		pop ax

		iret
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR HOOKING
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
hook:
		pusha

		xor ax,ax
		mov es,ax

		mov ax, [es:9*4]
		mov [oldisr], ax
		
		mov ax, [es:9*4+2]
		mov [oldisr + 2], ax

		cli
		mov word[es:9*4], kbisr
		mov [es:9*4+2], cs
		sti

	;	mov dx,Fishmove
	;	mov cx,oldisr
	;	sub dx,cx
	;	add dx,15
	;	mov cl,4
	;	shr dx,cl
		
		popa

		ret
		

;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR UN HOOKING
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
unhook:
		pusha

		xor ax,ax
		mov es,ax
		
		mov ax,[oldisr]
		mov bx,[oldisr+2]

		cli
		mov [es:9*4],ax
		mov [es:9*4+2],bx
		sti
		
		popa

		ret
		
		
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
;SUBROUTINE FOR LIVE 
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------

live:

       mov bx,80
       call fresh
       refresh:
       push bx
	call movesky
	call delay


        call movesea
	call delay

;	call movedeepsea
;	call delay

	mov bx,0x0adb		; parameter for deep sea weed color
	push bx
        call deepsea

        call fish
        cmp ax,10
        je endlive
        pop bx
        sub bx,1
        cmp bx,0
        jne refresh

        cmp ax,10
        jne live
	;jmp live
endlive

         ret
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
;SUBROUTINE FOR Refresh
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
fresh:
	pusha
;call clrscr
;call delay
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
        call clouds		; calling clouds fun
        mov bx,0x09db		; parameter for sea color
	push bx
	call sea		; calling sea fun
        mov bx,0x0adb		; parameter for ground color
	push bx
        mov bx,1280		; parameter for ground starting index
	push bx
        call ground		; calling ground fun
	call ships		; calling ship fun
	mov bx,0x0adb		; parameter for deep sea weed color
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
        call clouds		; calling clouds fun
        mov bx,0x09db		; parameter for sea color
	push bx
	call sea		; calling sea fun
        mov bx,0x0adb		; parameter for ground color
	push bx
        mov bx,1280		; parameter for ground starting index
	push bx
        call ground		; calling ground fun
	call ships		; calling ship fun
	mov bx,0x0adb		; parameter for deep sea weed color
	push bx
	call deepsea		; calling deep sea fun

        call fish

	call hook

        call live		; calling infinite live fun

	call unhook

        mov ax,0x4c00		; termination
	int 0x21


fishloc: dw 2960
oldisr:	dw 0,0			; space for saving old isr
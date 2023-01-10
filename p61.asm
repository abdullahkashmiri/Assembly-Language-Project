[org 0x0100]

jmp start

; printimg messages
messagename:db 'Enter Your Name : $'
messagewelcome: db 'Welcome to ...... Galaxy $'

introname:db 'Hello Dear : $'
intronamesize: db 13
introwelcome: db 'Welcome to THE FIN FINISH $'
introwelcomesize: db 26
introInstructions: db '<-----INSTRUCTIONS-----> $'
introInstructionssize: db 24

intromove1: db '1- RIGHT, LEFT ,UP ,DOWN $'
intromovesize1: db 25
intromove2: db '-> Keys will be used for Movement $'
intromovesize2: db 34

introscore1: db '2- Each Green Food has +10 points $'
introscoresize1: db 34
introscore2: db '3- Each Red Food has +50 points $'
introscoresize2: db 32

introdeveloper1: db 'Game Developers: 21L-7512 ABDULLAH DAR $'
introdeveloper1size: db 38
introdeveloper2: db '21L-1868 SAMI KHOKHER $'
introdeveloper2size: db 22


; input buffers
buffername: db 80
db 0
times 80 db 0

; functions

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
Printing_String:

        push di
        mov di, sp
        push ax
        push bx
        push es
        push bp
        push dx


        	mov ah, 0x13       ;bios print string service
	  	mov al, 0			; print on derired location
        	mov bh, 0          ;to print on page 0
        	mov cx, [di+12]      
		mov bl,cl		 ;attribute for string
 
	  	mov dh, [di+4]		; row
		mov dl, [di+6]		; col

        	push ds       
        	pop es         
        	mov bp, [di + 10]  ;putting string's ip in bp
        	mov cx, [di + 8]  ;putting size in cx
        	int 0x10

        pop dx
        pop bp
        pop es
        pop bx
        pop ax
        pop di

        ret 12

;--------------------------------------------------------------------------------------
Printing_Intro_page:
pusha
;----------------------------------------
; Printing Name
;----------------------------------------
xor bx,bx
mov bl,0x1b
push bx
mov ax,buffername+2	; string
push ax
xor ax,ax
mov al,[buffername+1]	; size of string
push ax
mov al,120	; coloumn
push ax
mov al,3			; row
push ax
call Printing_String

;----------------------------------------
; Printing Hello
;----------------------------------------

xor bx,bx
mov bl,0x1f
push bx
mov ax,introname		; string
push ax
xor ax,ax
mov al,[intronamesize]	; size of string
push ax
mov al,106			; coloumn
push ax
mov al,3			; row
push ax
call Printing_String

;----------------------------------------
; Printing Welcome Game
;----------------------------------------

xor bx,bx
mov bl,0x1f
push bx
mov ax,introwelcome		; string
push ax
xor ax,ax
mov al,[introwelcomesize]	; size of string
push ax
mov al,106			; coloumn
push ax
mov al,5			; row
push ax
call Printing_String

;----------------------------------------
; Printing Instructions Game
;----------------------------------------


xor bx,bx
mov bl,0x1e
push bx
mov ax,introInstructions		; string
push ax
xor ax,ax
mov al,[introInstructionssize]	; size of string
push ax
mov al,108			; coloumn
push ax
mov al,8			; row
push ax
call Printing_String

;----------------------------------------
; Printing Move Instructions 1 Game
;----------------------------------------


xor bx,bx
mov bl,0x1d
push bx
mov ax,intromove1		; string
push ax
xor ax,ax
mov al,[intromovesize1]	; size of string
push ax
mov al,102			; coloumn
push ax
mov al,10			; row
push ax
call Printing_String


;----------------------------------------
; Printing Move Instructions 2 Game
;----------------------------------------


xor bx,bx
mov bl,0x1b
push bx
mov ax,intromove2		; string
push ax
xor ax,ax
mov al,[intromovesize2]	; size of string
push ax
mov al,102 		; coloumn
push ax
mov al,12			; row
push ax
call Printing_String






;----------------------------------------
; Printing Score Instructions 1 Game
;----------------------------------------


xor bx,bx
mov bl,0x1a
push bx
mov ax,introscore1		; string
push ax
xor ax,ax
mov al,[introscoresize1]	; size of string
push ax
mov al,102			; coloumn
push ax
mov al,14			; row
push ax
call Printing_String



;----------------------------------------
; Printing Score Instructions 2 Game
;----------------------------------------


xor bx,bx
mov bl,0x14
push bx
mov ax,introscore2		; string
push ax
xor ax,ax
mov al,[introscoresize2]	; size of string
push ax
mov al,102			; coloumn
push ax
mov al,16			; row
push ax
call Printing_String



;----------------------------------------
; Printing Developer 1
;----------------------------------------


xor bx,bx
mov bl,0x2f
push bx
mov ax,introdeveloper1		; string
push ax
xor ax,ax
mov al,[introdeveloper1size]	; size of string
push ax
mov al,120			; coloumn
push ax
mov al,21			; row
push ax
call Printing_String




;----------------------------------------
; Printing Developer 2
;----------------------------------------


xor bx,bx
mov bl,0x2f
push bx
mov ax,introdeveloper2		; string
push ax
xor ax,ax
mov al,[introdeveloper2size]	; size of string
push ax
mov al,137		; coloumn
push ax
mov al,22		; row
push ax
call Printing_String





popa
ret
;--------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------



Layout:
push bp
mov bp,sp
pusha 

mov ax,0xb800
mov es,ax

mov dx,0x01db	; color

mov di,284
mov cx,18

layoutouterloop:
add di,70
mov ax,di
add ax,90
layoutinnerloop:
mov [es:di],dx
add di,2
cmp di,ax
jne layoutinnerloop
loop layoutouterloop


mov di,354			; top-left
mov word[es:di],0x1fc9

layouttop:
add di,2
mov word[es:di],0x1fcd
cmp di,442
jne layouttop

mov di,442			; top-right
mov word[es:di],0x1fbb


layoutright:
add di,160
mov word[es:di],0x1fba
cmp di,3162
jne layoutright

mov di,3162		; bottom-right
mov word[es:di],0x1fbc

layoutbottom:
sub di,2
mov word[es:di],0x1fcd
cmp di,3074
jne layoutbottom

mov di,3074		; bottom-left
mov word[es:di],0x1fc8

layoutleft:
sub di,160
mov word[es:di],0x1fba
cmp di,514
jne layoutleft



mov di,516		; top-left
mov word[es:di],0x9ac9

layouttop1:
add di,2
mov word[es:di],0x9acd
cmp di,600
jne layouttop1

mov di,600			; top-right
mov word[es:di],0x9abb


layoutright1:
add di,160
mov word[es:di],0x9aba
cmp di,3000
jne layoutright1

mov di,3000		; bottom-right
mov word[es:di],0x9abc

layoutbottom1:
sub di,2
mov word[es:di],0x9acd
cmp di,2916
jne layoutbottom1

mov di,2916		; bottom-left
mov word[es:di],0x9ac8

layoutleft1:
sub di,160
mov word[es:di],0x9aba
cmp di,676
jne layoutleft1



mov di,1156
mov word[es:di],0x9ac8

layoutmid1:
add di,2
mov word[es:di],0x9acd
cmp di,1240
jne layoutmid1
mov word[es:di],0x9abc


mov di,1316
mov word[es:di],0x9ac9

layoutmid2:
add di,2
mov word[es:di],0x9acd
cmp di,1400
jne layoutmid2
mov word[es:di],0x9abb

;--------------------------------------------Developer Layout

mov dx,0x02db	; color

mov di,3360
mov cx,4

layoutouterloop2:
add di,76
mov ax,di
add ax,84
layoutinnerloop2:
mov [es:di],dx
add di,2
cmp di,ax
jne layoutinnerloop2
loop layoutouterloop2
;--------------------------------------------------

mov di,3436			; top-left
mov word[es:di],0xaec9

layouttop2:
add di,2
mov word[es:di],0xaecd
cmp di,3518
jne layouttop2

mov di,3518			; top-right
mov word[es:di],0xaebb


layoutright2:
add di,160
mov word[es:di],0xaeba
cmp di,3998
jne layoutright2

mov di,3998		; bottom-right
mov word[es:di],0xaebc

layoutbottom2:
sub di,2
mov word[es:di],0xaecd
cmp di,3916
jne layoutbottom2

mov di,3916		; bottom-left
mov word[es:di],0xaec8

layoutleft2:
sub di,160
mov word[es:di],0xaeba
cmp di,3596
jne layoutleft2

;----------------------------------------------------------------



popa
pop bp
ret
;--------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------

Introduction_Page:
pusha

mov dx,messagename
mov ah,09
int 0x21

mov dx,buffername
mov ah,0x0a
int 0x21

mov bh,0
mov bl,[buffername+1]
mov byte[buffername+2+bx],'$'

call clrscr

call Layout

call Printing_Intro_page




popa
ret 
;-------------------------------------------------------------------
start:
;call clrscr


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


call Introduction_Page


























;-------------------------------------------------------------------------------------------------
;push ax
;push ds
;mov ax,messagename
;push ax
;call strlen 

;xor ax,ax

;mov al,messagename
;push ax
;mov ax,messagename+1
;push ax
;xor ax,ax
;mov al,20		; col
;push ax
;mov al,3
;push ax		; row

;call Taking_user_input


;mov dx,messagename
;mov ah,9
;int 0x21

;mov dx,buffername
;mov ah,0x0a
;int  0x21

;mov bh,0			; reading the size of user input and putting $ null at the end
;mov bl,[buffername+1]
;mov byte[buffername+2+bx],'$'



;mov ax,buffername+2
;push ax 			; string passing
;xor ax,ax
;mov al,[buffername+1]
;push ax			; size of string
;mov al,1			; row
;push ax
;mov al,1			; col
;push ax


;call Printing_with_input

mov ax,0x4c00
int 0x21




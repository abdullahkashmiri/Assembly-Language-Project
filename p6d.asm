;2- After taking data, redirect the user to new empty screen (call clearscreen function that we have
;already done) and in the middle of screen (12 th row, 30 th Column) display all the data (using
;BIOS/DOS Service).
[org 0x0100]

jmp start

; printing messages
messagename: db 'Enter Your Name : $'
messagerollno: db '                                                                                Enter Your Roll no : $'
messagecourse: db '                                                                                                                                                                Enter Your Course : $'
messagesection: db '                                                                                                                                                                                                                                                Enter Your Section : $'


amessagename: db 'Your Name : $'
amessagerollno: db 'Your Roll no : $'
amessagecourse: db 'Your Course : $'
amessagesection: db 'Your Section : $'


; input buffers

buffername: db 80
db 0
times 80 db 0

bufferrollno: db 80
db 0
times 80 db 0

buffercourse: db 80
db 0
times 80 db 0

buffersection: db 80
db 0
times 80 db 0


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

Printing_String
        push di
        mov di, sp
        push ax
        push bx
        push es
        push bp
        push dx
        	mov ah, 0x13       ;bios print string service
        	mov bh, 0          ;to print on page 0
        	mov bl, 0x0F       ;attribute for string
	mov dh,[di+4]		; row
	mov dl,[di+6]		; col
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
ret 10
start:
call clrscr

mov cx,15		; size
mov ah,0x13	; service 13 for writing string
mov al,1		; update cursor after printing
mov bh,0		; page no 0
mov bl,0x0f	; Text Attribute
mov dl,10		; coloumn
mov dh,10		; row
mov bp,messagename	; es:bp for printing
int 0x10


mov dx,buffername		; taking input from user here
mov ah,0x0a
int 0x21

mov bh,0			; reading the size of user input and putting $ null at the end
mov bl,[buffername+1]
mov byte[buffername+2+bx],'$'


;mov cx,200	; size
;mov ah,0x13	; service 13 for writing string
;mov al,0		; update cursor after printing
;mov bh,0		; page no 0
;mov bl,0x0f	; Text Attribute
;mov dl,10		; coloumn
;mov dh,10		; row
;mov bp,messagerollno	; es:bp for printing
;int 0x10





mov ax,0x4c00
int 0x21

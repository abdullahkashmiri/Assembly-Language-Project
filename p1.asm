;Question no 1

[org 0x0100]

jmp start

clrscr:		        
                        push es
			push ax
			push di

			mov ax, 0xb800
			mov es, ax					; point es to video base
			mov di, 0	
				
nextloc:	
                        mov word [es:di], 0x0720	                ; clear next char on screen
			add di, 2					; move to next screen location
			cmp di, 4000			        	; has the whole screen cleared
			jne nextloc			 		; if no clear next position

			pop di
			pop ax
			pop es
                        mov di,0
                        ret

star:		        
                        push es
			push ax
			push di
			mov ax, 0xb800
			mov es, ax					; point es to video base
			mov di, 1960

				
shiphor:	
                  ;      call delay                                      ; delay calling
                        mov word [es:di], 0x077E                        ; printing space
                        add di, 2		                
			cmp di, 2020			                ; has the whole screen cleared
		        jne shiphor					; if no clear next position	
 				
shipdown:	
                  ;      call delay                                      ; delay calling
                        mov word [es:di], 0x072F                        ; printing space
                        add di, 158		                
			cmp di, 2652			                ; has the whole screen cleared
		        jne shipdown					; if no clear next position	
                        

shipbottom:	
                  ;      call delay                                      ; delay calling
                        mov word [es:di], 0x077E                        ; printing space
                        sub di,2		                
			cmp di, 2608			                ; has the whole screen cleared
		        jne shipbottom					; if no clear next position	
                        mov di,2446
shipup:	
                  ;      call delay                                      ; delay calling
                        mov word [es:di], 0x075C                        ; printing space
                        sub di,162		                
			cmp di, 1798			                ; has the whole screen cleared
		        jne shipup					; if no clear next position	
 
                        mov di,0
		
movright:	
                        call delay                                      ; delay calling
                        mov word [es:di], 0x0720                        ; printing space
                        add di, 2	
                        mov word [es:di], 0x072A                        ; print star						                
			cmp di, 158			                ; has the whole screen cleared
		        jne movright					; if no clear next position
                        
movbottom:
                        call delay                                      ; delay calling                       
                        mov word [es:di], 0x0720                        ; printing space
                        add di, 160	                      
                        mov word [es:di], 0x072A                        ; print star
	         	call delay                                      ; delay calling			             
			cmp di, 3998			                ; has the whole screen cleared
			jne movbottom					
movleft:	
                        call delay                                      ; delay calling
                        mov word [es:di], 0x0720                        ; printing space
                        sub di, 2	
                        mov word [es:di], 0x072A                        ; print star						               
			cmp di, 3840			                ; has the whole screen cleared
			jne movleft	
movtop:
                        call delay                                      ; delay calling       
                        mov word [es:di], 0x0720                        ; printing space
                        sub di,160	                       
                        mov word [es:di], 0x072A                        ; print star
		        call delay                                      ; delay calling	        		
                 	cmp di, 0		                 	; has the whole screen cleared
			jne movtop
		        mov word [es:di], 0x072A                        ; print star                        
			pop di
			pop ax
			pop es
                        mov di,0
                        ret


delay:                  push cx
			mov cx, 0xFFFF
loop1:		loop loop1
			mov cx, 0xFFFF
loop2:		loop loop2
			pop cx
			ret


start:
    
       call clrscr 
       call star 









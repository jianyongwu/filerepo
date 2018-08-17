
	jmp near start
	name db 'y','0x7','z','0x7','h','0x7'
	number db 0,0,0,0,0
	
start:
	mov ax , 0x7c00
	mov ds , ax
	
	mov ax , 0xb800
	mov es , ax
	
	mov si , name
	mov di , 0x00
	
	mov cx , (number - name)/2
	
	rep movsw
	
;number鏍囧彿鍦板潃璁＄畻	
	mov ax , number
	mov bx , 0x10
	mov cx , 0x5
	
divget:
	xor dx , dx 
	div bx
	mov [si] , dl
	inc si
	loop divget

;鏍囧彿鍦板潃鏄剧ず
show:
	mov al , [si]
	mov ah , 0x7
	mov [es:di] , [ds:si]
	dec si
	cmp si , number
	jge show

	jmp near $

	times ($-$$) db 0x00
	db 0xaa55
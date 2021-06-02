
;******************************************
;��8086ϵ��΢���ӿ�ʵ��ϵͳ���γ����
;�����׼�����
;******************************************
code    segment
        assume cs:code,ds:code,es:code
dataDef:
	OUTSEG  equ  0ffdch             ;�ο��ƿ�
	OUTBIT  equ  0ffddh             ;λ���ƿ�/��ɨ��
	IN_KEY  equ  0ffdeh             ;���̶����
	LedBuf  db   6 dup(?)           ;��ʾ����
	Expression db 128 dup(?)
	len dw 0
	left_or_right db 0
	L db 0
	number_stack DW 40 dup(?)  ;����ջ
	number_index DW 0          ;����ջ����λ��
	operator_stack DB 20 dup(?);�����ջ
	operator_index DW 0         ;�����ջ����λ��
	the_ans DW 0, 0                ;���յ�������

	Ex_index DW 0               ;ÿ������ǰҪ��ʼ��
	num_or_operator DB 0         ;���ֻ�������� 0 �����֣� 1���ַ�

    ans_mode db 1 ; ��������ans_mode = 0��δ����; ans_mode = 1: ������Ϊ����; ans_mode = 2; ������Ϊż��
	is_pos_neg db 1 ;ans < 0, is_pos_neg = 1; ans >= 0, is_pos_neg = 2; ans ������Χ�� is_pos_neg = 3;
	out_ans dw 0000H ; ������ out_ans ʼ��>=0, ��is_pos_negһ���ʾthe_ans

	; оƬ��ַ
	PORT8259_0     EQU 0FFE0H
	PORT8259_1     EQU 0FFE1H
	PORT8253_CTL   EQU 0FFE7H
	PORT8253_0     EQU 0FFE4H
	PORT8253_2     EQU 0FFE6H
	PORT8255_PA    EQU 0FFD8H
	PORT8255_PB    EQU 0FFD9H
	PORT8255_PC    EQU 0FFDAH
	PORT8255_CTL   EQU 0FFDBH
    basic_tmp dw 1000,100,10,1
	dec_num db 4 dup(?)

        org  1000h
Start:
		mov ax, code
		mov ds, ax
        call init_all
        call reset_all
        mov  LedBuf+0,08eh
        mov  LedBuf+1,08eh
        mov  LedBuf+2,08eh
        mov  LedBuf+3,08eh   ;��ʾ�ַ�F
        mov  LedBuf+4,08eh
        mov  LedBuf+5,08eh

again:                       ;F������F��ʼ��
        call Disp
        call GetKey
        cmp al,0fh
        je S
        jmp again

S:
		call reset_all
        mov  LedBuf+0,0ffh
        mov  LedBuf+1,0ffh
        mov  LedBuf+2,0ffh
        mov  LedBuf+3,0c0h   ;��ʾ�ַ�0
        mov  LedBuf+4,0ffh
        mov  LedBuf+5,0ffh
        mov  cx,0            ;��ʼ��
        mov  ax,0
        mov  len,ax
        mov  left_or_right,al
        mov  Ex_index,ax
        mov  num_or_operator,al
        mov  operator_index,ax
        mov  number_index,ax

MLoop1:
		push cx
        call Disp		        ;��ʾ������ʱ�Ա���̷���
        call Disp
        call Disp
        call Disp
        call Disp
        call Disp
        call Disp
        call Disp
        call Disp
        call Disp
        call Disp
        call Disp
        call Disp
        call Disp
        call Disp
        call Disp
        call Disp
        call Disp
        call Disp
        call Disp
        call Delay
        call GetKey             ;ɨ����̲���ȡ��ֵ
		pop  cx
        cmp al,20h              ;û�а�������ʱ��GetKey����ֵΪ20h
        jz  MLoop1
        and  al,0fh             ;��ʾ����
		cmp  al,0fh             ;F
		jne  temp_Mloop2
		jmp  MLoop2
temp_Mloop2:
		cmp  al,0ah             ;����
		jb   L1
		mov  L,1                ;���ű�ǣ�Ϊ1�����Ѿ�����������
		cmp  al,0dh             ;����
		jne temp_label2
		jmp label2

temp_label2:
        mov  ah,len              ;�жϵ�ǰ�����Ƿ��Ǳ��ʽ�ĵ�һ���ַ�
		cmp  ah,00h
		jne  L3
		jmp  ER                  ;+��-��*��=Ϊ��һ���ַ�ʱ������
L3:
        cmp  al,0eh             ;E
		jne  temp_opera              ;�������
		jmp  opera
temp_opera:
        mov  bx,offset Expression
        mov  si,len
		mov  ah,[bx+si-1] ;��һ������
		cmp  ah,0ah               ;���ŵ���һ������Ϊ����
		jb  S1
		jmp label3
S1:     mov  cx,0
        mov  bx,offset Expression
        mov  si,len
		mov  [bx+si],al            ;����������ֵ������ʽ�ַ���
        mov  bx,offset LEDMAP
        mov ah,0
        add bx,ax
        mov al,[bx]
        mov  LedBuf+5,al           ;�������������������
        mov  ax,len
		inc  ax
		mov  len,ax                ;���ʽ���ȼ�1
		jmp  MLoop1

L1:                                 ;��ǰ����Ϊ����
        mov  si,len
        cmp  si,00h                 ;�жϵ�ǰ�ַ��Ƿ��ǵ�һ��
        je   L2
        mov  ah,Expression[si-1]    ;ȡ��ǰ�ַ�����һ���ַ�
		cmp  ah,0dh                 ;�Ƿ�������
		jne  L2
		mov  ah,left_or_right
		and  ah,01h
		cmp  ah,01h                 ;�ж��������Ż��������ţ�����Ϊ�����ţ�ż��Ϊ������
        je   L2
        jmp  ER                     ;�����ű���

L2:
        mov  ah,L                   ;�ж���һ�������Ƿ��Ƿ���
		cmp  ah,1
		jne   label1
        mov  LedBuf+0,0ffh          ;�Ƿ��Ŵ�������һ�����ֶ�����֮ǰ�����֣������������0
        mov  LedBuf+1,0ffh
        mov  LedBuf+2,0ffh
        mov  LedBuf+3,0ffh
        mov  LedBuf+4,0ffh
        mov  LedBuf+5,0ffh
        mov  cx,0                   ;cx��ֵ��ʾ���������ֵĸ���
        mov  ah,0
        mov  L,ah

label1:
        cmp cl,4                    ;��ǰ���ָ�������4λ������ӽ����ʽ��Ҳ����ʾ
        jb  temp_next
		jmp MLoop1
temp_next:

        mov  bx,offset Expression
        mov  si,len
		mov [bx+si],al             ;����ǰ������ӽ����ʽ
		mov ah,0
		mov bx,offset LEDMAP
		add bx,ax
		mov al,[bx]
		mov LedBuf+3,0ffh          ;Fʱ�ĸ�λΪ0�����
		mov ch,0
        mov si,cx
		mov LedBuf[si],al          ;����Ӧλ�õ�LedBuf�����޸�
        inc cx                     ;���ָ�����1
        mov ax,len                 ;���ȼ�1
		inc ax
        mov len,ax
		jmp MLoop1

label2:                            ;����
        mov bh,left_or_right
		inc bh
        mov left_or_right, bh               ;���Ÿ�����1
        mov si,len                 ;�жϵ�ǰ�Ƿ��Ǳ��ʽ�ĵ�һ��
        cmp si,0000h
        jnz op
        jmp S1
op:
		mov ah,left_or_right
		and ah,01h
		cmp ah,01h                 ;�жϵ�ǰ�������Ż���������
		jz  label4
        mov  bx,offset Expression   ;������
        mov si,len
        mov ah,[bx+si-1]
		cmp ah,0ah                 ;ȡ��һ������Ϊ����ʱ������
		jae  MP
		jmp S1
		MP:
		jmp ER
label4:                            ;������
        mov  bx,offset Expression
        mov si,len
        mov ah,[bx+si-1]           ;��һ������
		cmp ah,0ah                 ;Ϊ����ʱ������
		jb  ER
		cmp ah,0dh                 ;Ϊ����ʱ������
		je  ER
		jmp S1

label3:                            ;���������+��-��*�����ţ�ֻ��������Ҫ�жϣ����౨��
		cmp ah,0dh
		jne K
		mov ah,left_or_right                ;Ϊ����ʱ�ų�������
		and ah,01h
		cmp ah,01h
		je  K
		jmp S1
K:      jmp ER

MLoop2:                          ;�������F
        mov LedBuf+0,0ffh
        mov LedBuf+1,0ffh
        mov LedBuf+2,0ffh
        mov LedBuf+3,0c0h         ;��ʾ�ַ�0
        mov LedBuf+4,0ffh
        mov LedBuf+5,0ffh
        mov ax,0                  ;��ʼ��
		mov len,ax
		mov left_or_right,al
		mov L,al
        mov cx,0
        mov  Ex_index,ax
        mov  num_or_operator,al
        mov  operator_index,ax
        mov  number_index,ax
        call reset_all
		jmp MLoop1

ER:                             ;����
        mov  LedBuf+0,08eh
        mov  LedBuf+1,0ffh
        mov  LedBuf+2,0ffh
        mov  LedBuf+3,0ffh     ;��ʾ�ַ�F
        mov  LedBuf+4,0ffh
        mov  LedBuf+5,0ffh
		jmp  again
opera:                         ;E����
        mov  ah,left_or_right
		and  ah,01h
		cmp  ah,01h
		je   ER                ;������Ϊ���������Ȼ���󣬱���
        mov  si,len
        mov  ah,Expression[si-1]
		cmp  ah,0ah            ;+ ����
		je   ER
		cmp  ah,0bh            ;- ����
		je   ER
		cmp  ah,0ch            ;* ����
		je   ER
        mov si,len
        mov Expression[si],al
        add si,1
        mov len,si
        mov  al,0
        mov left_or_right,al
        call get_the_ans
        call analyze_ans

        jmp again
Disp:
        push cx
        mov  bx,offset LEDBuf
        mov  cl,6               ;��6���˶ι�
        mov  ah,00100000b       ;����߿�ʼ��ʾ
DLoop:
        mov  dx,OUTBIT
        mov  al,0
        out  dx,al              ;�����а˶ι�
        mov  al,[bx]
        mov  dx,OUTSEG
        out  dx,al

        mov  dx,OUTBIT
        mov  al,ah
        out  dx,al              ;��ʾһλ�˶ι�

        push ax
        mov  ah,2
        call Delay
        pop  ax

        shr  ah,1
        inc  bx
        dec  cl
        jnz  DLoop

        mov  dx,OUTBIT
        mov  al,0
        out  dx,al              ;�����а˶ι�
        pop cx
        ret

Delay:                          ;��ʱ�ӳ���
        push  cx
        mov   cx,256
        loop  $
        ;call Disp
        pop   cx
        ret

GetKey:                         ;��ɨ�ӳ���
        mov  al,0ffh            ;����ʾ��
        mov  dx,OUTSEG
        out  dx,al
        mov  bl,0
        mov  ah,0feh
        mov  cx,8
key1:   mov  al,ah
        mov  dx,OUTBIT
        out  dx,al
        shl  al,1
        mov  ah,al
        nop
        nop
        nop
        nop
        nop
        nop
        mov  dx,IN_KEY
        in   al,dx
        not  al
        nop
        nop
        and  al,0fh
        jnz  key2
        inc  bl
        loop key1
nkey:   mov  al,20h
        ret
key2:   test al,1
        je   key3
        mov  al,0
        jmp  key6
key3:   test al,2
        je   key4
        mov  al,8
        jmp  key6
key4:   test al,4
        je   key5
        mov  al,10h
        jmp  key6
key5:   test al,8
        je   nkey
        mov  al,18h
key6:   add  al,bl
        cmp  al,10h
        jnc  fkey
        mov  bx,offset KeyTable
        xlat
fkey:   ret

LedMap:                         ;�˶ι���ʾ��
        db   0c0h,0f9h,0a4h,0b0h,099h,092h,082h,0f8h
        db   080h,090h,088h,083h,0c6h,0a1h,086h,08eh,0ffh,0BFH

KeyTable:                       ;���붨��
        db   07h,04h,08h,05h,09h,06h,0ah,0bh
        db   01h,00h,02h,0fh,03h,0eh,0ch,0dh

;----------------------����ģ��-------------------------
get_the_ans proc near
once:
	call get_next_word
	cmp  num_or_operator, 0    ;�ж�ȡ��������������
	ja op_now
	;�����֣���������ջ
	mov si, number_index
	mov dx, 0
	mov number_stack[si], ax
	add si, 2
	mov number_stack[si], dx
	add si, 2
	mov number_index, si
	jmp once

op_now:
	mov si, operator_index
	cmp al, 0ch
	jb  temp_plus   ;������ǰ�����ǼӼ�
	cmp al, 0ch
	jz  temp_mul        ;������ǰ�����ǳ˺�
	cmp al, 0dh          ;������ǰ����������
	jz  pre_kuo

	;���������ǵȺŵ����
	again_eq:
	cmp si, 0
	jz the_lase
	dec si
	cmp operator_stack[si], 0ah
	jz eop1
	cmp operator_stack[si], 0bh
	jz eop2
	cmp operator_stack[si], 0ch
	jz eop3

	eop1:
	call make_plus
	jmp again_eq

	eop2:
	call make_decr
	jmp again_eq

	eop3:
	call make_mul
	jmp again_eq

	the_lase:
	mov si, number_index
	sub si, 4
	mov ax, number_stack[si]
	mov dx, number_stack[si + 2]
	mov the_ans[0], ax
	mov the_ans[2], dx
	ret                           ;���������겢�õ����ս��

temp_plus:
	jmp plus_decrease
temp_mul:
	jmp num_mul

;��ǰ�����Ϊ����
pre_kuo:
	cmp left_or_right, 0
	jz left
	;�����ŵ����
	dec si         ;ȡ����ջջ��
	cmp operator_stack[si], 0dh     ;������������
	jz op_4
	cmp operator_stack[si], 0ah     ;�����Ӻ���
	jz op_1
	cmp operator_stack[si], 0bh      ;����������
	jz op_2
	cmp operator_stack[si], 0ch     ;�����˺���
	jz op_3

	op_1:
	call make_plus
	jmp pre_kuo

	op_2:
	call make_decr
	jmp pre_kuo

	op_3:
	call make_mul
	jmp pre_kuo

	op_4:                                      ;���������������
	mov left_or_right, 0
	mov operator_index, si
	jmp once

	left:                                   ;�����ŵ����
	mov operator_stack[si], al
	inc si
	mov left_or_right, 1
	mov operator_index, si
	jmp once

;��ǰ�����Ϊ��
num_mul:
	cmp si, 0
	ja compare_pre2
	mov operator_stack[si], al
	inc si
	mov operator_index, si
	jmp once
  compare_pre2:
	dec si
	cmp operator_stack[si], 0ch
	jz op_mul
	inc si                              ;���ǰ�治�ǳ˺�
	mov operator_stack[si], al
	inc si
	mov operator_index, si
	jmp once

	op_mul:                                  ;���ǰ���ǳ˺�
	call make_mul
	jmp num_mul

;��ǰ�����Ϊ�ӻ��
plus_decrease:
	cmp si, 0
	ja cmpare_pre                             ;�����ǰ����ջΪ�գ�ֱ�ӷ���
	mov operator_stack[si], al
	inc si
	mov operator_index, si
	jmp once

  cmpare_pre:
	dec si
	cmp operator_stack[si], 0ah ;���ǰ��һλ�ǼӺ�
	jnz op2
	call make_plus
	jmp plus_decrease

    op2:
    cmp operator_stack[si], 0bh  ;���ǰ����һλ����
	jnz op3
	call make_decr
	jmp plus_decrease

    op3:
    cmp operator_stack[si], 0ch  ;���ǰ����һλ�˺�
	jnz op4
	call make_mul
	jmp plus_decrease

    op4:
	inc si                         ;�ȼӻ���
    mov operator_stack[si], al    ;���ǰ�������� ֱ�ӷŽ����Ϳ�����
	inc si
	mov operator_index, si
	jmp once

get_the_ans endp

;����ջջ�������������ӷ�����
make_plus proc near
	push si
	mov si, number_index
	sub si, 8
	mov cx, ax
	mov ax, number_stack[si]
	mov dx, number_stack[si + 2]
	add ax, number_stack[si + 4]
	adc dx, number_stack[si + 6]
	mov number_stack[si], ax
	mov number_stack[si + 2], dx
	add si, 4
	mov ax, cx
	mov number_index, si
	pop si
	ret
make_plus endp

;����ջջ��������������������
make_decr proc near
	push si
	mov si, number_index
	sub si, 8
	mov cx, ax
	mov ax, number_stack[si]
	mov dx, number_stack[si + 2]
	sub ax, number_stack[si + 4]
	sbb dx, number_stack[si + 6]
	mov number_stack[si], ax
	mov number_stack[si + 2], dx
	add si, 4
	mov ax, cx
	mov number_index, si
	pop si
	ret
make_decr endp

;����ջջ�������������˷�����
make_mul proc near
	push si
	mov si, number_index
	sub si, 8
	mov cx, ax
	mov ax, number_stack[si]        ;�˷����ﻹ��Ҫ��࿼�Ƿ�Χ  �����˿���֮ǰ������Χ�Ļᱻ���ǵ�--------------
;	mov dx, number_stack[number_index + 2]
	imul word ptr number_stack[si + 4]
;	sbb dx, number_stack[number_index + 6]
	mov number_stack[si], ax
	;add dx, number_stack[si + 2]   ;ԭ����λ����0�Ļ� ���ﻹ�ǿ��Ե�
	mov number_stack[si + 2], dx
	add si, 4
	mov ax, cx
	mov number_index, si
	pop si
	ret
make_mul endp

get_next_word proc near   ;��ȡ��һ����Ч��Ϣ, ���ص���Ҫ��������1.�������ֻ����������AX 2.�ж�ʱ���ֻ����������num_or_operator
    ;push bx
	mov si, Ex_index
	mov ax, 0
	mov al, Expression[si]     ;ȡ��һ���ֽ�
	cmp al, 0ah                     ;��10���Ƚ�
	jb digit
	mov num_or_operator, 1
	inc si	;�Ѿ��ж�������� �±�Ҫ���µ���һλ
	mov Ex_index, si
	;pop bx
	ret
digit:                            ;�жϵ�ǰλ������
	mov num_or_operator, 0
next_num:
	inc si                    ;�ƶ�����һλ
	mov bl, Expression[si]      ;ȡһ���ֽڳ���
	cmp bl, 0ah                      ;�жϵ�ǰλ�ǲ������֣����Ǿ���ɣ�����
	jae done
	shl ax, 1                      ;�����֣�����ǰax��10��Ȼ������µ�����bl��֮��ѭ��
	mov dx, ax
	shl ax, 1
	shl ax, 1
	add ax, dx
	add al, bl
	adc ah, 0
	jmp next_num
done:
	mov Ex_index, si
	;pop bx
	ret
get_next_word endp

; ------------- �������ģ�� -------
init_all: ; ��ʼ��оƬ����
	;0. ��ʼ���жϷ����ӳ���
	;1. ��ʼ��8259AоƬ
	;2. ��ʼ��8253оƬ
	;3. ��ʼ��8255оƬ

	; --------[[[[   ��ʼ���жϷ����ӳ���  ]]]] ----------
	MOV AX,OFFSET INT8259_0 ; ���Ϊ����
    MOV BX,32; 08H�ж�
    MOV [BX],AX ; �жϷ�������ƫ�Ƶ�ַд��

	MOV AX,0000H
    MOV BX,34
    MOV [BX],AX ; �жϷ������Ķε�ַд��003EH

	MOV AX,OFFSET INT8259_1 ; ���Ϊż��
	MOV BX,36; 09H�ж�
    MOV [BX],AX ; �жϷ�������ƫ�Ƶ�ַд��

    MOV AX,0000H
    MOV BX,38
    MOV [BX],AX ; �жϷ������Ķε�ַд��

	; --------[[[[   ��ʼ��8255оƬ  ]]]] ----------
	MOV AL,80H   ; 10000000 -> ȫ�������ڷ�ʽ0
	MOV DX,PORT8255_CTL
	OUT DX,AL

	MOV AL,00011111B ;ʹ3����ȫ��
	MOV DX,PORT8255_PA
	OUT DX,AL

	; --------[[[[   ��ʼ��8259оƬ  ]]]] ----------

	MOV AL,13H
    MOV DX,PORT8259_0
    OUT DX,AL ; д���ʼ��������ICW1, ICW1 = 00010011 --> ���ش��� + һƬ8259A + ��ҪICW4

    MOV AL,08H
    MOV DX,PORT8259_1
    OUT DX,AL ; д���ʼ��������ICW2, ICW2 = 08H --> 8259A�ж����ͺŷֱ�Ϊ��08H,09H,0AH,...0FH

    MOV AL,0BH
    OUT DX,AL ; д���ʼ��������ICW4, ICW4  = 00001111 --> ȫǶ�׷�ʽ + �� + �Զ�EOI��ʽ + 8088ģʽ
	; ע�⣺������Գ���03H(�����û��巽ʽ)

    MOV AL,11111100B
    OUT DX,AL ; д���ʼ��������OCW1, OCW1 = 11111100 --> ��IRQ0��1�⣬�����ж�Դ��������

	;MOV AL,20H
	;MOV DX,PORT8259_0
    ;OUT DX,AL ; д���ʼ��������OCW2, OCW2 = 00100000 --> ���ȼ���ѭ��
	; ע�������д������ɾ������

	; --------[[[[   ��ʼ��8253оƬ  ]]]] ----------
	MOV DX,PORT8253_CTL
	MOV AL,34H ; д�������0�����֣�00110100�� ������0����+��д��8λ����д��8λ
	OUT DX,AL

	MOV DX,PORT8253_0
    MOV AL,00H
    OUT DX,AL
    MOV AL,096H;4BH
    OUT DX,AL  ; д�������0������ֵ

	MOV DX,PORT8253_CTL
	MOV AL,0B4H ; д�������2�����֣�10110100�� ������2����+��д��8λ����д��8λ
	OUT DX,AL

	MOV DX,PORT8253_2
    MOV AL,00H
    OUT DX,AL
    MOV AL,96H;96H
    OUT DX,AL  ; д�������2������ֵ

	ret


reset_all: ;��ʼ��һ������ʱ��Ҫ�Ĳ���
	;1. 8253GATE���Ų��������
	;2. �趨ans_modeΪ0
	;3. �趨8255оƬ���������

	MOV AL,00100111B ;ʹ3����ȫ��,������������ͣ����
	MOV DX,PORT8255_PA
	OUT DX,AL
	mov byte ptr ans_mode,0
	ret


INT8259_0: ;�жϷ������,���Ϊ����
	CLI
	push ax
	push dx

	mov DX,PORT8255_PA
	IN AL,DX ;��ȡPA����

	MOV BL,00000001B
	XOR AL,BL ;��PA0ȡ��
	OUT DX,AL

	pop dx
	pop ax
	STI
	IRET


INT8259_1: ;�жϷ������,���Ϊż��
	CLI
	push ax
	push dx

	mov DX,PORT8255_PA
	IN AL,DX ;��ȡPA����

	MOV BL,00100110B
	XOR AL,BL ;��PA1,PA2ȡ��
	OUT DX,AL

	pop dx
	pop ax
	STI
	IRET


analyze_ans: ; ����������
	mov ax,the_ans[0]
	mov cx,the_ans[2]
	mov dx,8000H
	and cx,dx
    mov dx,the_ans[2]

	cmp cx,0H
	jz pos_num
	; ������Ը�������
	mov byte ptr is_pos_neg, 1;
	sub ax,1
	sbb dx,0
	not ax
	not dx ; ������ת��Ϊ����
	mov bx,100

	jmp next1
pos_num: ; ���������0����
	mov bx,1000
	mov byte ptr is_pos_neg, 2;
next1:
	mov word ptr out_ans, ax
	div bx
	cmp ax,10
	jb next2
	mov byte ptr is_pos_neg, 3 ; ������Χ���
next2:
	mov ax,out_ans
	mov cx,0001H
	and ax,cx
	mov byte ptr ans_mode,02H
	cmp ax,0
	jz next3
	mov byte ptr ans_mode,01H
next3:
    call display_ans ; ������
	RET


display_ans:; ��ʾ������
	mov al,is_pos_neg
	cmp al,03H ; ����Ƿ�Խ��
	jnz LL1
	mov  LedBuf+0,08eh
    mov  LedBuf+1,0ffh
    mov  LedBuf+2,0ffh
    mov  LedBuf+3,0ffh     ;��ʾ�ַ�F
    mov  LedBuf+4,0ffh
    mov  LedBuf+5,0ffh
	jmp LL2
	LL1:
		call set_out_para
		call show_ans ; �������ʾ
	LL2:
        ;call set_out_para
	    ret

set_out_para: ; �趨�������
	;1. 8253GATE�����������
	;2. 8255A�趨LED�����������ż��ʱ�����趨
    sti
	mov bl,ans_mode
	cmp bl,01H
	jnz paraL1

	MOV DX,PORT8255_PA
	MOV AL, 10101110B; ����������0, ���������
	OUT DX,AL

	jmp paraL2
	paraL1: ;ż��
         ;cmp bl,02H
         ;jnz paraL2
		MOV DX,PORT8255_PA
		MOV AL,  00010001B ; ����������2, �����������̵�
		OUT DX,AL
	paraL2:
    mov bh,ans_mode
	ret

;----�������ʾ----

show_ans:
	mov ax,out_ans
	mov si,0
    mov di,0
	mov cx,4
	mov bl,is_pos_neg
	cmp bl,2
	jz ansL
	dec cx
	inc si
     inc si
     inc di
	ansL:
		mov dx,00H
		mov bx,basic_tmp[si]
		div bx
		mov byte ptr dec_num[di],al
		mov ax,dx
		inc si
        inc si
        inc di
		loop ansL
	call showRes
	ret

showRes:
	mov cx,4
	mov si,0
	mov al,is_pos_neg
	cmp al,2
	jz showL
    mov bx,offset LEDMAP
    mov ah,0
    mov al,11h
    add bx,ax
    mov al,[bx]
	mov byte ptr LedBuf[si],al
	inc si
	dec cx
	showL:
		mov al,dec_num[si]
        mov bx,offset LEDMAP
        mov ah,0
        add bx,ax
        mov al,[bx]
		mov LedBuf[si],al
         inc si
		loop showL
  ret
  ;--------
code    ends
        end dataDef
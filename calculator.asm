
;******************************************
;　8086系列微机接口实验系统　课程设计
;　简易计算器
;******************************************
code    segment
        assume cs:code,ds:code,es:code
dataDef:
	OUTSEG  equ  0ffdch             ;段控制口
	OUTBIT  equ  0ffddh             ;位控制口/键扫口
	IN_KEY  equ  0ffdeh             ;键盘读入口
	LedBuf  db   6 dup(?)           ;显示缓冲
	Expression db 128 dup(?)
	len dw 0
	left_or_right db 0
	L db 0
	number_stack DW 40 dup(?)  ;数字栈
	number_index DW 0          ;数字栈顶的位置
	operator_stack DB 20 dup(?);运算符栈
	operator_index DW 0         ;运算符栈顶的位置
	the_ans DW 0, 0                ;最终的运算结果

	Ex_index DW 0               ;每次运算前要初始化
	num_or_operator DB 0         ;数字还是运算符 0 是数字， 1是字符

    ans_mode db 1 ; 运算结果，ans_mode = 0：未运算; ans_mode = 1: 计算结果为奇数; ans_mode = 2; 计算结果为偶数
	is_pos_neg db 1 ;ans < 0, is_pos_neg = 1; ans >= 0, is_pos_neg = 2; ans 超出范围， is_pos_neg = 3;
	out_ans dw 0000H ; 输出结果 out_ans 始终>=0, 与is_pos_neg一起表示the_ans

	; 芯片地址
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
        mov  LedBuf+3,08eh   ;显示字符F
        mov  LedBuf+4,08eh
        mov  LedBuf+5,08eh

again:                       ;F锁，按F初始化
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
        mov  LedBuf+3,0c0h   ;显示字符0
        mov  LedBuf+4,0ffh
        mov  LedBuf+5,0ffh
        mov  cx,0            ;初始化
        mov  ax,0
        mov  len,ax
        mov  left_or_right,al
        mov  Ex_index,ax
        mov  num_or_operator,al
        mov  operator_index,ax
        mov  number_index,ax

MLoop1:
		push cx
        call Disp		        ;显示，且延时以便键盘防抖
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
        call GetKey             ;扫描键盘并读取键值
		pop  cx
        cmp al,20h              ;没有按键按下时，GetKey返回值为20h
        jz  MLoop1
        and  al,0fh             ;显示键码
		cmp  al,0fh             ;F
		jne  temp_Mloop2
		jmp  MLoop2
temp_Mloop2:
		cmp  al,0ah             ;数字
		jb   L1
		mov  L,1                ;符号标记，为1代表已经按过符号了
		cmp  al,0dh             ;括号
		jne temp_label2
		jmp label2

temp_label2:
        mov  ah,len              ;判断当前按键是否是表达式的第一个字符
		cmp  ah,00h
		jne  L3
		jmp  ER                  ;+、-、*、=为第一个字符时，报错
L3:
        cmp  al,0eh             ;E
		jne  temp_opera              ;运算过程
		jmp  opera
temp_opera:
        mov  bx,offset Expression
        mov  si,len
		mov  ah,[bx+si-1] ;上一个符号
		cmp  ah,0ah               ;符号的上一个符号为符号
		jb  S1
		jmp label3
S1:     mov  cx,0
        mov  bx,offset Expression
        mov  si,len
		mov  [bx+si],al            ;将按键键码值输入表达式字符串
        mov  bx,offset LEDMAP
        mov ah,0
        add bx,ax
        mov al,[bx]
        mov  LedBuf+5,al           ;将输出符号输出到数码管
        mov  ax,len
		inc  ax
		mov  len,ax                ;表达式长度加1
		jmp  MLoop1

L1:                                 ;当前键码为数字
        mov  si,len
        cmp  si,00h                 ;判断当前字符是否是第一个
        je   L2
        mov  ah,Expression[si-1]    ;取当前字符的上一个字符
		cmp  ah,0dh                 ;是否是括号
		jne  L2
		mov  ah,left_or_right
		and  ah,01h
		cmp  ah,01h                 ;判断是左括号还是右括号，奇数为左括号，偶数为右括号
        je   L2
        jmp  ER                     ;右括号报错

L2:
        mov  ah,L                   ;判断上一个按键是否是符号
		cmp  ah,1
		jne   label1
        mov  LedBuf+0,0ffh          ;是符号代表是下一个数字而不是之前的数字，所以数码管清0
        mov  LedBuf+1,0ffh
        mov  LedBuf+2,0ffh
        mov  LedBuf+3,0ffh
        mov  LedBuf+4,0ffh
        mov  LedBuf+5,0ffh
        mov  cx,0                   ;cx的值表示已输入数字的个数
        mov  ah,0
        mov  L,ah

label1:
        cmp cl,4                    ;当前数字个数超过4位，则不添加进表达式，也不显示
        jb  temp_next
		jmp MLoop1
temp_next:

        mov  bx,offset Expression
        mov  si,len
		mov [bx+si],al             ;将当前键码添加进表达式
		mov ah,0
		mov bx,offset LEDMAP
		add bx,ax
		mov al,[bx]
		mov LedBuf+3,0ffh          ;F时的个位为0，清除
		mov ch,0
        mov si,cx
		mov LedBuf[si],al          ;将相应位置的LedBuf进行修改
        inc cx                     ;数字个数加1
        mov ax,len                 ;长度加1
		inc ax
        mov len,ax
		jmp MLoop1

label2:                            ;括号
        mov bh,left_or_right
		inc bh
        mov left_or_right, bh               ;括号个数加1
        mov si,len                 ;判断当前是否是表达式的第一个
        cmp si,0000h
        jnz op
        jmp S1
op:
		mov ah,left_or_right
		and ah,01h
		cmp ah,01h                 ;判断当前是左括号还是右括号
		jz  label4
        mov  bx,offset Expression   ;右括号
        mov si,len
        mov ah,[bx+si-1]
		cmp ah,0ah                 ;取上一个符号为符号时，报错
		jae  MP
		jmp S1
		MP:
		jmp ER
label4:                            ;左括号
        mov  bx,offset Expression
        mov si,len
        mov ah,[bx+si-1]           ;上一个符号
		cmp ah,0ah                 ;为数字时，报错
		jb  ER
		cmp ah,0dh                 ;为括号时，报错
		je  ER
		jmp S1

label3:                            ;四种情况，+、-、*、括号，只有括号需要判断，其余报错
		cmp ah,0dh
		jne K
		mov ah,left_or_right                ;为括号时排除左括号
		and ah,01h
		cmp ah,01h
		je  K
		jmp S1
K:      jmp ER

MLoop2:                          ;清除运算F
        mov LedBuf+0,0ffh
        mov LedBuf+1,0ffh
        mov LedBuf+2,0ffh
        mov LedBuf+3,0c0h         ;显示字符0
        mov LedBuf+4,0ffh
        mov LedBuf+5,0ffh
        mov ax,0                  ;初始化
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

ER:                             ;报错
        mov  LedBuf+0,08eh
        mov  LedBuf+1,0ffh
        mov  LedBuf+2,0ffh
        mov  LedBuf+3,0ffh     ;显示字符F
        mov  LedBuf+4,0ffh
        mov  LedBuf+5,0ffh
		jmp  again
opera:                         ;E运算
        mov  ah,left_or_right
		and  ah,01h
		cmp  ah,01h
		je   ER                ;括号数为奇数，则必然错误，报错
        mov  si,len
        mov  ah,Expression[si-1]
		cmp  ah,0ah            ;+ 报错
		je   ER
		cmp  ah,0bh            ;- 报错
		je   ER
		cmp  ah,0ch            ;* 报错
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
        mov  cl,6               ;共6个八段管
        mov  ah,00100000b       ;从左边开始显示
DLoop:
        mov  dx,OUTBIT
        mov  al,0
        out  dx,al              ;关所有八段管
        mov  al,[bx]
        mov  dx,OUTSEG
        out  dx,al

        mov  dx,OUTBIT
        mov  al,ah
        out  dx,al              ;显示一位八段管

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
        out  dx,al              ;关所有八段管
        pop cx
        ret

Delay:                          ;延时子程序
        push  cx
        mov   cx,256
        loop  $
        ;call Disp
        pop   cx
        ret

GetKey:                         ;键扫子程序
        mov  al,0ffh            ;关显示口
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

LedMap:                         ;八段管显示码
        db   0c0h,0f9h,0a4h,0b0h,099h,092h,082h,0f8h
        db   080h,090h,088h,083h,0c6h,0a1h,086h,08eh,0ffh,0BFH

KeyTable:                       ;键码定义
        db   07h,04h,08h,05h,09h,06h,0ah,0bh
        db   01h,00h,02h,0fh,03h,0eh,0ch,0dh

;----------------------运算模块-------------------------
get_the_ans proc near
once:
	call get_next_word
	cmp  num_or_operator, 0    ;判断取出来的是数字吗
	ja op_now
	;是数字，放入数字栈
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
	jb  temp_plus   ;遇到当前符号是加减
	cmp al, 0ch
	jz  temp_mul        ;遇到当前符号是乘号
	cmp al, 0dh          ;遇到当前符号是括号
	jz  pre_kuo

	;接下来就是等号的情况
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
	ret                           ;这里运算完并得到最终结果

temp_plus:
	jmp plus_decrease
temp_mul:
	jmp num_mul

;当前运算符为括号
pre_kuo:
	cmp left_or_right, 0
	jz left
	;右括号的情况
	dec si         ;取符号栈栈顶
	cmp operator_stack[si], 0dh     ;碰到左括号了
	jz op_4
	cmp operator_stack[si], 0ah     ;碰到加号了
	jz op_1
	cmp operator_stack[si], 0bh      ;碰到减号了
	jz op_2
	cmp operator_stack[si], 0ch     ;碰到乘号了
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

	op_4:                                      ;如果碰到了左括号
	mov left_or_right, 0
	mov operator_index, si
	jmp once

	left:                                   ;左括号的情况
	mov operator_stack[si], al
	inc si
	mov left_or_right, 1
	mov operator_index, si
	jmp once

;当前运算符为乘
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
	inc si                              ;如果前面不是乘号
	mov operator_stack[si], al
	inc si
	mov operator_index, si
	jmp once

	op_mul:                                  ;如果前面是乘号
	call make_mul
	jmp num_mul

;当前运算符为加或减
plus_decrease:
	cmp si, 0
	ja cmpare_pre                             ;如果当前符号栈为空，直接放入
	mov operator_stack[si], al
	inc si
	mov operator_index, si
	jmp once

  cmpare_pre:
	dec si
	cmp operator_stack[si], 0ah ;如果前面一位是加号
	jnz op2
	call make_plus
	jmp plus_decrease

    op2:
    cmp operator_stack[si], 0bh  ;如果前面是一位减号
	jnz op3
	call make_decr
	jmp plus_decrease

    op3:
    cmp operator_stack[si], 0ch  ;如果前面是一位乘号
	jnz op4
	call make_mul
	jmp plus_decrease

    op4:
	inc si                         ;先加回来
    mov operator_stack[si], al    ;如果前面是括号 直接放进来就可以了
	inc si
	mov operator_index, si
	jmp once

get_the_ans endp

;数字栈栈顶的两个数做加法运算
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

;数字栈栈顶的两个数做减法运算
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

;数字栈栈顶的两个数做乘法运算
make_mul proc near
	push si
	mov si, number_index
	sub si, 8
	mov cx, ax
	mov ax, number_stack[si]        ;乘法这里还需要多多考虑范围  连续乘可能之前超出范围的会被覆盖掉--------------
;	mov dx, number_stack[number_index + 2]
	imul word ptr number_stack[si + 4]
;	sbb dx, number_stack[number_index + 6]
	mov number_stack[si], ax
	;add dx, number_stack[si + 2]   ;原来高位就是0的话 这里还是可以的
	mov number_stack[si + 2], dx
	add si, 4
	mov ax, cx
	mov number_index, si
	pop si
	ret
make_mul endp

get_next_word proc near   ;获取下一个有效信息, 返回的主要有两个：1.存有数字或者运算符的AX 2.判断时数字还是运算符的num_or_operator
    ;push bx
	mov si, Ex_index
	mov ax, 0
	mov al, Expression[si]     ;取到一个字节
	cmp al, 0ah                     ;和10作比较
	jb digit
	mov num_or_operator, 1
	inc si	;已经判断是运算符 下标要更新到下一位
	mov Ex_index, si
	;pop bx
	ret
digit:                            ;判断当前位是数字
	mov num_or_operator, 0
next_num:
	inc si                    ;移动到下一位
	mov bl, Expression[si]      ;取一个字节出来
	cmp bl, 0ah                      ;判断当前位是不是数字，不是就完成，跳出
	jae done
	shl ax, 1                      ;是数字，将当前ax乘10，然后加上新的数字bl，之后循环
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

; ------------- 分析输出模块 -------
init_all: ; 初始化芯片参数
	;0. 初始化中断服务子程序
	;1. 初始化8259A芯片
	;2. 初始化8253芯片
	;3. 初始化8255芯片

	; --------[[[[   初始化中断服务子程序  ]]]] ----------
	MOV AX,OFFSET INT8259_0 ; 结果为奇数
    MOV BX,32; 08H中断
    MOV [BX],AX ; 中断服务程序的偏移地址写入

	MOV AX,0000H
    MOV BX,34
    MOV [BX],AX ; 中断服务程序的段地址写入003EH

	MOV AX,OFFSET INT8259_1 ; 结果为偶数
	MOV BX,36; 09H中断
    MOV [BX],AX ; 中断服务程序的偏移地址写入

    MOV AX,0000H
    MOV BX,38
    MOV [BX],AX ; 中断服务程序的段地址写入

	; --------[[[[   初始化8255芯片  ]]]] ----------
	MOV AL,80H   ; 10000000 -> 全部工作在方式0
	MOV DX,PORT8255_CTL
	OUT DX,AL

	MOV AL,00011111B ;使3个灯全灭
	MOV DX,PORT8255_PA
	OUT DX,AL

	; --------[[[[   初始化8259芯片  ]]]] ----------

	MOV AL,13H
    MOV DX,PORT8259_0
    OUT DX,AL ; 写入初始化控制字ICW1, ICW1 = 00010011 --> 边沿触发 + 一片8259A + 需要ICW4

    MOV AL,08H
    MOV DX,PORT8259_1
    OUT DX,AL ; 写入初始化控制字ICW2, ICW2 = 08H --> 8259A中断类型号分别为：08H,09H,0AH,...0FH

    MOV AL,0BH
    OUT DX,AL ; 写入初始化控制字ICW4, ICW4  = 00001111 --> 全嵌套方式 + ？ + 自动EOI方式 + 8088模式
	; 注意：这里可以尝试03H(不采用缓冲方式)

    MOV AL,11111100B
    OUT DX,AL ; 写入初始化控制字OCW1, OCW1 = 11111100 --> 除IRQ0和1外，其余中断源均被屏蔽

	;MOV AL,20H
	;MOV DX,PORT8259_0
    ;OUT DX,AL ; 写入初始化控制字OCW2, OCW2 = 00100000 --> 优先级不循环
	; 注：上三行代码可以删除试试

	; --------[[[[   初始化8253芯片  ]]]] ----------
	MOV DX,PORT8253_CTL
	MOV AL,34H ; 写入计数器0控制字，00110100， 计数器0控制+先写低8位，再写高8位
	OUT DX,AL

	MOV DX,PORT8253_0
    MOV AL,00H
    OUT DX,AL
    MOV AL,096H;4BH
    OUT DX,AL  ; 写入计数器0计数初值

	MOV DX,PORT8253_CTL
	MOV AL,0B4H ; 写入计数器2控制字，10110100， 计数器2控制+先写低8位，再写高8位
	OUT DX,AL

	MOV DX,PORT8253_2
    MOV AL,00H
    OUT DX,AL
    MOV AL,96H;96H
    OUT DX,AL  ; 写入计数器2计数初值

	ret


reset_all: ;开始新一轮运算时需要的操作
	;1. 8253GATE引脚不允许计数
	;2. 设定ans_mode为0
	;3. 设定8255芯片的输出引脚

	MOV AL,00100111B ;使3个灯全灭,两个计数器暂停计数
	MOV DX,PORT8255_PA
	OUT DX,AL
	mov byte ptr ans_mode,0
	ret


INT8259_0: ;中断服务程序,结果为奇数
	CLI
	push ax
	push dx

	mov DX,PORT8255_PA
	IN AL,DX ;获取PA输入

	MOV BL,00000001B
	XOR AL,BL ;将PA0取反
	OUT DX,AL

	pop dx
	pop ax
	STI
	IRET


INT8259_1: ;中断服务程序,结果为偶数
	CLI
	push ax
	push dx

	mov DX,PORT8255_PA
	IN AL,DX ;获取PA输入

	MOV BL,00100110B
	XOR AL,BL ;将PA1,PA2取反
	OUT DX,AL

	pop dx
	pop ax
	STI
	IRET


analyze_ans: ; 分析运算结果
	mov ax,the_ans[0]
	mov cx,the_ans[2]
	mov dx,8000H
	and cx,dx
    mov dx,the_ans[2]

	cmp cx,0H
	jz pos_num
	; 以下针对负数处理
	mov byte ptr is_pos_neg, 1;
	sub ax,1
	sbb dx,0
	not ax
	not dx ; 将负数转化为正数
	mov bx,100

	jmp next1
pos_num: ; 针对正数或0处理
	mov bx,1000
	mov byte ptr is_pos_neg, 2;
next1:
	mov word ptr out_ans, ax
	div bx
	cmp ax,10
	jb next2
	mov byte ptr is_pos_neg, 3 ; 超出范围标记
next2:
	mov ax,out_ans
	mov cx,0001H
	and ax,cx
	mov byte ptr ans_mode,02H
	cmp ax,0
	jz next3
	mov byte ptr ans_mode,01H
next3:
    call display_ans ; 输出结果
	RET


display_ans:; 显示运算结果
	mov al,is_pos_neg
	cmp al,03H ; 结果是否越界
	jnz LL1
	mov  LedBuf+0,08eh
    mov  LedBuf+1,0ffh
    mov  LedBuf+2,0ffh
    mov  LedBuf+3,0ffh     ;显示字符F
    mov  LedBuf+4,0ffh
    mov  LedBuf+5,0ffh
	jmp LL2
	LL1:
		call set_out_para
		call show_ans ; 数码管显示
	LL2:
        ;call set_out_para
	    ret

set_out_para: ; 设定输出参数
	;1. 8253GATE引脚允许计数
	;2. 8255A设定LED输出，奇数、偶数时参数设定
    sti
	mov bl,ans_mode
	cmp bl,01H
	jnz paraL1

	MOV DX,PORT8255_PA
	MOV AL, 10101110B; 启动计数器0, 并开启红灯
	OUT DX,AL

	jmp paraL2
	paraL1: ;偶数
         ;cmp bl,02H
         ;jnz paraL2
		MOV DX,PORT8255_PA
		MOV AL,  00010001B ; 启动计数器2, 并开启两个绿灯
		OUT DX,AL
	paraL2:
    mov bh,ans_mode
	ret

;----数码管显示----

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
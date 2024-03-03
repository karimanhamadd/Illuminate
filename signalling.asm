; signalling.asm

.include "m2560def.inc"
.cseg
.org 0

	ldi r16, 0xFF
	sts DDRL, r16 ; set DDRL as output
	out DDRB, r16 ; set DDRB as output
	clr r16 ; clear r16

	rjmp test_part_e
	; Test code


test_part_a:
	ldi r16, 0b00100001
	rcall set_leds
	rcall delay_long

	clr r16
	rcall set_leds
	rcall delay_long

	ldi r16, 0b00111000
	rcall set_leds
	rcall delay_short

	clr r16
	rcall set_leds
	rcall delay_long

	ldi r16, 0b00100001
	rcall set_leds
	rcall delay_long

	clr r16
	rcall set_leds

	rjmp end


test_part_b:
	ldi r17, 0b00101010
	rcall slow_leds
	ldi r17, 0b00010101
	rcall slow_leds
	ldi r17, 0b00101010
	rcall slow_leds
	ldi r17, 0b00010101
	rcall slow_leds

	rcall delay_long
	rcall delay_long

	ldi r17, 0b00101010
	rcall fast_leds
	ldi r17, 0b00010101
	rcall fast_leds
	ldi r17, 0b00101010
	rcall fast_leds
	ldi r17, 0b00010101
	rcall fast_leds
	ldi r17, 0b00101010
	rcall fast_leds
	ldi r17, 0b00010101
	rcall fast_leds
	ldi r17, 0b00101010
	rcall fast_leds
	ldi r17, 0b00010101
	rcall fast_leds

	rjmp end

test_part_c:
	ldi r16, 0b11111000
	push r16
	rcall leds_with_speed
	pop r16

	ldi r16, 0b11011100
	push r16
	rcall leds_with_speed
	pop r16

	ldi r20, 0b00100000
test_part_c_loop:
	push r20
	rcall leds_with_speed
	pop r20
	lsr r20
	brne test_part_c_loop

	rjmp end


test_part_d:
	ldi r21, 'E'
	push r21
	rcall encode_letter
	pop r21
	push r25
	rcall leds_with_speed
	pop r25

	rcall delay_long

	ldi r21, 'A'
	push r21
	rcall encode_letter
	pop r21
	push r25
	rcall leds_with_speed
	pop r25

	rcall delay_long


	ldi r21, 'M'
	push r21
	rcall encode_letter
	pop r21
	push r25
	rcall leds_with_speed
	pop r25

	rcall delay_long

	ldi r21, 'H'
	push r21
	rcall encode_letter
	pop r21
	push r25
	rcall leds_with_speed
	pop r25

	rcall delay_long

	rjmp end


test_part_e:
	ldi r25, HIGH(WORD02 << 1)
	ldi r24, LOW(WORD02 << 1)
	rcall display_message
	rjmp end

end:
    rjmp end




set_leds:

	; clear all the registers used for set_leds
	clr r20
	clr r21
	clr r22
	clr r23
	clr r24
	clr r25
	clr r26

	mov r22, r16 ; copy r16 into r22 (to manipulate the first 4 bits)
	mov r23, r16 ; copy r16 into r23 (to manipulate the last 2 bits)

	ldi r24, 0b00001111 ; mask for the first 4 bits
	ldi r25, 0b00110000 ; mask for the last 2 bits

	and r22, r24 ; apply mask on r22
	and r23, r25 ; apply mask on r23 

	clr r16 ; clear value in r16 after copying and masking

	; shift r23 to the right so that the last 2 bits are at the rightmost
	lsr r23 
	lsr r23
	lsr r23
	lsr r23	

	ldi r27, 4 ; cycle counter for the first 4 bits
	ldi r28, 2 ; cycle counter for the last 2 bits

	check_first_four_bits:
		clr r20 ; clear r20 every loop
		mov r20, r22 ; copy masked r22 to r20
		andi r20, 0b0000001 
		cpi r20, 0b00000001 ; check if the first bit in r20 is 1
		breq add_one_L ; if 1, enter add_one_L
	
		lsr r22 ; if not 1, shift r22 to the right by 1 to check the next bit
		dec r27 ; decrease cycle counter by 1

		cpi r27, 0 ; check if cycle counter is 0
		breq one_shift ; if 0, enter one_shift

		; if not 1, shift r21 to the left 2 times
		lsl r21
		lsl r21
	
		rjmp check_first_four_bits ; loop over check_first_four_bits again

	add_one_L:
		lsr r22 ; shift r22 to the right by 1 to check the next bit
		ori r21, 1 ; change rightmost bit to 1 in r21

		cpi r27, 1 ; check if cycle counter is 1
		breq one_shift ; if 1, enter one_shift

		; if not 1, shift r21 to the left 2 times
		lsl r21 
		lsl r21
	
		dec r27 ; decrease cycle counter by 1
		cpi r27, 0 ; check if cycle counter is 0
		breq one_shift ; if 0, enter one_shift

		rjmp check_first_four_bits ; loop over check_first_four_bits again

	one_shift:
		lsl r21 ; shift r21 to the left by 1
		rjmp check_last_two_bits ; enter check_last_two_bits

	check_last_two_bits:
		clr r20 ; clear r20 every loop
		mov r20, r23 ; copy masked r23 to r20
		andi r20, 0b00000001
		cpi r20, 0b00000001 ; check if the first bit in r20 is 1
		breq add_one_B ; if 1, enter add_one_B
	
		lsr r23 ; if not 1, shift r23 to the right by 1 to check the next bit
		dec r28 ; decrease cycle counter by 1
	
		cpi r28, 0 ; check if cycle counter is 0
		breq one_shift_B ; if 0, enter one_shift_B

		; if not 1, shift r26 to the left 2 times
		lsl r26
		lsl r26

		rjmp check_last_two_bits ; loop over check_last_two_bits again

	add_one_B:
		lsr r23 ; shift r23 to the right by 1 to check the next bit
		ori r26, 1 ; change rightmost bit to 1 in r26
	
		dec r28 ; decrease cycle counter by 1
		cpi r28, 0 ; check if cycle counter is 0
		breq one_shift_B ; if 0, enter one_shift_B

		; if not 0, shift r26 to the left 2 times
		lsl r26
		lsl r26

		rjmp check_last_two_bits ; loop over check_last_two_bits again

	one_shift_B:
		lsl r26 ; shift r26 to the left by 1
		rjmp finish ; enter finish

	finish:
		mov r16, r21 ; copy value in r21 (first 4 bits) to r16
		sts PORTL, r16 ; output the result in PORT L
		clr r16 ; clear r16
		mov r16, r26 ; copy the value in r26 (last 2 bits) to r16
		out PORTB, r16 ; output the result in PORT B
	
		ret ; return nothing


slow_leds:
	mov r16, r17 ; copy value in r17 to r16
    rcall set_leds ; call set_leds
	rcall delay_long ; call delay_long to show slow leds
	
	clr r16 ; clear r16
	rcall set_leds ; call set_leds again
	
	
    ret ; return nothing

fast_leds:
	mov r16, r17 ; copy value in r17 to r16
    rcall set_leds ; call set_leds
	rcall delay_short ; call delay_short to show fast leds
	
	clr r16 ; clear r16
	rcall set_leds ; call set_leds again
	

    ret ; return nothing


leds_with_speed:

	clr r16 ; clear r16
	in YH, SPH ; Load the high byte of the stack pointer into YH
	in YL, SPL ; Load the low byte of the stack pointer into YL

	ldd r16, Y+4 ; Load the value at the address Y+4 into register r16 (to access the stack)

	mov r17, r16 ; copy r16 to r17
	mov r22, r16 ; copy r16 to r22

	ldi r19, 0b11000000 ; mask to check the two topmost bits (bits 7 and 6)
	and r22, r19 ; isolate the topmost two bits
	cpi r22, 0b11000000 ; check if both bits are 1
	breq slow_leds ; if both are 1, enter slow_leds

	rcall fast_leds ; if both bits are 0, call fast_leds

	; For other patterns, do nothing (ignore)

	ret ; return nothing


encode_letter:

	in YH, SPH ; Load the high byte of the stack pointer into YH
	in YL, SPL ; Load the low byte of the stack pointer into YL

	ldd r16, Y+4 ; Load the value at the address Y+4 into register r16 (to access the stack)

	ldi r23, HIGH(PATTERNS<<1) ; Load the high byte of the PATTERNS address (shifted left by 1) into r23
	mov ZH, r23 ; copy r23 to ZH
	clr r23 ; clear r23
	ldi r23, LOW(PATTERNS<<1) ; Load the low byte of the PATTERNS address (shifted left by 1) into r23
	mov ZL, r23 ; copy r23 to ZL

	check_letter:
		ldi r18, 7 ; Access patterns 
		lpm r17, Z+ ; Check next character (. or o)
		cp r16, r17 ; check if r17 is equal to r16
		breq write_binary ; if equal, enter write_binary
		add ZL, r18 ; if not equal, check next character
		clr r18 ; clear r18
		adc ZH, r18 ; add carry to ZH 
		rjmp check_letter ; Loop check_letter again
	
	write_binary:
		lpm r19, Z+ ; Check next character (. or o)
		cpi r19, 'o' ; check if the current character is 'o'
		breq found_o_1 ; if 'o', enter found_o_1
		cpi r19, '.' ; check if the current character is '.'
		breq found_dot_1 ; if '.', enter found_dot_1
		rjmp write_binary ; Loop write_binary again

	found_o_1:
		ldi r20, 0b00100000 ; light up first light
		mov r1, r20 ; copy r20 to r1
		lpm r19, Z+ ; Check next character (. or o)
		cpi r19, 'o'
		breq found_o_2
		cpi r19, '.'
		breq found_dot_2
	found_dot_1:
		ldi r20, 0b00000000 ; dont light up first light
		mov r1, r20 ; copy r20 to r1
		lpm r19, Z+
		cpi r19, 'o'
		breq found_o_2
		cpi r19, '.'
		breq found_dot_2

	; repeat until the entire pattern is traversed through
	found_o_2:
		ldi r20, 0b00010000
		mov r2, r20
		lpm r19, Z+
		cpi r19, 'o'
		breq found_o_3
		cpi r19, '.'
		breq found_dot_3
	found_dot_2:
		ldi r20, 0b00000000
		mov r2, r20
		lpm r19, Z+
		cpi r19, 'o'
		breq found_o_3
		cpi r19, '.'
		breq found_dot_3

	found_o_3:
		ldi r20, 0b00001000
		mov r3, r20
		lpm r19, Z+
		cpi r19, 'o'
		breq found_o_4
		cpi r19, '.'
		breq found_dot_4
	found_dot_3:
		ldi r20, 0b00000000
		mov r3, r20
		lpm r19, Z+
		cpi r19, 'o'
		breq found_o_4
		cpi r19, '.'
		breq found_dot_4

	found_o_4:
		ldi r20, 0b00000100
		mov r4, r20
		lpm r19, Z+
		cpi r19, 'o'
		breq found_o_5
		cpi r19, '.'
		breq found_dot_5
	found_dot_4:
		ldi r20, 0b00000000
		mov r4, r20
		lpm r19, Z+
		cpi r19, 'o'
		breq found_o_5
		cpi r19, '.'
		breq found_dot_5
	 
	found_o_5:
		ldi r20, 0b00000010
		mov r5, r20
		lpm r19, Z+
		cpi r19, 'o'
		breq found_o_6
		cpi r19, '.'
		breq found_dot_6
	found_dot_5:
		ldi r20, 0b00000000
		mov r5, r20
		lpm r19, Z+
		cpi r19, 'o'
		breq found_o_6
		cpi r19, '.'
		breq found_dot_6

	found_o_6:
		ldi r20, 0b00000001
		mov r6, r20
		lpm r19, Z ; check if pattern is fast or slow
		cpi r19, 1 
		breq leds_slow ; if 1, pattern is slow
		cpi r19, 2 
		breq leds_fast ; if 2, pattern is fast

	found_dot_6:
		ldi r20, 0b00000000
		mov r6, r20
		lpm r19, Z
		cpi r19, 1
		breq leds_slow
		cpi r19, 2
		breq leds_fast

	leds_slow:
		ldi r20, 0b11000000 ; initialize for slow leds pattern
		mov r25, r20 ; copy r20 to r25

		eor r1, r2
		eor r1, r3
		eor r1, r4
		eor r1, r5
		eor r1, r6
		eor r25, r1 ; copy translated binary pattern found above to r25

		ret ; return r25

	leds_fast:
		ldi r20, 0b00000000 ; initialize for fastleds pattern
		mov r25, r20 ; copy r20 to r25

		eor r1, r2
		eor r1, r3
		eor r1, r4
		eor r1, r5
		eor r1, r6
		eor r25, r1 ; copy translated binary pattern found above to r25

		ret ; return r25



display_message:
	mov ZH, r25 ; copy r25 to ZH
	mov ZL, r24 ; copy r24 to ZL

	check_empty:
		lpm r15, Z+ ; Check next character (. or o)
		tst r15 ; check if the value in r15 is 0
		brne light ; if not 0, enter light
		ret ; if 0, return nothing

	light:
		mov r9, ZH ; copy the value of ZH in r9
		mov r10, ZL ; copy the value of ZH in r9
		push r15 ; push r15 to the stack
		rcall encode_letter ; call encode_letter
		pop r15 ; pop r15 from the stack
		mov ZH, r9 ; copy r9 to ZH again
		mov ZL, r10 ; copy r10 to ZL again
		push r25 ; push r25 to the stack
		rcall leds_with_speed ; call leds_with_speed
		pop r25 ; pop r25 from the stack
		rcall delay_short ; call delay_short twice
		rcall delay_short
		rjmp check_empty ; Loop check_empty until all the patterns are traversed through






; about one second
delay_long:
	push r16

	ldi r16, 14
delay_long_loop:
	rcall delay
	dec r16
	brne delay_long_loop

	pop r16
	ret


; about 0.25 of a second
delay_short:
	push r16

	ldi r16, 4
delay_short_loop:
	rcall delay
	dec r16
	brne delay_short_loop

	pop r16
	ret

; When wanting about a 1/5th of a second delay, all other
; code must call this function
;
delay:
	rcall delay_busywait
	ret


delay_busywait:
	push r16
	push r17
	push r18

	ldi r16, 0x08
delay_busywait_loop1:
	dec r16
	breq delay_busywait_exit

	ldi r17, 0xff
delay_busywait_loop2:
	dec r17
	breq delay_busywait_loop1

	ldi r18, 0xff
delay_busywait_loop3:
	dec r18
	breq delay_busywait_loop2
	rjmp delay_busywait_loop3

delay_busywait_exit:
	pop r18
	pop r17
	pop r16
	ret


; Some tables
;.cseg
;.org 0x600


PATTERNS:
	; LED pattern shown from left to right: "." means off, "o" means
    ; on, 1 means long/slow, while 2 means short/fast.
	.db "A", "..oo..", 1
	.db "B", ".o..o.", 2
	.db "C", "o.o...", 1
	.db "D", ".....o", 1
	.db "E", "oooooo", 1
	.db "F", ".oooo.", 2
	.db "G", "oo..oo", 2
	.db "H", "..oo..", 2
	.db "I", ".o..o.", 1
	.db "J", ".....o", 2
	.db "K", "....oo", 2
	.db "L", "o.o.o.", 1
	.db "M", "oooooo", 2
	.db "N", "oo....", 1
	.db "O", ".oooo.", 1
	.db "P", "o.oo.o", 1
	.db "Q", "o.oo.o", 2
	.db "R", "oo..oo", 1
	.db "S", "....oo", 1
	.db "T", "..oo..", 1
	.db "U", "o.....", 1
	.db "V", "o.o.o.", 2
	.db "W", "o.o...", 2
	.db "X", "oo....", 2
	.db "Y", "..oo..", 2
	.db "Z", "o.....", 2
	.db "-", "o...oo", 1   

WORD00: .db "HELLOWORLD", 0, 0
WORD01: .db "THE", 0
WORD02: .db "QUICK", 0
WORD03: .db "BROWN", 0
WORD04: .db "FOX", 0
WORD05: .db "JUMPED", 0, 0
WORD06: .db "OVER", 0, 0
WORD07: .db "THE", 0
WORD08: .db "LAZY", 0, 0
WORD09: .db "DOG", 0

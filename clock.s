; to assemble: vasm6502_oldstyle -Fbin -dotdir clock.s -o clock.out

    .org    $6000

; kernel routines
IOSAVE          = $FF4A
IOREST          = $FF3F

; kernal addresses
KYBD            = $C000
KBSTROBE        = $C010
IRQ_VECTOR_L    = $03FE
IRQ_VECTOR_H    = $03FF
INT_ENABLE      = $C05C  ; sets annuciater 2 low

; constants
LEFT_ARROW      = $88
RIGHT_ARROW     = $95
CLOCK_X_OFFSET  = 1
CLOCK_Y_OFFSET  = 8
TICKS_PER_MIN   = 95

; zero page addresses
tmp             = $1D ; 2 bytes
row_ptr         = $1F ; 2 bytes
char_x          = $21 ; 1 byte
char_y          = $22 ; 1 byte
storx           = $23 ; 1 byte
story           = $24 ; 1 byte
ticks           = $25 ; 1 byte
blink           = $26 ; 1 byte
hours           = $27 ; 1 byte
minutes         = $28 ; 1 byte

                .macro add
                clc
                lda \1
                adc #\2
                sta \1
                .endmacro

                lda     #0
                sta     ticks
                sta     blink
                sta     hours
                sta     minutes
                lda     #$aa
                sta     blink

                jsr     clear_screen

                lda     #<int_handler
                sta     IRQ_VECTOR_L
                lda     #>int_handler
                sta     IRQ_VECTOR_H
                cli

                bit     INT_ENABLE
                
                jsr     draw_clock

main_loop:      bit     KYBD                ; wait for a key press to adjust time
                bpl     main_loop
                lda     KYBD
                cmp     #"M" | $80          ; M increments minutes
                bne     .check_h_key
                jsr     inc_minutes
                jmp     .done
.check_h_key:   cmp     #"H" | $80          ; H increments minutes
                bne     .exit
                jsr     inc_hours
.done:          jsr     draw_clock
                bit     KBSTROBE            ; reset keyboard
                jmp     main_loop
.exit:          rts                         ; any other key exits

; draw the current time on the screen
draw_clock:     lda     #CLOCK_X_OFFSET
                sta     char_x
                lda     #CLOCK_Y_OFFSET
                sta     char_y

                lda     hours
                jsr     print_dec
                add     char_x, 5   ; leave space for separator
                lda     minutes
                jsr     print_dec
                rts

;=======================================================================
; this interrupt handler is called every time the pendulum of the
; clock passes in front of the sensor. It does three things:
;  - increment the tick counter until it reaches 95 (the number
;     of times the wall clock ticks per minute)
;  - if the counter reaches 95, it increments the clock to the next
;    minute.
;  - it blinks the separator between the hours and the minutes
;
; At the start it delays for two reasons:
;  - to sync the blinking of the separator with the tick sound
;    of the clock (the clock ticks at the extreme left and right
;    positions of the pendulum, but the sensor is placed in the center)
;  - on rare occasions the sensor sends multiple interrupts where it
;    should send just one. Possibly because the pendulum only causes
;    a very weak response in the sensor, so it may go out and back in
;    reach of the sensor in passing it.
;=======================================================================
int_handler:    jsr     IOSAVE                  ; save the registers
                jsr     delay
                jsr     blink_separator
                inc     ticks
                lda     ticks
                cmp     #TICKS_PER_MIN + 1
                bne     .done
                lda     #0
                sta     ticks
                jsr     inc_time
                jsr     draw_clock
.done:          jsr     IOREST                  ; restore the registers
                rti

;=======================================================================
; delay for the approximate time it takes the pendulum to go from center
; to where the clock ticks so it looks like the separator blinks at the
; same time as the clock ticks
;=======================================================================
delay:          ldx     #0
                ldy     #$80
.loop:          inx
                bne     .loop
                iny
                bne     .loop
                rts

;=======================================================================
; Hide the separator if it's visible or show it if it's not
;=======================================================================
blink_separator:asl     blink
                bcc     .carry_clear
                inc     blink
.carry_clear:   ldy     #(CLOCK_Y_OFFSET+1)*2
                jsr     .blink_pixel
                ldy     #(CLOCK_Y_OFFSET+2)*2
                jsr     .blink_pixel
                ldy     #(CLOCK_Y_OFFSET+4)*2
                jsr     .blink_pixel
                ldy     #(CLOCK_Y_OFFSET+5)*2
.blink_pixel:   lda     screen_rows,y
                sta     row_ptr
                lda     screen_rows+1,y
                sta     row_ptr+1
                ldy     #18 + CLOCK_X_OFFSET
                jsr     pixel
                iny
                jsr     pixel
.done:          rts

;=======================================================================
; This is called once a minute, and increments the minutes, and also
; the hours if the minutes went from 59 to 00
;=======================================================================
inc_time:       jsr     inc_minutes
                bcc     .done
                jsr     inc_hours
.done:          rts

;=======================================================================
; Go to the next minutes. If it rolls over from 00 to 59, this is
; signaled to the calling routine by setting the carry flag
;=======================================================================
inc_minutes:    lda     minutes
                cmp     #$59        ; BCD
                beq     .rollover
                sed
                clc
                lda     #1
                adc     minutes
                sta     minutes
                cld
                clc
                bcc     .done       ; always
.rollover:      lda     #0
                sta     minutes
                sec
.done:          rts

;=======================================================================
; Go to the next hour.
;=======================================================================
inc_hours:      lda     hours
                cmp     #$23        ; BCD
                beq     .rollover
                sed
                clc
                lda     #1
                adc     hours
                sta     hours
                cld
                jmp     .done
.rollover:      lda     #0
                sta     hours
.done:          rts

;=======================================================================
; Print a decimal number to the screen.
;  A: the number (from 0 to 59) to print
; this number is used as an index into a table to holds the two decimal
; digits to print. Example, the number 42 points to the two bytes 4 and 2
;=======================================================================
print_dec:      pha
                lsr
                lsr
                lsr
                lsr
                jsr     draw_number ; print first digit
                add     char_x,8
                pla
                and     #$0f
                jsr     draw_number ; print second digit
                add     char_x,8
                rts

;=======================================================================
; Draw a number. The number to draw is in A.
; x and y position are in char_x and char_y
;=======================================================================
draw_number:    stx     storx
                sty     story

                ; store start address of number bitmap in tmp
                asl
                asl
                asl
                clc
                adc     #<bitmaps
                sta     tmp
                lda     #>bitmaps
                sta     tmp+1
                bcc     .ahead
                inc     tmp+1

.ahead:         lda     char_x          ; modify code further down this routine
                clc
                adc     #8
                sta     .endx+1

                ldx     #0
.row_loop:      ; store start address of screen row in row_ptr
                txa
                clc
                adc     char_y
                asl
                tay
                lda     screen_rows,y
                sta     row_ptr
                lda     screen_rows+1,y
                sta     row_ptr+1

                txa
                tay             ; no txy on original 6502
                lda     (tmp),y     ; load one bitmap byte into A
                
                ldy     char_x
.col_loop:      ; draw one byte (column) of the number
                rol     a
                jsr     pixel
                iny
.endx:          ; label used as address for self modifying code
                cpy     #8          ; modified above
                bne     .col_loop
                inx
                cpx     #7
                bne     .row_loop
                ldx     storx
                ldy     story
                rts

;========================================================
; Draw a pixel on the screen row pointed to by row_ptr
; and the column indexed by y. When carry is set: draw
; a block, when it's clear, draw a space.
;========================================================
pixel:          pha
                lda     #" "        ; inverse space (block)
                bcs     .ahead
                ora     #$80        ; space
.ahead:         sta     (row_ptr),y
                pla
                rts

;========================================================
; Clear the screen by printing spaces in every position
;========================================================
clear_screen:   ldx     #46 ; 23 x 2
.row_loop:      ldy     #39
                lda     screen_rows,x
                sta     tmp
                lda     screen_rows+1,x
                sta     tmp+1
                lda     #(" "|$80)
.col_loop:      sta     (tmp),y
                dey
                bpl     .col_loop
                dex
                dex
                bpl     .row_loop
                rts

; the shape of the numbers
bitmaps:        .byte   %01111111
                .byte   %01110011
                .byte   %01110011
                .byte   %01110011
                .byte   %01110011
                .byte   %01110011
                .byte   %01111111
                .byte   %00000000

                .byte   %00011100
                .byte   %00111100
                .byte   %00111100
                .byte   %00011100
                .byte   %00011100
                .byte   %00011100
                .byte   %01111111
                .byte   %00000000

                .byte   %01111111
                .byte   %00000011
                .byte   %00000011
                .byte   %01111111
                .byte   %01110000
                .byte   %01110000
                .byte   %01111111
                .byte   %00000000

                .byte   %01111111
                .byte   %00000011
                .byte   %00000011
                .byte   %00111111
                .byte   %00000011
                .byte   %00000011
                .byte   %01111111
                .byte   %00000000

                .byte   %01110011
                .byte   %01110011
                .byte   %01110011
                .byte   %01111111
                .byte   %00000011
                .byte   %00000011
                .byte   %00000011
                .byte   %00000000

                .byte   %01111111
                .byte   %01110000
                .byte   %01110000
                .byte   %01111111
                .byte   %00000011
                .byte   %00000011
                .byte   %01111111
                .byte   %00000000

                .byte   %01111111
                .byte   %01110000
                .byte   %01110000
                .byte   %01111111
                .byte   %01110011
                .byte   %01110011
                .byte   %01111111
                .byte   %00000000

                .byte   %01111111
                .byte   %00000011
                .byte   %00000011
                .byte   %00000011
                .byte   %00000011
                .byte   %00000011
                .byte   %00000011
                .byte   %00000000

                .byte   %01111111
                .byte   %01110011
                .byte   %01110011
                .byte   %01111111
                .byte   %01110011
                .byte   %01110011
                .byte   %01111111
                .byte   %00000000

                .byte   %01111111
                .byte   %01110011
                .byte   %01110011
                .byte   %01111111
                .byte   %00000011
                .byte   %00000011
                .byte   %00000011
                .byte   %00000000

; screen memory in the Apple ][ isn't contiguous, so calculating
; the memory location of a character on the screen isn't straight
; forward.
screen_rows:    .word   $0400
                .word   $0480
                .word   $0500
                .word   $0580
                .word   $0600
                .word   $0680
                .word   $0700
                .word   $0780
                .word   $0428
                .word   $04A8
                .word   $0528
                .word   $05A8
                .word   $0628
                .word   $06A8
                .word   $0728
                .word   $07A8
                .word   $0450
                .word   $04D0
                .word   $0550
                .word   $05D0
                .word   $0650
                .word   $06D0
                .word   $0750
                .word   $07D0
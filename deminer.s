.segment "HEADER"
  ; .byte "NES", $1A      ; iNES header identifier
  .byte $4E, $45, $53, $1A
  .byte 2               ; 2x 16KB PRG code
  .byte 1               ; 1x  8KB CHR data
  .byte $01, $00        ; mapper 0, vertical mirroring

.segment "VECTORS"
  ;; When an NMI happens (once per frame if enabled) the label nmi:
  .addr nmi
  ;; When the processor first turns on or is reset, it will jump to the label reset:
  .addr reset
  ;; External interrupt IRQ (unused)
  .addr 0

; "nes" linker config requires a STARTUP section, even if it's empty
.segment "STARTUP"

.segment "ZEROPAGE"
  Cursor_x: .res 1
  Cursor_y: .res 1
  temp: .res 1
  pad: .res 1
  timer: .res 1
  q_head: .res 1
  q_tail: .res 1
  save_a: .res 1
  save_x: .res 1
  save_y: .res 1
  seed: .res 1
.segment "RODATA"
palettes:
   ; Background Palette
   .incbin "palettes.pal"  ; include the palette data from palettes.pal files
.incbin "palettes.pal"  ; include the palette data from palettes.pal files
  ; Sprite Palette
  ;.byte $20,$0f,$10,$16,$20,$30,$10,$00,$20,$0f,$2d,$30,$20,$00,$30,$10
  ;.incbin "palettes.pal" ; include the palette data from palettes.pal files
.segment "CHARS"


  .incbin "chars.chr"   ; include the CHR data from chars.chr file
.segment "BSS"
  .res $0200     ; reserve 512 bytes of zeroed memory     
  q_start:.res 256
  matrix: .res 256 ;; 16x16 grid
.segment "CODE"
;;Custom ABI
.macro CALL_PROC procname, arg1,  arg2, arg3
    .ifnblank arg1
        sta save_a
        lda arg1
    .endif
    .ifnblank arg2
        stx save_x
        ldx arg2
    .endif
    .ifnblank arg3
        sty save_y
        ldy arg3
    .endif
    jsr procname
    ;; restore registers exept for A (Return value)

    .ifnblank arg2
      
        ldx save_x
    .endif
    .ifnblank arg3
        
        ldy save_y
    .endif
   
.endmacro 

.macro adx arg
   
    pha 
    txa 
     clc
    adc #arg
    tax 
    pla
.endmacro
.proc mod2y
    pha 
    tya 
     and #$01
    tya
    pla
    rts
.endproc
;;END ABI


.proc wait_ppu_ready
  bit $2002
  bpl wait_ppu_ready 
  rts
.endproc

.proc load_palette
  lda $2002 ;; reset du latch 
  
;; set PPU address to $3F00
  lda #$3F
  sta $2006 
  lda #$00
  sta $2006
;; load the palette data
  ldx #$00
  @loop:
    lda palettes, x
    sta $2007
    inx
    cpx #$20 ;; 32 bytes in palette
    bne @loop
  rts
.endproc

reset:
sei		; disable IRQs
  cld		; disable decimal mode
  ldx #$40
  stx $4017	; disable APU frame IRQ
  ldx #$ff 	; Set up stack
  txs		;  .
  inx		; now X = 0
  stx $2000	; disable NMI
  stx $2001 	; disable rendering
  stx $4010 	; disable DMC IRQs


    jsr wait_ppu_ready ;; first wait for PPU to be ready

  lda #$00
  clear_memory:
    sta $0000, x
    sta $0100, x
    sta $0200, x
    sta $0300, x
    sta $0400, x
    sta $0500, x
    sta $0600, x
    sta $0700, x
    inx
    bne clear_memory ; clear 256 bytes of memory
;;
  
;;
  jsr wait_ppu_ready ;; second wait for PPU to be ready
  jsr load_palette ;; load the palette
  jsr FillBackground ;; Fill the background with the palette
   jsr GenerateMatrix
 enable_rendering:
  lda #%10000000	; Enable NMI
  sta $2000
  lda #%00011110	; Enable Sprites
  sta $2001
 ;;setup base value
  lda #$A6
  sta seed
  lda #$0b ;; X pos
  sta Cursor_x
  lda #$0c ;; Y pos
  sta Cursor_y
 
forever:
  jmp forever

;Generate Random Matrix
.proc GenerateMatrix
  ldx #$00
@loop:
  jsr randomCoinFlip
  sta matrix, x
  inx
  bne @loop
;;ici x == 0
ldx #$00
NeighborLoop:
  lda matrix, x
  bne mine
  jsr getNeighborMines
  asl a ;; les cases vides sont toute paire
  sta matrix, x
  mine:
  inx
  bne NeighborLoop
  rts
.endproc
.proc getNeighborMines

  ;On place le curseur en haut à gauche
  txa
  clc
  sbc #$11 ;; coin supérieur gauche
  
  tax

  lda #00
.repeat 3
  clc
  .repeat 3
   ldy matrix, x
    jsr mod2y
    sty save_y
    adc save_y
   inx
   .endrepeat
   adx 12

.endrepeat  
  rts
.endproc

 ;;Macros
 .macro drawChar px, py, charIndex
  lda #py
  sta $2004
  lda #charIndex
  sta $2004
  lda #00
  sta $2004
  lda #px
  sta $2004
 .endmacro

.proc DrawFirstEmptySprite
  ldx #$00  ; Start with sprite index 0
  lda  #$00
  sta $2003  ; Set SPR-RAM address to 0
@loop:
  sta $2004
  inx
  cpx #$08
  bne @loop  ; Draw 4 sprites
  ldx #$00
  rts
.endproc

.proc FillBackground

  ; Incrément +1 (bit2=0 de $2000)
  lda $2000
  and #%11111011
  sta $2000

  ; Set PPU address to $2000
  lda $2002     ; Reset the PPU latch
  ldx #$00 ;Low byte of PPU address
  ldy #$20 ;High byte of PPU address
  
  sty $2006
  stx $2006  ; Set PPU address
  
  ldy #$00  ; Start with the first row
  lda #$00
@loop:

  sta $2007  ; Write data to PPU
  inx
  bne @loop  ; Loop until all 256 bytes are written
  iny
  cpy #$04
  bne @loop  
  rts
.endproc

.proc mul8
  sec
  asl a
  asl a
  asl a
  rts
.endproc
.proc drawCursor
  lda Cursor_y
  jsr mul8
  sbc #$02 ;; offset de la ligne
  
  sta $2004
  lda #$b
  sta $2004
  lda #$01 ;; a changé en fonction du temps
  sta $2004
  lda Cursor_x
  jsr mul8
 
  sta $2004
  rts
.endproc

.proc readController
 readController:
    lda #$01
    sta $4016       ; strobe = 1
    lda #$00
    sta $4016       ; strobe = 0

    ldx #$08        ; 8 boutons à lire
    lda #$00
    sta pad         ; effacer l’état précédent

read_loop:
    lda $4016       ; lire un bit
    lsr a           ; mettre dans C
    rol pad         ; décaler dans mémoire $20
    dex
    bne read_loop
    rts

  .endproc
.proc HandleInput
  jsr readController

  ; Droite (bit 0)
  lda pad
  and #%00000001
  beq check_left

  inc Cursor_x

check_left:
  lda pad
  and #%00000010
  beq check_down

  dec Cursor_x

check_down:
  lda pad
  and #%00000100
  beq check_up
  
  inc Cursor_y


check_up:
  lda pad
  and #%00001000
  beq endHandleInput
  
  dec Cursor_y

endHandleInput:
  rts
  .endproc
;; NMI handler
nmi:
  

  ;jsr DrawFirstEmptySprite ;; Draw the first empty sprite
 
  jsr DrawFirstEmptySprite ;; Draw the first empty sprite
  
  inc timer
  lda timer
  cmp #$06 ;; every 6 frames 
  bne not_time 
  lda #$00
  sta timer

  jsr HandleInput
  not_time:
  jsr drawCursor


rti
 
;;Utils Proc 

.proc enqueue 
ldx q_tail
sta q_start, x
inc q_tail
rts
.endproc

.proc dequeue 
ldx q_head
lda q_start, x
inc q_head
rts
.endproc

.proc random
  lda seed
  ; LFSR algorithm
  lsr a
  bcc no_xor
  eor #$B8 ; Polynomial x^8 + x^6 + x^5 + x^4 + 1
  sta seed
  jmp done
no_xor:
  sta seed
done:
  rts
.endproc

.proc randomCoinFlip
  lda seed
  bne nonNull
  lda #$A1 ;; initial seed value if seed was 0

nonNull:
  ; LFSR algorithm
  lsr a
  bcc done
  eor #$B8 ; Polynomial x^8 + x^6 + x^5 + x^4 + 1
done:
  sta seed
  and #$01 ;; keep only the least significant bit
  rts
.endproc
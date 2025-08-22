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
.segment "CODE"

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
;; setup base value
  lda #$0b ;; X pos
  sta Cursor_x
  lda #$0c ;; Y pos
  sta Cursor_y
;;
  jsr wait_ppu_ready ;; second wait for PPU to be ready
  jsr load_palette ;; load the palette
  jsr FillBackground ;; Fill the background with the palette
 enable_rendering:
  lda #%10000000	; Enable NMI
  sta $2000
  lda #%00011110	; Enable Sprites
  sta $2001

forever:
  jmp forever


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
 

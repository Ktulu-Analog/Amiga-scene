; VieuMikro 2006 vieille intro
; logo : 320x119*4 raw norm
; vm2006 : 960*32*4 raw blit
; module : protracker

org  $2f000
load $2f000


>extern "dh1:asm/copper/demo/ANALOGO-bulles",$45000
>extern "dh1:asm/copper/demo/vm2006.blit",$3a000
>EXTERN "dh1:asm/copper/demo/fontes.raw",$3e000
>EXTERN "dh1:soundtracker/modules/mod.sll8remix",$50000

;**************************************************

j:	jsr inits
	jsr clear_c_est_clair
	jsr initlogo
	jsr mt_init
	jsr put
	jsr rol_init
	jsr rolrout
	move.w #$4000,$dff09a
	move.l $6c,oldirq+2
	move.l #newirq,$6c
	move.w #$c000,$dff09a


tika:	cmpi.b #256,$dff006
	bne.s tika	
	jsr moveit
	jsr moveit2
	jsr modmove
	jsr sinmove
	jsr rolrout
	jsr R_Machin1
	jsr R_Machin2
	jsr R_Machin3
	jsr R_Machin4
	jsr thisiszelab
	jsr scrollroutine;	Scrolling...
	jsr mt_music
	jsr spectrum
	jsr movespectrum
	jsr clrvumetre
	jsr vu_metre
	btst #6,$bfe001
	bne.s tika



	move.w #$4000,$dff09a
	move.l oldirq+2,$6c
	move.w #$c000,$dff09a
	jsr mt_end
	jsr the_end
	rts


;*************************************************************************
;* INITS...								 *
;*************************************************************************


newirq:
	movem.l d0-d7/a0-a6,-(sp)
	jsr cls
	jsr print
	jsr fillit
	movem.l (sp)+,d0-d7/a0-a6

oldirq:
	jmp $00000000

inits:	
	bset #1,$bfe001;                Filtre supprime !
	move.l $4,a6
	jsr -132(a6);                   Multi-tache supprime !
	move.l $4,a6
	move.l (a6),a6
	move.l (a6),a6
	move.l a6,gfxbase
	move.l #newcopper,$dff080;      Installe la nouvelle copper-liste
	clr.w $dff088
	move.w #$0020,$dff096
	rts

;*************************************************************************
;* THE END...								 *
;*************************************************************************

the_end:
	move.l gfxbase,a0
	move.l 38(a0),$dff080
	clr.w $dff088
	move.w #$8020,$dff096
	move.l $4,a6
	jsr run
	jsr -138(a6)
	rts



gfxbase:
	dc.l 0

honga:	
	dc.w 0

;************************************************************************
;* Routine de scroll...							*
;************************************************************************

scrollroutine:
	cmp.w #6,honga
	bne.s call_it
	clr.w honga
	jsr nxtcar

call_it:
	jsr scrolly
	rts

scrolly:
	btst #14,$dff002
	bne scrolly
	move.l #$3f008,a1
	sub.l #2,a1
	move.l #$3f008,$dff050
	move.l a1,$dff054
	move.w #12,$dff064
	move.w #12,$dff066
	move.L #$ffffffff,$dff044
	move.w #%1101100111110000,$dff040
	clr.w $dff042
	move.w #15*64+26,$dff058
	add.w #1,honga
	rts

nxtcar:
	moveq #0,d4
	add.l #1,textptr
	cmp.l #endtext,textptr
	bne.s continue_next_character_please
	move.l #text,textptr

continue_next_character_please:
	move.l textptr(pc),a0
	move.b (a0),d0
	cmp.b #32,d0
	beq.L no_letters_so_fuck_off
	lea let(pc),a1

fuckinshit:
	move.b (a1)+,d1
	cmp.b d0,d1
	beq.s character_found
	add.b #1,d4
	cmp.b #100,d4
	bne fuckinshit
	rts

character_found:
	btst #14,$dff002
	bne character_found
	move.l d4,d1
	divu #20,d1
	move.w d1,d2
	swap d1
	and.l #$ffff,d1
	and.l #$ffff,d2
	mulu #16*40,d2
	lsl.w #1,d1
	add.l d1,d2
	add.l fontsadr,d2
	move.l d2,$dff050
	move.l cible,$dff054
	move.l #$ffffffff,$dff044
	clr.w $dff042
	move.w #%0000100111110000,$dff040
	move.w #38,$dff064
	move.w #62,$dff066
	move.w #15*64+1,$dff058

no_letters_so_fuck_off:
	rts

fontsadr:
	dc.l $3e000

cible:
	dc.l $3f036


;************************************************************************
;* Routine ClearScreen...						*
;************************************************************************

clear_c_est_clair:
	move.l cible,a0
	sub.l #36,a0
	move.l #$500,d0

loop_clear:
	clr.b (a0)+	
	dbf d0,loop_clear
	rts


***************************************************************
*     NOW BLACKY IS TYPING, NOTICE THE STYLE DIFFERENCE       *
*   BETWEEEEEN THE FOLLOWING ROUTINES AND THE SCROLL ROUTINE  *
*                       SAUCE STORMY !!!!!!!!                 *
***************************************************************

initlogo:
	lea logo_effet,a0
	move.l #$1907fffe,d0
	move.L #$01800000,d1
	move.l #$01020000,d2
	move.l #$01080078,d3
	move.l #$010a0078,d4
	move.l #118,d5

IL_lp1:
	move.l d0,(a0)+
	move.l d1,(a0)+
	move.l d2,(a0)+
	move.l d3,(a0)+
	move.l d4,(a0)+
	add.l #$01000000,d0
	dbra d5,IL_lp1
	rts

ModMove:
	cmp.l #EndMTab,ModPtr
	bne NEMod
	move.l #ModTable,ModPtr

NEMod:
	move.l #118,d7
	move.l ModPtr,a0
	add.l #2,ModPtr
	lea logo_effet+14,a1

MM_lp1:
	move.w (a0),(a1)
	add.l #4,a1
	move.w (a0)+,(a1)
	add.l #16,a1
	cmp.l #EndMTab,a0
	bne NEMod2
	lea ModTable,a0

NEMod2:
	dbra d7,MM_lp1
	rts

SinMove:
	cmp.l #EndSinTab,SinPtr
	bne NESin
	move.l #SinTable,SinPtr

NESin:
	move.l #118,d7
	move.l SinPtr,a0
	add.l #2,SinPtr
	lea logo_effet+10,a1

MS_lp1:
	move.w (a0)+,(a1)
	add.l #20,a1
	cmp.l #EndSinTab,a0
	bne NESin2
	lea SinTable,a0

NESin2:
	dbra d7,MS_lp1
	rts

moveit:
	cmp.w #2306,sinpt
	bmi pasdepsin
	sub.w #2306,sinpt

pasdepsin:
	lea sinus,a0
	move.w sinpt,d0
	move.w (a0,d0.w),d0
	move.w d0,d1
	lsr.w #3,d0
	and.l #$f,d1
	not.w d1
	and.w #$f,d1
	move.w d1,d2
	lsl.w #4,d1
	or.w d2,d1
	lea dec+2,a0
	move.w d1,(a0)
	lea planes,a0
	add.w #$a014,d0
	move.w d0,6(a0)
	add.w #120,d0
	move.w d0,14(a0)
	add.w #120,d0
	move.w d0,22(a0)
	add.w #120,d0
	move.w d0,30(a0)
	add.W #12,sinpt
	rts

moveit2:
	cmp.w #2306,sinpt2
	bmi pasdepsin2
	sub.w #2306,sinpt2

pasdepsin2:
	lea sinus,a0
	move.w sinpt2,d0
	move.w (a0,d0.w),d0
	move.w d0,d1
	lsr.w #3,d0
	and.l #$f,d1
	not.w d1
	and.w #$f,d1
	move.w d1,d2
	lsl.w #4,d1
	or.w d2,d1
	lea dec2+2,a0
	move.w d1,(a0)
	lea planes2,a0
	add.w #$be14,d0
	move.w d0,6(a0)
	add.w #120,d0
	move.w d0,14(a0)
	add.w #120,d0
	move.w d0,22(a0)
	add.w #120,d0
	move.w d0,30(a0)
	add.W #12,sinpt2
	rts

vu_metre:
	lea logo_effet+6,a0
	add.l #360,a0
	tst.w mt_voice1
	beq nonote1
	move.W #$111,(a0)
	move.W #$333,20(a0)
	move.W #$555,40(a0)
	move.W #$777,60(a0)
	move.W #$999,80(a0)
	move.W #$bbb,100(a0)
	move.w #$ccc,120(a0)
	move.W #$ddd,140(a0)
	move.w #$eee,160(a0)
	move.W #$fff,180(a0)
	move.w #$eee,200(a0)
	move.W #$ddd,220(a0)
	move.w #$ccc,240(a0)
	move.W #$bbb,260(a0)
	move.W #$999,280(a0)
	move.W #$777,300(a0)
	move.W #$555,320(a0)
	move.W #$333,340(a0)
	move.W #$111,360(a0)
	
nonote1:
	add.l #480,a0
	tst.w mt_voice2
	beq nonote2
	move.W #$111,(a0)
	move.W #$333,20(a0)
	move.W #$555,40(a0)
	move.W #$777,60(a0)
	move.W #$999,80(a0)
	move.W #$bbb,100(a0)
	move.w #$ccc,120(a0)
	move.W #$ddd,140(a0)
	move.w #$eee,160(a0)
	move.W #$fff,180(a0)
	move.w #$eee,200(a0)
	move.W #$ddd,220(a0)
	move.w #$ccc,240(a0)
	move.W #$bbb,260(a0)
	move.W #$999,280(a0)
	move.W #$777,300(a0)
	move.W #$555,320(a0)
	move.W #$333,340(a0)
	move.W #$111,360(a0)

nonote2:
	add.l #480,a0
	tst.w mt_voice3
	beq nonote3
	move.W #$111,(a0)
	move.W #$333,20(a0)
	move.W #$555,40(a0)
	move.W #$777,60(a0)
	move.W #$999,80(a0)
	move.W #$bbb,100(a0)
	move.w #$ccc,120(a0)
	move.W #$ddd,140(a0)
	move.w #$eee,160(a0)
	move.W #$fff,180(a0)
	move.w #$eee,200(a0)
	move.W #$ddd,220(a0)
	move.w #$ccc,240(a0)
	move.W #$bbb,260(a0)
	move.W #$999,280(a0)
	move.W #$777,300(a0)
	move.W #$555,320(a0)
	move.W #$333,340(a0)
	move.W #$111,360(a0)

nonote3:
	add.l #480,a0
	tst.w mt_voice4
	beq nonote4
	move.W #$111,(a0)
	move.W #$333,20(a0)
	move.W #$555,40(a0)
	move.W #$777,60(a0)
	move.W #$999,80(a0)
	move.W #$bbb,100(a0)
	move.w #$ccc,120(a0)
	move.W #$ddd,140(a0)
	move.w #$eee,160(a0)
	move.W #$fff,180(a0)
	move.w #$eee,200(a0)
	move.W #$ddd,220(a0)
	move.w #$ccc,240(a0)
	move.W #$bbb,260(a0)
	move.W #$999,280(a0)
	move.W #$777,300(a0)
	move.W #$555,320(a0)
	move.W #$333,340(a0)
	move.W #$111,360(a0)

nonote4:
	rts

clrvumetre:
	lea logo_effet+6,a0
	move.W #118,d0

VClr_LP1:
	tst.w (a0)
	beq NextOne
	sub.w #$111,(a0)

NextOne:
	add.L #20,a0
	dbra d0,VClr_LP1
	rts

put:
	lea splits+2,a0
	lea scolors,a1
	move.L #895,d0

ici_mettre:
	move.w (a1)+,(a0)
	cmp.l #scolorsf,a1
	bne pas_fin_col
	lea scolors,a1

pas_fin_col:
	add.L #4,a0
	dbra d0,ici_mettre
	rts

thisiszelab:
	btst #14,$dff002
	bne thisiszelab
	move.L splits,d0
	lea splits+2,a0
	move.w (a0)+,d6
	move.l a0,$dff050	;BLTAPTL
	sub.l #4,a0
	move.l a0,$dff054	;BLTDPTL
	move.l #2,$dff064	;MODA
	move.l #2,$dff066	;MODD
	clr.w $dff042
	move.l #$-1,$dff044
	move.w #%0000100111110000,$dff040
	move.w #56*64+32,$dff058
	move.l d0,spf-4
	rts

rol_init:
	lea rouleau,a0
	move.l #59,d7
	move.l #$f3e1fffe,d0
	move.l #$01800000,d1

rolLP1:
	move.l d0,(a0)+
	move.l d1,(a0)+
	add.l #$01000000,d0
	dbra d7,rolLP1
	rts

rolrout:
	cmp.l #endrcol,rcolpt
	bne NERcol
	move.l #Rcol,RColPt

NERcol:
	lea rouleau+6,a2
	lea lum,a0
	move.l RColpt,a1
	move.l #59,d7

RRolLP1:
	move.l #0,d3
	move.w (a0)+,d0
	move.w (a1)+,d1
	move.w d1,d2
	and.w #$f,d2
	beq noB
	move.w d0,d3

noB:
	lsl.w #4,d0
	move.w d1,d2
	and.W #$f0,d2
	beq noV
	or.W d0,d3

noV:
	lsl.W #4,d0
	and.W #$f00,d1
	beq noR
	or.w d0,d3

nor:
	move.w d3,(a2)
	add.l #8,a2
	cmp.l #endrcol,a1
	bne NERcol2
	lea Rcol,a1

NERcol2:
	dbra d7,RrolLP1
	add.l #2,rcolpt
	rts

R_Machin1:
	cmp.l #EC_Ma1,C_Ma1ptr
	bne NE_RM1
	move.L #C_Ma1,C_Ma1ptr

NE_RM1:
	move.l #55,d0
	move.l C_Ma1ptr,a0
	lea Machin1+2,a1
	lea Machin12+2,a2

RM1_LP:
	move.w (a0),(a1)
	move.w (a0)+,(a2)
	add.l #4,a1
	add.l #4,a2
	cmp.l #EC_Ma1,a0
	bne NE_RM12
	Lea C_Ma1,a0

NE_RM12:
	dbra d0,RM1_lp
	add.l #2,C_Ma1ptr
	rts

R_Machin2:
	cmp.l #EC_Ma2,C_Ma2ptr
	bne NE_RM2
	move.L #C_Ma2,C_Ma2ptr

NE_RM2:
	move.l #55,d0
	move.l C_Ma2ptr,a0
	lea Machin2+2,a1
	lea Machin22+2,a2

RM2_LP:
	move.w (a0),(a1)
	move.w (a0)+,(a2)
	add.l #4,a1
	add.l #4,a2
	cmp.l #EC_Ma2,a0
	bne NE_RM22
	Lea C_Ma2,a0

NE_RM22:
	dbra d0,RM2_lp
	add.l #2,C_Ma2ptr
	rts

R_Machin3:
	cmp.l #EC_Ma3,C_Ma3ptr
	bne NE_RM3
	move.L #C_Ma3,C_Ma3ptr

NE_RM3:
	move.l #55,d0
	move.l C_Ma3ptr,a0
	lea Machin3+2,a1
	lea Machin4+2,a2

RM3_LP:
	move.w (a0),(a1)
	move.w (a0)+,(a2)
	add.l #4,a1
	add.l #4,a2
	cmp.l #EC_Ma3,a0
	bne NE_RM32
	Lea C_Ma3,a0

NE_RM32:
	dbra d0,RM3_lp
	add.l #2,C_Ma3ptr
	rts

R_Machin4:
	cmp.l #EC_Ma4,C_Ma4ptr
	bne NE_RM4
	move.L #C_Ma4,C_Ma4ptr

NE_RM4:
	move.l #55,d0
	move.l C_Ma4ptr,a0
	lea Machin5+2,a1
	lea Machin6+2,a2

RM4_LP:
	move.w (a0),(a1)
	move.w (a0)+,(a2)
	add.l #4,a1
	add.l #4,a2
	cmp.l #EC_Ma4,a0
	bne NE_RM42
	Lea C_Ma4,a0

NE_RM42:
	dbra d0,RM4_lp
	add.l #2,C_Ma4ptr
	rts

spectrum:
	tst.w mt_voice1
	beq nofrec1
	move.w mt_voice1,d0
	jsr frecsearch

nofrec1:
	tst.w mt_voice2
	beq nofrec2
	move.w mt_voice2,d0
	jsr frecsearch

nofrec2:
	tst.w mt_voice3
	beq nofrec3
	move.w mt_voice3,d0
	jsr frecsearch

nofrec3:
	tst.w mt_voice4
	beq nofrec4
	move.w mt_voice4,d0

frecsearch:
	lea frectab,a0
	moveq #0,d1

Spec_LP1:
	cmp.w (a0,d1.w),d0
	beq inithaut
	add.w #2,d1
	bra Spec_LP1

inithaut:
	lea hauttab,a0
	add.w #2,d1
	move.w #0,(a0,d1.w)

nofrec4:
	rts

movespectrum:
	lea hauttab,a0
	move.l #36,d0

MSP_LP1:
	cmp.W #35,(a0)
	beq NLPT
	add.w #1,(a0)

NLPT:
	add.L #2,a0
	dbra d0,MSP_LP1
	rts
	dc.w 35

hauttab:
	blk.w 40,35

frectab:
	dc.w $358,$328,$2fa,$2d0,$2a6,$280,$25c,$23a,$21a
	dc.w $1fc,$1e0,$1c5,$1ac,$194,$17d,$168,$153,$140
	dc.w $12e,$11d,$10d,$0fe,$0f0,$0e2,$0d6,$0ca,$0be
	dc.w $0b4,$0aa,$0a0,$097,$08f,$087,$07f,$078,$071

C_Ma1:

	dc.w $01f,$02f,$03f
	dc.w $04f,$05f,$06f,$07f,$08f,$09f,$0af,$0bf,$0cf,$0df,$0ef,$0ff
	dc.w $1ff,$2ff,$3ff,$4ff,$5ff,$6ff,$7ff,$8ff,$9ff,$aff,$bff,$cff
	dc.w $dff,$eff,$fff,$ffe,$ffd,$ffc,$ffb,$ffa,$ff9,$ff8,$ff7,$ff6
	dc.w $ff5,$ff4,$ff3,$ff2,$ff1,$ff0,$fe1,$fd2,$fc3,$fb4,$fa5,$f96
	dc.w $f87,$f78,$f69,$f5a,$f4b,$f3c,$f2d,$f1e,$f0f,$f1f,$f2f,$f3f
	dc.w $f4f,$f5f,$f6f,$f7f,$f8f,$f9f,$faf,$fbf,$fcf,$fdf,$fef,$fff
	dc.w $fef,$fdf,$fcf,$fbf,$faf,$f9f,$f8f,$f7f,$f6f,$f5f,$f4f,$f3f
	dc.w $f2f,$f1f,$f0f,$f1e,$f2d,$f3c,$f4b,$f5a,$f69,$f78,$f87,$f96
	dc.w $fa5,$fb4,$fc3,$fd2,$fe1,$ff0,$ff1,$ff2,$ff3,$ff4,$ff5,$ff6
	dc.w $ff7,$ff8,$ff9,$ffa,$ffb,$ffc,$ffd,$ffe,$fff,$eff,$dff,$cff
	dc.w $bff,$aff,$9ff,$8ff,$7ff,$6ff,$5ff,$4ff,$3ff,$2ff,$1ff,$0ff
	dc.w $0ef,$0df,$0cf,$0bf,$0af,$09f,$08f,$07f,$06f,$05f,$04f,$03f
	dc.w $02f,$01f,$00f
	
EC_Ma1:

C_Ma1ptr:
	dc.l C_Ma1

C_Ma2:

	dc.w $01f,$02f,$03f
	dc.w $04f,$05f,$06f,$07f,$08f,$09f,$0af,$0bf,$0cf,$0df,$0ef,$0ff
	dc.w $1ff,$2ff,$3ff,$4ff,$5ff,$6ff,$7ff,$8ff,$9ff,$aff,$bff,$cff
	dc.w $dff,$eff,$fff,$ffe,$ffd,$ffc,$ffb,$ffa,$ff9,$ff8,$ff7,$ff6
	dc.w $ff5,$ff4,$ff3,$ff2,$ff1,$ff0,$fe1,$fd2,$fc3,$fb4,$fa5,$f96
	dc.w $f87,$f78,$f69,$f5a,$f4b,$f3c,$f2d,$f1e,$f0f,$f1f,$f2f,$f3f
	dc.w $f4f,$f5f,$f6f,$f7f,$f8f,$f9f,$faf,$fbf,$fcf,$fdf,$fef,$fff
	dc.w $fef,$fdf,$fcf,$fbf,$faf,$f9f,$f8f,$f7f,$f6f,$f5f,$f4f,$f3f
	dc.w $f2f,$f1f,$f0f,$f1e,$f2d,$f3c,$f4b,$f5a,$f69,$f78,$f87,$f96
	dc.w $fa5,$fb4,$fc3,$fd2,$fe1,$ff0,$ff1,$ff2,$ff3,$ff4,$ff5,$ff6
	dc.w $ff7,$ff8,$ff9,$ffa,$ffb,$ffc,$ffd,$ffe,$fff,$eff,$dff,$cff
	dc.w $bff,$aff,$9ff,$8ff,$7ff,$6ff,$5ff,$4ff,$3ff,$2ff,$1ff,$0ff
	dc.w $0ef,$0df,$0cf,$0bf,$0af,$09f,$08f,$07f,$06f,$05f,$04f,$03f
	dc.w $02f,$01f,$00f

EC_Ma2:

C_Ma2ptr:
	dc.l C_Ma2


C_Ma3:
	dc.w $00f,$00e,$00d,$00c,$00b,$00a,$009,$008,$007,$006,$005,$004
	dc.w $003,$002,$001,$000,$001,$002,$003,$004,$005,$006,$007,$008
	dc.w $009,$00a,$00b,$00c,$00d,$00e

EC_Ma3:

C_Ma3ptr:
	dc.l C_Ma3

C_Ma4:
	dc.w $00f,$00e,$00d,$00c,$00b,$00a,$009,$008,$007,$006,$005,$004
	dc.w $003,$002,$001,$000,$001,$002,$003,$004,$005,$006,$007,$008
	dc.w $009,$00a,$00b,$00c,$00d,$00e

EC_Ma4:

C_Ma4ptr:
	dc.l C_Ma4

lum:
dc.w $0,$1,$1,$1,$1,$1,$2,$2,$2,$3,$3,$3,$4,$4,$5,$5,$6,$6,$7,$7,$8,$9
dc.w $a,$a,$b,$b,$c,$d,$e,$f,$f,$f,$f,$e,$d,$c,$b,$b,$a,$a,$9,$8,$7,$7
dc.w $6,$6,$5,$5,$4,$4,$3,$3,$3,$2,$2,$2,$1,$1,$1,$1,$1,$0


RCol:
	dc.W $f00,$f00,$f00,$f00,$f00,$f00,$f00,$f00,$f00,$f00
	dc.w $fff,$fff,$fff,$fff,$fff,$fff,$fff,$fff,$fff,$fff

Endrcol:

Rcolpt:
	dc.l Rcol

sinpt:
	dc.W 0

sinpt2:
	dc.W 1152

ModPtr:
	dc.l ModTable

SinPtr:
	dc.l SinTable

scolors:

dc.w $001,$012,$123,$234,$345,$456,$567,$678,$789,$89a,$9ab,$abc,$bcd
dc.w $cde,$def
dc.w $efe,$fed,$edc,$dcb,$cba,$ba9,$a98,$987,$876,$765,$654,$543,$432
dc.w $321

scolorsf:

ModTable:
	dc.w $78

EndMTab:


; *************************************
; *  Table pour l'ondulation du logo  *
; *************************************
SinTable:

dc.w $ff,$ff,$ff,$ff,$ee,$ee,$ee,$ee,$dd,$dd,$dd,$dd,$cc,$cc,$cc,$cc
dc.w $dd,$dd,$dd,$dd,$ee,$ee,$ee,$ee,$ff,$ff,$ff,$ff,$ee,$ee,$ee,$ee
dc.w $dd,$dd,$dd,$dd,$cc,$cc,$cc,$cc,$dd,$dd,$dd,$dd,$ee,$ee,$ee,$ee
dc.w $ff,$ff,$ff,$ff,$ee,$ee,$ee,$ee,$dd,$dd,$dd,$dd,$cc,$cc,$cc,$cc
dc.w $dd,$dd,$dd,$dd,$ee,$ee,$ee,$ee,$ff,$ff,$ff,$ff,$ee,$ee,$ee,$ee
dc.w $dd,$dd,$dd,$dd,$cc,$cc,$cc,$cc,$dd,$dd,$dd,$dd,$ee,$ee,$ee,$ee
dc.w $ff,$ff,$ff,$ff,$ee,$ee,$ee,$ee,$dd,$dd,$dd,$dd,$cc,$cc,$cc,$cc
dc.w $dd,$dd,$dd,$dd,$ee,$ee,$ee,$ee,$ff,$ff,$ff,$ff,$ee,$ee,$ee,$ee
dc.w $dd,$dd,$dd,$dd,$cc,$cc,$cc,$cc,$dd,$dd,$dd,$dd,$ee,$ee,$ee,$ee
dc.w $ff,$ff,$ff,$ff,$ee,$ee,$ee,$ee,$dd,$dd,$dd,$dd,$cc,$cc,$cc,$cc
dc.w $dd,$dd,$dd,$dd,$ee,$ee,$ee,$ee,$ff,$ff,$ff,$ff,$ee,$ee,$ee,$ee
dc.w $dd,$dd,$dd,$dd,$cc,$cc,$cc,$cc,$dd,$dd,$dd,$dd,$ee,$ee,$ee,$ee
dc.w $ff,$ff,$ff,$ff,$ee,$ee,$ee,$ee,$dd,$dd,$dd,$dd,$cc,$cc,$cc,$cc
dc.w $dd,$dd,$dd,$dd,$ee,$ee,$ee,$ee,$ff,$ff,$ff,$ff,$ee,$ee,$ee,$ee
dc.w $dd,$dd,$dd,$dd,$cc,$cc,$cc,$cc,$dd,$dd,$dd,$dd,$ee,$ee,$ee,$ee


dc.w $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
dc.w $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
dc.w $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
dc.w $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
dc.w $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
dc.w $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff


dc.w $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ee,$ee,$ee,$ee,$ee,$ee,$ee,$ee
dc.w $dd,$dd,$dd,$dd,$dd,$dd,$dd,$dd,$cc,$cc,$cc,$cc,$cc,$cc,$cc,$cc
dc.w $dd,$dd,$dd,$dd,$dd,$dd,$dd,$dd,$ee,$ee,$ee,$ee,$ee,$ee,$ee,$ee
dc.w $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ee,$ee,$ee,$ee,$ee,$ee,$ee,$ee
dc.w $dd,$dd,$dd,$dd,$dd,$dd,$dd,$dd,$cc,$cc,$cc,$cc,$cc,$cc,$cc,$cc
dc.w $dd,$dd,$dd,$dd,$dd,$dd,$dd,$dd,$ee,$ee,$ee,$ee,$ee,$ee,$ee,$ee
dc.w $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ee,$ee,$ee,$ee,$ee,$ee,$ee,$ee
dc.w $dd,$dd,$dd,$dd,$dd,$dd,$dd,$dd,$cc,$cc,$cc,$cc,$cc,$cc,$cc,$cc
dc.w $dd,$dd,$dd,$dd,$dd,$dd,$dd,$dd,$ee,$ee,$ee,$ee,$ee,$ee,$ee,$ee
dc.w $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ee,$ee,$ee,$ee,$ee,$ee,$ee,$ee
dc.w $dd,$dd,$dd,$dd,$dd,$dd,$dd,$dd,$cc,$cc,$cc,$cc,$cc,$cc,$cc,$cc
dc.w $dd,$dd,$dd,$dd,$dd,$dd,$dd,$dd,$ee,$ee,$ee,$ee,$ee,$ee,$ee,$ee
dc.w $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ee,$ee,$ee,$ee,$ee,$ee,$ee,$ee
dc.w $dd,$dd,$dd,$dd,$dd,$dd,$dd,$dd,$cc,$cc,$cc,$cc,$cc,$cc,$cc,$cc
dc.w $dd,$dd,$dd,$dd,$dd,$dd,$dd,$dd,$ee,$ee,$ee,$ee,$ee,$ee,$ee,$ee
dc.w $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ee,$ee,$ee,$ee,$ee,$ee,$ee,$ee
dc.w $dd,$dd,$dd,$dd,$dd,$dd,$dd,$dd,$cc,$cc,$cc,$cc,$cc,$cc,$cc,$cc
dc.w $dd,$dd,$dd,$dd,$dd,$dd,$dd,$dd,$ee,$ee,$ee,$ee,$ee,$ee,$ee,$ee
dc.w $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ee,$ee,$ee,$ee,$ee,$ee,$ee,$ee
dc.w $dd,$dd,$dd,$dd,$dd,$dd,$dd,$dd,$cc,$cc,$cc,$cc,$cc,$cc,$cc,$cc
dc.w $dd,$dd,$dd,$dd,$dd,$dd,$dd,$dd,$ee,$ee,$ee,$ee,$ee,$ee,$ee,$ee
dc.w $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ee,$ee,$ee,$ee,$ee,$ee,$ee,$ee
dc.w $dd,$dd,$dd,$dd,$dd,$dd,$dd,$dd,$cc,$cc,$cc,$cc,$cc,$cc,$cc,$cc
dc.w $dd,$dd,$dd,$dd,$dd,$dd,$dd,$dd,$ee,$ee,$ee,$ee,$ee,$ee,$ee,$ee


dc.w $ff,$ff,$ff,$ff,$00,$00,$00,$00,$ff,$ff,$ff,$ff,$00,$00,$00,$00
dc.w $ff,$ff,$ff,$ff,$00,$00,$00,$00,$ff,$ff,$ff,$ff,$00,$00,$00,$00
dc.w $ff,$ff,$ff,$ff,$00,$00,$00,$00,$ff,$ff,$ff,$ff,$00,$00,$00,$00
dc.w $ff,$ff,$ff,$ff,$00,$00,$00,$00,$ff,$ff,$ff,$ff,$00,$00,$00,$00
dc.w $ff,$ff,$ff,$ff,$00,$00,$00,$00,$ff,$ff,$ff,$ff,$00,$00,$00,$00
dc.w $ff,$ff,$ff,$ff,$00,$00,$00,$00,$ff,$ff,$ff,$ff,$00,$00,$00,$00
dc.w $ff,$ff,$ff,$ff,$00,$00,$00,$00,$ff,$ff,$ff,$ff,$00,$00,$00,$00
dc.w $ff,$ff,$ff,$ff,$00,$00,$00,$00,$ff,$ff,$ff,$ff,$00,$00,$00,$00
dc.w $ff,$ff,$ff,$ff,$00,$00,$00,$00,$ff,$ff,$ff,$ff,$00,$00,$00,$00
dc.w $ff,$ff,$ff,$ff,$00,$00,$00,$00,$ff,$ff,$ff,$ff,$00,$00,$00,$00
dc.w $ff,$ff,$ff,$ff,$00,$00,$00,$00,$ff,$ff,$ff,$ff,$00,$00,$00,$00
dc.w $ff,$ff,$ff,$ff,$00,$00,$00,$00,$ff,$ff,$ff,$ff,$00,$00,$00,$00
dc.w $ff,$ff,$ff,$ff,$00,$00,$00,$00,$ff,$ff,$ff,$ff,$00,$00,$00,$00
dc.w $ff,$ff,$ff,$ff,$00,$00,$00,$00,$ff,$ff,$ff,$ff,$00,$00,$00,$00
dc.w $ff,$ff,$ff,$ff,$00,$00,$00,$00,$ff,$ff,$ff,$ff,$00,$00,$00,$00
dc.w $ff,$ff,$ff,$ff,$00,$00,$00,$00,$ff,$ff,$ff,$ff,$00,$00,$00,$00

dc.w $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
dc.w $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
dc.w $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
dc.w $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
dc.w $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
dc.w $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
dc.w $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
dc.w $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff


dc.w $ff,$ff,$ff,$ff,$ee,$ee,$ee,$ee,$dd,$dd,$dd,$dd,$cc,$cc,$cc,$cc
dc.w $dd,$dd,$dd,$dd,$ee,$ee,$ee,$ee,$ff,$ff,$ff,$ff,$ee,$ee,$ee,$ee
dc.w $dd,$dd,$dd,$dd,$cc,$cc,$cc,$cc,$dd,$dd,$dd,$dd,$ee,$ee,$ee,$ee
dc.w $ff,$ff,$ff,$ff,$ee,$ee,$ee,$ee,$dd,$dd,$dd,$dd,$cc,$cc,$cc,$cc
dc.w $dd,$dd,$dd,$dd,$ee,$ee,$ee,$ee,$ff,$ff,$ff,$ff,$ee,$ee,$ee,$ee
dc.w $dd,$dd,$dd,$dd,$cc,$cc,$cc,$cc,$dd,$dd,$dd,$dd,$ee,$ee,$ee,$ee
dc.w $ff,$ff,$ff,$ff,$ee,$ee,$ee,$ee,$dd,$dd,$dd,$dd,$cc,$cc,$cc,$cc
dc.w $dd,$dd,$dd,$dd,$ee,$ee,$ee,$ee,$ff,$ff,$ff,$ff,$ee,$ee,$ee,$ee
dc.w $dd,$dd,$dd,$dd,$cc,$cc,$cc,$cc,$dd,$dd,$dd,$dd,$ee,$ee,$ee,$ee
dc.w $ff,$ff,$ff,$ff,$ee,$ee,$ee,$ee,$dd,$dd,$dd,$dd,$cc,$cc,$cc,$cc
dc.w $dd,$dd,$dd,$dd,$ee,$ee,$ee,$ee,$ff,$ff,$ff,$ff,$ee,$ee,$ee,$ee
dc.w $dd,$dd,$dd,$dd,$cc,$cc,$cc,$cc,$dd,$dd,$dd,$dd,$ee,$ee,$ee,$ee
dc.w $ff,$ff,$ff,$ff,$ee,$ee,$ee,$ee,$dd,$dd,$dd,$dd,$cc,$cc,$cc,$cc
dc.w $dd,$dd,$dd,$dd,$ee,$ee,$ee,$ee,$ff,$ff,$ff,$ff,$ee,$ee,$ee,$ee
dc.w $dd,$dd,$dd,$dd,$cc,$cc,$cc,$cc,$dd,$dd,$dd,$dd,$ee,$ee,$ee,$ee
dc.w $ff,$ff,$ff,$ff,$ee,$ee,$ee,$ee,$dd,$dd,$dd,$dd,$cc,$cc,$cc,$cc
dc.w $dd,$dd,$dd,$dd,$ee,$ee,$ee,$ee,$ff,$ff,$ff,$ff,$ee,$ee,$ee,$ee
dc.w $dd,$dd,$dd,$dd,$cc,$cc,$cc,$cc,$dd,$dd,$dd,$dd,$ee,$ee,$ee,$ee
dc.w $ff,$ff,$ff,$ff,$ee,$ee,$ee,$ee,$dd,$dd,$dd,$dd,$cc,$cc,$cc,$cc
dc.w $dd,$dd,$dd,$dd,$ee,$ee,$ee,$ee,$ff,$ff,$ff,$ff,$ee,$ee,$ee,$ee
dc.w $dd,$dd,$dd,$dd,$cc,$cc,$cc,$cc,$dd,$dd,$dd,$dd,$ee,$ee,$ee,$ee
dc.w $ff,$ff,$ff,$ff,$ee,$ee,$ee,$ee,$dd,$dd,$dd,$dd,$cc,$cc,$cc,$cc
dc.w $dd,$dd,$dd,$dd,$ee,$ee,$ee,$ee,$ff,$ff,$ff,$ff,$ee,$ee,$ee,$ee
dc.w $dd,$dd,$dd,$dd,$cc,$cc,$cc,$cc,$dd,$dd,$dd,$dd,$ee,$ee,$ee,$ee

dc.w $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
dc.w $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
dc.w $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
dc.w $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
dc.w $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
dc.w $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
dc.w $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
dc.w $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff

dc.w $ff,$ee,$dd,$cc,$dd,$ee,$ff,$ee,$dd,$cc,$dd,$ee,$ff,$ee,$dd,$cc
dc.w $dd,$ee,$ff,$ee,$dd,$cc,$dd,$ee,$ff,$ee,$dd,$cc,$dd,$ee,$ff,$ee
dc.w $dd,$cc,$dd,$ee,$ff,$ee,$dd,$cc,$dd,$ee,$ff,$ee,$dd,$cc,$dd,$ee
dc.w $ff,$ee,$dd,$cc,$dd,$ee


dc.w $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
dc.w $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
dc.w $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
dc.w $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
dc.w $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
dc.w $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
dc.w $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
dc.w $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
dc.w $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
dc.w $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff



dc.w $ff,$ee,$dd,$cc,$dd,$ee,$ff,$ee,$dd,$cc,$dd,$ee,$ff,$ee,$dd,$cc
dc.w $dd,$ee,$ff,$ee,$dd,$cc,$dd,$ee,$ff,$ee,$dd,$cc,$dd,$ee,$ff,$ee
dc.w $dd,$cc,$dd,$ee,$ff,$ee,$dd,$cc,$dd,$ee,$ff,$ee,$dd,$cc,$dd,$ee
dc.w $ff,$ee,$dd,$cc,$dd,$ee

dc.w $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
dc.w $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
dc.w $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
dc.w $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
dc.w $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
dc.w $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff


EndSinTab:


;**************************
;*  Nouvelle Copper-List  *
;**************************

newcopper:
	dc.w $008e,$2c81,$0090,$74f1;	DIWSTRT & DIWSTOP
	dc.w $092,$0036,$094,$00ce;	DDFSTRT & DDFSTOP
	dc.w $102,$0000;		BPLCON1 (pas de decalage)
	dc.w $104,$1000;                Sprites derriere l'ecran...
	dc.w $100,$4200;                4 bitplanes
	dc.w $108,$0078,$10a,$0078;     Modulo de 80 (4 bitplanes)

	dc.w $00e0,$0004,$00e2,$5000;     Pointeur bitplane 1...
	dc.w $00e4,$0004,$00e6,$5028;     Pointeur bitplane 2...
	dc.w $00e8,$0004,$00ea,$5050;     Pointeur bitplane 3...
	dc.w $00ec,$0004,$00ee,$5078;	  Pointeur bitplane 4...

			; couleurs du logo


	dc.w	$0180,$0000 ,$0182,$09cf ,$0184,$0dff ,$0186,$0cef
	dc.w	$0188,$0bdf ,$018a,$0fff ,$018c,$07bf ,$018e,$06ae
	dc.w	$0190,$059d ,$0192,$048c ,$0194,$037b ,$0196,$026a
	dc.w	$0198,$0159 ,$019a,$0048 ,$019c,$0037 ,$019e,$0025



logo_effet:
	blk.w 10*119,0	;debut a 19
	dc.w $100,0,$102,0		
	dc.w $8f07,$fffe

machin1:
	blk.w 56*2,$180		; des machins de couleurs
	dc.W $9007,$fffe
machin12:
	blk.w 56*2,$180		; des machins de couleurs
	dc.w $180,$0

dec:	
	dc.W $102,0
	dc.w $108,$01b2,$10a,$01b2
	dc.w $0092,$0028,$0094,$00d8
	dc.w $92e1,$fffe,$180,$000

planes:
	dc.w $00e0,$0003,$00e2,$a028;     Pointeur bitplane 1...
	dc.w $00e4,$0003,$00e6,$a0a0;     Pointeur bitplane 2...
	dc.w $00e8,$0003,$00ea,$a118;     Pointeur bitplane 3...
	dc.w $00ec,$0003,$00ee,$a190;	  Pointeur bitplane 4...
	dc.w $100,$4200
	dc.w $9707,$fffe,$180,$000
	dc.w $9807,$fffe,$180,$000
	dc.w $9907,$fffe,$180,$000
	dc.w $9a07,$fffe,$180,$000
	dc.w $9b07,$fffe,$180,$000
	dc.w $9c07,$fffe,$180,$000
	dc.w $9d07,$fffe,$180,$000

	dc.w $a107,$fffe,$100,0

dec2:
	dc.W $102,0
	dc.w $a207,$fffe,$180,$0

planes2:
	dc.w $00e0,$0003,$00e2,$be28;     Pointeur bitplane 1...
	dc.w $00e4,$0003,$00e6,$a0a0;     Pointeur bitplane 2...
	dc.w $00e8,$0003,$00ea,$a118;     Pointeur bitplane 3...
	dc.w $00ec,$0003,$00ee,$a190;	  Pointeur bitplane 4...
	dc.w $100,$4200
	dc.w $a907,$fffe,$180,$000
	dc.w $aa07,$fffe,$180,$000
	dc.w $ab07,$fffe,$180,$000
	dc.w $ac07,$fffe,$180,$000	
	dc.w $ad07,$fffe,$180,$000
	dc.w $ae07,$fffe,$180,$000
	dc.w $b207,$fffe,$100,0,$102,0
	dc.w $b307,$fffe

machin2:
	blk.w 56*2,$180			; encore des machins de couleurs
	dc.w $b407,$fffe

machin22:
	blk.w 56*2,$180			; encore des machins de couleurs
	dc.w $180,0


	dc.W $b707,$fffe,$108,0,$182,$fff,$180,$0
	dc.w $92,$38,$94,$d0,$100,$1200,$102,$55
	dc.w $00e0,$0003,$00e2,$5000
	dc.w $b807,$fffe,$180,$000,$182,$000
	dc.w $b907,$fffe,$180,$000,$182,$000
	dc.w $ba07,$fffe,$180,$001,$182,$000
	dc.w $bb07,$fffe,$180,$002,$182,$001
	dc.w $bc07,$fffe,$180,$003,$182,$002
	dc.w $bd07,$fffe,$180,$004,$182,$003
	dc.w $be07,$fffe,$180,$005,$182,$004
	dc.w $bf07,$fffe,$180,$006,$182,$005
	dc.w $c007,$fffe,$180,$007,$182,$006
	dc.w $c107,$fffe,$180,$008,$182,$007
	dc.w $c207,$fffe,$180,$009,$182,$008
	dc.w $c307,$fffe,$180,$00a,$182,$009
	dc.w $c407,$fffe,$180,$00b,$182,$00a
	dc.w $c507,$fffe,$180,$00c,$182,$00b
	dc.w $c607,$fffe,$180,$00d,$182,$00c
	dc.w $c707,$fffe,$180,$00e,$182,$00d
	dc.w $c807,$fffe,$180,$00e,$182,$00e
	dc.w $c907,$fffe,$180,$00f,$182,$00f
	dc.w $ca07,$fffe,$180,$00f,$182,$00f
	dc.w $cb07,$fffe,$180,$00f,$182,$00e
	dc.w $cc07,$fffe,$180,$00e,$182,$00d
	dc.w $cd07,$fffe,$180,$00e,$182,$00c
	dc.w $ce07,$fffe,$180,$00d,$182,$00b
	dc.w $cf07,$fffe,$180,$00c,$182,$00a
	dc.w $d007,$fffe,$180,$00b,$182,$009
	dc.w $d107,$fffe,$180,$00a,$182,$008
	dc.w $d207,$fffe,$180,$009,$182,$007
	dc.w $d307,$fffe,$180,$008,$182,$006
	dc.w $d407,$fffe,$180,$007,$182,$005
	dc.w $d507,$fffe,$180,$006,$182,$004
	dc.w $d607,$fffe,$180,$005,$182,$003
	dc.w $d707,$fffe,$180,$004,$182,$002
	dc.w $d807,$fffe,$180,$003,$182,$001
	dc.w $d907,$fffe,$180,$002,$182,$000
	dc.w $da07,$fffe,$180,$001,$182,$000
	dc.w $db07,$fffe,$180,$000,$182,$000
	dc.w $dc07,$fffe,$180,$000,$182,0
	dc.w $dd07,$fffe

machin3:
	blk.W 56*2,$180
	dc.w $de07,$fffe

machin4:
	blk.W 56*2,$180
	dc.w $180,0
	dc.w $e007,$fffe,$180,$000
	dc.w $0092,$0028,$0094,$00d8
	dc.w $100,$1200
	dc.w $108,18,$10a,18
	dc.w $00e0,$0003,$00e2,$f008
	dc.w $e007,$fffe

splits:
	blk.w 1792,$182
spf:
	dc.W $180,$0
	dc.w $f107,$fffe

machin5:
	blk.w 56*2,$0180
	dc.w $f207,$fffe

machin6:
	blk.W 56*2,$180
	dc.w $180,0,$100,0

rouleau:

	blk.w 60*4,0
	dc.w $ffff,$fffe		;Fin de la copper-liste...

ancienneCopper:
	dc.l 0


;***** TEXTE DU SCROLL...

text:

	dc.b	" *** YEP  *** VIEUMIKRO 2006 OLD SCHOOL INTRO"
	dc.b    " ROULAIZE              "
	dc.b	"CODE : KTULU *** GFX : BAOBAB *** MUSIC : SLL *** "
	dc.b	"                      "


endtext:
	even
textptr:
	dc.l text

let:
	dc.b " ! %&* '() +,-. 0123456789:;/= ? ABCDEFGHIJKLMNOPQRSTUVWXYZ"
	even

sinus:
	dc.w	$00A0,$00A0,$00A1,$00A2,$00A3,$00A4,$00A5,$00A6
	dc.w	$00A6,$00A7,$00A8,$00A9,$00AA,$00AB,$00AC,$00AD
	dc.w	$00AD,$00AE,$00AF,$00B0,$00B1,$00B2,$00B3,$00B4
	dc.w	$00B4,$00B5,$00B6,$00B7,$00B8,$00B9,$00BA,$00BA
	dc.w	$00BB,$00BC,$00BD,$00BE,$00BF,$00C0,$00C0,$00C1
	dc.w	$00C2,$00C3,$00C4,$00C5,$00C6,$00C6,$00C7,$00C8
	dc.w	$00C9,$00CA,$00CB,$00CB,$00CC,$00CD,$00CE,$00CF
	dc.w	$00D0,$00D0,$00D1,$00D2,$00D3,$00D4,$00D5,$00D5
	dc.w	$00D6,$00D7,$00D8,$00D9,$00DA,$00DA,$00DB,$00DC
	dc.w	$00DD,$00DE,$00DE,$00DF,$00E0,$00E1,$00E2,$00E2
	dc.w	$00E3,$00E4,$00E5,$00E6,$00E6,$00E7,$00E8,$00E9
	dc.w	$00E9,$00EA,$00EB,$00EC,$00EC,$00ED,$00EE,$00EF
	dc.w	$00F0,$00F0,$00F1,$00F2,$00F3,$00F3,$00F4,$00F5
	dc.w	$00F6,$00F6,$00F7,$00F8,$00F8,$00F9,$00FA,$00FB
	dc.w	$00FB,$00FC,$00FD,$00FD,$00FE,$00FF,$0100,$0100
	dc.w	$0101,$0102,$0102,$0103,$0104,$0104,$0105,$0106
	dc.w	$0106,$0107,$0108,$0108,$0109,$010A,$010A,$010B
	dc.w	$010C,$010C,$010D,$010E,$010E,$010F,$010F,$0110
	dc.w	$0111,$0111,$0112,$0113,$0113,$0114,$0114,$0115
	dc.w	$0116,$0116,$0117,$0117,$0118,$0118,$0119,$011A
	dc.w	$011A,$011B,$011B,$011C,$011C,$011D,$011D,$011E
	dc.w	$011E,$011F,$0120,$0120,$0121,$0121,$0122,$0122
	dc.w	$0123,$0123,$0124,$0124,$0125,$0125,$0126,$0126
	dc.w	$0126,$0127,$0127,$0128,$0128,$0129,$0129,$012A
	dc.w	$012A,$012B,$012B,$012B,$012C,$012C,$012D,$012D
	dc.w	$012D,$012E,$012E,$012F,$012F,$012F,$0130,$0130
	dc.w	$0131,$0131,$0131,$0132,$0132,$0132,$0133,$0133
	dc.w	$0133,$0134,$0134,$0134,$0135,$0135,$0135,$0136
	dc.w	$0136,$0136,$0136,$0137,$0137,$0137,$0138,$0138
	dc.w	$0138,$0138,$0139,$0139,$0139,$0139,$013A,$013A
	dc.w	$013A,$013A,$013B,$013B,$013B,$013B,$013B,$013C
	dc.w	$013C,$013C,$013C,$013C,$013C,$013D,$013D,$013D
	dc.w	$013D,$013D,$013D,$013E,$013E,$013E,$013E,$013E
	dc.w	$013E,$013E,$013E,$013E,$013F,$013F,$013F,$013F
	dc.w	$013F,$013F,$013F,$013F,$013F,$013F,$013F,$013F
	dc.w	$013F,$013F,$013F,$013F,$013F,$013F,$013F,$013F
	dc.w	$013F,$013F,$013F,$013F,$013F,$013F,$013F,$013F
	dc.w	$013F,$013F,$013F,$013F,$013F,$013F,$013F,$013F
	dc.w	$013F,$013F,$013F,$013F,$013F,$013E,$013E,$013E
	dc.w	$013E,$013E,$013E,$013E,$013E,$013D,$013D,$013D
	dc.w	$013D,$013D,$013D,$013D,$013C,$013C,$013C,$013C
	dc.w	$013C,$013B,$013B,$013B,$013B,$013B,$013A,$013A
	dc.w	$013A,$013A,$013A,$0139,$0139,$0139,$0139,$0138
	dc.w	$0138,$0138,$0138,$0137,$0137,$0137,$0136,$0136
	dc.w	$0136,$0135,$0135,$0135,$0135,$0134,$0134,$0134
	dc.w	$0133,$0133,$0133,$0132,$0132,$0132,$0131,$0131
	dc.w	$0130,$0130,$0130,$012F,$012F,$012F,$012E,$012E
	dc.w	$012D,$012D,$012D,$012C,$012C,$012B,$012B,$012A
	dc.w	$012A,$012A,$0129,$0129,$0128,$0128,$0127,$0127
	dc.w	$0126,$0126,$0125,$0125,$0124,$0124,$0123,$0123
	dc.w	$0122,$0122,$0121,$0121,$0120,$0120,$011F,$011F
	dc.w	$011E,$011E,$011D,$011D,$011C,$011C,$011B,$011B
	dc.w	$011A,$0119,$0119,$0118,$0118,$0117,$0117,$0116
	dc.w	$0115,$0115,$0114,$0114,$0113,$0112,$0112,$0111
	dc.w	$0111,$0110,$010F,$010F,$010E,$010D,$010D,$010C
	dc.w	$010B,$010B,$010A,$010A,$0109,$0108,$0108,$0107
	dc.w	$0106,$0106,$0105,$0104,$0103,$0103,$0102,$0101
	dc.w	$0101,$0100,$00FF,$00FF,$00FE,$00FD,$00FD,$00FC
	dc.w	$00FB,$00FA,$00FA,$00F9,$00F8,$00F7,$00F7,$00F6
	dc.w	$00F5,$00F5,$00F4,$00F3,$00F2,$00F2,$00F1,$00F0
	dc.w	$00EF,$00EF,$00EE,$00ED,$00EC,$00EC,$00EB,$00EA
	dc.w	$00E9,$00E8,$00E8,$00E7,$00E6,$00E5,$00E5,$00E4
	dc.w	$00E3,$00E2,$00E1,$00E1,$00E0,$00DF,$00DE,$00DD
	dc.w	$00DD,$00DC,$00DB,$00DA,$00D9,$00D8,$00D8,$00D7
	dc.w	$00D6,$00D5,$00D4,$00D4,$00D3,$00D2,$00D1,$00D0
	dc.w	$00CF,$00CF,$00CE,$00CD,$00CC,$00CB,$00CA,$00CA
	dc.w	$00C9,$00C8,$00C7,$00C6,$00C5,$00C4,$00C4,$00C3
	dc.w	$00C2,$00C1,$00C0,$00BF,$00BE,$00BE,$00BD,$00BC
	dc.w	$00BB,$00BA,$00B9,$00B8,$00B8,$00B7,$00B6,$00B5
	dc.w	$00B4,$00B3,$00B2,$00B2,$00B1,$00B0,$00AF,$00AE
	dc.w	$00AD,$00AC,$00AB,$00AB,$00AA,$00A9,$00A8,$00A7
	dc.w	$00A6,$00A5,$00A4,$00A4,$00A3,$00A2,$00A1,$00A0
	dc.w	$00A0,$009F,$009F,$009E,$009D,$009C,$009B,$009A
	dc.w	$0099,$0098,$0098,$0097,$0096,$0095,$0094,$0093
	dc.w	$0092,$0091,$0091,$0090,$008F,$008E,$008D,$008C
	dc.w	$008B,$008A,$008A,$0089,$0088,$0087,$0086,$0085
	dc.w	$0084,$0084,$0083,$0082,$0081,$0080,$007F,$007E
	dc.w	$007E,$007D,$007C,$007B,$007A,$0079,$0079,$0078
	dc.w	$0077,$0076,$0075,$0074,$0073,$0073,$0072,$0071
	dc.w	$0070,$006F,$006E,$006E,$006D,$006C,$006B,$006A
	dc.w	$006A,$0069,$0068,$0067,$0066,$0065,$0065,$0064
	dc.w	$0063,$0062,$0061,$0061,$0060,$005F,$005E,$005D
	dc.w	$005D,$005C,$005B,$005A,$0059,$0059,$0058,$0057
	dc.w	$0056,$0056,$0055,$0054,$0053,$0053,$0052,$0051
	dc.w	$0050,$004F,$004F,$004E,$004D,$004D,$004C,$004B
	dc.w	$004A,$004A,$0049,$0048,$0047,$0047,$0046,$0045
	dc.w	$0044,$0044,$0043,$0042,$0042,$0041,$0040,$0040
	dc.w	$003F,$003E,$003D,$003D,$003C,$003B,$003B,$003A
	dc.w	$0039,$0039,$0038,$0037,$0037,$0036,$0035,$0035
	dc.w	$0034,$0034,$0033,$0032,$0032,$0031,$0030,$0030
	dc.w	$002F,$002F,$002E,$002D,$002D,$002C,$002C,$002B
	dc.w	$002A,$002A,$0029,$0029,$0028,$0027,$0027,$0026
	dc.w	$0026,$0025,$0025,$0024,$0024,$0023,$0022,$0022
	dc.w	$0021,$0021,$0020,$0020,$001F,$001F,$001E,$001E
	dc.w	$001D,$001D,$001C,$001C,$001B,$001B,$001A,$001A
	dc.w	$0019,$0019,$0018,$0018,$0018,$0017,$0017,$0016
	dc.w	$0016,$0015,$0015,$0014,$0014,$0014,$0013,$0013
	dc.w	$0012,$0012,$0012,$0011,$0011,$0010,$0010,$0010
	dc.w	$000F,$000F,$000F,$000E,$000E,$000E,$000D,$000D
	dc.w	$000D,$000C,$000C,$000C,$000B,$000B,$000B,$000A
	dc.w	$000A,$000A,$0009,$0009,$0009,$0009,$0008,$0008
	dc.w	$0008,$0008,$0007,$0007,$0007,$0007,$0006,$0006
	dc.w	$0006,$0006,$0005,$0005,$0005,$0005,$0005,$0004
	dc.w	$0004,$0004,$0004,$0004,$0004,$0003,$0003,$0003
	dc.w	$0003,$0003,$0003,$0002,$0002,$0002,$0002,$0002
	dc.w	$0002,$0002,$0002,$0002,$0001,$0001,$0001,$0001
	dc.w	$0001,$0001,$0001,$0001,$0001,$0001,$0001,$0001
	dc.w	$0001,$0001,$0001,$0001,$0001,$0001,$0001,$0001
	dc.w	$0001,$0001,$0001,$0001,$0001,$0001,$0001,$0001
	dc.w	$0001,$0001,$0001,$0001,$0001,$0001,$0001,$0001
	dc.w	$0001,$0001,$0001,$0001,$0001,$0002,$0002,$0002
	dc.w	$0002,$0002,$0002,$0002,$0002,$0003,$0003,$0003
	dc.w	$0003,$0003,$0003,$0003,$0004,$0004,$0004,$0004
	dc.w	$0004,$0005,$0005,$0005,$0005,$0005,$0006,$0006
	dc.w	$0006,$0006,$0007,$0007,$0007,$0007,$0008,$0008
	dc.w	$0008,$0008,$0009,$0009,$0009,$0009,$000A,$000A
	dc.w	$000A,$000B,$000B,$000B,$000C,$000C,$000C,$000D
	dc.w	$000D,$000D,$000E,$000E,$000E,$000F,$000F,$000F
	dc.w	$0010,$0010,$0010,$0011,$0011,$0012,$0012,$0012
	dc.w	$0013,$0013,$0014,$0014,$0014,$0015,$0015,$0016
	dc.w	$0016,$0017,$0017,$0017,$0018,$0018,$0019,$0019
	dc.w	$001A,$001A,$001B,$001B,$001C,$001C,$001D,$001D
	dc.w	$001E,$001E,$001F,$001F,$0020,$0020,$0021,$0021
	dc.w	$0022,$0022,$0023,$0023,$0024,$0025,$0025,$0026
	dc.w	$0026,$0027,$0027,$0028,$0028,$0029,$002A,$002A
	dc.w	$002B,$002B,$002C,$002D,$002D,$002E,$002E,$002F
	dc.w	$0030,$0030,$0031,$0032,$0032,$0033,$0033,$0034
	dc.w	$0035,$0035,$0036,$0037,$0037,$0038,$0039,$0039
	dc.w	$003A,$003B,$003B,$003C,$003D,$003D,$003E,$003F
	dc.w	$003F,$0040,$0041,$0042,$0042,$0043,$0044,$0044
	dc.w	$0045,$0046,$0047,$0047,$0048,$0049,$0049,$004A
	dc.w	$004B,$004C,$004C,$004D,$004E,$004F,$004F,$0050
	dc.w	$0051,$0052,$0052,$0053,$0054,$0055,$0055,$0056
	dc.w	$0057,$0058,$0059,$0059,$005A,$005B,$005C,$005D
	dc.w	$005D,$005E,$005F,$0060,$0060,$0061,$0062,$0063
	dc.w	$0064,$0065,$0065,$0066,$0067,$0068,$0069,$0069
	dc.w	$006A,$006B,$006C,$006D,$006E,$006E,$006F,$0070
	dc.w	$0071,$0072,$0073,$0073,$0074,$0075,$0076,$0077
	dc.w	$0078,$0078,$0079,$007A,$007B,$007C,$007D,$007D
	dc.w	$007E,$007F,$0080,$0081,$0082,$0083,$0083,$0084
	dc.w	$0085,$0086,$0087,$0088,$0089,$008A,$008A,$008B
	dc.w	$008C,$008D,$008E,$008F,$0090,$0090,$0091,$0092
	dc.w	$0093,$0094,$0095,$0096,$0097,$0097,$0098,$0099
	dc.w	$009A,$009B,$009C,$009D,$009E,$009E,$009F,$00A0
	dc.w	$00A0

sinf:


mt_data=$50000

mt_init:lea	mt_data,a0
	lea	mt_mulu(pc),a1
	move.l	#mt_data+$c,d0
	moveq	#$1f,d1
	moveq	#$1e,d3
mt_lop4:move.l	d0,(a1)+
	add.l	d3,d0
	dbf	d1,mt_lop4

	lea	$3b8(a0),a1
	moveq	#$7f,d0
	moveq	#0,d1
	moveq	#0,d2
mt_lop2:move.b	(a1)+,d1
	cmp.b	d2,d1
	ble.s	mt_lop
	move.l	d1,d2
mt_lop:	dbf	d0,mt_lop2
	addq.w	#1,d2

	asl.l	#8,d2
	asl.l	#2,d2
	lea	4(a1,d2.l),a2
	lea	mt_samplestarts(pc),a1
	add.w	#$2a,a0
	moveq	#$1e,d0
mt_lop3:clr.l	(a2)
	move.l	a2,(a1)+
	moveq	#0,d1
	move.b	d1,2(a0)
	move.w	(a0),d1
	asl.l	#1,d1
	add.l	d1,a2
	add.l	d3,a0
	dbf	d0,mt_lop3

	move.l	$78.w,mt_oldirq-mt_samplestarts-$7c(a1)
	or.b	#2,$bfe001
	move.b	#6,mt_speed-mt_samplestarts-$7c(a1)
	moveq	#0,d0
	lea	$dff000,a0
	move.w	d0,$a8(a0)
	move.w	d0,$b8(a0)
	move.w	d0,$c8(a0)
	move.w	d0,$d8(a0)
	move.b	d0,mt_songpos-mt_samplestarts-$7c(a1)
	move.b	d0,mt_counter-mt_samplestarts-$7c(a1)
	move.w	d0,mt_pattpos-mt_samplestarts-$7c(a1)
	rts


mt_end:	moveq	#0,d0
	lea	$dff000,a0
	move.w	d0,$a8(a0)
	move.w	d0,$b8(a0)
	move.w	d0,$c8(a0)
	move.w	d0,$d8(a0)
	move.w	#$f,$dff096
	rts


mt_music:
	lea	mt_data,a0
	lea	mt_voice1(pc),a4
	addq.b	#1,mt_counter-mt_voice1(a4)
	move.b	mt_counter(pc),d0
	cmp.b	mt_speed(pc),d0
	blt	mt_nonew
	moveq	#0,d0
	move.b	d0,mt_counter-mt_voice1(a4)
	move.w	d0,mt_dmacon-mt_voice1(a4)
	lea	mt_data,a0
	lea	$3b8(a0),a2
	lea	$43c(a0),a0

	moveq	#0,d1
	move.b	mt_songpos(pc),d0
	move.b	(a2,d0.w),d1
	lsl.w	#8,d1
	lsl.w	#2,d1
	add.w	mt_pattpos(pc),d1

	lea	$dff0a0,a5
	lea	mt_samplestarts-4(pc),a1
	lea	mt_playvoice(pc),a6
	jsr	(a6)
	addq.l	#4,d1
	lea	$dff0b0,a5
	lea	mt_voice2(pc),a4
	jsr	(a6)
	addq.l	#4,d1
	lea	$dff0c0,a5
	lea	mt_voice3(pc),a4
	jsr	(a6)
	addq.l	#4,d1
	lea	$dff0d0,a5
	lea	mt_voice4(pc),a4
	jsr	(a6)

	move.w	mt_dmacon(pc),d0
	beq.s	mt_nodma

	lea	$bfd000,a3
	move.b	#$7f,$d00(a3)
	move.w	#$2000,$dff09c
	move.w	#$a000,$dff09a
	move.l	#mt_irq1,$78.w
	moveq	#0,d0
	move.b	d0,$e00(a3)
	move.b	#$a8,$400(a3)
	move.b	d0,$500(a3)
	or.w	#$8000,mt_dmacon-mt_voice4(a4)
	move.b	#$11,$e00(a3)
	move.b	#$81,$d00(a3)

mt_nodma:
	add.w	#$10,mt_pattpos-mt_voice4(a4)
	cmp.w	#$400,mt_pattpos-mt_voice4(a4)
	bne.s	mt_exit
mt_next:clr.w	mt_pattpos-mt_voice4(a4)
	clr.b	mt_break-mt_voice4(a4)
	addq.b	#1,mt_songpos-mt_voice4(a4)
	and.b	#$7f,mt_songpos-mt_voice4(a4)
	move.b	-2(a2),d0
	cmp.b	mt_songpos(pc),d0
	bne.s	mt_exit
	move.b	-1(a2),mt_songpos-mt_voice4(a4)
mt_exit:tst.b	mt_break-mt_voice4(a4)
	bne.s	mt_next
	rts

mt_nonew:
	lea	$dff0a0,a5
	lea	mt_com(pc),a6
	jsr	(a6)
	lea	mt_voice2(pc),a4
	lea	$dff0b0,a5
	jsr	(a6)
	lea	mt_voice3(pc),a4
	lea	$dff0c0,a5
	jsr	(a6)
	lea	mt_voice4(pc),a4
	lea	$dff0d0,a5
	jsr	(a6)
	tst.b	mt_break-mt_voice4(a4)
	bne.s	mt_next
	rts

mt_irq1:tst.b	$bfdd00
	move.w	mt_dmacon(pc),$dff096
	move.l	#mt_irq2,$78.w
	move.w	#$2000,$dff09c
	rte

mt_irq2:tst.b	$bfdd00
	movem.l	a3/a4,-(a7)
	lea	mt_voice1(pc),a4
	lea	$dff000,a3
	move.l	$a(a4),$a0(a3)
	move.w	$e(a4),$a4(a3)
	move.l	$a+$1c(a4),$b0(a3)
	move.w	$e+$1c(a4),$b4(a3)
	move.l	$a+$38(a4),$c0(a3)
	move.w	$e+$38(a4),$c4(a3)
	move.l	$a+$54(a4),$d0(a3)
	move.w	$e+$54(a4),$d4(a3)
	movem.l	(a7)+,a3/a4
	move.b	#0,$bfde00
	move.b	#$7f,$bfdd00
	move.l	mt_oldirq(pc),$78.w
	move.w	#$2000,$dff09c
	move.w	#$2000,$dff09a
	rte

mt_playvoice:
	move.l	(a0,d1.l),(a4)
	moveq	#0,d2
	move.b	2(a4),d2
	lsr.b	#4,d2
	move.b	(a4),d0
	and.b	#$f0,d0
	or.b	d0,d2
	beq	mt_oldinstr

	asl.w	#2,d2
	move.l	(a1,d2.l),4(a4)
	move.l	mt_mulu(pc,d2.w),a3
	move.w	(a3)+,8(a4)
	move.w	(a3)+,$12(a4)
	move.l	4(a4),d0
	moveq	#0,d3
	move.w	(a3)+,d3
	beq	mt_noloop
	asl.w	#1,d3
	add.l	d3,d0
	move.l	d0,$a(a4)
	move.w	-2(a3),d0
	add.w	(a3),d0
	move.w	d0,8(a4)
	bra	mt_hejaSverige

mt_mulu:blk.l	$20,0

mt_noloop:
	add.l	d3,d0
	move.l	d0,$a(a4)
mt_hejaSverige:
	move.w	(a3),$e(a4)
	move.w	$12(a4),8(a5)

mt_oldinstr:
	move.w	(a4),d3
	and.w	#$fff,d3
	beq	mt_com2
	tst.w	8(a4)
	beq.s	mt_stopsound
	move.b	2(a4),d0
	and.b	#$f,d0
	cmp.b	#5,d0
	beq.s	mt_setport
	cmp.b	#3,d0
	beq.s	mt_setport

	move.w	d3,$10(a4)
	move.w	$1a(a4),$dff096
	clr.b	$19(a4)

	move.l	4(a4),(a5)
	move.w	8(a4),4(a5)
	move.w	$10(a4),6(a5)

	move.w	$1a(a4),d0
	or.w	d0,mt_dmacon-mt_playvoice(a6)
	bra	mt_com2

mt_stopsound:
	move.w	$1a(a4),$dff096
	bra	mt_com2

mt_setport:
	move.w	(a4),d2
	and.w	#$fff,d2
	move.w	d2,$16(a4)
	move.w	$10(a4),d0
	clr.b	$14(a4)
	cmp.w	d0,d2
	beq.s	mt_clrport
	bge	mt_com2
	move.b	#1,$14(a4)
	bra	mt_com2
mt_clrport:
	clr.w	$16(a4)
	rts

mt_port:moveq	#0,d0
	move.b	3(a4),d2
	beq.s	mt_port2
	move.b	d2,$15(a4)
	move.b	d0,3(a4)
mt_port2:
	tst.w	$16(a4)
	beq.s	mt_rts
	move.b	$15(a4),d0
	tst.b	$14(a4)
	bne.s	mt_sub
	add.w	d0,$10(a4)
	move.w	$16(a4),d0
	cmp.w	$10(a4),d0
	bgt.s	mt_portok
	move.w	$16(a4),$10(a4)
	clr.w	$16(a4)
mt_portok:
	move.w	$10(a4),6(a5)
mt_rts:	rts

mt_sub:	sub.w	d0,$10(a4)
	move.w	$16(a4),d0
	cmp.w	$10(a4),d0
	blt.s	mt_portok
	move.w	$16(a4),$10(a4)
	clr.w	$16(a4)
	move.w	$10(a4),6(a5)
	rts

mt_sin:
dc.b $00,$18,$31,$4a,$61,$78,$8d,$a1,$b4,$c5,$d4,$e0,$eb,$f4,$fa,$fd
dc.b $ff,$fd,$fa,$f4,$eb,$e0,$d4,$c5,$b4,$a1,$8d,$78,$61,$4a,$31,$18

mt_vib:	move.b	$3(a4),d0
	beq.s	mt_vib2
	move.b	d0,$18(a4)

mt_vib2:move.b	$19(a4),d0
	lsr.w	#2,d0
	and.w	#$1f,d0
	moveq	#0,d2
	move.b	mt_sin(pc,d0.w),d2
	move.b	$18(a4),d0
	and.w	#$f,d0
	mulu	d0,d2
	lsr.w	#7,d2
	move.w	$10(a4),d0
	tst.b	$19(a4)
	bmi.s	mt_vibsub
	add.w	d2,d0
	bra.s	mt_vib3
mt_vibsub:
	sub.w	d2,d0
mt_vib3:move.w	d0,6(a5)
	move.b	$18(a4),d0
	lsr.w	#2,d0
	and.w	#$3c,d0
	add.b	d0,$19(a4)
	rts


mt_arplist:
dc.b 0,1,2,0,1,2,0,1,2,0,1,2,0,1,2,0,1,2,0,1,2,0,1,2,0,1,2,0,1,2,0,1

mt_arp:	moveq	#0,d0
	move.b	mt_counter(pc),d0
	move.b	mt_arplist(pc,d0.w),d0
	beq.s	mt_normper
	cmp.b	#2,d0
	beq.s	mt_arp2
mt_arp1:move.b	3(a4),d0
	lsr.w	#4,d0
	bra.s	mt_arpdo
mt_arp2:move.b	3(a4),d0
	and.w	#$f,d0
mt_arpdo:
	asl.w	#1,d0
	move.w	$10(a4),d1
	lea	mt_periods(pc),a0
mt_arp3:cmp.w	(a0)+,d1
	blt.s	mt_arp3
	move.w	-2(a0,d0.w),6(a5)
	rts

mt_normper:
	move.w	$10(a4),6(a5)
	rts

mt_com:	move.w	2(a4),d0
	and.w	#$fff,d0
	beq.s	mt_normper
	move.b	2(a4),d0
	and.b	#$f,d0
	beq.s	mt_arp
	cmp.b	#6,d0
	beq.s	mt_volvib
	cmp.b	#4,d0
	beq	mt_vib
	cmp.b	#5,d0
	beq.s	mt_volport
	cmp.b	#3,d0
	beq	mt_port
	cmp.b	#1,d0
	beq.s	mt_portup
	cmp.b	#2,d0
	beq.s	mt_portdown
	move.w	$10(a4),6(a5)
	cmp.b	#$a,d0
	beq.s	mt_volslide
	rts

mt_portup:
	moveq	#0,d0
	move.b	3(a4),d0
	sub.w	d0,$10(a4)
	move.w	$10(a4),d0
	cmp.w	#$71,d0
	bpl.s	mt_portup2
	move.w	#$71,$10(a4)
mt_portup2:
	move.w	$10(a4),6(a5)
	rts

mt_portdown:
	moveq	#0,d0
	move.b	3(a4),d0
	add.w	d0,$10(a4)
	move.w	$10(a4),d0
	cmp.w	#$358,d0
	bmi.s	mt_portdown2
	move.w	#$358,$10(a4)
mt_portdown2:
	move.w	$10(a4),6(a5)
	rts

mt_volvib:
	 bsr	mt_vib2
	 bra.s	mt_volslide
mt_volport:
	 bsr	mt_port2

mt_volslide:
	moveq	#0,d0
	move.b	3(a4),d0
	lsr.b	#4,d0
	beq.s	mt_vol3
	add.b	d0,$13(a4)
	cmp.b	#$40,$13(a4)
	bmi.s	mt_vol2
	move.b	#$40,$13(a4)
mt_vol2:move.w	$12(a4),8(a5)
	rts

mt_vol3:move.b	3(a4),d0
	and.b	#$f,d0
	sub.b	d0,$13(a4)
	bpl.s	mt_vol4
	clr.b	$13(a4)
mt_vol4:move.w	$12(a4),8(a5)
	rts

mt_com2:move.b	2(a4),d0
	and.b	#$f,d0
	beq	mt_rts
	cmp.b	#$e,d0
	beq.s	mt_filter
	cmp.b	#$d,d0
	beq.s	mt_pattbreak
	cmp.b	#$b,d0
	beq.s	mt_songjmp
	cmp.b	#$c,d0
	beq.s	mt_setvol
	cmp.b	#$f,d0
	beq.s	mt_setspeed
	rts

mt_filter:
	move.b	3(a4),d0
	and.b	#1,d0
	asl.b	#1,d0
	and.b	#$fd,$bfe001
	or.b	d0,$bfe001
	rts

mt_pattbreak:
	move.b	#1,mt_break-mt_playvoice(a6)
	rts

mt_songjmp:
	move.b	#1,mt_break-mt_playvoice(a6)
	move.b	3(a4),d0
	subq.b	#1,d0
	move.b	d0,mt_songpos-mt_playvoice(a6)
	rts

mt_setvol:
	cmp.b	#$40,3(a4)
	bls.s	mt_sv2
	move.b	#$40,3(a4)
mt_sv2:	moveq	#0,d0
	move.b	3(a4),d0
	move.b	d0,$13(a4)
	move.w	d0,8(a5)
	rts

mt_setspeed:
	moveq	#0,d0
	move.b	3(a4),d0
	cmp.b	#$1f,d0
	bls.s	mt_sp2
	moveq	#$1f,d0
mt_sp2:	tst.w	d0
	bne.s	mt_sp3
	moveq	#1,d0
mt_sp3:	move.b	d0,mt_speed-mt_playvoice(a6)
	rts

mt_periods:
dc.w $0358,$0328,$02fa,$02d0,$02a6,$0280,$025c,$023a,$021a,$01fc,$01e0
dc.w $01c5,$01ac,$0194,$017d,$0168,$0153,$0140,$012e,$011d,$010d,$00fe
dc.w $00f0,$00e2,$00d6,$00ca,$00be,$00b4,$00aa,$00a0,$0097,$008f,$0087
dc.w $007f,$0078,$0071,$0000

mt_speed:	dc.b	6
mt_counter:	dc.b	0
mt_pattpos:	dc.w	0
mt_songpos:	dc.b	0
mt_break:	dc.b	0
mt_dmacon:	dc.w	0
mt_samplestarts:blk.l	$1f,0
mt_voice1:	blk.w	13,0
		dc.w	1
mt_voice2:	blk.w	13,0
		dc.w	2
mt_voice3:	blk.w	13,0
		dc.w	4
mt_voice4:	blk.w	13,0
		dc.w	8
mt_oldirq:	dc.l	0




cls:
btst #14,$dff002
bne.s cls
move.w #0,$dff066
move.l #$01000000,$dff040
move.l #$35000,$dff054
move.w #40*64+20,$dff058
rts





print:
move.l #37,d7
lea hauttab-2,a0
moveq #0,d0		;x1
moveq #0,d1
moveq #0,d2
moveq #0,d3


PRTLP1:
move.w (a0),d1
move.L d0,d2
add.L #8,d2
move.w 2(a0),d3
movem.l d0-d7/a0-a6,-(sp)
jsr line
movem.l (sp)+,d0-d7/a0-a6
add.L #8,d0
add.L #2,a0
dbra d7,PRTLP1
rts


line:
	btst #14,$dff002
	bne.s line
	movem.l d0-d7/a0-a6,-(sp)
	lea $dff000,a5
	move #-1,$72(a5)
	move #-1,$44(a5)
	move #40,$60(a5)
	move #40,$66(a5)
	move #$8000,$74(a5)
lea $35000,a6
	cmp.w d1,d3
	beq noline
	bgt.s nohi
	exg d0,d2
	exg d1,d3
nohi:
	move d0,d4
	move d1,d5
	mulu #40,d5
	add d5,a6
	lsr #4,d4
	add d4,d4
	lea (a6,d4.w),a6
	sub d0,d2
	sub d1,d3
	moveq #15,d5
	and.l d5,d0
	move d0,d4
	ror.l #4,d0
	eor.w d5,d4
	moveq #0,d5
	bset d4,d5
	move #4,d0
	tst d2
	bpl l1
	addq.w #1,d0
	neg d2
l1:
	cmp d2,d3
	ble l2
	exg d2,d3
	subq #4,d0
	add d0,d0
l2:
	move d3,d4
	sub d2,d4
	add d4,d4
	add d4,d4
	add d3,d3
	moveq #0,d6
	move d3,d6
	sub d2,d6
	bpl l3
	or #16,d0
l3:
	add d3,d3
	add d0,d0
	add d0,d0
	addq #1,d2
	lsl #6,d2
	addq #2,d2
	swap d3
	move d4,d3
	or.l #$0b5a0003,d0
waitblit:
	btst #14,$dff002
	bne waitblit
	eor d5,(a6)	
	move.l d3,$62(a5)
	move d6,$52(a5)
	move.l a6,$48(a5)
	move.l a6,$54(a5)
	move.L d0,$40(a5)
	move d2,$58(a5)
noline:
	movem.l (sp)+,d0-d7/a0-a6
	rts



fillit:
btst #14,$dff002
bne fillit
move.l #$35000,a0	
add.l #[36*40]-2,a0
move.W #$09f0,$dff040
move.W #%10010,$dff042
move.l a0,$dff050
move.l a0,$dff054
move.W #0,$dff064
move.W #0,$dff066
move.W #38*64+20,$dff058
rts


run:
	move.l $4,a6
	lea intui,a1
	moveq #0,d0
	jsr -408(a6)
	move.l d0,a6
	clr.l d0
	lea texte(pc),a0
	move.l #50,d1
	jsr -90(a6)
	move.l a6,a1
	move.l $4,a6
	jsr -414(a6)
	rts
intui:
	dc.b 'intuition.library',0
texte:	
	dc.b 200/256,80,15,'Software failure.      Press left mouse button to continue',0,1
	dc.b 240/256,140,40,'Avec les compliments de Blatte Info Services',0,0

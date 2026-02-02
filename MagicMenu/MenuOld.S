;		      ANALOG FRENCH MENU II

;  How to use it:
;  --------------
;
;	- Modify the picture named 'names320-91.iff'
;	- Save it and with a converter write it as 'Names.r' with
;	  the size of 320*91 in 4 bp (RAWBLITTER)
;	- Take the number of file in the variable NB (max 20)

;	- Modify the loadlist at the end of this source
;	- Modify the scrolltext
;	- Modify the module
;	- Write your executable with WO command




;ASMONE
	ifnd	ASMONE
	opt c-,ow-,o+
	endc
	ifd	ASMONE
	incdir	"dh1:data/sources/analog/MagicPocket/"
	endc

NB=3

* ------------------------------

exec		equ	4
CloseLib	equ	-414
OpenLib		equ	-408
Execute		equ	-222
Forbid		equ	-132
Permit		equ	-138
Supervisor	equ	-30
AllocMem	equ	-$c6
AllocAbs	equ	-$cc
FreeMem		equ	-$d2

* ------------------------------

*LoadView	equ	-222
WaitBlit	equ	-228
WaitTOF		equ	-270
OwnBlit		equ	-456
DisownBlit	equ	-462


* -----------------------------------

	section analog,code

	rsreset
Dma	rs.w 1
intena	rs.w 1
intvbl	rs.l 1
GfxBase	rs.l 1
OldView	rs.l 1
VBR	rs.l 1
Aga	rs.b 2

* -----------------------------------
Timer	rs.l 1
VARSIZE	rs.w 0


* ------------------------------------

start:
	movem.l	d0-d7/a0-a6,-(sp)
	movea.l	a5,a6
	move.w	#Varsize-1,d7

.clear
	clr.b (a6)+
	dbf d7,.clear
	
	movea.l 4.w,a6
	lea GfxName(pc),a1
	moveq #$0,d0
	jsr OpenLib(a6)
	tst.l d0
	beq NoGfx
	
	move.l d0,GfxBase(a5)
	move.l d0,a6
	
	move.l $22(a6),OldView(a5)
	
	suba.l	a1,a1
	jsr LoadView(a6)
	
	jsr WaitOF(a6)
	jsr WaitOF(a6)
	jsr WaitBlit(a6)
	
	movea.l 4.w,a6
	jsr Forbid(a6)
	
	bsr getVBR



clearbitpl:
	clr.b (a0)+
	dbra d0,clearbitpl
	move.l #$3b000,a0
	move.l #$2ff4,d0

clearbitpl2:
	clr.b (a0)+
	dbra d0,clearbitpl2
	move.l exec,a6
	jsr forbid(a6)
	move.l exec,a6
	lea gfxname,a1
	jsr openlib(a6)
	move.l d0,gfxbase


	; init copper logo
	lea	logocop(pc),a0
	move.l	#Logo,d0
	move.w	d0,6(a0)
	swap	d0
	move.w	d0,2(a0)
	swap	d0
	add.l	#2200,d0
	move.w	d0,14(a0)
	swap	d0
	move.w	d0,10(a0)
	swap	d0
	add.l	#2200,d0
	move.w	d0,22(a0)
	swap	d0
	move.w	d0,18(a0)
	swap	d0
	add.l	#2200,d0
	move.w	d0,30(a0)
	swap	d0
	move.w	d0,26(a0)

	jsr calcmod
	jsr makedecor
	jsr initsprite
	jsr initnames
	jsr initcopper
	jsr mt_init

	move.w #$4000,$dff09a
	move.l $6c,oldirq+2
	move.l #newirq,$6c
	move.w #$c000,$dff09a

	move.l gfxbase,a0
	add.l #$32,a0
	move.l (a0),oldcopper
	move.l #newcopper,(a0)


wait:
	jsr scroll
	jsr clearit
waitit:
	cmp.b #200,$dff006
	bne waitit

	jsr moveportes
	cmp.w #8,depla
	bne cont_depla
	jsr nxtcar
	clr.w depla
cont_depla:
	jsr mouse
	jsr decaleanim
	jsr make_it
	move.l attente,d0
	move.l affiche,d1
	move.l d1,attente
	move.l d0,affiche
	lea splanes,a0
	move.w d0,6(a0)
	swap d0
	move.w d0,2(a0)

	tst.w portespd
	bne wait
	btst #6,$bfe001
	bne wait

	lea	splanes,a0
	move.l	#LoadGfx,d0
	move.w	d0,6(a0)
	swap	d0
	move.w	d0,2(a0)


	lea newcopper,a0
	move.w #0,6(a0)
	move.w #0,2(a0)
	move.w #0,14(a0)
	move.w #0,10(a0)
	move.w #0,22(a0)
	move.w #0,18(a0)
	move.w #0,30(a0)
	move.w #0,26(a0)

	move.w posx,d7
	sub.w #$51,d7
	sub.w #43,d7
	add.w portepos,d7
	divu #82,d7
	move.w d7,$d0		;numero de la porte

	move.w d7,d6		
	mulu #82,d6
	add.w #43,d6		;position de la porte en pixels

	move.w portepos,d5
	add.w bitpos,d5
	sub.w #$5a,d5

	sub.w d5,d6
	bgt onfaisplus

	sub.w #$5,d6
	move.l #ranims,currenta
	move.l #ranimsf,currentf
	move.l #kid10,currents
onfaismoins:
	cmp.b #200,$dff006
	bne onfaismoins
	sub.w #1,bitpos
	move.w #-1,animflg
	jsr decaleanim
	add.w #1,d6
	bne onfaismoins
	bra makesmile

onfaisplus:
	move.l #anims,currenta
	move.l #animsf,currentf
	move.l #kid1,currents
onfaisplus2:
	cmp.b #200,$dff006
	bne onfaisplus2
	add.w #1,bitpos
	move.w #-1,animflg
	jsr decaleanim
	sub.w #1,d6
	bne onfaisplus2

makesmile:
	lea smiletab,a1
	move.w #4,d0
makesmile2:
	move.l (a1)+,a0

	move.l spr1pos,(a0)
	move.l spr1pos,140(a0)
	move.l spr2pos,280(a0)
	move.l spr2pos,420(a0)

	move.l a0,d2
	lea newcopper,a2
	move.w d2,38(a2)
	swap d2
	move.w d2,34(a2)
	swap d2
	add.l #140,d2
	move.w d2,46(a2)
	swap d2
	move.w d2,42(a2)
	swap d2
	add.l #140,d2
	move.w d2,54(a2)
	swap d2
	move.w d2,50(a2)
	swap d2
	add.l #140,d2
	move.w d2,62(a2)
	swap d2
	move.w d2,58(a2)

	move.w #$a0,d1
makesmile3:
	cmp.b #00,$dff006
	bne makesmile3
	dbra d1,makesmile3
	dbra d0,makesmile2

findumenu:
	move.w #$100,d0
findumenu2:
	cmp.b #200,$dff006
	bne findumenu2
	dbra d0,findumenu2

	jsr mt_end
	move.w #$4000,$dff09a
	move.l oldirq+2,$6c
	move.w #$c000,$dff09a

	
	move.l gfxbase,a0
	add.l #$32,a0
	move.L oldcopper,(a0)
	move.l exec,a6
	move.l gfxbase,a1
	jsr closelib(a6)
	move.l exec,a6
	jsr permit(a6)
	movem.l (sp)+,d0-d7/a0-a6

openlibrary   = -408			; ExecLibrary
closelibrary  = -414			;
;execute       = -222			;

	bsr.W	ChoisiPrg		; Choisi le prg a charger
	move.l	4.w,a6			; open dos library
	lea	DosName(pc),a1		;
	moveq	#0,d0			;
	jsr	OpenLibrary(a6)		;
	move.l	d0,DosBase		;
	beq.s	Error			;

	move.l	DosBase(pc),a6		;
	move.l	Command(pc),d1		;
	moveq	#0,d2			;
	moveq	#0,d3			;
	jsr	Execute(a6)		;
      Error:				;
	moveq	#0,d0			;
      RTS				; fin du programme, sortie

	DosBase:		dc.l 0	;
	Command:		dc.l 0	;
	DosName:		dc.b 'dos.library',0



; ===============================================================
;  ROUTINE QUI DETERMINE LE PRG A CHARGER
; ===============================================================
CHOISIPRG:
	move.w	$d0.w,d0		; n° de la ligne courante
	lsl.w	#2,d0			;
	lea	TablePrg(pc),a0		; table des adr des noms
	move.l	(a0,d0.w),d0		;
	move.l	d0,Command		;
	rts				;

TablePrg:
	dc.l	prg00,prg01,prg02,prg03,prg04,prg05
	dc.l	prg06,prg07,prg08,prg09,prg10,prg11
	dc.l	prg12,prg13,prg14,prg15,prg16,prg17
	dc.l	prg18,prg19,prg20,prg21,prg22,prg23



newirq:
	movem.l d0-d7/a0-a6,-(sp)
	jsr mt_music
	movem.l (sp)+,d0-d7/a0-a6
oldirq:
	jmp $0000


initsprite:
	lea newcopper,a0
	move.l #pointeur1,d0
	move.w d0,6(a0)
	swap d0
	move.w d0,2(a0)
	move.l #pointeur2,d0
	move.w d0,14(a0)
	swap d0
	move.w d0,10(a0)
	move.l #pointeur3,d0
	move.w d0,22(a0)
	swap d0
	move.w d0,18(a0)
	move.l #pointeur4,d0
	move.w d0,30(a0)
	swap d0
	move.w d0,26(a0)

	move.l #kid1,d0
	move.w d0,38(a0)
	swap d0
	move.w d0,34(a0)
	swap d0
	add.l #144,d0
	move.w d0,46(a0)
	swap d0
	move.w d0,42(a0)
	swap d0
	add.l #144,d0
	move.w d0,54(a0)
	swap d0
	move.w d0,50(a0)
	swap d0
	add.l #144,d0
	move.w d0,62(a0)
	swap d0
	move.w d0,58(a0)
	rts

initnames:
	move.l nbportes,d7
	sub.w #2,d7
	move.l #53,d6		;posx
	moveq #0,d3		;num de la case

initnames_lp:
	btst #14&7,$dff002
	bne initnames_lp

	move.l d3,d0
	divs #5,d0
	move.l d0,d1
	swap d1
	mulu #160*23,d0
	mulu #8,d1
	add.w d1,d0	
	move.l names,a0
	add.w d0,a0

	move.l #45,d0
	move.w modulo,d1
	mulu #4,d1
	mulu d1,d0
	move.l d6,d5
	move.l d5,d4
	and.w #$f,d4
	lsr.w #3,d5
	add.l d0,d5
	add.l dest,d5
	lea namesmask,a1
	move.l a0,$dff04c
	move.l a1,$dff050
	move.l d5,$dff054
	move.l d5,$dff048
	move.w modulo,d0
	sub.w #10,d0
	move.w d0,$dff060
	move.w #30,$dff062
	move.w #-10,$dff064
	move.w d0,$dff066
	ror.w #4,d4
	move.w d4,$dff042
	or.w #$0fca,d4
	move.w d4,$dff040
	move.w #-1,$dff044
	move.w #0,$dff046
	move.w #23*4*64+5,$dff058
	addq #1,d3
	add.l #82,d6
	dbra d7,initnames_lp
	rts

initcopper:
	lea ssblock,a0
	lea sscolors,a1
	move.w #54,d7
	move.l #$2bd1fffe,d0
inco2:	
	move.l d0,(a0)+
	move.w #15,d6
	move.w #$01a0,d1
inco1:
	move.w d1,(a0)+
	move.w (a1),(a0)+
	add.w #$2,d1
	dbra d6,inco1
	add.l #$01000000,d0
	add.l #2,a1
	dbra d7,inco2
	rts	






calcmod:
	move.l nbportes,d0
	mulu #80,d0
	add.l #43,d0
	add.l #43,d0
	subq #4,d0	
	divu #8,d0
	move.w d0,modulo
	mulu #4,d0
	sub.w #40,d0
	lea copmod+2,a0
	move.w 	d0,(a0)
	move.w 	d0,4(a0)
	lea planes+6,a0
	move.w dest+2,(a0)
	move.w modulo,d0
	add.w dest+2,d0
	move.w d0,8(a0)
	add.w modulo,d0
	move.w d0,16(a0)
	add.w modulo,d0
	move.w d0,24(a0)
	move.l nbportes,d0
	subq #1,d0
	mulu #82,d0
	add.l #43,d0
	add.l #43,d0
	sub.w #288,d0
	move.w d0,portemax
	rts

makedecor:
	btst #14&7,$dff002
	bne makedecor
	move.l source,a0
	move.l mask,a1
	add.l bob1,a0
	move.l a0,$dff050
	move.l dest,$dff054
	move.w modulo,d0
	sub.w #6,d0
	move.w #34,$dff064
	move.w d0,$dff066
	move.w #$09f0,$dff040
	move.w #$0000,$dff042
	move.w #740*64+3,$dff058


	move.l nbportes,d7
	subq #2,d7
	move.l #43,d6
makedecor2:
	btst #14&7,$dff002
	bne makedecor2

	move.l d6,d5
	move.l d5,d4
	and.w #$f,d4
	lsr.w #3,d5
	add.l dest,d5
	move.l source,a0
	add.l bob2,a0
	lea maskit,a1
	add.l bob2,a1
	move.l a0,$dff04c
	move.l a1,$dff050
	move.l d5,$dff054
	move.l d5,$dff048
	move.w modulo,d0
	sub.w #14,d0
	move.w d0,$dff060
	move.w #26,$dff062
	move.w #-14,$dff064
	move.w d0,$dff066
	ror.w #4,d4
	move.w d4,$dff042
	or.w #$0fca,d4
	move.w d4,$dff040
	move.w #-1,$dff044
	move.w #0,$dff046
	move.w #740*64+7,$dff058
	add.l #82,d6
	dbra d7,makedecor2


makedecor3:
	btst #14&7,$dff002
	bne makedecor3

	move.l d6,d5
	move.l d5,d4
	and.w #$f,d4
	lsr.w #3,d5
	add.l dest,d5
	move.l source,a0
	add.l bob3,a0
	lea maskit,a1
	add.l bob3,a1
	move.l a0,$dff04c
	move.l a1,$dff050
	move.l d5,$dff054
	move.l d5,$dff048
	move.w modulo,d0
	sub.w #8,d0
	move.w d0,$dff060
	move.w #32,$dff062
	move.w #-8,$dff064
	move.w d0,$dff066
	ror.w #4,d4
	move.w d4,$dff042
	or.w #$0fca,d4
	move.w d4,$dff040
	move.w #-1,$dff044
	move.w #0,$dff046
	move.w #740*64+4,$dff058
	rts


moveportes:
	moveq #0,d0
	move.w portepos,d0
	lea planes,a1
	divs #16,d0
	swap d0
	neg.w d0
	and.w #$f,d0
	sub.w #1,d0
	move.w d0,d1
	lsl.w #4,d0
	or.w d0,d1
	move.w d1,podec+2
	move.w #0,d0
	swap d0
	add.w d0,d0
	add.l dest,d0
	move.w 6(a1),d1
	move.w 14(a1),d2
	sub.w d1,d2
	move.w d0,6(a1)
	swap d0
	move.w d0,2(a1)
	swap d0
	add.w d2,d0
	move.w d0,14(a1)
	swap d0
	move.w d0,10(a1)
	swap d0
	add.w d2,d0
	move.w d0,22(a1)
	swap d0
	move.W d0,18(a1)
	swap d0
	add.w d2,d0
	move.w d0,30(a1)
	swap d0
	move.W d0,26(a1)
	rts



mouse:
	clr.l d0
	clr.l d1
	move.b anc1,d0
	move.b $dff00a,d1
	sub.b d0,d1
	ext.w d1
	add.w d1,posy
	move.b $dff00a,anc1
	clr.l d0
	clr.l d1
	move.b anc2,d0
	move.b $dff00b,d1
	sub.b d0,d1
	ext.w d1
	add.w d1,posx
	move.b $dff00b,anc2

	move.w posx,d0
	cmp.w #$15a,d0
	bls pasXmax
	move.w #$15a,posx
pasXmax:
	cmp.w #$51,d0
	bge pasXmin
	move.w #$51,posx
pasXmin:

	move.w posy,d0
	cmp.w #$e1,d0
	bls pasymax
	move.w #$e1,posy
pasymax:
	move.w posy,d0
	cmp.w #$3c,d0
	bge pasymin
	move.w #$3c,posy
pasymin:
	move.w posx,d0
	add.w depx,d0
	move.w d0,d1
 	and.w #%00000001,d1
	or.w #%10000000,d1
	move.b d1,pointeur1+3
	move.b d1,pointeur2+3
	lsr.w #1,d0
	move.b d0,pointeur1+1
	move.b d0,pointeur2+1

	move.w posx,d0
	add.w depx,d0
	add.w #16,d0
	move.w d0,d1
 	and.w #%00000001,d1
	or.w #%10000000,d1
	move.b d1,pointeur3+3
	move.b d1,pointeur4+3
	lsr.w #1,d0
	move.b d0,pointeur3+1
	move.b d0,pointeur4+1

	move.w posy,d0
	add.w depy,d0
	move.b d0,pointeur1
	move.b d0,pointeur2
	move.b d0,pointeur3
	move.b d0,pointeur4
	and.w #%1111111100000000,d0
	tst.w d0
	beq pdpl
	or.b #%100,pointeur1+3
	or.b #%100,pointeur2+3
	or.b #%100,pointeur3+3
	or.b #%100,pointeur4+3
pdpl:
	move.w posy,d0
	add.w depy,d0
	add.w #32,d0
	move.b d0,pointeur1+2
	move.b d0,pointeur2+2
	move.b d0,pointeur3+2
	move.b d0,pointeur4+2
	and.w #%1111111100000000,d0
	tst.w d0
	beq pddl
	or.b #%010,pointeur1+3
	or.b #%010,pointeur2+3
	or.b #%010,pointeur3+3
	or.b #%010,pointeur4+3

pddl:
	clr.w portespd
	cmp.w #$51+50,posx
	bgt pasbougebpg
	move.w #-1,portespd
pasbougebpg:
	cmp.w #$16d-50,posx
	ble pasbougebpd
	move.w #1,portespd
pasbougebpd:

	cmp.w #-1,portespd
	bne pasbougebpg2
	tst.w portepos
	beq  pasbougebpg2
	move.w portespd,d0
	add.w d0,portepos
	move.w #1,animflg
	move.l #ranims,currenta
	move.l #ranimsf,currentf
	move.l #kid10,currents
pasbougebpg2:
	cmp.w #1,portespd
	bne pasbougebpd2
	move.w portepos,d0
	cmp.w portemax,d0
	beq pasbougebpd2
	move.w portespd,d0
	add.w d0,portepos
	move.w #1,animflg
	move.l #anims,currenta
	move.l #animsf,currentf
	move.l #kid1,currents
pasbougebpd2:
	rts


scroll:
	btst #14&7,$dff002
	bne scroll
	move.l #$3b008,a0
	move.L a0,a1
	sub.l #2,a1
	move.l a0,$dff050
	move.l a1,$dff054
	move.w #12,$dff064
	move.w #12,$dff066
	move.L #$ffffffff,$dff044
	move.w #%1110100111110000,$dff040
	clr.w $dff042
	move.w #16*64+26,$dff058
	add.w #1,depla
	rts


make_it:
	btst #14&7,$dff002
	bne make_it
	move.l sinpt,a0
	move.l #19,d7
	move.l #$3b008,a1
	move.l attente,a2
	move.w #38,$dff062
	move.w #62,$dff064
	move.w #38,$dff066
	move.w #-1,$dff046
	move.w #$0dfc,$dff040
	clr.w $dff042
	move.w #%1000000000000000,d1
ala:
	move.L #15,d2
ici:
	btst #14&7,$dff002
	bne ici
	moveq #0,d0
	move.w (a0)+,d0
	add.l a2,d0
	move.l a1,$dff050
	move.l d0,$dff04c
	move.l d0,$dff054
	move.w d1,$dff044
	move.w #16*64+1,$dff058
	ror.w #1,d1

	add.l #2,a0
	cmp.l #sinf,a0
	bmi cont_make2
	sub.l #360*2,a0
cont_make2:

	dbra d2,ici
	add.l #2,a1
	add.l #2,a2

	dbra d7,ala
	add.l #4,sinpt
	cmp.l #sinf,sinpt
	bmi cont_make
	sub.l #360*2,sinpt
cont_make:
	rts


nxtcar:
	add.l #1,car
	cmp.l #crf,car
	bne.s deb
	move.L #text,car
deb:
	move.l car,a0
	move.b (a0),d0
	lea let,a1
	moveq #-1,d1
lettre:
	add.l #1,d1
	cmp.l #150,d1
	beq nolettre
	move.b (a1)+,d2
	cmp.b d0,d2
	bne lettre
	divu #20,d1
	move.w d1,d2
	swap d1
	and.l #$ffff,d1
	mulu #17*40,d2
	mulu #2,d1
	add.l d1,d2
	add.l pic,d2
	lea $3b036,a1
	move.l d2,a0
	move.w #16,d7
la:
	move.w (a0),(a1)
	add.l #40,a0
	add.l #64,a1
	dbra d7,la
nolettre:
	rts

clearit:
	moveq #0,d0
	moveq #0,d1
	moveq #0,d2
	moveq #0,d3
	moveq #0,d4
	moveq #0,d5
	moveq #0,d6
	moveq #0,d7
	move.l d0,a0
	move.l d0,a1
	move.l d0,a2
	move.l d0,a3
	move.l d0,a4
	move.l d0,a5
	move.l d0,a6
	
	move.l a7,pile
	move.l attente,a7
	add.l #60*40,a7
	dcb.l 20,$48e7fffe
	move.l pile,a7


clearit2:
	btst #14&7,$dff002
	bne clearit2
	move.w #0,$dff066
	move.l attente,$dff054
	move.l #$01000000,$dff040
	move.w #30*64+20,$dff058
	rts




	rts

pile:	dc.l 0

decaleanim:
	tst.w animflg
	bne makeanims
	lea currents,a2
	jsr putspr
	rts

makeanims:
	sub.w #1,animdelay
	bne noanims
	move.w #5,animdelay
	move.l currenta,a2
	move.l currentf,a3
	move.l a2,a0
	lea 4(a0),a1
	move.w #8,d0
	move.l (a0),d1
daloop:
	move.l (a1)+,(a0)+
	dbra d0,daloop
	move.l d1,(a3)
putspr:
	move.l (a2),d0
	lea newcopper,a0
	move.w d0,38(a0)
	swap d0
	move.w d0,34(a0)
	swap d0
	add.l #144,d0
	move.w d0,46(a0)
	swap d0
	move.w d0,42(a0)
	swap d0
	add.l #144,d0
	move.w d0,54(a0)
	swap d0
	move.w d0,50(a0)
	swap d0
	add.l #144,d0
	move.w d0,62(a0)
	swap d0
	move.w d0,58(a0)

	move.w bitpos,d0
	add.w #$51,d0
	move.w d0,d1
	and.w #$1,d1
	or.w #$82,d1
	move.w d1,spr1pos+2
	move.w d1,spr2pos+2
	lsr.w #1,d0
	or.w #$e000,d0
	move.w d0,spr1pos
	add.w #8,d0
	move.w d0,spr2pos
	move.l (a2),a0
	move.l spr1pos,(a0)
	move.l spr1pos,144(a0)
	move.l spr2pos,288(a0)
	move.l spr2pos,432(a0)
noanims:
	move.w #0,animflg
	rts
	






maskit:
	dc.l %11111111111111111111111111111111,%11111111111110001111111111111111
	dc.l %11111111111111111111111111111111,%11111111111111111111111111111111
	dc.l %11100000000000001111111111111111,%11111111111111111111111111111000
	dc.l %00000000000000000000000000000000,%00000000000000000000000000000000
	dc.l %00000000000000000000000000000000,%00000000000000000000000000000000

namesmask:
	dc.l %11111111111111111111111111111111,%11111111111111111111111111000000

newcopper:
	dc.w $0120,$0000,$0122,$0000
	dc.w $0124,$0000,$0126,$0000
	dc.w $0128,$0000,$012a,$0000
	dc.w $012c,$0000,$012e,$0000
	dc.w $0130,$0000,$0132,$0000
	dc.w $0134,$0000,$0136,$0000
	dc.w $0138,$0000,$013a,$0000
	dc.w $013c,$0000,$013e,$0000
	dc.w $008e,$2c81,$0090,$2ec1
	dc.w $0092,$0038,$0094,$00d0
	dc.W $0100,$5200,$108,0,$10a,0,$102,0

      LogoCop:
	dc.w $00e0,$ffff,$00e2,$ffff
	dc.w $00e4,$ffff,$00e6,$ffff
	dc.w $00e8,$ffff,$00ea,$ffff
	dc.w $00ec,$ffff,$00ee,$ffff
splanes:
	dc.w $00f0,$ffff,$00f2,$ffff
	dc.w $0180,$0000,$0182,$0226,$0184,$0648,$0186,$066c
	dc.w $0188,$088e,$018a,$0eee,$018c,$0ea8,$018e,$0e86
	dc.w $0190,$0c64,$0192,$0840,$0194,$0600,$0196,$0064
	dc.w $0198,$0086,$019a,$08c8,$019c,$0ee6,$019e,$0000

ssblock:
	dcb.w 34*55,0
;	dc.w $2be1,$fffe,$180,$f


	dc.w $6307,$fffe,$100,$0000
	dc.w $008e,$2c91,$0090,$20b1
	dc.w $0180,$0000,$0182,$0226,$0184,$0648,$0186,$066c
	dc.w $0188,$088e,$018a,$0eee,$018c,$0ea8,$018e,$0e86
	dc.w $0190,$0c64,$0192,$0840,$0194,$0600,$0196,$0064
	dc.w $0198,$0086,$019a,$08c8,$019c,$0ee6,$019e,$0000
	dc.w $01a0,$0000,$01a2,$0226,$01a4,$0648,$01a6,$066c
	dc.w $01a8,$088e,$01aa,$0eee,$01ac,$0ea8,$01ae,$0e86
	dc.w $01b0,$0c64,$01b2,$0840,$01b4,$0600,$01b6,$0064
	dc.w $01b8,$0086,$01ba,$08c8,$01bc,$0ee6,$01be,$0000
	dc.w $6807,$fffe		;,$180,$f
	dc.w $0100,$4200
copmod:
	dc.w $0108,$009f,$010a,$00a0
podec:	
	dc.w $0102,$0000
planes:
	dc.w $00e0,$0004,$00e2,$2000
	dc.w $00e4,$0004,$00e6,$2028
	dc.w $00e8,$0004,$00ea,$2050
	dc.w $00ec,$0004,$00ee,$2078
	dc.W $ffff,$fffe

oldcopper:	dc.l 0
gfxname:	dc.b 'graphics.library',0,0
;gfxbase:	dc.l 0
ordtab:		dc.w 1,2,2,2,2,3
postab:		dc.w 0,40
bob1:		dc.l 0
bob2:		dc.l 6
bob3:		dc.l $12
source:		dc.l fin		; $33000
mask:		dc.l $3b000
dest:		dc.l $56500
names:		dc.l namesGFX
modulo:		dc.w 0
sensflg:	dc.w 0
nbportes:	dc.l NB+1
portepos:	dc.w 0
portemax:	dc.w 0
portespd:	dc.w 0
reste:		dc.w 0
anc1:		dc.w 0
anc2:		dc.w 0
posx:		dc.w 0
depx:		dc.w $40
posy:		dc.w 0
select:		dc.w 0
depy:		dc.w $2c
let: 		dc.b "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890!.,'"
		dc.b ":?()-/_abcde ",0
pic:		dc.l fonts		;$3a500		;blk.b 6915,0
depla:		dc.w 0
attente:	dc.l $3c000
affiche:	dc.l $3ca00
anims:		dc.l kid7,kid2,kid3,kid4,kid5,kid6,kid7,kid8,kid7
animsf:		dc.l kid2
ranims:		dc.l kid11,kid12,kid13,kid14,kid15,kid16,kid11,kid17,kid11
ranimsf:	dc.l kid12
smiletab:	dc.l smile1,smile2,smile3,smile4,smile5
currenta:	dc.l anims
currentf:	dc.l animsf
currents:	dc.l kid1
animdelay:	dc.w 6
animflg:	dc.w 0
bitpos:		dc.w $c0
posdebporte:	dc.w 0
spr1pos:	dc.w $e088,$0083
spr2pos:	dc.w $e090,$0083
car:		dc.l text



pointeur1:
	dc.w 	$2c40,$3000
	dc.w	$0300,$0740,$0280,$0580,$0080,$0780,$0080,$0780
	dc.w	$0780,$0780,$0440,$0400,$0480,$0780,$0280,$0580
	dc.w	$0084,$079d,$078a,$0796,$0042,$003e,$0482,$007e
	dc.w	$02c2,$03be,$0d41,$0b3f,$2abd,$3673,$853f,$fce0
	dc.w	$9a6e,$f9f1,$edff,$e3df,$36af,$0fdf,$1357,$798f
	dc.w	$0956,$0a4f,$0cb5,$008e,$07e2,$001c,$0004,$03f8
	dc.w	$0000,$01ff,$00ea,$0004,$0033,$0004,$00d3,$00c7
	dc.w	$0059,$00cf,$00d4,$004b,$005d,$00cb,$005f,$00cb
	dc.w	$0000,$0000

pointeur2:
	dc.w 	$2c40,$3000
	dc.w	$0300,$04c0,$0780,$0040,$0780,$0040,$0780,$0040
	dc.w	$0780,$0040,$0400,$03c0,$0780,$0040,$0780,$0040
	dc.w	$078c,$0053,$079e,$0041,$001e,$07e1,$001e,$07e1
	dc.w	$039e,$0461,$0f1f,$10e0,$3e3f,$41e0,$fc20,$03ff
	dc.w	$f860,$07ff,$e1df,$1fff,$068f,$7fff,$1107,$7fff
	dc.w	$0846,$17bf,$0084,$0f7f,$0000,$07ff,$0000,$03ff
	dc.w	$0000,$01ff,$0000,$00ff,$0000,$00ff,$00c3,$003c
	dc.w	$00cb,$0034,$00cb,$0034,$00cb,$0034,$00cb,$0034
	dc.w	$0000,$0000

pointeur3:
	dc.w 	$2c40,$3000
	dc.w	$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	dc.w	$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	dc.w	$0000,$0000,$0800,$c000,$6400,$a000,$1400,$f000
	dc.w	$9400,$f000,$9400,$f000,$e800,$dc00,$f800,$8400
	dc.w	$b000,$cc00,$fc00,$fc00,$fc00,$fc00,$a400,$c000
	dc.w	$9c00,$1000,$6800,$6400,$3800,$0400,$e000,$1c00
	dc.w	$0000,$fc00,$4800,$4400,$a000,$1c00,$9800,$8400
	dc.w	$4800,$4400,$8800,$8400,$4800,$4400,$8800,$8400
	dc.w	$0000,$0000
	
pointeur4:
	dc.w 	$2c40,$3000
	dc.w	$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	dc.w	$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	dc.w	$0000,$0000,$0000,$f800,$6000,$9c00,$7000,$8c00
	dc.w	$f000,$8c00,$f000,$8c00,$f800,$8400,$8000,$fc00
	dc.w	$8000,$fc00,$fc00,$fc00,$fc00,$fc00,$8000,$fc00
	dc.w	$1000,$ec00,$6000,$9c00,$0000,$fc00,$0000,$fc00
	dc.w	$0000,$fc00,$4000,$bc00,$0000,$fc00,$8000,$7c00
	dc.w	$4000,$bc00,$8000,$7c00,$4000,$bc00,$8000,$7c00
	dc.w	$0000,$0000

sin:
	dc.w 800,800,800,840,840,840,880,880
	dc.w 880,920,920,920,960,960,960,1000
	dc.w 1000,1000,1040,1040,1040,1080,1080,1080
	dc.w 1120,1120,1120,1160,1160,1160,1200,1200
	dc.w 1200,1200,1240,1240,1240,1280,1280,1280
	dc.w 1280,1320,1320,1320,1320,1360,1360,1360
	dc.w 1360,1400,1400,1400,1400,1400,1440,1440
	dc.w 1440,1440,1440,1480,1480,1480,1480,1480
	dc.w 1480,1520,1520,1520,1520,1520,1520,1520
	dc.w 1560,1560,1560,1560,1560,1560,1560,1560
	dc.w 1560,1560,1560,1560,1560,1560,1560,1560
	dc.w 1560,1560,1560,1560,1560,1560,1560,1560
	dc.w 1560,1560,1560,1560,1560,1560,1560,1560
	dc.w 1560,1560,1560,1560,1560,1520,1520,1520
	dc.w 1520,1520,1520,1520,1480,1480,1480,1480
	dc.w 1480,1480,1440,1440,1440,1440,1440,1400
	dc.w 1400,1400,1400,1400,1360,1360,1360,1360
	dc.w 1320,1320,1320,1320,1280,1280,1280,1280
	dc.w 1240,1240,1240,1200,1200,1200,1200,1160
	dc.w 1160,1160,1120,1120,1120,1080,1080,1080
	dc.w 1040,1040,1040,1000,1000,1000,960,960
	dc.w 960,920,920,920,880,880,880,840
	dc.w 840,840,800,800,800,760,760,720
	dc.w 720,720,680,680,680,640,640,640
	dc.w 600,600,600,560,560,560,520,520
	dc.w 520,480,480,480,440,440,440,400
	dc.w 400,400,360,360,360,360,320,320
	dc.w 320,280,280,280,280,240,240,240
	dc.w 240,200,200,200,200,160,160,160
	dc.w 160,160,120,120,120,120,120,80
	dc.w 80,80,80,80,80,40,40,40
	dc.w 40,40,40,40,0,0,0,0
	dc.w 0,0,0,0,0,0,0,0
	dc.w 0,0,0,0,0,0,0,0
	dc.w 0,0,0,0,0,0,0,0
	dc.w 0,0,0,0,0,0,0,0
	dc.w 0,40,40,40,40,40,40,40
	dc.w 80,80,80,80,80,80,120,120
	dc.w 120,120,120,160,160,160,160,160
	dc.w 200,200,200,200,240,240,240,240
	dc.w 280,280,280,280,320,320,320,360
	dc.w 360,360,360,400,400,400,440,440
	dc.w 440,480,480,480,520,520,520,560
	dc.w 560,560,600,600,600,640,640,640
	dc.w 680,680,680,720,720,720,760,760
	dc.w 800,800,800,840,840,840,880,880
sinf:
sinpt:	dc.l sin




kid1:				;144
	dc.w $e088,$0083
	dc.w $0003,$0007,$0004,$000a,$0018,$0025,$0069,$0002
	dc.w $006b,$0008,$006f,$000c,$0aff,$033f,$07ff,$0007
	dc.w $01ff,$00df,$00bd,$0178,$057c,$051f,$0e06,$0a85
	dc.w $0b63,$0b64,$0634,$0627,$01db,$0013,$009e,$0315
	dc.w $053f,$022f,$067f,$007f,$016f,$008a,$05c7,$0407
	dc.w $039f,$009e,$0146,$067e,$00bd,$073c,$005b,$0798
	dc.w $003f,$03c0,$000f,$01f0,$0004,$00e1,$0036,$00d0
	dc.w $00d3,$00af,$014d,$00d4,$01b6,$007e,$01e7,$0018
	dc.w $01d7,$0037
	dc.l $0000,$0000

	dc.w $e088,$0083
	dc.w $0000,$0003,$0001,$0001,$0002,$0002,$0034,$0034
	dc.w $0074,$007c,$0070,$007c,$1c3f,$003f,$0007,$0007
	dc.w $00df,$013f,$0138,$00df,$011f,$04f8,$0487,$0f78
	dc.w $0467,$0f98,$01c7,$07f8,$03e3,$03fc,$07e4,$07b5
	dc.w $07cf,$076f,$0787,$06ff,$07f2,$05ea,$03fb,$07df
	dc.w $0060,$00fe,$007e,$0781,$003c,$07c3,$0018,$07e7
	dc.w $0000,$03ff,$0000,$01ff,$001e,$00c1,$000f,$00c6
	dc.w $0040,$0040,$0023,$0000,$0001,$0000,$0000,$0000
	dc.w $0008,$0000
	dc.l $0000,$0000

	dc.w $e090,$0083
	dc.w $1000,$fc00,$1400,$7380,$d280,$30c0,$ed00,$1d40
	dc.w $83c0,$7f80,$fce0,$0000,$ffe0,$f3e0,$9880,$6060
	dc.w $fdf0,$fef0,$ffb0,$acd0,$fe70,$df30,$fff0,$7bf0
	dc.w $39c0,$8000,$d2e0,$ca00,$b040,$8780,$ca80,$0e00
	dc.w $fc00,$fc00,$fe00,$0200,$cb00,$c300,$c580,$4180
	dc.w $ffc0,$8200,$8580,$7840,$cf00,$0080,$cd00,$5e00
	dc.w $5a00,$3d00,$b000,$7e00,$7800,$ec00,$f000,$cc00
	dc.w $f800,$e600,$ba80,$36e0,$f550,$0310,$0fe0,$f600
	dc.w $7d30,$0b10
	dc.l $0000,$0000

	dc.w $e090,$0083
	dc.w $0000,$1000,$8c00,$8000,$0f00,$0380,$0280,$0380
	dc.w $0000,$0380,$0000,$07c0,$f3e0,$f3e0,$0000,$7fe0
	dc.w $fcf0,$fcf0,$cc90,$8f90,$9f30,$9cb0,$7bf0,$fc70
	dc.w $8000,$7fe0,$c200,$3de0,$8000,$7fc0,$0e00,$f180
	dc.w $fc00,$fc00,$0000,$0200,$3c00,$ff00,$3e00,$7f80
	dc.w $8000,$8200,$0000,$ffc0,$0000,$ff80,$4c00,$bf00
	dc.w $1800,$ff00,$3000,$fe00,$6000,$e000,$c000,$c000
	dc.w $e000,$e000,$c100,$8000,$f8e0,$9040,$0600,$0600
	dc.w $f0e0,$6020
	dc.l $0000,$0000

kid2:
	dc.w $0000,$0000
	dc.w $0001,$0003,$0002,$0005,$000c,$0012,$0034,$0001
	dc.w $0035,$0004,$0037,$0006,$057f,$019f,$03ff,$0003
	dc.w $00ff,$006f,$005e,$00bc,$02be,$028f,$0703,$0542
	dc.w $05b1,$05b2,$031a,$0313,$00ed,$0009,$004f,$018a
	dc.w $029f,$0117,$033f,$003f,$00b7,$0045,$02e3,$0203
	dc.w $01cf,$004f,$009e,$011e,$002d,$01cc,$0013,$01e0
	dc.w $000f,$00f0,$008f,$00f0,$0105,$00e1,$0036,$03d0
	dc.w $04d3,$03af,$074d,$00d4,$03b6,$047e,$07e7,$0398
	dc.w $0767,$06e7
	dc.l $0000,$0000

	dc.w $0000,$0000
	dc.w $0000,$0001,$0000,$0000,$0001,$0001,$001a,$001a
	dc.w $003a,$003e,$0038,$003e,$0e1f,$001f,$0003,$0003
	dc.w $006f,$009f,$009c,$006f,$008f,$027c,$0243,$07bc
	dc.w $0233,$07cc,$00e3,$03fc,$01f1,$01fe,$03f2,$03da
	dc.w $03e7,$03b7,$03c3,$037f,$03f9,$02f5,$01fd,$03ef
	dc.w $0030,$007f,$001e,$01e1,$000c,$01f3,$0000,$01ff
	dc.w $0000,$00ff,$0080,$00ff,$001f,$00c1,$000f,$00c6
	dc.w $0040,$0040,$0023,$0000,$0001,$0000,$0380,$0380
	dc.w $0618,$0600
	dc.l $0000,$0000

	dc.w $0000,$0000
	dc.w $8800,$fe00,$0a00,$39c0,$6940,$9860,$f680,$0ea0
	dc.w $c1e0,$3fc0,$fe70,$0000,$fff0,$f9f0,$cc40,$b030
	dc.w $fef8,$ff78,$ffd8,$5668,$7f38,$ef98,$7ff8,$bdf8
	dc.w $9ce0,$4000,$6970,$e500,$d820,$c3c0,$6540,$8700
	dc.w $fe00,$fe00,$ff00,$8100,$e580,$6180,$e2c0,$a0c0
	dc.w $ffc0,$4100,$8500,$78c0,$ce00,$0180,$cc00,$5f00
	dc.w $5800,$3f00,$f200,$4c00,$f800,$e600,$f690,$fce0
	dc.w $f2d0,$ee10,$bff0,$3e10,$f120,$0710,$0800,$f000
	dc.w $7800,$0800
	dc.l $0000,$0000

	dc.w $0000,$0000
	dc.w $0000,$8800,$c600,$c000,$0780,$01c0,$0140,$01c0
	dc.w $0000,$01c0,$0000,$03e0,$f9f0,$f9f0,$8000,$bff0
	dc.w $fe78,$fe78,$6648,$c7c8,$cf98,$4e58,$bdf8,$7e38
	dc.w $c000,$3ff0,$e100,$1ef0,$c000,$3fe0,$0700,$f8c0
	dc.w $fe00,$fe00,$8000,$8100,$1e00,$7f80,$9f00,$bfc0
	dc.w $4000,$4100,$0000,$ffc0,$0000,$ff80,$4c00,$bf00
	dc.w $1800,$ff00,$4000,$c000,$e000,$e000,$f300,$f200
	dc.w $e1e0,$e0c0,$ce10,$8e10,$f8e0,$9020,$0000,$0000
	dc.w $f000,$6000
	dc.l $0000,$0000

kid3:
	dc.w $0000,$0000
	dc.w $0000,$0000,$0006,$000f,$0008,$0014,$0031,$004a
	dc.w $00d3,$0004,$00d7,$0010,$00df,$0018,$15ff,$067f
	dc.w $0fff,$000e,$03ff,$01bf,$017b,$02f1,$0af9,$0a3f
	dc.w $1c0d,$150a,$16c6,$16c9,$0c69,$0c4f,$03b7,$0027
	dc.w $013d,$062a,$0a7f,$045f,$0cff,$00fe,$02df,$0115
	dc.w $0b8f,$080e,$073f,$013d,$013d,$023c,$005b,$0398
	dc.w $0027,$07c0,$001e,$0fe1,$01b0,$1e9e,$0268,$3c00
	dc.w $1a60,$35e0,$29b6,$1a87,$76de,$0fc0,$7ce1,$331e
	dc.w $2cef,$1ce1
	dc.l $0000,$0000

	dc.w $0000,$0000
	dc.w $0000,$0000,$0000,$0006,$0003,$0003,$0004,$0004
	dc.w $0068,$0068,$00e8,$00f8,$00e0,$00f8,$387f,$007f
	dc.w $000e,$000e,$01bf,$027f,$0271,$01bf,$023f,$09f1
	dc.w $090e,$1ef1,$08cf,$1f30,$038f,$0ff0,$07c7,$07f8
	dc.w $0fc8,$0f6b,$0f9f,$0edf,$0f0e,$0dfe,$0fe4,$0bd5
	dc.w $07f6,$0fbe,$00c1,$01fd,$003c,$03c3,$0018,$03e7
	dc.w $0000,$07ff,$0000,$0fff,$0060,$1e0e,$01f0,$3c60
	dc.w $0800,$0800,$0478,$0010,$003f,$0012,$3000,$3000
	dc.w $031e,$000c
	dc.l $0000,$0000

	dc.w $0000,$0000
	dc.w $0000,$0000,$2000,$f800,$2800,$e700,$a500,$6180
	dc.w $da00,$3a80,$0780,$ff40,$f9c0,$0000,$ffc0,$e7c0
	dc.w $3100,$c0c0,$fbe0,$fde0,$ff60,$59a0,$fce0,$be60
	dc.w $ffe0,$f7e0,$7380,$0000,$a5c0,$9400,$6080,$0f00
	dc.w $9500,$1c00,$f800,$f800,$fc00,$0400,$9600,$8600
	dc.w $8b00,$8300,$ff80,$0400,$0a00,$f180,$1c00,$0380
	dc.w $1800,$3f80,$3f80,$3680,$1a40,$1e40,$0f00,$0cc0
	dc.w $1ed0,$039c,$125a,$0dc2,$1ffc,$19c0,$2624,$1fe2
	dc.w $0000,$0000
	dc.l $0000,$0000

	dc.w $0000,$0000
	dc.w $0000,$0000,$0000,$2000,$1800,$0000,$1e00,$0700
	dc.w $0500,$0700,$0000,$0700,$0000,$0f80,$e7c0,$e7c0
	dc.w $0000,$ffc0,$f9e0,$f9e0,$9920,$1f20,$3e60,$3960
	dc.w $f7e0,$f8e0,$0000,$ffc0,$8400,$7bc0,$0000,$ff80
	dc.w $1c00,$e300,$f800,$f800,$0000,$0400,$7800,$fe00
	dc.w $7c00,$ff00,$0000,$0400,$0000,$ff80,$0000,$ff80
	dc.w $1800,$ff80,$3100,$3100,$1980,$1800,$0c00,$0c00
	dc.w $0260,$0240,$003c,$0018,$19c0,$19c0,$001c,$0004
	dc.w $0000,$0000
	dc.l $0000,$0000

kid4:
	dc.w $0000,$0000
	dc.w $0000,$0000,$0000,$0000,$0006,$000f,$0008,$0014
	dc.w $0031,$004a,$00d3,$0004,$00d7,$0010,$00df,$0018
	dc.w $15ff,$067f,$0fff,$000e,$03ff,$01bf,$017b,$02f1
	dc.w $0af9,$0a3f,$1c0d,$150a,$16c6,$16c9,$0c69,$0c4f
	dc.w $03b7,$0027,$013d,$062a,$0a7f,$045f,$0cff,$00fe
	dc.w $02df,$0115,$0b8f,$080e,$073f,$013d,$027a,$0479
	dc.w $20b7,$3f30,$504e,$0f81,$283c,$67c2,$5c00,$37f8
	dc.w $22c0,$5e00,$14a0,$2c20,$0e68,$3200,$03f8,$0c00
	dc.w $0090,$0790
	dc.l $0000,$0000

	dc.w $0000,$0000
	dc.w $0000,$0000,$0000,$0000,$0000,$0006,$0003,$0003
	dc.w $0004,$0004,$0068,$0068,$00e8,$00f8,$00e0,$00f8
	dc.w $387f,$007f,$000e,$000e,$01bf,$027f,$0271,$01bf
	dc.w $023f,$09f1,$090e,$1ef1,$08cf,$1f30,$038f,$0ff0
	dc.w $07c7,$07f8,$0fc8,$0f6b,$0f9f,$0edf,$0f0e,$0dfe
	dc.w $0fe4,$0bd5,$07f6,$0fbe,$00c1,$01fd,$0078,$0787
	dc.w $0030,$1fcf,$3000,$1fff,$1000,$07fe,$0000,$03f8
	dc.w $01e0,$0000,$03c0,$0000,$01f0,$0000,$0000,$0000
	dc.w $0060,$0000
	dc.l $0000,$0000

	dc.w $0000,$0000
	dc.w $0000,$0000,$0000,$0000,$2000,$f800,$2800,$e700
	dc.w $a500,$6180,$da00,$3a80,$0780,$ff40,$f9c0,$0000
	dc.w $ffc0,$e7c0,$3100,$c0c0,$fbe0,$fde0,$ff60,$59a0
	dc.w $fce0,$be60,$ffe0,$f7e0,$7380,$0000,$a5c0,$9400
	dc.w $6080,$0f00,$9500,$1c00,$f800,$f800,$fc00,$0400
	dc.w $9600,$8600,$8b00,$8300,$ff80,$0400,$1480,$e300
	dc.w $0100,$fe00,$1d80,$9e00,$0be0,$0da0,$0a90,$0f98
	dc.w $13c8,$0930,$0bb4,$06e7,$0496,$0370,$0fff,$0e70
	dc.w $0989,$07f8
	dc.l $0000,$0000

	dc.w $0000,$0000
	dc.w $0000,$0000,$0000,$0000,$0000,$2000,$1800,$0000
	dc.w $1e00,$0700,$0500,$0700,$0000,$0700,$0000,$0f80
	dc.w $e7c0,$e7c0,$0000,$ffc0,$f9e0,$f9e0,$9920,$1f20
	dc.w $3e60,$3960,$f7e0,$f8e0,$0000,$ffc0,$8400,$7bc0
	dc.w $0000,$ff80,$1c00,$e300,$f800,$f800,$0000,$0400
	dc.w $7800,$fe00,$7c00,$ff00,$0000,$0400,$0000,$ff00
	dc.w $0000,$ff80,$1c00,$9f80,$0840,$0c40,$0a60,$0e00
	dc.w $0500,$0100,$0098,$0090,$000f,$0006,$0e70,$0e70
	dc.w $0007,$0001
	dc.l $0000,$0000

kid5:
	dc.w $0000,$0000
	dc.w $0000,$0000,$0006,$000f,$0008,$0014,$0031,$004a
	dc.w $00d3,$0004,$00d7,$0010,$00df,$0018,$15ff,$067f
	dc.w $0fff,$000e,$03ff,$01bf,$017b,$02f1,$0af9,$0a3f
	dc.w $1c0d,$150a,$16c6,$16c9,$0c69,$0c4f,$03b7,$0027
	dc.w $013d,$062a,$0a7f,$045f,$0cff,$00fe,$02df,$0115
	dc.w $0b8f,$080e,$073f,$013d,$013d,$023c,$005b,$0798
	dc.w $0027,$0fc0,$001e,$1fe0,$00b0,$1f80,$1a78,$35e8
	dc.w $29b6,$1a86,$36de,$0fc0,$3ce1,$331e,$2cef,$1ce1
	dc.w $0000,$0000
	dc.l $0000,$0000

	dc.w $0000,$0000
	dc.w $0000,$0000,$0000,$0006,$0003,$0003,$0004,$0004
	dc.w $0068,$0068,$00e8,$00f8,$00e0,$00f8,$387f,$007f
	dc.w $000e,$000e,$01bf,$027f,$0271,$01bf,$023f,$09f1
	dc.w $090e,$1ef1,$08cf,$1f30,$038f,$0ff0,$07c7,$07f8
	dc.w $0fc8,$0f6b,$0f9f,$0edf,$0f0e,$0dfe,$0fe4,$0bd5
	dc.w $07f6,$0fbe,$00c1,$01fd,$003c,$03c3,$0018,$07e7
	dc.w $0000,$0fff,$0000,$1ffe,$0078,$1e20,$0800,$0808
	dc.w $0478,$0010,$003f,$0012,$3000,$3000,$031e,$000c
	dc.w $0000,$0000
	dc.l $0000,$0000

	dc.w $0000,$0000
	dc.w $0000,$0000,$2000,$f800,$2800,$e700,$a500,$6180
	dc.w $da00,$3a80,$0780,$ff00,$f9c0,$0000,$ffc0,$e7c0
	dc.w $3100,$c0c0,$fbe0,$fde0,$ff60,$59a0,$fce0,$be60
	dc.w $ffe0,$f7e0,$7380,$0000,$a5c0,$9400,$6080,$0f00
	dc.w $9500,$1c00,$f800,$f800,$fc00,$0400,$9600,$8600
	dc.w $8b00,$8300,$ff00,$0400,$0a00,$f100,$5f00,$e000
	dc.w $6200,$7c00,$2300,$3c00,$2f80,$3680,$6a40,$7e40
	dc.w $4f00,$24c0,$6ed0,$5b9c,$b25a,$2dc2,$3ffe,$39c2
	dc.w $2624,$1fe2
	dc.l $0000,$0000

	dc.w $0000,$0000
	dc.w $0000,$0000,$0000,$2000,$1800,$0000,$1e00,$0700
	dc.w $0500,$0700,$0000,$0700,$0000,$0f80,$e7c0,$e7c0
	dc.w $0000,$ffc0,$f9e0,$f9e0,$9920,$1f20,$3e60,$3960
	dc.w $f7e0,$f8e0,$0000,$ffc0,$8400,$7bc0,$0000,$ff80
	dc.w $1c00,$e300,$f800,$f800,$0000,$0400,$7800,$fe00
	dc.w $7c00,$ff00,$0000,$0400,$0000,$ff00,$4000,$ff00
	dc.w $6000,$7f00,$2000,$3f00,$2100,$3100,$6980,$7800
	dc.w $1400,$0400,$4260,$4240,$203c,$2018,$39c2,$39c2
	dc.w $001c,$0004
	dc.l $0000,$0000

kid6:
	dc.w $0000,$0000
	dc.w $0001,$0003,$0002,$0005,$000c,$0012,$0034,$0001
	dc.w $0035,$0004,$0037,$0006,$057f,$019f,$03ff,$0003
	dc.w $00ff,$006f,$005e,$00bc,$02be,$028f,$0703,$0542
	dc.w $05b1,$05b2,$031a,$0313,$00ed,$0009,$004f,$018a
	dc.w $029f,$0117,$033f,$003f,$00b7,$0045,$02e3,$0203
	dc.w $01cf,$004f,$009e,$011e,$002d,$01cc,$0013,$01e0
	dc.w $000f,$01f0,$020b,$01f8,$00d3,$03af,$034d,$00d4
	dc.w $01b6,$007e,$01e7,$0198,$0167,$00e7,$0000,$0000
	dc.w $0000,$0000
	dc.l $0000,$0000

	dc.w $0000,$0000
	dc.w $0000,$0001,$0000,$0000,$0001,$0001,$001a,$001a
	dc.w $003a,$003e,$0038,$003e,$0e1f,$001f,$0003,$0003
	dc.w $006f,$009f,$009c,$006f,$008f,$027c,$0243,$07bc
	dc.w $0233,$07cc,$00e3,$03fc,$01f1,$01fe,$03f2,$03da
	dc.w $03e7,$03b7,$03c3,$037f,$03f9,$02f5,$01fd,$03ef
	dc.w $0030,$007f,$001e,$01e1,$000c,$01f3,$0000,$01ff
	dc.w $0000,$01ff,$0007,$01e2,$0040,$0040,$0023,$0000
	dc.w $0001,$0000,$0180,$0180,$0018,$0000,$0000,$0000
	dc.w $0000,$0000
	dc.l $0000,$0000

	dc.w $0000,$0000
	dc.w $8800,$fe00,$0a00,$39c0,$6940,$9860,$f680,$0ea0
	dc.w $c1e0,$3fc0,$fe70,$0000,$fff0,$f9f0,$cc40,$b030
	dc.w $fef8,$ff78,$ffd8,$5668,$7f38,$ef98,$7ff8,$bdf8
	dc.w $9ce0,$4000,$6970,$e500,$d820,$c3c0,$6540,$8700
	dc.w $fe00,$fe00,$ff00,$8100,$e580,$6180,$e2c0,$a0c0
	dc.w $ffc0,$4100,$c500,$58c0,$8600,$1980,$f000,$1e00
	dc.w $7000,$fc00,$2000,$7c00,$f800,$fc00,$bc00,$2a00
	dc.w $f500,$0600,$0e80,$f6e0,$7ed0,$0c10,$01f0,$0010
	dc.w $0120,$0110
	dc.l $0000,$0000

	dc.w $0000,$0000
	dc.w $0000,$8800,$c600,$c000,$0780,$01c0,$0140,$01c0
	dc.w $0000,$01c0,$0000,$03e0,$f9f0,$f9f0,$8000,$bff0
	dc.w $fe78,$fe78,$6648,$c7c8,$cf98,$4e58,$bdf8,$7e38
	dc.w $c000,$3ff0,$e100,$1ef0,$c000,$3fe0,$0700,$f8c0
	dc.w $fe00,$fe00,$8000,$8100,$1e00,$7f80,$9f00,$bfc0
	dc.w $4000,$4100,$4000,$bfc0,$0000,$ff80,$1000,$fe00
	dc.w $7000,$fc00,$a000,$3c00,$f800,$f800,$c800,$8800
	dc.w $fc00,$9400,$0500,$0400,$f5e0,$64c0,$0010,$0010
	dc.w $00e0,$0020
	dc.l $0000,$0000

kid7:
	dc.w $0000,$0000
	dc.w $0000,$0000,$0001,$0003,$0002,$0005,$000c,$0012
	dc.w $0034,$0001,$0035,$0004,$0037,$0006,$057f,$019f
	dc.w $07ff,$0003,$00ff,$006f,$005e,$00bc,$02be,$028f
	dc.w $0f03,$0542,$05b1,$05b2,$031a,$0313,$00ed,$0009
	dc.w $004f,$018a,$029f,$0117,$033f,$003f,$00b7,$0045
	dc.w $02e3,$0203,$019f,$009e,$009e,$011e,$002d,$01cc
	dc.w $0013,$01e0,$000f,$00f0,$0004,$003e,$0007,$0011
	dc.w $000d,$001a,$0014,$000d,$001b,$0007,$001e,$0019
	dc.w $0016,$000e
	dc.l $0000,$0000

	dc.w $0000,$0000
	dc.w $0000,$0000,$0000,$0001,$0000,$0000,$0001,$0001
	dc.w $001a,$001a,$003a,$003e,$0038,$003e,$0e1f,$001f
	dc.w $0003,$0003,$006f,$009f,$009c,$006f,$008f,$027c
	dc.w $0243,$07bc,$0233,$07cc,$00e3,$03fc,$01f1,$01fe
	dc.w $03f2,$03da,$03e7,$03b7,$03c3,$037f,$03f9,$02f5
	dc.w $01fd,$03ef,$0060,$00fe,$001e,$01e1,$000c,$01f3
	dc.w $0000,$01ff,$0000,$00ff,$0005,$003c,$0008,$0000
	dc.w $0004,$0004,$0002,$0000,$0000,$0000,$0018,$0018
	dc.w $0001,$0000
	dc.l $0000,$0000

	dc.w $0000,$0000
	dc.w $0000,$0000,$8800,$fe00,$0a00,$39c0,$6940,$9860
	dc.w $f680,$0ea0,$c1e0,$3fc0,$fe70,$0000,$fff0,$f9f0
	dc.w $cc40,$b030,$fef8,$ff78,$ffd8,$5668,$7f38,$ef98
	dc.w $7ff0,$bdf0,$9ce0,$4000,$6970,$e500,$d820,$c3c0
	dc.w $6540,$8700,$ff00,$fe80,$ff00,$8100,$e580,$6180
	dc.w $e2c0,$a0c0,$ffc0,$8200,$8500,$78c0,$9e00,$2180
	dc.w $e000,$7e00,$e000,$6000,$5000,$0000,$6000,$0000
	dc.w $3800,$fe00,$db00,$4300,$6f00,$e000,$7080,$8f00
	dc.w $7780,$7080
	dc.l $0000,$0000

	dc.w $0000,$0000
	dc.w $0000,$0000,$0000,$8800,$c600,$c000,$0780,$01c0
	dc.w $0140,$01c0,$0000,$01c0,$0000,$03e0,$f9f0,$f9f0
	dc.w $8000,$bff0,$fe78,$fe78,$6648,$c7c8,$cf98,$4e58
	dc.w $bdf0,$7e30,$c000,$3ff0,$e100,$1ef0,$c000,$3fe0
	dc.w $0700,$f8c0,$fe00,$fe00,$8000,$8100,$1e00,$7f80
	dc.w $9f00,$bfc0,$8000,$8200,$0000,$ffc0,$0000,$ff80
	dc.w $6000,$be00,$a000,$a000,$e000,$0000,$f000,$6000
	dc.w $0000,$0000,$3c00,$0800,$1f80,$0900,$0000,$0000
	dc.w $8f00,$0600
	dc.l $0000,$0000

kid8:
	dc.w $0000,$0000
	dc.w $0000,$0000,$0000,$0000,$0006,$000f,$0008,$0014
	dc.w $0031,$004a,$00d3,$0004,$00d7,$0010,$00df,$0018
	dc.w $15ff,$067f,$0fff,$000e,$03ff,$01bf,$017b,$02f1
	dc.w $0af9,$0a3f,$1c0d,$150a,$16c6,$16c9,$0c69,$0c4f
	dc.w $03b7,$0027,$013d,$062a,$0a7f,$045f,$0cff,$00fe
	dc.w $02df,$0115,$0b8f,$080e,$073f,$013d,$013d,$023c
	dc.w $005b,$0398,$0027,$03c0,$001e,$01e1,$0018,$001f
	dc.w $000d,$000f,$0006,$0005,$0007,$0004,$0007,$0007
	dc.w $0006,$0001
	dc.l $0000,$0000

	dc.w $0000,$0000
	dc.w $0000,$0000,$0000,$0000,$0000,$0006,$0003,$0003
	dc.w $0004,$0004,$0068,$0068,$00e8,$00f8,$00e0,$00f8
	dc.w $387f,$007f,$000e,$000e,$01bf,$027f,$0271,$01bf
	dc.w $023f,$09f1,$090e,$1ef1,$08cf,$1f30,$038f,$0ff0
	dc.w $07c7,$07f8,$0fc8,$0f6b,$0f9f,$0edf,$0f0e,$0dfe
	dc.w $0fe4,$0bd5,$07f6,$0fbe,$00c1,$01fd,$003c,$03c3
	dc.w $0018,$03e7,$0000,$03ff,$0000,$01ff,$0018,$001f
	dc.w $000c,$000c,$0004,$0004,$0004,$0004,$0007,$0007
	dc.w $0000,$0000
	dc.l $0000,$0000

	dc.w $0000,$0000
	dc.w $0000,$0000,$0000,$0000,$2000,$f800,$2800,$e700
	dc.w $a500,$6180,$da00,$3a80,$0780,$ff40,$f9c0,$0000
	dc.w $ffc0,$e7c0,$3100,$c0c0,$fbe0,$fde0,$ff60,$59a0
	dc.w $fce0,$be60,$ffe0,$f7e0,$7380,$0000,$a5c0,$9400
	dc.w $6080,$0f00,$9500,$1c00,$f800,$f800,$fc00,$0400
	dc.w $9600,$8600,$8b00,$8300,$ff80,$0400,$7a00,$8180
	dc.w $2400,$c300,$1400,$e000,$0800,$c000,$6c00,$a000
	dc.w $2600,$de00,$9b70,$a870,$6de0,$fc00,$ce10,$31e0
	dc.w $cef0,$ce10
	dc.l $0000,$0000

	dc.w $0000,$0000
	dc.w $0000,$0000,$0000,$0000,$0000,$2000,$1800,$0000
	dc.w $1e00,$0700,$0500,$0700,$0000,$0700,$0000,$0f80
	dc.w $e7c0,$e7c0,$0000,$ffc0,$f9e0,$f9e0,$9920,$1f20
	dc.w $3e60,$3960,$f7e0,$f8e0,$0000,$ffc0,$8400,$7bc0
	dc.w $0000,$ff80,$1c00,$e300,$f800,$f800,$0000,$0400
	dc.w $7800,$fe00,$7c00,$ff00,$0000,$0400,$0000,$ff80
	dc.w $0000,$ff00,$0000,$fc00,$3c00,$8000,$1e00,$8c00
	dc.w $0000,$8000,$4790,$0110,$03f0,$0120,$0000,$0000
	dc.w $31e0,$00c0
	dc.l $0000,$0000



kid10:
	dc.w $0000,$0000
	dc.w $0004,$001f,$0014,$00e7,$00a5,$0186,$005b,$015c
	dc.w $01e0,$00ff,$039f,$0000,$03ff,$03e7,$008c,$0303
	dc.w $07df,$07bf,$06ff,$059a,$073f,$067d,$07ff,$07ef
	dc.w $01ce,$0000,$03a5,$0029,$0106,$00f0,$00a9,$0038
	dc.w $001f,$001f,$003f,$0020,$0069,$0061,$00d1,$00c1
	dc.w $01ff,$0020,$00d0,$010f,$0079,$0080,$0059,$003d
	dc.w $002d,$005e,$0006,$003f,$000f,$001b,$0007,$0019
	dc.w $000f,$0033,$00ae,$03b6,$0557,$0460,$03f8,$0037
	dc.w $065f,$0468
	dc.l $0000,$0000

	dc.w $0000,$0000
	dc.w $0000,$0004,$0018,$0000,$0078,$00e0,$00a0,$00e0
	dc.w $0000,$00e0,$0000,$01f0,$03e7,$03e7,$0000,$03ff
	dc.w $079f,$079f,$0499,$04f8,$067c,$069c,$07ef,$071f
	dc.w $0000,$03ff,$0021,$03de,$0000,$01ff,$0038,$00c7
	dc.w $001f,$001f,$0000,$0020,$001e,$007f,$003e,$00ff
	dc.w $0000,$0020,$0000,$01ff,$0000,$00ff,$0019,$007e
	dc.w $000c,$007f,$0006,$003f,$0003,$0003,$0001,$0001
	dc.w $0003,$0003,$0041,$0000,$038f,$0104,$0030,$0030
	dc.w $0387,$0203
	dc.l $0000,$0000

	dc.w $0000,$0000
	dc.w $6000,$f000,$1000,$2800,$8c00,$5200,$cb00,$2000
	dc.w $eb00,$0800,$fb00,$1800,$ffa8,$fe60,$fff0,$7000
	dc.w $ffc0,$fd80,$de80,$8f40,$9f50,$fc50,$b038,$50a8
	dc.w $6368,$9368,$9630,$f230,$edc0,$e400,$bc80,$5460
	dc.w $fe50,$fa20,$ff30,$7f00,$fb40,$a880,$f1d0,$7010
	dc.w $fce0,$bc80,$b140,$3f30,$de80,$1e70,$ed00,$0cf0
	dc.w $7e00,$01e0,$f800,$07c0,$1000,$c380,$b600,$8580
	dc.w $e580,$fa80,$d940,$1580,$b6c0,$3f00,$73c0,$8c00
	dc.w $75c0,$7600
	dc.l $0000,$0000

	dc.w $0000,$0000
	dc.w $0000,$6000,$c000,$c000,$2000,$2000,$1600,$1600
	dc.w $1700,$1f00,$0700,$1f00,$fe1c,$fe00,$7000,$7000
	dc.w $fd80,$fe40,$8e40,$fd80,$fc40,$8f90,$7090,$8f78
	dc.w $f310,$0cf8,$f1c0,$0ff0,$e3e0,$1fe0,$13f0,$d6f0
	dc.w $f9f0,$fb70,$70f0,$7fb0,$27f0,$abd0,$6fe0,$7df0
	dc.w $8300,$bf80,$3f00,$c0f0,$1e00,$e1f0,$0c00,$f3f0
	dc.w $0000,$ffe0,$0000,$ffc0,$3c00,$c180,$f800,$b180
	dc.w $8100,$8100,$e200,$8000,$c000,$8000,$0000,$0000
	dc.w $8800,$0000
	dc.l $0000,$0000

kid11:
	dc.w $0000,$0000
	dc.w $0000,$0000,$0008,$003f,$0028,$01ce,$014b,$030c
	dc.w $00b7,$02b8,$03c1,$01fe,$073f,$0000,$07ff,$07cf
	dc.w $0119,$0606,$0fbf,$0f7f,$0dff,$0b35,$0e7f,$0cfb
	dc.w $07ff,$07de,$039c,$0001,$074b,$0053,$020d,$01e1
	dc.w $0153,$0070,$007f,$00bf,$007f,$0040,$00d3,$00c3
	dc.w $01a3,$0182,$01ff,$0020,$0050,$018f,$003c,$00c2
	dc.w $0003,$003f,$0003,$0003,$0005,$0000,$0003,$0000
	dc.w $000e,$003f,$006d,$0061,$007b,$0003,$0087,$0078
	dc.w $00f7,$0087
	dc.l $0000,$0000

	dc.w $0000,$0000
	dc.w $0000,$0000,$0000,$0008,$0031,$0001,$00f0,$01c0
	dc.w $0140,$01c0,$0000,$01c0,$0000,$03e0,$07cf,$07cf
	dc.w $0000,$07fe,$0f3f,$0f3f,$0933,$09f1,$0cf9,$0d39
	dc.w $07de,$063f,$0001,$07fe,$0043,$07bc,$0001,$03fe
	dc.w $0070,$018f,$003f,$003f,$0000,$0040,$003c,$00ff
	dc.w $007c,$01fe,$0000,$0020,$0000,$01ff,$0000,$00ff
	dc.w $0003,$003e,$0002,$0002,$0003,$0000,$0007,$0003
	dc.w $0000,$0000,$001e,$0008,$00fc,$0048,$0000,$0000
	dc.w $0078,$0030
	dc.l $0000,$0000

	dc.w $0000,$0000
	dc.w $0000,$0000,$c000,$e000,$2000,$5000,$1800,$a400
	dc.w $9600,$4000,$d600,$1000,$f600,$3000,$ff50,$fcc0
	dc.w $fff0,$e000,$ff80,$fb00,$bd00,$1e80,$3ea0,$f8a0
	dc.w $6078,$a150,$c6d0,$26d0,$2c60,$e460,$db80,$c800
	dc.w $7900,$a8c0,$fca0,$f440,$fe60,$fe00,$f680,$5100
	dc.w $e3a0,$e020,$fcc0,$bc80,$bc80,$3c40,$da00,$19c0
	dc.w $e400,$03c0,$f800,$0780,$1000,$3e00,$7000,$4400
	dc.w $5800,$ac00,$9400,$5800,$6c00,$f000,$3c00,$cc00
	dc.w $3400,$3800
	dc.l $0000,$0000

	dc.w $0000,$0000
	dc.w $0000,$0000,$0000,$c000,$8000,$8000,$4000,$4000
	dc.w $2c00,$2c00,$2e00,$3e00,$0e00,$3e00,$fc38,$fc00
	dc.w $e000,$e000,$fb00,$fc80,$1c80,$fb00,$f880,$1f20
	dc.w $e120,$1ef0,$e620,$19f0,$e380,$1fe0,$c7c0,$3fc0
	dc.w $27e0,$ade0,$f3e0,$f6e0,$e1e0,$ff60,$4fe0,$57a0
	dc.w $dfc0,$fbe0,$8300,$bf80,$3c00,$c3c0,$1800,$e7c0
	dc.w $0000,$ffc0,$8000,$ff80,$d000,$1e00,$8800,$0000
	dc.w $1000,$1000,$2000,$0000,$0000,$0000,$0c00,$0c00
	dc.w $c000,$0000
	dc.l $0000,$0000

kid12:
	dc.w $0000,$0000
	dc.w $0008,$003f,$0028,$01ce,$014b,$030c,$00b7,$02b8
	dc.w $03c1,$01fe,$073f,$0000,$07ff,$07cf,$0119,$0606
	dc.w $0fbf,$0f7f,$0dff,$0b35,$0e7f,$0cfb,$0fff,$0fde
	dc.w $039c,$0001,$074b,$0053,$020d,$01e1,$0153,$0070
	dc.w $003f,$003f,$007f,$0040,$00d3,$00c3,$01a3,$0182
	dc.w $01ff,$0041,$0050,$018f,$0039,$00c0,$0019,$007d
	dc.w $000d,$007e,$0027,$0019,$000f,$0033,$04b7,$039f
	dc.w $05a7,$043b,$07fe,$043e,$0247,$0470,$0008,$0007
	dc.w $000f,$0008
	dc.l $0000,$0000

	dc.w $0000,$0000
	dc.w $0000,$0008,$0031,$0001,$00f0,$01c0,$0140,$01c0
	dc.w $0000,$01c0,$0000,$03e0,$07cf,$07cf,$0000,$07fe
	dc.w $0f3f,$0f3f,$0933,$09f1,$0cf9,$0d39,$0fde,$0e3f
	dc.w $0001,$07fe,$0043,$07bc,$0001,$03fe,$0070,$018f
	dc.w $003f,$003f,$0000,$0040,$003c,$00ff,$007c,$01fe
	dc.w $0001,$0041,$0000,$01ff,$0000,$00ff,$0019,$007e
	dc.w $000c,$007f,$0001,$0001,$0003,$0003,$0067,$0027
	dc.w $03c3,$0183,$0439,$0438,$038f,$0204,$0000,$0000
	dc.w $0007,$0003
	dc.l $0000,$0000

	dc.w $0000,$0000
	dc.w $c000,$e000,$2000,$5000,$1800,$a400,$9600,$4000
	dc.w $d600,$1000,$f600,$3000,$ff50,$fcc0,$ffe0,$e000
	dc.w $ff80,$fb00,$bd00,$1e80,$3ea0,$f8a0,$6070,$a150
	dc.w $c6d0,$26d0,$2c60,$e460,$db80,$c800,$7900,$a8c0
	dc.w $fca0,$f440,$fe60,$fe00,$f680,$5100,$e3a0,$e020
	dc.w $f9c0,$7900,$bc80,$3c40,$da00,$19c0,$e400,$03c0
	dc.w $7800,$0780,$f880,$0780,$d040,$c380,$b600,$85e0
	dc.w $e590,$fae0,$d970,$1580,$b6e0,$3f10,$73f0,$8ce0
	dc.w $7370,$73b0
	dc.l $0000,$0000

	dc.w $0000,$0000
	dc.w $0000,$c000,$8000,$8000,$4000,$4000,$2c00,$2c00
	dc.w $2e00,$3e00,$0e00,$3e00,$fc38,$fc00,$e000,$e000
	dc.w $fb00,$fc80,$1c80,$fb00,$f880,$1f20,$e120,$1ef0
	dc.w $e620,$19f0,$e380,$1fe0,$c7c0,$3fc0,$27e0,$ade0
	dc.w $f3e0,$f6e0,$e1e0,$ff60,$4fe0,$57a0,$dfc0,$fbe0
	dc.w $0600,$7f00,$3c00,$c3c0,$1800,$e7c0,$0000,$ffc0
	dc.w $0000,$ff80,$0080,$ff80,$fc00,$c180,$f800,$b180
	dc.w $8100,$8100,$e200,$8000,$c000,$8000,$00e0,$00e0
	dc.w $8c30,$0030
	dc.l $0000,$0000

kid13:
	dc.w $0000,$0000
	dc.w $0000,$0000,$0002,$000f,$000a,$0073,$0052,$00c3
	dc.w $002d,$00ae,$00f0,$017f,$01cf,$0000,$01ff,$01f3
	dc.w $0046,$0181,$03ef,$03df,$037f,$02cd,$039f,$033e
	dc.w $03ff,$03f7,$00e7,$0000,$01d2,$0014,$0083,$0078
	dc.w $0054,$001c,$000f,$000f,$001f,$0010,$0034,$0030
	dc.w $0068,$0060,$00ff,$0010,$0028,$00c7,$001c,$00e0
	dc.w $000c,$00fe,$00fe,$00b6,$012c,$013c,$0078,$0198
	dc.w $05bc,$1ce0,$2d24,$21d8,$1ffc,$01cc,$1232,$23fc
	dc.w $0000,$0000
	dc.l $0000,$0000

	dc.w $0000,$0000
	dc.w $0000,$0000,$0000,$0002,$000c,$0000,$003c,$0070
	dc.w $0050,$0070,$0000,$0070,$0000,$00f8,$01f3,$01f3
	dc.w $0000,$01ff,$03cf,$03cf,$024c,$027c,$033e,$034e
	dc.w $03f7,$038f,$0000,$01ff,$0010,$01ef,$0000,$00ff
	dc.w $001c,$0063,$000f,$000f,$0000,$0010,$000f,$003f
	dc.w $001f,$007f,$0000,$0010,$0000,$00ff,$0000,$00ff
	dc.w $000c,$00ff,$0046,$0046,$00cc,$000c,$0018,$0018
	dc.w $0320,$0120,$1e00,$0c00,$01cc,$01cc,$1c00,$1000
	dc.w $0000,$0000
	dc.l $0000,$0000

	dc.w $0000,$0000
	dc.w $0000,$0000,$3000,$f800,$0800,$9400,$c600,$2900
	dc.w $e580,$1000,$7580,$8400,$fd80,$0c00,$ffd4,$ff30
	dc.w $7ff8,$b800,$ffe0,$fec0,$ef40,$47a0,$cfa8,$fe28
	dc.w $d81c,$a854,$31b4,$49b4,$cb18,$f918,$76e0,$7200
	dc.w $de40,$2a30,$ff28,$fd10,$ff98,$3f80,$fda0,$d440
	dc.w $f8e8,$b808,$fe70,$5e40,$5e40,$9e20,$6d00,$0ce0
	dc.w $7200,$01f0,$3c00,$43f8,$06c0,$3cbc,$0b20,$001e
	dc.w $032c,$03d6,$36ca,$70ac,$3db7,$01f8,$439f,$3c66
	dc.w $7b9a,$439c
	dc.l $0000,$0000

	dc.w $0000,$0000
	dc.w $0000,$0000,$0000,$3000,$6000,$6000,$1000,$1000
	dc.w $0b00,$0b00,$0b80,$0f80,$0380,$0f80,$ff0e,$ff00
	dc.w $3800,$b800,$fec0,$ff20,$c720,$7ec0,$7e20,$47c8
	dc.w $b848,$c7bc,$7988,$867c,$f8e0,$07f8,$71f0,$8ff0
	dc.w $09f8,$eb78,$fcf8,$fdb8,$3878,$3fd8,$13f8,$d5e8
	dc.w $37f0,$bef8,$4180,$5fc0,$1e00,$e1e0,$0c00,$f3e0
	dc.w $0000,$fff0,$0000,$7ff8,$0300,$383c,$07c0,$031e
	dc.w $0008,$0008,$0f10,$0400,$7e00,$2400,$0006,$0006
	dc.w $3c60,$1800
	dc.l $0000,$0000

kid14:
	dc.w $0000,$0000
	dc.w $0000,$0000,$0000,$0000,$0004,$001f,$0014,$00e7
	dc.w $00a5,$0186,$005b,$015c,$01e0,$02ff,$039f,$0000
	dc.w $03ff,$03e7,$008c,$0303,$07df,$07bf,$06ff,$059a
	dc.w $073f,$067d,$07ff,$07ef,$01ce,$0000,$03a5,$0029
	dc.w $0106,$00f0,$00a9,$0038,$001f,$001f,$003f,$0020
	dc.w $0069,$0061,$00d1,$00c1,$01ff,$0020,$0128,$00c7
	dc.w $0080,$007f,$01b8,$0079,$07d0,$05b0,$0950,$19f0
	dc.w $13c8,$0c90,$2dd0,$e760,$6920,$0ec0,$fff0,$0e70
	dc.w $9190,$1fe0
	dc.l $0000,$0000

	dc.w $0000,$0000
	dc.w $0000,$0000,$0000,$0000,$0000,$0004,$0018,$0000
	dc.w $0078,$00e0,$00a0,$00e0,$0000,$00e0,$0000,$01f0
	dc.w $03e7,$03e7,$0000,$03ff,$079f,$079f,$0499,$04f8
	dc.w $067c,$069c,$07ef,$071f,$0000,$03ff,$0021,$03de
	dc.w $0000,$01ff,$0038,$00c7,$001f,$001f,$0000,$0020
	dc.w $001e,$007f,$003e,$00ff,$0000,$0020,$0000,$00ff
	dc.w $0000,$01ff,$0038,$01f9,$0210,$0230,$0650,$0070
	dc.w $00a0,$0080,$1900,$0900,$f000,$6000,$0e70,$0e70
	dc.w $e000,$8000
	dc.l $0000,$0000

	dc.w $0000,$0000
	dc.w $0000,$0000,$0000,$0000,$6000,$f000,$1000,$2800
	dc.w $8c00,$5200,$cb00,$2000,$eb00,$0800,$fb00,$1800
	dc.w $ffa8,$fe60,$fff0,$7000,$ffc0,$fd80,$de80,$8f40
	dc.w $9f50,$fc50,$b038,$50a8,$6368,$9368,$9630,$f230
	dc.w $edc0,$e400,$bc80,$5460,$fe50,$fa20,$ff30,$7f00
	dc.w $fb40,$a880,$f1d0,$7010,$fce0,$bc80,$5e40,$9e20
	dc.w $ed04,$0cfc,$720a,$81f0,$3c14,$43e6,$003a,$1fec
	dc.w $0344,$007a,$0528,$0434,$1670,$004c,$1fc0,$0030
	dc.w $0900,$09e0
	dc.l $0000,$0000

	dc.w $0000,$0000
	dc.w $0000,$0000,$0000,$0000,$0000,$6000,$c000,$c000
	dc.w $2000,$2000,$1600,$1600,$1700,$1f00,$0700,$1f00
	dc.w $fe1c,$fe00,$7000,$7000,$fd80,$fe40,$8e40,$fd80
	dc.w $fc40,$8f90,$7090,$8f78,$f310,$0cf8,$f1c0,$0ff0
	dc.w $e3e0,$1fe0,$13f0,$d6f0,$f9f0,$fb70,$70f0,$7fb0
	dc.w $27f0,$abd0,$6fe0,$7df0,$8300,$bf80,$1e00,$e1e0
	dc.w $0c00,$f3f8,$000c,$fff8,$0008,$7fe0,$0000,$1fc0
	dc.w $0780,$0000,$03c0,$0000,$0f80,$0000,$0000,$0000
	dc.w $0600,$0000
	dc.l $0000,$0000

kid15:
	dc.w $0000,$0000
	dc.w $0000,$0000,$0002,$000f,$000a,$0073,$0052,$00c3
	dc.w $002d,$00ae,$00f0,$007f,$01cf,$0000,$01ff,$01f3
	dc.w $0046,$0181,$03ef,$03df,$037f,$02cd,$039f,$033e
	dc.w $03ff,$03f7,$00e7,$0000,$01d2,$0014,$0083,$0078
	dc.w $0054,$001c,$000f,$000f,$001f,$0010,$0034,$0030
	dc.w $0068,$0060,$007f,$0010,$0028,$0047,$007d,$0003
	dc.w $0023,$001f,$0062,$001e,$00fa,$00b6,$012b,$013f
	dc.w $0079,$0192,$05bb,$1ced,$2d26,$21da,$3ffe,$21ce
	dc.w $1232,$23fc
	dc.l $0000,$0000

	dc.w $0000,$0000
	dc.w $0000,$0000,$0000,$0002,$000c,$0000,$003c,$0070
	dc.w $0050,$0070,$0000,$0070,$0000,$00f8,$01f3,$01f3
	dc.w $0000,$01ff,$03cf,$03cf,$024c,$027c,$033e,$034e
	dc.w $03f7,$038f,$0000,$01ff,$0010,$01ef,$0000,$00ff
	dc.w $001c,$0063,$000f,$000f,$0000,$0010,$000f,$003f
	dc.w $001f,$007f,$0000,$0010,$0000,$007f,$0001,$007f
	dc.w $0003,$007f,$0002,$007e,$0042,$0046,$00cb,$000f
	dc.w $0014,$0010,$0321,$0121,$1e02,$0c02,$21ce,$21ce
	dc.w $1c00,$1000
	dc.l $0000,$0000

	dc.w $0000,$0000
	dc.w $0000,$0000,$3000,$f800,$0800,$9400,$c600,$2900
	dc.w $e580,$1000,$7580,$8400,$fd80,$0c00,$ffd4,$ff30
	dc.w $7ff8,$b800,$ffe0,$fec0,$ef40,$47a0,$cfa8,$fe28
	dc.w $d81c,$a854,$31b4,$49b4,$cb18,$f918,$76e0,$7200
	dc.w $de40,$2a30,$ff28,$fd10,$ff98,$3f80,$fda0,$d440
	dc.w $f8e8,$b808,$fe70,$5e40,$5e40,$9e20,$6d00,$8cf0
	dc.w $7200,$01f8,$3c00,$03fc,$0680,$00fc,$0f2c,$0bd6
	dc.w $36ca,$30ac,$3db6,$01f8,$c39e,$3c66,$7b9a,$439c
	dc.w $0000,$0000
	dc.l $0000,$0000

	dc.w $0000,$0000
	dc.w $0000,$0000,$0000,$3000,$6000,$6000,$1000,$1000
	dc.w $0b00,$0b00,$0b80,$0f80,$0380,$0f80,$ff0e,$ff00
	dc.w $3800,$b800,$fec0,$ff20,$c720,$7ec0,$7e20,$47c8
	dc.w $b848,$c7bc,$7988,$867c,$f8e0,$07f8,$71f0,$8ff0
	dc.w $09f8,$eb78,$fcf8,$fdb8,$3878,$3fd8,$13f8,$d5e8
	dc.w $37f0,$bef8,$4180,$5fc0,$1e00,$e1e0,$0c00,$f3f0
	dc.w $0000,$7ff8,$0000,$3ffc,$0f00,$023c,$0008,$0808
	dc.w $0f10,$0400,$7e00,$2400,$0006,$0006,$3c60,$1800
	dc.w $0000,$0000
	dc.l $0000,$0000

kid16:
	dc.w $0000,$0000
	dc.w $0011,$007f,$0050,$039c,$0296,$0619,$016f,$0570
	dc.w $0783,$03fc,$0e7f,$0000,$0fff,$0f9f,$0233,$0c0d
	dc.w $1f7f,$1eff,$1bff,$166a,$1cfe,$19f7,$1ffe,$1fbd
	dc.w $0739,$0002,$0e96,$00a7,$041b,$03c3,$02a6,$00e1
	dc.w $007f,$007f,$00ff,$0081,$01a7,$0186,$0347,$0305
	dc.w $03ff,$0082,$00a3,$031a,$0061,$0198,$000f,$0078
	dc.w $000e,$003f,$0004,$003e,$001f,$003f,$003d,$0054
	dc.w $00af,$0060,$0170,$076f,$0b7e,$0830,$0f80,$0800
	dc.w $0480,$0880
	dc.l $0000,$0000

	dc.w $0000,$0000
	dc.w $0000,$0011,$0063,$0003,$01e0,$0380,$0280,$0380
	dc.w $0000,$0380,$0000,$07c0,$0f9f,$0f9f,$0001,$0ffd
	dc.w $1e7f,$1e7f,$1266,$13e3,$19f3,$1a72,$1fbd,$1c7e
	dc.w $0003,$0ffc,$0087,$0f78,$0003,$07fc,$00e0,$031f
	dc.w $007f,$007f,$0001,$0081,$0078,$01fe,$00f9,$03fd
	dc.w $0002,$0082,$0002,$03fd,$0000,$01ff,$0008,$007f
	dc.w $000e,$003f,$0005,$003c,$001f,$001f,$0013,$0011
	dc.w $003f,$0029,$00a0,$0020,$07af,$0326,$0800,$0800
	dc.w $0700,$0400
	dc.l $0000,$0000

	dc.w $0000,$0000
	dc.w $8000,$c000,$4000,$a000,$3000,$4800,$2c00,$8000
	dc.w $ac00,$2000,$ec00,$6000,$fea0,$f980,$ffc0,$c000
	dc.w $ff00,$f600,$7a00,$3d00,$7d40,$f140,$c0e0,$42a0
	dc.w $8da0,$4da0,$58c0,$c8c0,$b700,$9000,$f200,$5180
	dc.w $f940,$e880,$fcc0,$fc00,$ed00,$a200,$c740,$c040
	dc.w $f380,$f200,$7900,$7880,$b400,$3380,$c800,$0780
	dc.w $f000,$0f80,$d040,$1f80,$cb00,$f5c0,$b2c0,$2b00
	dc.w $6d80,$7e00,$e780,$1980,$e680,$e700,$0000,$0000
	dc.w $0000,$0000
	dc.l $0000,$0000

	dc.w $0000,$0000
	dc.w $0000,$8000,$0000,$0000,$8000,$8000,$5800,$5800
	dc.w $5c00,$7c00,$1c00,$7c00,$f870,$f800,$c000,$c000
	dc.w $f600,$f900,$3900,$f600,$f100,$3e40,$c240,$3de0
	dc.w $cc40,$33e0,$c700,$3fc0,$8f80,$7f80,$4fc0,$5bc0
	dc.w $e7c0,$edc0,$c3c0,$fec0,$9fc0,$af40,$bf80,$f7c0
	dc.w $0c00,$fe00,$7800,$8780,$3000,$cf80,$0000,$ff80
	dc.w $0000,$ff80,$e000,$4780,$0200,$0200,$c400,$0000
	dc.w $8000,$0000,$0180,$0180,$1800,$0000,$0000,$0000
	dc.w $0000,$0000
	dc.l $0000,$0000

kid17:
	dc.w $0000,$0000
	dc.w $0000,$0000,$0000,$0000,$0008,$003f,$0028,$01ce
	dc.w $014b,$030c,$00b7,$02b8,$03c1,$05fe,$073f,$0000
	dc.w $07ff,$07cf,$0119,$0606,$0fbf,$0f7f,$0dff,$0b35
	dc.w $0e7f,$0cfb,$0fff,$0fde,$039c,$0001,$074b,$0053
	dc.w $020d,$01e1,$0153,$0070,$003f,$003f,$007f,$0040
	dc.w $00d3,$00c3,$01a3,$0182,$03ff,$0041,$00bd,$0302
	dc.w $0049,$0186,$0051,$000e,$0020,$0007,$006c,$000b
	dc.w $00c9,$00f7,$1db2,$1c2b,$0f6d,$007e,$10e7,$0f19
	dc.w $1ee6,$10e7
	dc.l $0000,$0000

	dc.w $0000,$0000
	dc.w $0000,$0000,$0000,$0000,$0000,$0008,$0031,$0001
	dc.w $00f0,$01c0,$0140,$01c0,$0000,$01c0,$0000,$03e0
	dc.w $07cf,$07cf,$0000,$07fe,$0f3f,$0f3f,$0933,$09f1
	dc.w $0cf9,$0d39,$0fde,$0e3f,$0001,$07fe,$0043,$07bc
	dc.w $0001,$03fe,$0070,$018f,$003f,$003f,$0000,$0040
	dc.w $003c,$00ff,$007c,$01fe,$0001,$0041,$0000,$03ff
	dc.w $0000,$01ff,$0000,$007f,$0078,$0003,$00f0,$0063
	dc.w $0000,$0002,$13c4,$1100,$1f80,$0900,$0001,$0001
	dc.w $0f18,$0600
	dc.l $0000,$0000

	dc.w $0000,$0000
	dc.w $0000,$0000,$0000,$0000,$c000,$e000,$2000,$5000
	dc.w $1800,$a400,$9600,$4000,$d600,$1000,$f600,$3000
	dc.w $ff50,$fcc0,$ffe0,$e000,$ff80,$fb00,$bd00,$1e80
	dc.w $3ea0,$f8a0,$6070,$a150,$c6d0,$26d0,$2c60,$e460
	dc.w $db80,$c800,$7900,$a8c0,$fca0,$f440,$fe60,$fe00
	dc.w $f680,$5100,$e3a0,$e020,$f9c0,$7900,$7900,$7880
	dc.w $b400,$3380,$c800,$0780,$f000,$0f00,$3000,$f000
	dc.w $6000,$e000,$c000,$4000,$c000,$4000,$c000,$c000
	dc.w $c000,$0000
	dc.l $0000,$0000

	dc.w $0000,$0000
	dc.w $0000,$0000,$0000,$0000,$0000,$c000,$8000,$8000
	dc.w $4000,$4000,$2c00,$2c00,$2e00,$3e00,$0e00,$3e00
	dc.w $fc38,$fc00,$e000,$e000,$fb00,$fc80,$1c80,$fb00
	dc.w $f880,$1f20,$e120,$1ef0,$e620,$19f0,$e380,$1fe0
	dc.w $c7c0,$3fc0,$27e0,$ade0,$f3e0,$f6e0,$e1e0,$ff60
	dc.w $4fe0,$57a0,$dfc0,$fbe0,$0600,$7f00,$7800,$8780
	dc.w $3000,$cf80,$0000,$ff80,$0000,$ff00,$3000,$f000
	dc.w $6000,$6000,$4000,$4000,$4000,$4000,$c000,$c000
	dc.w $0000,$0000
	dc.l $0000,$0000


smile1:				;132
	dc.w $0000,$0000
	dc.w $0000,$0000,$01ab,$01ec,$0543,$0472,$0441,$0c61
	dc.w $0b81,$07e1,$1cb1,$0330,$1e31,$01fe,$1cf8,$00f8
	dc.w $1ffb,$1ffd,$07ff,$36b1,$3ffb,$2d7f,$07f7,$0bee
	dc.w $06bf,$0980,$3972,$3f3e,$79c0,$187f,$a792,$439e
	dc.w $c7ff,$047f,$37ff,$3980,$4b80,$2d80,$771e,$0500
	dc.w $07c3,$043c,$5540,$727f,$29a1,$263f,$5060,$0f80
	dc.w $2320,$101f,$0cdf,$131f,$07fe,$000e,$0ede,$00ee
	dc.w $0e5a,$006a,$11fb,$0e08,$0eca,$00f2,$0000,$0000
	dc.l $0000,$0000

	dc.w $0000,$0000
	dc.w $0000,$0000,$0010,$0000,$038c,$000e,$039e,$001f
	dc.w $001e,$001f,$0030,$00cf,$0000,$0000,$00f8,$0307
	dc.w $1ff1,$2ff1,$2531,$1c3f,$2c7f,$1471,$0fee,$33f1
	dc.w $0f80,$107f,$0f3e,$30c1,$e040,$7fbf,$fb9e,$ebe1
	dc.w $f87f,$dc7f,$c000,$f180,$f07f,$797f,$f8ff,$7dde
	dc.w $7800,$1c00,$707f,$0f80,$203f,$5fc0,$0000,$7fff
	dc.w $0fc0,$311f,$003f,$101f,$000f,$000e,$0f0f,$0e0e
	dc.w $1f89,$0208,$0008,$0008,$1f01,$0600,$0000,$0000
	dc.l $0000,$0000

	dc.w $0000,$0000
	dc.w $0000,$0000,$f000,$0000,$f400,$1800,$ec00,$1e00
	dc.w $f600,$0a00,$f200,$0b00,$f600,$0f00,$7f00,$0000
	dc.w $ff00,$ff00,$fc00,$ad80,$ff80,$5e80,$fc00,$fa00
	dc.w $ec00,$7200,$f180,$3c00,$7b80,$9800,$fc80,$3d00
	dc.w $fc20,$fc20,$ffe0,$3dc0,$eea0,$f720,$6f60,$7660
	dc.w $ffc0,$0dc0,$b080,$8d80,$3340,$0d00,$e280,$1c40
	dc.w $f700,$8680,$0e00,$7100,$fc00,$0000,$ea00,$0c00
	dc.w $e500,$0600,$1f00,$e000,$ed00,$0e00,$0000,$0000
	dc.l $0000,$0000

	dc.w $0000,$0000
	dc.w $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	dc.w $0400,$0400,$0400,$e000,$0000,$0000,$0000,$f800
	dc.w $ff00,$fe80,$4c80,$0f00,$1e80,$1d00,$fe00,$f980
	dc.w $7e00,$8100,$3c00,$c200,$19e0,$e5a0,$3fe0,$fda0
	dc.w $ffc0,$fe60,$3c00,$3dc0,$05c0,$67e0,$8580,$a7e0
	dc.w $0c00,$0dc0,$8180,$7e40,$0100,$fec0,$0000,$ffc0
	dc.w $7800,$5180,$8000,$0100,$0000,$0000,$f000,$e000
	dc.w $f800,$2000,$0000,$0000,$f000,$6000,$0000,$0000
	dc.l $0000,$0000

smile2:
	dc.w $0000,$0000
	dc.w $0000,$0000,$01ab,$01ec,$0543,$0472,$0441,$0c61
	dc.w $0b81,$07e1,$1cb1,$0330,$1e31,$01fe,$1cf8,$00f8
	dc.w $1ffb,$1ffd,$07ff,$36b1,$3ffb,$2d7f,$07f7,$0bee
	dc.w $06bf,$0980,$3972,$3f3e,$79c0,$187f,$a792,$439e
	dc.w $c7ff,$047f,$37ff,$3980,$4b80,$2d80,$771e,$0500
	dc.w $07c3,$043c,$5540,$727f,$29a1,$263f,$5060,$0f80
	dc.w $2320,$101f,$0cdf,$131f,$07fe,$000e,$0ede,$00ee
	dc.w $0e5a,$006a,$11fb,$0e08,$0eca,$00f2,$0000,$0000
	dc.l $0000,$0000

	dc.w $0000,$0000
	dc.w $0000,$0000,$0010,$0000,$038c,$000e,$039e,$001f
	dc.w $001e,$001f,$0030,$00cf,$0000,$0000,$00f8,$0307
	dc.w $1ff1,$2ff1,$2531,$1c3f,$2c7f,$1471,$0fee,$33f1
	dc.w $0f80,$107f,$0f3e,$30c1,$e040,$7fbf,$fb9e,$ebe1
	dc.w $f87f,$dc7f,$c000,$f180,$f07f,$797f,$f8ff,$7dde
	dc.w $7800,$1c00,$707f,$0f80,$203f,$5fc0,$0000,$7fff
	dc.w $0fc0,$311f,$003f,$101f,$000f,$000e,$0f0f,$0e0e
	dc.w $1f89,$0208,$0008,$0008,$1f01,$0600,$0000,$0000
	dc.l $0000,$0000

	dc.w $0000,$0000
	dc.w $0000,$0000,$f000,$0000,$f400,$1800,$ec00,$1e00
	dc.w $f600,$0a00,$f200,$0b00,$f600,$0f00,$7f00,$0000
	dc.w $ff00,$ff00,$fc00,$ad80,$ff80,$5e80,$e800,$da00
	dc.w $fe20,$7e00,$ec48,$3c08,$6690,$9e78,$8d00,$7cf8
	dc.w $9218,$f1e0,$dc18,$43e0,$9630,$71c0,$e560,$f480
	dc.w $f780,$0d80,$b880,$9980,$3300,$0d00,$e280,$1c00
	dc.w $f700,$8680,$0e00,$7100,$fc00,$0000,$ea00,$0c00
	dc.w $e500,$0600,$1f00,$e000,$ed00,$0e00,$0000,$0000
	dc.l $0000,$0000

	dc.w $0000,$0000
	dc.w $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	dc.w $0400,$0400,$0400,$e000,$0000,$0000,$0000,$f800
	dc.w $ff00,$fe80,$4c80,$0f00,$1e80,$1d00,$fa00,$c580
	dc.w $7e70,$8170,$3c30,$c3f8,$1e00,$e1f0,$fc00,$03e0
	dc.w $f000,$0fe0,$4000,$3fe0,$f000,$0fc0,$e400,$0b80
	dc.w $1d80,$0380,$9980,$6600,$0100,$fe80,$0000,$ff80
	dc.w $7800,$5180,$8000,$0100,$0000,$0000,$f000,$e000
	dc.w $f800,$2000,$0000,$0000,$f000,$6000,$0000,$0000
	dc.l $0000,$0000

smile3:
	dc.w $0000,$0000
	dc.w $0000,$0000,$01ab,$01ec,$0543,$0472,$0441,$0c61
	dc.w $0b81,$07e1,$1ffb,$0ffd,$1ff1,$16bf,$1ff9,$1579
	dc.w $17f1,$13e0,$034c,$3053,$3a1a,$2e0f,$0526,$0b0f
	dc.w $053f,$0b70,$3a92,$3e7e,$7880,$185f,$a792,$439e
	dc.w $c7ff,$047f,$37ff,$3980,$4b80,$2d80,$771e,$0500
	dc.w $07c3,$043c,$5540,$727f,$29a1,$263f,$5060,$0f80
	dc.w $2320,$101f,$0cdf,$131f,$07fe,$000e,$0ede,$00ee
	dc.w $0e5a,$006a,$11fb,$0e08,$0eca,$00f2,$0000,$0000
	dc.l $0000,$0000

	dc.w $0000,$0000
	dc.w $0000,$0000,$0010,$0000,$038c,$000e,$039e,$001f
	dc.w $001e,$001f,$0ff1,$0ff1,$1531,$1431,$1479,$1477
	dc.w $13e0,$2fff,$20e0,$1f5f,$2e0e,$11f1,$0f0e,$30f1
	dc.w $0f30,$10cf,$0e1e,$31e1,$e040,$7fbf,$fb9e,$ebe1
	dc.w $f87f,$dc7f,$c000,$f180,$f07f,$797f,$f8ff,$7dde
	dc.w $7800,$1c00,$707f,$0f80,$203f,$5fc0,$0000,$7fff
	dc.w $0fc0,$311f,$003f,$101f,$000f,$000e,$0f0f,$0e0e
	dc.w $1f89,$0208,$0008,$0008,$1f01,$0600,$0000,$0000
	dc.l $0000,$0000

	dc.w $0000,$0000
	dc.w $0000,$0000,$f000,$0000,$f400,$1800,$ec00,$1e00
	dc.w $f500,$0f80,$ffa0,$ffe0,$ff90,$aff0,$ff08,$5d78
	dc.w $feb2,$fb30,$59cf,$4500,$8cf8,$0a88,$14f8,$9b38
	dc.w $96dc,$593c,$2bff,$cd9f,$37f6,$03cf,$fffe,$3fc0
	dc.w $fff8,$fff8,$fff8,$3ff8,$efe0,$f7e0,$6f80,$7780
	dc.w $ff80,$0f80,$b000,$8d00,$3300,$0d00,$e280,$1c00
	dc.w $f700,$8680,$0e00,$7100,$fc00,$0000,$ea00,$0c00
	dc.w $e500,$0600,$1f00,$e000,$ed00,$0e00,$0000,$0000
	dc.l $0000,$0000

	dc.w $0000,$0000
	dc.w $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
	dc.w $0780,$0040,$ffe0,$fe00,$4df0,$0d00,$1d7a,$1d84
	dc.w $fb33,$fccf,$e103,$5eff,$0887,$f774,$1c07,$e3be
	dc.w $1c03,$e31b,$0d80,$f39e,$03c0,$ffc6,$3fc0,$ffc0
	dc.w $fff8,$fff8,$3ff8,$3ff8,$07e0,$67e0,$8780,$a780
	dc.w $0f80,$0f80,$8100,$7e80,$0100,$fe80,$0000,$ff80
	dc.w $7800,$5180,$8000,$0100,$0000,$0000,$f000,$e000
	dc.w $f800,$2000,$0000,$0000,$f000,$6000,$0000,$0000
	dc.l $0000,$0000

smile4:
	dc.w $0000,$0000
	dc.w $0000,$0000,$01ab,$01ec,$0777,$0657,$07ff,$0f71
	dc.w $0ff9,$0c01,$1cb1,$0b30,$1e31,$11fe,$1de8,$1008
	dc.w $1611,$11e0,$024c,$3153,$3a1a,$2e0f,$0566,$0b0f
	dc.w $06bf,$0a70,$3912,$3c3e,$7920,$185f,$a79f,$438e
	dc.w $c7f0,$045e,$37ff,$3980,$4b80,$2d80,$771e,$0500
	dc.w $07c3,$043c,$5540,$727f,$29a1,$263f,$5060,$0f80
	dc.w $2320,$101f,$0cdf,$131f,$07fe,$000e,$0ede,$00ee
	dc.w $0e5a,$006a,$11fb,$0e08,$0eca,$00f2,$0000,$0000
	dc.l $0000,$0000

	dc.w $0000,$0000
	dc.w $0000,$0000,$0010,$0000,$059b,$0413,$0671,$0671
	dc.w $0c06,$0c07,$0830,$08cf,$1000,$1000,$1008,$13f7
	dc.w $1000,$2fff,$20e0,$1f5f,$2e0e,$11f1,$0f0e,$30f1
	dc.w $0e30,$11cf,$0c1e,$33e1,$e040,$7fbf,$fb8e,$ebff
	dc.w $f85e,$dc61,$c000,$f180,$f07f,$797f,$f8ff,$7dde
	dc.w $7800,$1c00,$707f,$0f80,$203f,$5fc0,$0000,$7fff
	dc.w $0fc0,$311f,$003f,$101f,$000f,$000e,$0f0f,$0e0e
	dc.w $1f89,$0208,$0008,$0008,$1f01,$0600,$0000,$0000
	dc.l $0000,$0000

	dc.w $0000,$0000
	dc.w $0000,$0000,$c900,$7f00,$d640,$97c0,$fd00,$ddc0
	dc.w $fad0,$02c0,$f638,$0900,$f738,$0d00,$ff18,$0100
	dc.w $0f88,$f180,$4440,$59c0,$a3a2,$3e62,$54dc,$9a3e
	dc.w $acfc,$4a80,$31fc,$85fc,$93fc,$03fc,$7ff8,$3ff8
	dc.w $fff0,$7ff0,$ffc0,$1fc0,$f780,$fb80,$7b80,$7d80
	dc.w $ff80,$0f80,$b000,$8d00,$3300,$0d00,$e280,$1c00
	dc.w $f700,$8680,$0e00,$7100,$fc00,$0000,$ea00,$0c00
	dc.w $e500,$0600,$1f00,$e000,$ed00,$0e00,$0000,$0000
	dc.l $0000,$0000

	dc.w $0000,$0000
	dc.w $0000,$0000,$7f00,$0080,$67c0,$0800,$9dc0,$9e20
	dc.w $02d0,$0f30,$0018,$e6f8,$01f8,$01b8,$01fc,$f9d8
	dc.w $017e,$fe6a,$e03e,$5f36,$3e1c,$c11c,$1e00,$e100
	dc.w $0e80,$f180,$05fc,$fbfc,$03fc,$fffc,$3ff8,$fff8
	dc.w $7ff0,$fff0,$1fc0,$1fc0,$0380,$7380,$8180,$b980
	dc.w $0f80,$0f80,$8100,$7e80,$0100,$fe80,$0000,$ff80
	dc.w $7800,$5180,$8000,$0100,$0000,$0000,$f000,$e000
	dc.w $f800,$2000,$0000,$0000,$f000,$6000,$0000,$0000
	dc.l $0000,$0000

smile5:
	dc.w $0000,$0000
	dc.w $0000,$0000,$01ab,$01ec,$0777,$0657,$07ff,$0f71
	dc.w $0ff9,$0c01,$1cb1,$0b30,$1e31,$11fe,$1de8,$1008
	dc.w $1611,$11e0,$024c,$3153,$3a1a,$2e0f,$0566,$0b0f
	dc.w $06bf,$0a70,$3912,$3c3e,$7920,$185f,$a79f,$438e
	dc.w $c7f0,$045e,$37ff,$3980,$4b80,$2d80,$771e,$0500
	dc.w $07c3,$043c,$5540,$727f,$29a1,$263f,$5060,$0f80
	dc.w $2320,$101f,$0cdf,$131f,$07fe,$000e,$0ede,$00ee
	dc.w $0e5a,$006a,$11fb,$0e08,$0eca,$00f2,$0000,$0000
	dc.l $0000,$0000

	dc.w $0000,$0000
	dc.w $0000,$0000,$0010,$0000,$059b,$0413,$0671,$0671
	dc.w $0c06,$0c07,$0830,$08cf,$1000,$1000,$1008,$13f7
	dc.w $1000,$2fff,$20e0,$1f5f,$2e0e,$11f1,$0f0e,$30f1
	dc.w $0e30,$11cf,$0c1e,$33e1,$e040,$7fbf,$fb8e,$ebff
	dc.w $f85e,$dc61,$c000,$f180,$f07f,$797f,$f8ff,$7dde
	dc.w $7800,$1c00,$707f,$0f80,$203f,$5fc0,$0000,$7fff
	dc.w $0fc0,$311f,$003f,$101f,$000f,$000e,$0f0f,$0e0e
	dc.w $1f89,$0208,$0008,$0008,$1f01,$0600,$0000,$0000
	dc.l $0000,$0000

	dc.w $0000,$0000
	dc.w $0000,$0000,$c900,$7f00,$d640,$97c0,$fd00,$ddc0
	dc.w $fad0,$02c0,$f638,$0900,$f738,$0d00,$0f18,$0100
	dc.w $0788,$f980,$a840,$39c0,$73a2,$8662,$34dc,$da3e
	dc.w $a9fc,$4c80,$13fc,$81fc,$9bfc,$07fc,$7ff8,$3ff8
	dc.w $fff0,$7ff0,$ffc0,$1fc0,$f780,$fb80,$7b80,$7d80
	dc.w $ff80,$0f80,$b000,$8d00,$3300,$0d00,$e280,$1c00
	dc.w $f700,$8680,$0e00,$7100,$fc00,$0000,$ea00,$0c00
	dc.w $e500,$0600,$1f00,$e000,$ed00,$0e00,$0000,$0000
	dc.l $0000,$0000

	dc.w $0000,$0000
	dc.w $0000,$0000,$7f00,$0080,$67c0,$0800,$9dc0,$9e20
	dc.w $02d0,$0f30,$0018,$e6f8,$01f8,$01b8,$01fc,$f9d8
	dc.w $017e,$fe6a,$383e,$c736,$061c,$f91c,$1e00,$e100
	dc.w $0c80,$f380,$01fc,$fffc,$03fc,$fffc,$3ff8,$fff8
	dc.w $7ff0,$fff0,$1fc0,$1fc0,$0380,$7380,$8180,$b980
	dc.w $0f80,$0f80,$8100,$7e80,$0100,$fe80,$0000,$ff80
	dc.w $7800,$5180,$8000,$0100,$0000,$0000,$f000,$e000
	dc.w $f800,$2000,$0000,$0000,$f000,$6000,$0000,$0000
	dc.l $0000,$0000



*****************************************
* Pro-Packer v2.11 Replay-Routine.	*
* Based upon the PT1.1B-Replayer	*
* by Lars 'ZAP' Hamre/Amiga Freelancers.*
* Modified by Estrup/Static Bytes.	*
*****************************************

mt_lev6use=		1		; 0=NO, 1=YES
mt_finetuneused=	0		; 0=NO, 1=YES

mt_init	LEA	mt_data,A0
	MOVE.L	A0,mt_SongDataPtr
	LEA	250(A0),A1
	MOVE.W	#511,D0
	MOVEQ	#0,D1
mtloop	MOVE.L	D1,D2
	SUBQ.W	#1,D0
mtloop2	MOVE.B	(A1)+,D1
	CMP.W	D2,D1
	BGT.S	mtloop
	DBRA	D0,mtloop2
	ADDQ	#1,D2

	MOVE.W	D2,D3
	MULU	#128,D3
	ADD.L	#766,D3
	ADD.L	mt_SongDataPtr(PC),D3
	MOVE.L	D3,mt_LWTPtr

	LEA	mt_SampleStarts(PC),A1
	MULU	#128,D2
	ADD.L	#762,D2
	ADD.L	(A0,D2.L),D2
	ADD.L	mt_SongDataPtr(PC),D2
	ADDQ.L	#4,D2
	MOVE.L	D2,A2
	MOVEQ	#30,D0
mtloop3	MOVE.L	A2,(A1)+
	MOVEQ	#0,D1
	MOVE.W	(A0),D1
	ADD.L	D1,D1
	ADD.L	D1,A2
	LEA	8(A0),A0
	DBRA	D0,mtloop3

	OR.B	#2,$BFE001
	lea	mt_speed(PC),A4
	MOVE.B	#6,(A4)
	CLR.B	mt_counter-mt_speed(A4)
	CLR.B	mt_SongPos-mt_speed(A4)
	CLR.W	mt_PatternPos-mt_speed(A4)
mt_end	LEA	$DFF096,A0
	CLR.W	$12(A0)
	CLR.W	$22(A0)
	CLR.W	$32(A0)
	CLR.W	$42(A0)
	MOVE.W	#$F,(A0)
	RTS

mt_music
	MOVEM.L	D0-D4/D7/A0-A6,-(SP)
	ADDQ.B	#1,mt_counter
	MOVE.B	mt_counter(PC),D0
	CMP.B	mt_speed(PC),D0
	BLO.S	mt_NoNewNote
	CLR.B	mt_counter
	TST.B	mt_PattDelTime2
	BEQ.S	mt_GetNewNote
	BSR.S	mt_NoNewAllChannels
	BRA.W	mt_dskip

mt_NoNewNote
	BSR.S	mt_NoNewAllChannels
	BRA.W	mt_NoNewPosYet

mt_NoNewAllChannels
	LEA	$DFF090,A5
	LEA	mt_chan1temp-44(PC),A6
	BSR.W	mt_CheckEfx
	BSR.W	mt_CheckEfx
	BSR.W	mt_CheckEfx
	BRA.W	mt_CheckEfx

mt_GetNewNote
	MOVE.L	mt_SongDataPtr(PC),A0
	LEA	(A0),A3
	LEA	122(A0),A2	;pattpo
	LEA	762(A0),A0	;patterndata
	CLR.W	mt_DMACONtemp

	LEA	$DFF090,A5
	LEA	mt_chan1temp-44(PC),A6
	BSR.S	mt_DoVoice
	BSR.S	mt_DoVoice
	BSR.B	mt_DoVoice
	BSR.B	mt_DoVoice
	BRA.W	mt_SetDMA

mt_DoVoice
	MOVEQ	#0,D0
	MOVEQ	#0,D1
	MOVE.B	mt_SongPos(PC),D0
	LEA	128(A2),A2
	MOVE.B	(A2,D0.W),D1
	MOVE.W	mt_PatternPos(PC),D2
	LSL	#7,D1
	LSR.W	#1,D2
	ADD.W	D2,D1
	LEA	$10(A5),A5
	LEA	44(A6),A6

	TST.L	(A6)
	BNE.S	mt_plvskip
	BSR.W	mt_PerNop
mt_plvskip
	MOVE.W	(A0,D1.W),D1
	LSL.W	#2,D1
	MOVE.L	A0,-(sp)
	MOVE.L	mt_LWTPtr(PC),A0
	MOVE.L	(A0,D1.W),(A6)
	MOVE.L	(sp)+,A0
	MOVE.B	2(A6),D2
	AND.L	#$F0,D2
	LSR.B	#4,D2
	MOVE.B	(A6),D0
	AND.B	#$F0,D0
	OR.B	D0,D2
	BEQ.B	mt_SetRegs
	MOVEQ	#0,D3
	LEA	mt_SampleStarts(PC),A1
	SUBQ	#1,D2
	MOVE	D2,D4
	ADD	D2,D2
	ADD	D2,D2
	LSL	#3,D4
	MOVE.L	(A1,D2.L),4(A6)
	MOVE.W	(A3,D4.W),8(A6)
	MOVE.W	(A3,D4.W),40(A6)
	MOVE.W	2(A3,D4.W),18(A6)
	MOVE.L	4(A6),D2	; Get start
	MOVE.W	4(A3,D4.W),D3	; Get repeat
	BEQ.S	mt_NoLoop
	MOVE.W	D3,D0		; Get repeat
	ADD.W	D3,D3
	ADD.L	D3,D2		; Add repeat
	ADD.W	6(A3,D4.W),D0	; Add replen
	MOVE.W	D0,8(A6)

mt_NoLoop
	MOVE.L	D2,10(A6)
	MOVE.L	D2,36(A6)
	MOVE.W	6(A3,D4.W),14(A6)	; Save replen
	MOVE.B	19(A6),9(A5)	; Set volume
mt_SetRegs
	MOVE.W	(A6),D0
	AND.W	#$0FFF,D0
	BEQ.W	mt_CheckMoreEfx	; If no note

	IF mt_finetuneused=1 THEN
	MOVE.W	2(A6),D0
	AND.W	#$0FF0,D0
	CMP.W	#$0E50,D0
	BEQ.S	mt_DoSetFineTune
	ENDC

	MOVE.B	2(A6),D0
	AND.B	#$0F,D0
	CMP.B	#3,D0	; TonePortamento
	BEQ.S	mt_ChkTonePorta
	CMP.B	#5,D0
	BEQ.S	mt_ChkTonePorta
	CMP.B	#9,D0	; Sample Offset
	BNE.S	mt_SetPeriod
	BSR.W	mt_CheckMoreEfx
	BRA.S	mt_SetPeriod

mt_ChkTonePorta
	BSR.W	mt_SetTonePorta
	BRA.W	mt_CheckMoreEfx

mt_DoSetFineTune
	BSR.W	mt_SetFineTune

mt_SetPeriod
	MOVEM.L	D1/A1,-(SP)
	MOVE.W	(A6),D1
	AND.W	#$0FFF,D1

	IF mt_finetuneused=0 THEN
	MOVE.W	D1,16(A6)

	ELSE
mt_SetPeriod2
	LEA	mt_PeriodTable(PC),A1
	MOVEQ	#36,D7
mt_ftuloop
	CMP.W	(A1)+,D1
	BHS.S	mt_ftufound
	DBRA	D7,mt_ftuloop
mt_ftufound
	MOVEQ	#0,D1
	MOVE.B	18(A6),D1
	LSL	#3,D1
	MOVE	D1,D0
	LSL	#3,D1
	ADD	D0,D1
	MOVE.W	-2(A1,D1.W),16(A6)
	ENDC

	MOVEM.L	(SP)+,D1/A1

	MOVE.W	2(A6),D0
	AND.W	#$0FF0,D0
	CMP.W	#$0ED0,D0 ; Notedelay
	BEQ.W	mt_CheckMoreEfx

	MOVE.W	20(A6),$DFF096
	BTST	#2,30(A6)
	BNE.S	mt_vibnoc
	CLR.B	27(A6)
mt_vibnoc
	BTST	#6,30(A6)
	BNE.S	mt_trenoc
	CLR.B	29(A6)
mt_trenoc
	MOVE.L	4(A6),(A5)	; Set start
	MOVE.W	8(A6),4(A5)	; Set length
	MOVE.W	16(A6),6(A5)	; Set period
	MOVE.W	20(A6),D0
	OR.W	D0,mt_DMACONtemp
	BRA.W	mt_CheckMoreEfx
 
mt_SetDMA
	IF mt_lev6use=1 THEN
	lea	$bfd000,a3
	move.b	#$7f,$d00(a3)
	move.w	#$2000,$dff09c
	move.w	#$a000,$dff09a
	move.l	$78.w,mt_oldirq
	move.l	#mt_irq1,$78.w
	moveq	#0,d0
	move.b	d0,$e00(a3)
	move.b	#$a8,$400(a3)
	move.b	d0,$500(a3)
	move.b	#$11,$e00(a3)
	move.b	#$81,$d00(a3)
	OR.W	#$8000,mt_DMACONtemp
	BRA.w	mt_dskip

	ELSE
	OR.W	#$8000,mt_DMACONtemp
	bsr.w	mt_WaitDMA
	ENDC

	IF mt_lev6use=1 THEN
mt_irq1:tst.b	$bfdd00
	MOVE.W	mt_dmacontemp(pc),$DFF096
	move.w	#$2000,$dff09c
	move.l	#mt_irq2,$78.w
	rte

	ELSE
	MOVE.W	mt_dmacontemp(pc),$DFF096
	bsr.w	mt_WaitDMA
	ENDC

	IF mt_lev6use=1 THEN
mt_irq2:tst.b	$bfdd00
	movem.l	a5-a6,-(a7)
	ENDC

	LEA	$DFF0A0,A5
	LEA	mt_chan1temp(PC),A6
	MOVE.L	10(A6),(A5)
	MOVE.W	14(A6),4(A5)
	MOVE.L	54(A6),$10(A5)
	MOVE.W	58(A6),$14(A5)
	MOVE.L	98(A6),$20(A5)
	MOVE.W	102(A6),$24(A5)
	MOVE.L	142(A6),$30(A5)
	MOVE.W	146(A6),$34(A5)

	IF mt_lev6use=1 THEN
	move.b	#0,$bfde00
	move.b	#$7f,$bfdd00
	move.l	mt_oldirq(pc),$78.w
	move.w	#$2000,$dff09c
	movem.l	(a7)+,a5-a6
	rte
	ENDC

mt_dskip
	lea	mt_speed(PC),A4
	ADDQ.W	#4,mt_PatternPos-mt_speed(A4)
	MOVE.B	mt_PattDelTime-mt_speed(A4),D0
	BEQ.S	mt_dskc
	MOVE.B	D0,mt_PattDelTime2-mt_speed(A4)
	CLR.B	mt_PattDelTime-mt_speed(A4)
mt_dskc	TST.B	mt_PattDelTime2-mt_speed(A4)
	BEQ.S	mt_dska
	SUBQ.B	#1,mt_PattDelTime2-mt_speed(A4)
	BEQ.S	mt_dska
	SUBQ.W	#4,mt_PatternPos-mt_speed(A4)
mt_dska	TST.B	mt_PBreakFlag-mt_speed(A4)
	BEQ.S	mt_nnpysk
	SF	mt_PBreakFlag-mt_speed(A4)
	MOVEQ	#0,D0
	MOVE.B	mt_PBreakPos(PC),D0
	CLR.B	mt_PBreakPos-mt_speed(A4)
	LSL	#2,D0
	MOVE.W	D0,mt_PatternPos-mt_speed(A4)
mt_nnpysk
	CMP.W	#256,mt_PatternPos-mt_speed(A4)
	BLO.S	mt_NoNewPosYet
mt_NextPosition	
	MOVEQ	#0,D0
	MOVE.B	mt_PBreakPos(PC),D0
	LSL	#2,D0
	MOVE.W	D0,mt_PatternPos-mt_speed(A4)
	CLR.B	mt_PBreakPos-mt_speed(A4)
	CLR.B	mt_PosJumpFlag-mt_speed(A4)
	ADDQ.B	#1,mt_SongPos-mt_speed(A4)
	AND.B	#$7F,mt_SongPos-mt_speed(A4)
	MOVE.B	mt_SongPos(PC),D1
	MOVE.L	mt_SongDataPtr(PC),A0
	CMP.B	248(A0),D1
	BLO.S	mt_NoNewPosYet
	CLR.B	mt_SongPos-mt_speed(A4)
mt_NoNewPosYet	
	lea	mt_speed(PC),A4
	TST.B	mt_PosJumpFlag-mt_speed(A4)
	BNE.S	mt_NextPosition
	MOVEM.L	(SP)+,D0-D4/D7/A0-A6
	RTS

mt_CheckEfx
	LEA	$10(A5),A5
	LEA	44(A6),A6
	BSR.W	mt_UpdateFunk
	MOVE.W	2(A6),D0
	AND.W	#$0FFF,D0
	BEQ.S	mt_PerNop
	MOVE.B	2(A6),D0
	MOVEQ	#$0F,D1
	AND.L	D1,D0
	BEQ.S	mt_Arpeggio
	SUBQ	#1,D0
	BEQ.W	mt_PortaUp
	SUBQ	#1,D0
	BEQ.W	mt_PortaDown
	SUBQ	#1,D0
	BEQ.W	mt_TonePortamento
	SUBQ	#1,D0
	BEQ.W	mt_Vibrato
	SUBQ	#1,D0
	BEQ.W	mt_TonePlusVolSlide
	SUBQ	#1,D0
	BEQ.W	mt_VibratoPlusVolSlide
	SUBQ	#8,D0
	BEQ.W	mt_E_Commands
SetBack	MOVE.W	16(A6),6(A5)
	ADDQ	#7,D0
	BEQ.W	mt_Tremolo
	SUBQ	#3,D0
	BEQ.W	mt_VolumeSlide
mt_Return2
	RTS

mt_PerNop
	MOVE.W	16(A6),6(A5)
	RTS

mt_Arpeggio
	MOVEQ	#0,D0
	MOVE.B	mt_counter(PC),D0
	DIVS	#3,D0
	SWAP	D0
	TST.W	D0
	BEQ.S	mt_Arpeggio2
	SUBQ	#2,D0
	BEQ.S	mt_Arpeggio1
	MOVEQ	#0,D0
	MOVE.B	3(A6),D0
	LSR.B	#4,D0
	BRA.S	mt_Arpeggio3

mt_Arpeggio2
	MOVE.W	16(A6),6(A5)
	RTS

mt_Arpeggio1
	MOVE.B	3(A6),D0
	AND.W	#15,D0
mt_Arpeggio3
	ADD.W	D0,D0
	LEA	mt_PeriodTable(PC),A0

	IF mt_finetuneused=1 THEN
	MOVEQ	#0,D1
	MOVE.B	18(A6),D1
	LSL	#3,D1
	MOVE	D1,D2
	LSL	#3,D1
	ADD	D2,D1
	ADD.L	D1,A0
	ENDC

	MOVE.W	16(A6),D1
	MOVEQ	#36,D7
mt_arploop
	CMP.W	(A0)+,D1
	BHS.S	mt_Arpeggio4
	DBRA	D7,mt_arploop
	RTS

mt_Arpeggio4
	MOVE.W	-2(A0,D0.W),6(A5)
	RTS

mt_FinePortaUp
	TST.B	mt_counter
	BNE.S	mt_Return2
	MOVE.B	#$0F,mt_LowMask
mt_PortaUp
	MOVEQ	#0,D0
	MOVE.B	3(A6),D0
	AND.B	mt_LowMask(PC),D0
	MOVE.B	#$FF,mt_LowMask
	SUB.W	D0,16(A6)
	MOVE.W	16(A6),D0
	AND.W	#$0FFF,D0
	CMP.W	#113,D0
	BPL.S	mt_PortaUskip
	AND.W	#$F000,16(A6)
	OR.W	#113,16(A6)
mt_PortaUskip
	MOVE.W	16(A6),D0
	AND.W	#$0FFF,D0
	MOVE.W	D0,6(A5)
	RTS	
 
mt_FinePortaDown
	TST.B	mt_counter
	BNE.W	mt_Return2
	MOVE.B	#$0F,mt_LowMask
mt_PortaDown
	CLR.W	D0
	MOVE.B	3(A6),D0
	AND.B	mt_LowMask(PC),D0
	MOVE.B	#$FF,mt_LowMask
	ADD.W	D0,16(A6)
	MOVE.W	16(A6),D0
	AND.W	#$0FFF,D0
	CMP.W	#856,D0
	BMI.S	mt_PortaDskip
	AND.W	#$F000,16(A6)
	OR.W	#856,16(A6)
mt_PortaDskip
	MOVE.W	16(A6),D0
	AND.W	#$0FFF,D0
	MOVE.W	D0,6(A5)
	RTS

mt_SetTonePorta
	MOVEM.L	A0,-(SP)
	MOVE.W	(A6),D2
	AND.W	#$0FFF,D2
	LEA	mt_PeriodTable(PC),A0

	IF	mt_finetuneused=1 THEN
	MOVEQ	#0,D0
	MOVE.B	18(A6),D0
	ADD	D0,D0
	MOVE	D0,D7
	ADD	D0,D0
	ADD	D0,D0
	ADD	D0,D7
	LSL	#3,D0
	ADD	D7,D0
	ADD.L	D0,A0
	ENDC

	MOVEQ	#0,D0
mt_StpLoop
	CMP.W	(A0,D0.W),D2
	BHS.S	mt_StpFound
	ADDQ	#2,D0
	CMP.W	#37*2,D0
	BLO.S	mt_StpLoop
	MOVEQ	#35*2,D0
mt_StpFound
	BTST	#3,18(A6)
	BEQ.S	mt_StpGoss
	TST.W	D0
	BEQ.S	mt_StpGoss
	SUBQ	#2,D0
mt_StpGoss
	MOVE.W	(A0,D0.W),D2
	MOVE.L	(SP)+,A0
	MOVE.W	D2,24(A6)
	MOVE.W	16(A6),D0
	CLR.B	22(A6)
	CMP.W	D0,D2
	BEQ.S	mt_ClearTonePorta
	BGE.W	mt_Return2
	MOVE.B	#1,22(A6)
	RTS

mt_ClearTonePorta
	CLR.W	24(A6)
	RTS

mt_TonePortamento
	MOVE.B	3(A6),D0
	BEQ.S	mt_TonePortNoChange
	MOVE.B	D0,23(A6)
	CLR.B	3(A6)
mt_TonePortNoChange
	TST.W	24(A6)
	BEQ.W	mt_Return2
	MOVEQ	#0,D0
	MOVE.B	23(A6),D0
	TST.B	22(A6)
	BNE.S	mt_TonePortaUp
mt_TonePortaDown
	ADD.W	D0,16(A6)
	MOVE.W	24(A6),D0
	CMP.W	16(A6),D0
	BGT.S	mt_TonePortaSetPer
	MOVE.W	24(A6),16(A6)
	CLR.W	24(A6)
	BRA.S	mt_TonePortaSetPer

mt_TonePortaUp
	SUB.W	D0,16(A6)
	MOVE.W	24(A6),D0
	CMP.W	16(A6),D0
	BLT.S	mt_TonePortaSetPer
	MOVE.W	24(A6),16(A6)
	CLR.W	24(A6)

mt_TonePortaSetPer
	MOVE.W	16(A6),D2
	MOVE.B	31(A6),D0
	AND.B	#$0F,D0
	BEQ.S	mt_GlissSkip
	LEA	mt_PeriodTable(PC),A0

	IF mt_finetuneused=1 THEN
	MOVEQ	#0,D0
	MOVE.B	18(A6),D0
	LSL	#3,D0
	MOVE	D0,D1
	LSL	#3,D0
	ADD	D1,D0
	ADD.L	D0,A0
	ENDC

	MOVEQ	#0,D0
mt_GlissLoop
	CMP.W	(A0,D0.W),D2
	BHS.S	mt_GlissFound
	ADDQ	#2,D0
	CMP.W	#36*2,D0
	BLO.S	mt_GlissLoop
	MOVEQ	#35*2,D0
mt_GlissFound
	MOVE.W	(A0,D0.W),D2
mt_GlissSkip
	MOVE.W	D2,6(A5) ; Set period
	RTS

mt_Vibrato
	MOVE.B	3(A6),D0
	BEQ.S	mt_Vibrato2
	MOVE.B	26(A6),D2
	AND.B	#$0F,D0
	BEQ.S	mt_vibskip
	AND.B	#$F0,D2
	OR.B	D0,D2
mt_vibskip
	MOVE.B	3(A6),D0
	AND.B	#$F0,D0
	BEQ.S	mt_vibskip2
	AND.B	#$0F,D2
	OR.B	D0,D2
mt_vibskip2
	MOVE.B	D2,26(A6)
mt_Vibrato2
	MOVE.B	27(A6),D0
	LEA	mt_VibratoTable(PC),A4
	LSR.W	#2,D0
	AND.W	#$001F,D0
	MOVE.B	30(A6),D2
	AND.W	#$03,D2
	BEQ.S	mt_vib_sine
	LSL.B	#3,D0
	CMP.B	#1,D2
	BEQ.S	mt_vib_rampdown
	MOVE.B	#255,D2
	BRA.S	mt_vib_set
mt_vib_rampdown
	TST.B	27(A6)
	BPL.S	mt_vib_rampdown2
	MOVE.B	#255,D2
	SUB.B	D0,D2
	BRA.S	mt_vib_set
mt_vib_rampdown2
	MOVE.B	D0,D2
	BRA.S	mt_vib_set
mt_vib_sine
	MOVE.B	0(A4,D0.W),D2
mt_vib_set
	MOVE.B	26(A6),D0
	AND.W	#15,D0
	MULU	D0,D2
	LSR.W	#7,D2
	MOVE.W	16(A6),D0
	TST.B	27(A6)
	BMI.S	mt_VibratoNeg
	ADD.W	D2,D0
	BRA.S	mt_Vibrato3
mt_VibratoNeg
	SUB.W	D2,D0
mt_Vibrato3
	MOVE.W	D0,6(A5)
	MOVE.B	26(A6),D0
	LSR.W	#2,D0
	AND.W	#$003C,D0
	ADD.B	D0,27(A6)
	RTS

mt_TonePlusVolSlide
	BSR.W	mt_TonePortNoChange
	BRA.W	mt_VolumeSlide

mt_VibratoPlusVolSlide
	BSR.S	mt_Vibrato2
	BRA.W	mt_VolumeSlide

mt_Tremolo
	MOVE.B	3(A6),D0
	BEQ.S	mt_Tremolo2
	MOVE.B	28(A6),D2
	AND.B	#$0F,D0
	BEQ.S	mt_treskip
	AND.B	#$F0,D2
	OR.B	D0,D2
mt_treskip
	MOVE.B	3(A6),D0
	AND.B	#$F0,D0
	BEQ.S	mt_treskip2
	AND.B	#$0F,D2
	OR.B	D0,D2
mt_treskip2
	MOVE.B	D2,28(A6)
mt_Tremolo2
	MOVE.B	29(A6),D0
	LEA	mt_VibratoTable(PC),A4
	LSR.W	#2,D0
	AND.W	#$001F,D0
	MOVEQ	#0,D2
	MOVE.B	30(A6),D2
	LSR.B	#4,D2
	AND.B	#$03,D2
	BEQ.S	mt_tre_sine
	LSL.B	#3,D0
	CMP.B	#1,D2
	BEQ.S	mt_tre_rampdown
	MOVE.B	#255,D2
	BRA.S	mt_tre_set
mt_tre_rampdown
	TST.B	27(A6)
	BPL.S	mt_tre_rampdown2
	MOVE.B	#255,D2
	SUB.B	D0,D2
	BRA.S	mt_tre_set
mt_tre_rampdown2
	MOVE.B	D0,D2
	BRA.S	mt_tre_set
mt_tre_sine
	MOVE.B	0(A4,D0.W),D2
mt_tre_set
	MOVE.B	28(A6),D0
	AND.W	#15,D0
	MULU	D0,D2
	LSR.W	#6,D2
	MOVEQ	#0,D0
	MOVE.B	19(A6),D0
	TST.B	29(A6)
	BMI.S	mt_TremoloNeg
	ADD.W	D2,D0
	BRA.S	mt_Tremolo3
mt_TremoloNeg
	SUB.W	D2,D0
mt_Tremolo3
	BPL.S	mt_TremoloSkip
	CLR.W	D0
mt_TremoloSkip
	CMP.W	#$40,D0
	BLS.S	mt_TremoloOk
	MOVE.W	#$40,D0
mt_TremoloOk
	MOVE.W	D0,8(A5)
	MOVE.B	28(A6),D0
	LSR.W	#2,D0
	AND.W	#$003C,D0
	ADD.B	D0,29(A6)
	RTS

mt_SampleOffset
	MOVEQ	#0,D0
	MOVE.B	3(A6),D0
	BEQ.S	mt_sononew
	MOVE.B	D0,32(A6)
mt_sononew
	MOVE.B	32(A6),D0
	LSL.W	#7,D0
	CMP.W	8(A6),D0
	BGE.S	mt_sofskip
	SUB.W	D0,8(A6)
	ADD.W	D0,D0
	ADD.L	D0,4(A6)
	RTS
mt_sofskip
	MOVE.W	#$0001,8(A6)
	RTS

mt_VolumeSlide
	MOVEQ	#0,D0
	MOVE.B	3(A6),D0
	LSR.B	#4,D0
	TST.B	D0
	BEQ.S	mt_VolSlideDown
mt_VolSlideUp
	ADD.B	D0,19(A6)
	CMP.B	#$40,19(A6)
	BMI.S	mt_vsuskip
	MOVE.B	#$40,19(A6)
mt_vsuskip
	MOVE.B	19(A6),9(A5)
	RTS

mt_VolSlideDown
	MOVE.B	3(A6),D0
	AND.W	#$0F,D0
mt_VolSlideDown2
	SUB.B	D0,19(A6)
	BPL.S	mt_vsdskip
	CLR.B	19(A6)
mt_vsdskip
	MOVE.B	19(A6),9(A5)
	RTS

mt_PositionJump
	MOVE.B	3(A6),D0
	SUBQ	#1,D0
	MOVE.B	D0,mt_SongPos
mt_pj2	CLR.B	mt_PBreakPos
	ST 	mt_PosJumpFlag
	RTS

mt_VolumeChange
	MOVE.B	3(A6),D0
	CMP.B	#$40,D0
	BLS.S	mt_VolumeOk
	MOVEQ	#$40,D0
mt_VolumeOk
	MOVE.B	D0,19(A6)
	MOVE.B	D0,9(A5)
	RTS

mt_PatternBreak
	MOVEQ	#0,D0
	MOVE.B	3(A6),D0
	MOVE.W	D0,D2
	LSR.B	#4,D0
	ADD	D0,D0
	MOVE	D0,D1
	ADD	D0,D0
	ADD	D0,D0
	ADD	D1,D0
	AND.B	#$0F,D2
	ADD.B	D2,D0
	CMP.B	#63,D0
	BHI.S	mt_pj2
	MOVE.B	D0,mt_PBreakPos
	ST	mt_PosJumpFlag
	RTS

mt_SetSpeed
	MOVE.B	3(A6),D0
	BEQ.W	mt_Return2
	CLR.B	mt_counter
	MOVE.B	D0,mt_speed
	RTS

mt_CheckMoreEfx
	BSR.W	mt_UpdateFunk
	MOVE.B	2(A6),D0
	AND.B	#$0F,D0
	SUB.B	#9,D0
	BEQ.W	mt_SampleOffset
	SUBQ	#2,D0
	BEQ.W	mt_PositionJump
	SUBQ	#1,D0
	BEQ.B	mt_VolumeChange
	SUBQ	#1,D0
	BEQ.S	mt_PatternBreak
	SUBQ	#1,D0
	BEQ.S	mt_E_Commands
	SUBQ	#1,D0
	BEQ.S	mt_SetSpeed
	BRA.W	mt_PerNop

mt_E_Commands
	MOVE.B	3(A6),D0
	AND.W	#$F0,D0
	LSR.B	#4,D0
	BEQ.S	mt_FilterOnOff
	SUBQ	#1,D0
	BEQ.W	mt_FinePortaUp
	SUBQ	#1,D0
	BEQ.W	mt_FinePortaDown
	SUBQ	#1,D0
	BEQ.S	mt_SetGlissControl
	SUBQ	#1,D0
	BEQ.B	mt_SetVibratoControl

	IF mt_finetuneused=1 THEN
	SUBQ	#1,D0
	BEQ.B	mt_SetFineTune
	SUBQ	#1,D0

	ELSE
	SUBQ	#2,D0
	ENDC

	BEQ.B	mt_JumpLoop
	SUBQ	#1,D0
	BEQ.W	mt_SetTremoloControl
	SUBQ	#2,D0
	BEQ.W	mt_RetrigNote
	SUBQ	#1,D0
	BEQ.W	mt_VolumeFineUp
	SUBQ	#1,D0
	BEQ.W	mt_VolumeFineDown
	SUBQ	#1,D0
	BEQ.W	mt_NoteCut
	SUBQ	#1,D0
	BEQ.W	mt_NoteDelay
	SUBQ	#1,D0
	BEQ.W	mt_PatternDelay
	BRA.W	mt_FunkIt

mt_FilterOnOff
	MOVE.B	3(A6),D0
	AND.B	#1,D0
	ADD.B	D0,D0
	AND.B	#$FD,$BFE001
	OR.B	D0,$BFE001
	RTS	

mt_SetGlissControl
	MOVE.B	3(A6),D0
	AND.B	#$0F,D0
	AND.B	#$F0,31(A6)
	OR.B	D0,31(A6)
	RTS

mt_SetVibratoControl
	MOVE.B	3(A6),D0
	AND.B	#$0F,D0
	AND.B	#$F0,30(A6)
	OR.B	D0,30(A6)
	RTS

mt_SetFineTune
	MOVE.B	3(A6),D0
	AND.B	#$0F,D0
	MOVE.B	D0,18(A6)
	RTS

mt_JumpLoop
	TST.B	mt_counter
	BNE.W	mt_Return2
	MOVE.B	3(A6),D0
	AND.B	#$0F,D0
	BEQ.S	mt_SetLoop
	TST.B	34(A6)
	BEQ.S	mt_jumpcnt
	SUBQ.B	#1,34(A6)
	BEQ.W	mt_Return2
mt_jmploop 	MOVE.B	33(A6),mt_PBreakPos
	ST	mt_PBreakFlag
	RTS

mt_jumpcnt
	MOVE.B	D0,34(A6)
	BRA.S	mt_jmploop

mt_SetLoop
	MOVE.W	mt_PatternPos(PC),D0
	LSR	#2,D0
	MOVE.B	D0,33(A6)
	RTS

mt_SetTremoloControl
	MOVE.B	3(A6),D0
	AND.B	#$0F,D0
	LSL.B	#4,D0
	AND.B	#$0F,30(A6)
	OR.B	D0,30(A6)
	RTS

mt_RetrigNote
	MOVE.L	D1,-(SP)
	MOVE.B	3(A6),D0
	AND.W	#$0F,D0
	BEQ.S	mt_rtnend
	MOVEQ	#0,d1
	MOVE.B	mt_counter(PC),D1
	BNE.S	mt_rtnskp
	MOVE.W	(A6),D1
	AND.W	#$0FFF,D1
	BNE.S	mt_rtnend
	MOVEQ	#0,D1
	MOVE.B	mt_counter(PC),D1
mt_rtnskp
	DIVU	D0,D1
	SWAP	D1
	TST.W	D1
	BNE.S	mt_rtnend
mt_DoRetrig
	MOVE.W	20(A6),$DFF096	; Channel DMA off
	MOVE.L	4(A6),(A5)	; Set sampledata pointer
	MOVE.W	8(A6),4(A5)	; Set length
	BSR.W	mt_WaitDMA
	MOVE.W	20(A6),D0
	BSET	#15,D0
	MOVE.W	D0,$DFF096
	BSR.W	mt_WaitDMA
	MOVE.L	10(A6),(A5)
	MOVE.L	14(A6),4(A5)
mt_rtnend
	MOVE.L	(SP)+,D1
	RTS

mt_VolumeFineUp
	TST.B	mt_counter
	BNE.W	mt_Return2
	MOVE.B	3(A6),D0
	AND.W	#$F,D0
	BRA.W	mt_VolSlideUp

mt_VolumeFineDown
	TST.B	mt_counter
	BNE.W	mt_Return2
	MOVE.B	3(A6),D0
	AND.W	#$0F,D0
	BRA.W	mt_VolSlideDown2

mt_NoteCut
	MOVE.B	3(A6),D0
	AND.W	#$0F,D0
	CMP.B	mt_counter(PC),D0
	BNE.W	mt_Return2
	CLR.B	19(A6)
	CLR.W	8(A5)
	RTS

mt_NoteDelay
	MOVE.B	3(A6),D0
	AND.W	#$0F,D0
	CMP.B	mt_Counter(PC),D0
	BNE.W	mt_Return2
	MOVE.W	(A6),D0
	BEQ.W	mt_Return2
	MOVE.L	D1,-(SP)
	BRA.W	mt_DoRetrig

mt_PatternDelay
	TST.B	mt_counter
	BNE.W	mt_Return2
	MOVE.B	3(A6),D0
	AND.W	#$0F,D0
	TST.B	mt_PattDelTime2
	BNE.W	mt_Return2
	ADDQ.B	#1,D0
	MOVE.B	D0,mt_PattDelTime
	RTS

mt_FunkIt
	TST.B	mt_counter
	BNE.W	mt_Return2
	MOVE.B	3(A6),D0
	AND.B	#$0F,D0
	LSL.B	#4,D0
	AND.B	#$0F,31(A6)
	OR.B	D0,31(A6)
	TST.B	D0
	BEQ.W	mt_Return2
mt_UpdateFunk
	MOVEM.L	D1/A0,-(SP)
	MOVEQ	#0,D0
	MOVE.B	31(A6),D0
	LSR.B	#4,D0
	BEQ.S	mt_funkend
	LEA	mt_FunkTable(PC),A0
	MOVE.B	(A0,D0.W),D0
	ADD.B	D0,35(A6)
	BTST	#7,35(A6)
	BEQ.S	mt_funkend
	CLR.B	35(A6)

	MOVE.L	10(A6),D0
	MOVEQ	#0,D1
	MOVE.W	14(A6),D1
	ADD.L	D1,D0
	ADD.L	D1,D0
	MOVE.L	36(A6),A0
	ADDQ.L	#1,A0
	CMP.L	D0,A0
	BLO.S	mt_funkok
	MOVE.L	10(A6),A0
mt_funkok
	MOVE.L	A0,36(A6)
	NEG.B	(A0)
	SUBQ.B	#1,(A0)
mt_funkend
	MOVEM.L	(SP)+,D1/A0
	RTS

mt_WaitDMA
	MOVEQ	#3,D0
mt_WaitDMA2
	MOVE.B	$DFF006,D1
mt_WaitDMA3
	CMP.B	$DFF006,D1
	BEQ.S	mt_WaitDMA3
	DBF	D0,mt_WaitDMA2
	RTS

mt_FunkTable dc.b 0,5,6,7,8,10,11,13,16,19,22,26,32,43,64,128

mt_VibratoTable	
	dc.b   0,24,49,74,97,120,141,161
	dc.b 180,197,212,224,235,244,250,253
	dc.b 255,253,250,244,235,224,212,197
	dc.b 180,161,141,120,97,74,49,24

mt_PeriodTable
; Tuning 0, Normal
	dc.w	856,808,762,720,678,640,604,570,538,508,480,453
	dc.w	428,404,381,360,339,320,302,285,269,254,240,226
	dc.w	214,202,190,180,170,160,151,143,135,127,120,113
; Tuning 1
	dc.w	850,802,757,715,674,637,601,567,535,505,477,450
	dc.w	425,401,379,357,337,318,300,284,268,253,239,225
	dc.w	213,201,189,179,169,159,150,142,134,126,119,113
; Tuning 2
	dc.w	844,796,752,709,670,632,597,563,532,502,474,447
	dc.w	422,398,376,355,335,316,298,282,266,251,237,224
	dc.w	211,199,188,177,167,158,149,141,133,125,118,112
; Tuning 3
	dc.w	838,791,746,704,665,628,592,559,528,498,470,444
	dc.w	419,395,373,352,332,314,296,280,264,249,235,222
	dc.w	209,198,187,176,166,157,148,140,132,125,118,111
; Tuning 4
	dc.w	832,785,741,699,660,623,588,555,524,495,467,441
	dc.w	416,392,370,350,330,312,294,278,262,247,233,220
	dc.w	208,196,185,175,165,156,147,139,131,124,117,110
; Tuning 5
	dc.w	826,779,736,694,655,619,584,551,520,491,463,437
	dc.w	413,390,368,347,328,309,292,276,260,245,232,219
	dc.w	206,195,184,174,164,155,146,138,130,123,116,109
; Tuning 6
	dc.w	820,774,730,689,651,614,580,547,516,487,460,434
	dc.w	410,387,365,345,325,307,290,274,258,244,230,217
	dc.w	205,193,183,172,163,154,145,137,129,122,115,109
; Tuning 7
	dc.w	814,768,725,684,646,610,575,543,513,484,457,431
	dc.w	407,384,363,342,323,305,288,272,256,242,228,216
	dc.w	204,192,181,171,161,152,144,136,128,121,114,108
; Tuning -8
	dc.w	907,856,808,762,720,678,640,604,570,538,508,480
	dc.w	453,428,404,381,360,339,320,302,285,269,254,240
	dc.w	226,214,202,190,180,170,160,151,143,135,127,120
; Tuning -7
	dc.w	900,850,802,757,715,675,636,601,567,535,505,477
	dc.w	450,425,401,379,357,337,318,300,284,268,253,238
	dc.w	225,212,200,189,179,169,159,150,142,134,126,119
; Tuning -6
	dc.w	894,844,796,752,709,670,632,597,563,532,502,474
	dc.w	447,422,398,376,355,335,316,298,282,266,251,237
	dc.w	223,211,199,188,177,167,158,149,141,133,125,118
; Tuning -5
	dc.w	887,838,791,746,704,665,628,592,559,528,498,470
	dc.w	444,419,395,373,352,332,314,296,280,264,249,235
	dc.w	222,209,198,187,176,166,157,148,140,132,125,118
; Tuning -4
	dc.w	881,832,785,741,699,660,623,588,555,524,494,467
	dc.w	441,416,392,370,350,330,312,294,278,262,247,233
	dc.w	220,208,196,185,175,165,156,147,139,131,123,117
; Tuning -3
	dc.w	875,826,779,736,694,655,619,584,551,520,491,463
	dc.w	437,413,390,368,347,328,309,292,276,260,245,232
	dc.w	219,206,195,184,174,164,155,146,138,130,123,116
; Tuning -2
	dc.w	868,820,774,730,689,651,614,580,547,516,487,460
	dc.w	434,410,387,365,345,325,307,290,274,258,244,230
	dc.w	217,205,193,183,172,163,154,145,137,129,122,115
; Tuning -1
	dc.w	862,814,768,725,684,646,610,575,543,513,484,457
	dc.w	431,407,384,363,342,323,305,288,272,256,242,228
	dc.w	216,203,192,181,171,161,152,144,136,128,121,114

mt_chan1temp	dcb.l	5
		dc.w	1
		dcb.w	21
		dc.w	2
		dcb.w	21
		dc.w	4
		dcb.w	21
		dc.w	8
		dcb.w	11

mt_SampleStarts	dcb.l	31,0

mt_SongDataPtr	dc.l 0
mt_LWTPtr	dc.l 0
mt_oldirq	dc.l 0

mt_speed	dc.b 6
mt_counter	dc.b 0
mt_SongPos	dc.b 0
mt_PBreakPos	dc.b 0
mt_PosJumpFlag	dc.b 0
mt_PBreakFlag	dc.b 0
mt_LowMask	dc.b 0
mt_PattDelTime	dc.b 0
mt_PattDelTime2	dc.b 0,0
mt_PatternPos	dc.w 0
mt_DMACONtemp	dc.w 0


sscolors:
	DC.W	$0444,$0444,$0555,$0666,$0666,$0777,$0888
	DC.W	$0999,$0999,$0AAA,$0BBB,$0BBB,$0CCC,$0DDD,$0EEE,$0EEE
	DC.W	$0FFF,$0FFF,$0FEF,$0FEF,$0FDF,$0FDF,$0FDF,$0FCF,$0FCF
	DC.W	$0FBF,$0FBF,$0FBF,$0FAF,$0FAF,$0F9F,$0F9F,$0F9F,$0F9E
	DC.W	$0F9D,$0F9C,$0F9B,$0F9A,$0F99,$0FA8,$0FA7,$0FA6,$0FA5
	DC.W	$0FA4,$0FA3,$0FB2,$0FB1,$0FB0,$0FB0,$0FB0,$0FB1,$0FB2
	DC.W	$0FA3,$0FA4,$0FA5,$0FA6,$0FA7,$0FA8,$0F99,$0F9A,$0F9B



; *********************************************************
; HERE IS THE SCROLLTEXT
;	you can use as fonts :
;	"ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890!.,':?()-/abcd "
; *********************************************************
text:

	dc.b " YUPS...       ANALOG IS STILL TRYING TO REACH THE SKY (IT "
	dc.b "SEEMS TO HAVE NO LIMITS) WITH THE MELTING POT 23 !!     "
	dc.b "ENJOY THIS COOOOOL NEW MENU !!!!    LET US GO WITH THE CREDITS: "
	dc.b "MENU CODED BY BLACKY AND KTULU,  MUSIC BY MONTY,  GFXS BY"
	dc.b " BAOBAB,  ANABOOT CODED BY MILENA  AND PACKING BY  abcd         "
	dc.b "        OK, NOW LET US CONTINUE WITH SUM PERSONNAL GREETINGS "
	dc.b "FROM  abcd .......... "
	DC.B "ALCATRAZ (HEAD)      "
	DC.B "ANDROMEDA (WILLIAM)     "
	DC.B "ARTEFACT (BUKAROO)      "
	DC.B "BASS (FTB)     "
	DC.B "BJORN A. LYNNE      "
	DC.B "BYTEBUSTERS (PROSECUTOR)      "
	DC.B "DEVILS (CPT JEWELS)      "
	DC.B "DRIFTERS (CORONAL)      "
	DC.B "E.O.C 1999 (TEO)      "
	DC.B "FUSION (OPTIMA, ACID, WARLORD)      "
	DC.B "GRACE (RISK)      "
	DC.B "HEMOROIDS (SPOCK)      "
	DC.B "INFECT (GADGET)      "
	DC.B "ITALIAN BAD BOYS (FRANZ)      "
	DC.B "MIDNIGHT SUN (SWANTTI)      "
	DC.B "OPUS MAG (DEGA MAX)      "
	DC.B "PLASMA (HAMMER)      "
	DC.B "RAM JAM (ACBS)      "
	DC.B "REBELS (SPEEDHAWK, ASTRO)      "
	DC.B "SANITY (KR'33)      "
	DC.B "SHINING (MEL O'DEE)      "
	DC.B "SHINING 8 (GHOSTRIDER)       "
	DC.B "SILENTS (JOHN DOE)      "
	DC.B "SPACEBALLS (NICK)      "
	DC.B "U.D.O. (DO-X)      "
	DC.B "UGA (RON FONTEINE)      "
	dc.b "          THAT'S ALL... CALL OUR BOARDS AND CONTACT US !!!!!!!"
	dc.b "....................................................."
	dc.b "                             "
crf:
	even

; *********************************************************
; NAMES OF THE FILES YOU SHOULD LOAD
;		you must write here the name used to load
;		the files in your pack
; *********************************************************
Prg00:		dc.b "df0:loader",0	; nom du prg de la case 0
Prg01:		dc.b "df0:minimo",0
Prg02:		dc.b "df0:",0		; ...
Prg03:		dc.b "df0:",0
Prg04:		dc.b "df0:",0
Prg05:		dc.b "df0:",0
Prg06:		dc.b "df0:",0
Prg07:		dc.b "df0:",0
Prg08:		dc.b "df0:",0
Prg09:		dc.b "df0:",0
Prg10:		dc.b "df0:",0
Prg11:		dc.b "df0:",0
Prg12:		dc.b "df0:",0
Prg13:		dc.b "df0:",0
Prg14:		dc.b "df0:",0
Prg15:		dc.b "df0:",0
Prg16:		dc.b "df0:",0
Prg17:		dc.b "df0:",0
Prg18:		dc.b "df0:",0
Prg19:		dc.b "df0:",0
Prg20:		dc.b "df0:",0
Prg21:		dc.b "df0:",0
Prg22:		dc.b "df0:",0
Prg23:		dc.b "df0:",0
even


* -----------------------------------------
mt_data	incbin	"mod.bs1"
logo	incbin	"magic.logo.rn"
fin	incbin	"magic.fin2"
fonts	incbin	"fonts.r"
	dcb.b	40*2,0
namesGFX	incbin	"names.r"
loadGFX		incbin	"load"

bot:


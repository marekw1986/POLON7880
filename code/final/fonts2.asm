CHARS:
;
	DB 000H, 000H, 000H, 000H, 000H, 000H, 000H, 000H  ;Blank
	DB 020H, 020H, 020H, 020H, 020H, 000H, 020H, 000H  ;!
	DB 050H, 050H, 050H, 000H, 000H, 000H, 000H, 000H  ;"
	DB 050H, 050H, 0F8H, 050H, 0F8H, 050H, 050H, 000H  ;#
	DB 020H, 078H, 0A0H, 070H, 028H, 0F0H, 020H, 000H  ;$
	DB 0C0H, 0C8H, 010H, 020H, 040H, 098H, 018H, 000H  ;%
	DB 040H, 0A0H, 0A0H, 040H, 0A8H, 090H, 068H, 000H  ;&
	DB 020H, 020H, 020H, 000H, 000H, 000H, 000H, 000H  ;'
	DB 020H, 040H, 080H, 080H, 080H, 040H, 020H, 000H  ;(
	DB 020H, 010H, 008H, 008H, 008H, 010H, 020H, 000H  ;)
	DB 020H, 0A8H, 070H, 020H, 070H, 0A8H, 020H, 000H  ;*
	DB 000H, 020H, 020H, 0F8H, 020H, 020H, 000H, 000H  ;+
	DB 000H, 000H, 000H, 000H, 020H, 020H, 040H, 000H  ;, 
	DB 000H, 000H, 000H, 0F8H, 000H, 000H, 000H, 000H  ;-
	DB 000H, 000H, 000H, 000H, 000H, 000H, 020H, 000H  ;.
	DB 000H, 008H, 010H, 020H, 040H, 080H, 000H, 000H  ;;/
	DB 070H, 088H, 098H, 0A8H, 0C8H, 088H, 070H, 000H  ;0
	DB 020H, 060H, 020H, 020H, 020H, 020H, 070H, 000H  ;1
	DB 070H, 088H, 008H, 030H, 040H, 080H, 0F8H, 000H  ;2
	DB 0F8H, 008H, 010H, 030H, 008H, 088H, 070H, 000H  ;3
	DB 010H, 030H, 050H, 090H, 0F8H, 010H, 010H, 000H  ;4
	DB 0F8H, 080H, 0F0H, 008H, 008H, 088H, 070H, 000H  ;5
	DB 038H, 040H, 080H, 0F0H, 088H, 088H, 070H, 000H  ;6
	DB 0F8H, 008H, 010H, 020H, 040H, 040H, 040H, 000H  ;7
	DB 070H, 088H, 088H, 070H, 088H, 088H, 070H, 000H  ;8
	DB 070H, 088H, 088H, 078H, 008H, 010H, 0E0H, 000H  ;9
	DB 000H, 000H, 020H, 000H, 020H, 000H, 000H, 000H  ;:
	DB 000H, 000H, 020H, 000H, 020H, 020H, 040H, 000H  ;;
	DB 010H, 020H, 040H, 080H, 040H, 020H, 010H, 000H  ;<
	DB 000H, 000H, 0F8H, 000H, 0F8H, 000H, 000H, 000H  ;=
	DB 040H, 020H, 010H, 008H, 010H, 020H, 040H, 000H  ;>
	DB 070H, 088H, 010H, 020H, 020H, 000H, 020H, 000H  ;?
                                                 
	DB 070H, 088H, 0A8H, 0B8H, 0B0H, 080H, 078H, 000H  ;@
	DB 020H, 050H, 088H, 088H, 0F8H, 088H, 088H, 000H  ;A
	DB 0F0H, 088H, 088H, 0F0H, 088H, 088H, 0F0H, 000H  ;B
	DB 070H, 088H, 080H, 080H, 080H, 088H, 070H, 000H  ;C
	DB 0F0H, 088H, 088H, 088H, 088H, 088H, 0F0H, 000H  ;D
	DB 0F8H, 080H, 080H, 0F0H, 080H, 080H, 0F8H, 000H  ;E
	DB 0F8H, 080H, 080H, 0F0H, 080H, 080H, 080H, 000H  ;F
	DB 078H, 080H, 080H, 080H, 098H, 088H, 078H, 000H  ;G
	DB 088H, 088H, 088H, 0F8H, 088H, 088H, 088H, 000H  ;H
	DB 070H, 020H, 020H, 020H, 020H, 020H, 070H, 000H  ;I
	DB 008H, 008H, 008H, 008H, 008H, 088H, 070H, 000H  ;J
	DB 088H, 090H, 0A0H, 0C0H, 0A0H, 090H, 088H, 000H  ;K
	DB 080H, 080H, 080H, 080H, 080H, 080H, 0F8H, 000H  ;L
	DB 088H, 0D8H, 0A8H, 0A8H, 088H, 088H, 088H, 000H  ;M
	DB 088H, 088H, 0C8H, 0A8H, 098H, 088H, 088H, 000H  ;N
	DB 070H, 088H, 088H, 088H, 088H, 088H, 070H, 000H  ;O
	DB 0F0H, 088H, 088H, 0F0H, 080H, 080H, 080H, 000H  ;P
	DB 070H, 088H, 088H, 088H, 0A8H, 090H, 068H, 000H  ;Q
	DB 0F0H, 088H, 088H, 0F0H, 0A0H, 090H, 088H, 000H  ;R
	DB 070H, 088H, 080H, 070H, 008H, 088H, 070H, 000H  ;S
	DB 0F8H, 020H, 020H, 020H, 020H, 020H, 020H, 000H  ;T
	DB 088H, 088H, 088H, 088H, 088H, 088H, 070H, 000H  ;U
	DB 088H, 088H, 088H, 088H, 088H, 050H, 020H, 000H  ;V
	DB 088H, 088H, 088H, 0A8H, 0A8H, 0D8H, 088H, 000H  ;W
	DB 088H, 088H, 050H, 020H, 050H, 088H, 088H, 000H  ;X
	DB 088H, 088H, 050H, 020H, 020H, 020H, 020H, 000H  ;Y
	DB 0F8H, 008H, 010H, 020H, 040H, 080H, 0F8H, 000H  ;Z
	DB 078H, 040H, 040H, 040H, 040H, 040H, 078H, 000H  ;[
	DB 000H, 080H, 040H, 020H, 010H, 008H, 000H, 000H  ;\
	DB 0F0H, 010H, 010H, 010H, 010H, 010H, 0F0H, 000H  ;]
	DB 000H, 000H, 020H, 050H, 088H, 000H, 000H, 000H  ;^
	DB 000H, 000H, 000H, 000H, 000H, 000H, 000H, 0F8H  ;_
	DB 040H, 020H, 010H, 000H, 000H, 000H, 000H, 000H  ;`
	DB 000H, 000H, 070H, 088H, 0F8H, 088H, 088H, 000H ;a
	DB 000H, 000H, 0F0H, 048H, 070H, 048H, 0F0H, 000H ;b
	DB 000H, 000H, 078H, 080H, 080H, 080H, 078H, 000H ;c
	DB 000H, 000H, 0F0H, 048H, 048H, 048H, 0F0H, 000H ;d
	DB 000H, 000H, 0F0H, 080H, 0E0H, 080H, 0F0H, 000H ;e
	DB 000H, 000H, 0F0H, 080H, 0E0H, 080H, 080H, 000H ;f
	DB 000H, 000H, 078H, 080H, 0B8H, 088H, 070H, 000H ;g
	DB 000H, 000H, 088H, 088H, 0F8H, 088H, 088H, 000H ;h
	DB 000H, 000H, 070H, 020H, 020H, 020H, 0F8H, 000H ;i
	DB 000H, 000H, 070H, 020H, 020H, 0A0H, 0E0H, 000H ;j
	DB 000H, 000H, 090H, 0A0H, 0C0H, 0A0H, 090H, 000H ;k
	DB 000H, 000H, 080H, 080H, 080H, 080H, 0F8H, 000H ;l
	DB 000H, 000H, 088H, 0D8H, 0A8H, 088H, 088H, 000H ;m
	DB 000H, 000H, 088H, 0C8H, 0A8H, 098H, 088H, 000H ;n
	DB 000H, 000H, 070H, 088H, 088H, 088H, 070H, 000H ;o
	DB 000H, 000H, 0F0H, 088H, 0F0H, 080H, 080H, 000H ;p
	DB 000H, 000H, 0F8H, 088H, 0A8H, 090H, 0E8H, 000H ;q
	DB 000H, 000H, 0F8H, 088H, 0F8H, 0A0H, 090H, 000H ;r
	DB 000H, 000H, 078H, 080H, 070H, 008H, 0F0H, 000H ;s
	DB 000H, 000H, 0F8H, 020H, 020H, 020H, 020H, 000H ;t
	DB 000H, 000H, 088H, 088H, 088H, 088H, 070H, 000H ;u
	DB 000H, 000H, 088H, 088H, 090H, 0A0H, 040H, 000H ;v
	DB 000H, 000H, 088H, 088H, 0A8H, 0D8H, 088H, 000H ;w
	DB 000H, 000H, 088H, 060H, 020H, 060H, 088H, 000H ;x
	DB 000H, 000H, 088H, 050H, 020H, 020H, 020H, 000H ;y
	DB 000H, 000H, 0F8H, 010H, 020H, 040H, 0F8H, 000H ;z
	DB 038H, 040H, 020H, 0C0H, 020H, 040H, 038H, 000H  ;{
	DB 040H, 020H, 010H, 008H, 010H, 020H, 040H, 000H  ;>
	DB 0E0H, 010H, 020H, 018H, 020H, 010H, 0E0H, 000H  ;}
	DB 040H, 0A8H, 010H, 000H, 000H, 000H, 000H, 000H  ;~
	DB 0A8H, 050H, 0A8H, 050H, 0A8H, 050H, 0A8H, 000H  ;DEL
;
CHARS_END:

PL_CHARS:

PL_CHARS_END:

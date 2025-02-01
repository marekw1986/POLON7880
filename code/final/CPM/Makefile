# Example Makefile for CP/M 2.2 bdos and ccp using Macro Assembler AS

#all: boot.bin boot.lst ccp.bin ccp.lst bdos.bin bdos.lst bios.bin bios.lst
#all: cpm22.bin boot.lst ccp.lst bdos.lst bios.lst

all: poloncpm.img

poloncpm.img : cpm22.bin
	rm -f poloncpm.img
	mkfs.cpm -f polon-2k-8m -b cpm22.bin poloncpm.img
	cpmcp -f polon-2k-8m poloncpm.img ./filesystem/* 0:
	cpmcp -f polon-2k-8m poloncpm.img ./adventure/adv-B03/* 0:

boot.p boot.lst: boot.asm
	asl -o boot.p -L -OLIST boot.lst boot.asm -cpu 8080
	
#boot.bin: boot.p
#	p2bin -l '$$00' -r '$$0000-$$21ff' boot.p
	
ccp.p ccp.lst: ccp.asm
	asl -D origin=2200h -o ccp.p -L -OLIST ccp.lst ccp.asm -cpu 8080

#ccp.bin: ccp.p
#	p2bin -l '$$00' -r '$$2200-$$29ff' ccp.p

bdos.p bdos.lst: bdos.asm
	asl -D origin=2a00h -o bdos.p -L -OLIST bdos.lst bdos.asm -cpu 8080

#bdos.bin: bdos.p
#	p2bin -l '$$00' -r '$$2a00-$$37ff' bdos.p
	
bios.p bios.lst: bios.asm
	asl -o bios.p -L -OLIST bios.lst bios.asm -cpu 8080
	
#bios.bin: bios.p
#	p2bin -l '$$00' bios.p

cpm22.bin: boot.p ccp.p bdos.p bios.p
	p2bin -l '$$00' boot.p ccp.p bdos.p bios.p cpm22.bin

clean:
	rm -f *.p *.bin *.lst *.img

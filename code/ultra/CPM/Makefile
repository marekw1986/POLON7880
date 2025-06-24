# Example Makefile for CP/M 2.2 bdos and ccp using Macro Assembler AS

#all: boot.bin boot.lst ccp.bin ccp.lst bdos.bin bdos.lst bios.bin bios.lst
#all: cpm22.bin boot.lst ccp.lst bdos.lst bios.lst

all: poloncpm.img

poloncpm.img : cpm22.bin
	rm -f poloncpm.img
	mkfs.cpm -f polon-2k-8m -b cpm22.bin poloncpm.img
	cpmcp -f polon-2k-8m poloncpm.img ./filesystem/* 0:
	cpmcp -f polon-2k-8m poloncpm.img ./adventure/adv-B03/* 0:
	cpmcp -f polon-2k-8m poloncpm.img ./basic/* 0:
	cpmcp -f polon-2k-8m poloncpm.img ./zork1/* 0:
#	cpmcp -f polon-2k-8m poloncpm.img ./sargon/* 0:

prepare_files: 
	git clone https://github.com/Z80-Retro/cpm-2.2.git
	mv ./cpm-2.2/filesystem ./
	rm -rf ./cpm-2.2

#boot.p boot.lst: boot.asm
#	asl -o boot.p -L -OLIST boot.lst boot.asm -cpu 8080
	
ccp.asm bdos.asm:
	git clone https://github.com/brouhaha/cpm22.git
	cp ./cpm22/bdos.asm ./
	cp ./cpm22/ccp.asm ./
	sed -i '/.cpu	8080/d' ./bdos.asm
	sed -i '/.cpu	8080/d' ./ccp.asm
	rm -rf ./cpm22
	
ccp.p ccp.lst: ccp.asm
	asl -D origin=0dc00h -o ccp.p -L -OLIST ccp.lst ccp.asm -cpu 8080

bdos.p bdos.lst: bdos.asm
	asl -D origin=0e406h -o bdos.p -L -OLIST bdos.lst bdos.asm -cpu 8080
	
bios.p bios.lst: bios.asm
	asl -o bios.p -L -OLIST bios.lst bios.asm -cpu 8080
	
cpm22.bin: ccp.p bdos.p bios.p
	p2bin -l '$$00' ccp.p bdos.p bios.p cpm22.bin

clean:
	rm -f *.p *.bin *.lst *.img
	rm -rf ./cpm-2.2

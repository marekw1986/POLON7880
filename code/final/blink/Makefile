# Adjust the run address to match the .org in the source code
all: blink.bin blink.hex

blink.bin: blink.hex
	objcopy --input-target=ihex --output-target=binary blink.hex blink.bin

blink.hex: blink.asm
	a85 blink.asm -l blink.lst -o blink.hex 

clean:
	$(RM) *.lst *.hex *.bin

distclean: clean

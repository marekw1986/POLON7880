# Adjust the run address to match the .org in the source code
all: blink_ultra.bin blink_ultra.hex

blink_ultra.bin: blink_ultra.hex
	objcopy --input-target=ihex --output-target=binary blink_ultra.hex blink_ultra.bin

blink_ultra.hex: blink_ultra.asm
	a85 blink_ultra.asm -l blink_ultra.lst -o blink_ultra.hex 

clean:
	$(RM) *.lst *.hex *.bin

distclean: clean

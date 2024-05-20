# Adjust the run address to match the .org in the source code
all: test.bin test.hex

test.bin: test.hex
	objcopy --input-target=ihex --output-target=binary test.hex test.bin

test.hex: test.asm
	a85 test.asm -l test.lst -o test.hex 

clean:
	$(RM) *.lst *.hex *.bin

distclean: clean

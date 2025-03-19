# Adjust the run address to match the .org in the source code
all: test_keyboard.bin test_keyboard.hex

test_keyboard.bin: test_keyboard.hex
	objcopy --input-target=ihex --output-target=binary test_keyboard.hex test_keyboard.bin

test_keyboard.hex: test_keyboard.asm
	a85 test_keyboard.asm -l test_keyboard.lst -o test_keyboard.hex 

clean:
	$(RM) *.lst *.hex *.bin

distclean: clean

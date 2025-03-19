# Adjust the run address to match the .org in the source code
all: test_uart.bin test_uart.hex

test_uart.bin: test_uart.hex
	objcopy --input-target=ihex --output-target=binary test_uart.hex test_uart.bin

test_uart.hex: test_uart.asm
	a85 test_uart.asm -l test_uart.lst -o test_uart.hex 

clean:
	$(RM) *.lst *.hex *.bin

distclean: clean

# Adjust the run address to match the .org in the source code
all: tinybasic-2.0.hex

tinybasic-2.0.hex: tinybasic-2.0.asm
	a85 tinybasic-2.0.asm -l tinybasic-2.0.lst -o tinybasic-2.0.hex 

clean:
	$(RM) *.lst *.hex

distclean: clean

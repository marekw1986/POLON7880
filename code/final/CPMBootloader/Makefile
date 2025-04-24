# Adjust the run address to match the .org in the source code
all: bootloader.hex

bootloader.hex: bootloader.asm
	a85 bootloader.asm -l bootloader.lst -o bootloader.hex
	./get_labels.py > labels.asm
	cp ./labels.asm ../CPM/labels.asm

clean:
	$(RM) *.lst *.hex labels.asm

distclean: clean

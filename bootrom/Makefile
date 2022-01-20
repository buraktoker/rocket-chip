bootrom_img = bootrom.img

GCC=riscv64-unknown-elf-gcc
OBJCOPY=riscv64-unknown-elf-objcopy

all: $(bootrom_img)

%.img: %.bin
	dd if=$< of=$@ bs=4096 count=1

%.bin: %.elf
	$(OBJCOPY) -O binary $< $@

%.elf: %.S linker.ld
	$(GCC) -march=rv64g -O0 -Tlinker.ld $< -nostdlib -static -Wl,--no-gc-sections -o $@
	riscv64-unknown-elf-objdump -D bootrom.elf > bootrom.dump

dump:
	riscv64-unknown-elf-objdump -D bootrom.elf > bootrom.dump

clean:
	rm -f bootrom.img

ARMGNU ?= arm-none-eabi

XCPU = -mcpu=cortex-m0plus

AOPS = --warn --fatal-warnings $(XCPU)
COPS = -Wall -Wno-pointer-sign -Os -ffreestanding $(XCPU)
LOPS = -nostdlib
# -nostartfiles
#~ LOCAL_TOOL_PATH = ../tool/gcc-arm-none-eabi/bin/
LOCAL_TOOL_PATH = 

# Windows Drive Letter (eg: D:\ == d)
DRIVE ?= d
UF2_TARGET_DIR = /mnt/$(DRIVE)
SRC := $(wildcard src/*.c)
OBJ := $(patsubst src/%.c,obj/%.o,$(SRC))
INC := $(patsubst src/%.c,inc/%.h,$(SRC))

.PHONY: clean all

all : dirs src/kernel.c $(INC) $(OBJ) program.uf2

dirs: inc obj

inc:
	mkdir inc

obj:
	mkdir obj

program.uf2 : program.elf picoUF2
	./picoUF2 program.elf program.uf2

picoUF2: main.cpp
	g++ main.cpp -O2 -o picoUF2

program.elf : memmap.ld start.o gram.o $(OBJ)
	$(LOCAL_TOOL_PATH)$(ARMGNU)-ld $(LOPS) -T memmap.ld start.o gram.o $(OBJ) -o program.elf
	$(LOCAL_TOOL_PATH)$(ARMGNU)-objdump -D program.elf > program.list

clean:
	rm -f *.o
	rm -f *.elf
	rm -f *.list
	rm -f *.uf2

start.o : start.s
	$(LOCAL_TOOL_PATH)$(ARMGNU)-as $(AOPS) start.s -o start.o

$(UF2_TARGET_DIR):
	mkdir -p UF2_TARGET_DIR

testRun: all | $(UF2_TARGET_DIR)
	sudo mount -t drvfs $(DRIVE): $(UF2_TARGET_DIR)
	cp program.uf2 $(UF2_TARGET_DIR)

src/kernel.c: src_fith/kernel.fith tool/fith2StringConst
	(cd src_fith && ../tool/fith2StringConst kernel.fith)
#~ 	./tool/fith2StringConst src_fith/kernel.fith

tool/headerGenerator.c: tool/headerGenerator.re
	re2c -W -i tool/headerGenerator.re -o tool/headerGenerator.c

tool/headerGenerator: tool/headerGenerator.c
	gcc tool/headerGenerator.c -o tool/headerGenerator -Os -Wall -Wextra -Wno-pointer-sign

tool/fith2StringConst: tool/fith2StringConst.c
	gcc tool/fith2StringConst.c -o tool/fith2StringConst -Os -Wall -Wextra -Wno-pointer-sign

obj/%.o: src/%.c localTypes.h gram.h
	$(LOCAL_TOOL_PATH)$(ARMGNU)-gcc $(COPS) -mthumb -c $< -o $@

inc/%.h: src/%.c tool/headerGenerator
	./tool/headerGenerator $<

tool/lemon: tool/lemon.c
	gcc -Os tool/lemon.c -o tool/lemon

gram.o: gram.c localTypes.h
	$(LOCAL_TOOL_PATH)$(ARMGNU)-gcc $(COPS) -mthumb -c $< -o $@

gram.c: tool/lemon gram.y
	./tool/lemon gram.y -s -p

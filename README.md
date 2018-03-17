# nes-emulator
- [Creating a NES emulator in C++11 part 1/2](https://www.youtube.com/watch?v=y71lli8MS8s&t=29s)
- [Creating a NES emulator in C++11 part 2/2](https://www.youtube.com/watch?v=XZWw745wPXY&t=17s)

## Compile
```
$ g++ apu.cpp cpu.cpp gamepak.cpp io.cpp ppu.cpp -lSDL2 -I./
```

## Componets
- Ricoh 2A03 CPU: Based on the 6502 MOS 8-bit microprocessor.
	- CA65: An assembler to produce machine code for 6502 CPUs. It is distributed as part of the CC65 package. 
- 2KiB of RAM for use by the CPU
- 2C02 PPU
- 2Kib of RAM for use by th PPU

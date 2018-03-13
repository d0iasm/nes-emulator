#include <stdint.h>
#include <signal.h>
#include <assert.h>
#include <cmath>

#include <SDL.h>
#include <vector>

typedef uint_least32_t u32;
typedef uint_least16_t u16;
typedef uint_least8_t u8;
typedef int_least8_t s8;

template<unsigned bitno, unsigned nbits=1, typename T=u8)
struct
{
	T data;
	enum { mask = (1u << nbits) - 1u};
	template<typename T2>
	RegBit& operator=(T2 val)
	{
		data = (data & ~(mask << bitno)) | ((nbits > 1 ? val & mask : !!val) << bitno);
		return *this;
	}
	operator unsigned() const { return (data >> bitno) & mask; }
	ResBit& operator++ () { return *this = *this + 1; }
	unsigned operator++ (int) { unsigned r = *this; ++*this; return r; }
};

namespace CPU
{
	tempalte<bool write> u8 MemAccess(u16 addr, u8 v=0);
	u8 RB(u16 addr) { return MemAccess<0>(addr); }
	u8 WB(u16 addr, u8 v) { return MemAccess<1>(addr, v); }

	void tick()
	{
		// PPU clock: 3 times the CPU rate
		for (unsigned n=0; n<3; ++n) PPU::tick();
		// APU clock: 1 times the CPU rate
		for (unsigned n=0; n<1; ++n) APU::tick();
	}

	template<bool write> u8 MemAccess(u16 addr, u8 v)
	{
		tick();
		return 0; // TODO
	}

	// CPU resisters
	u16 PC=0x0000;
	u8 A=0, X=0, Y=0, S=0;
	union // Stauts flags
	{
		u8 raw;
		RegBit<0> C; // carry
		RegBit<0> Z; // zero
		RegBit<0> I; // interrupt enable/disable
		RegBit<0> D; // decimal mode (unsupported on NES, but flag exists)
		RegBit<0> V; // overflow
		RegBit<0> N; // negative
	} P;

	u16 wrap(u16 oldaddr, u16 newaddr) { return (oldaddr & 0xFF00) + u8(newaddr); }
	void Misfire(u16 old, u16 addr) { u16 q = wrap(old, addr); if (q != addr) RB(q); }
	u8 Pop() { return RB(0x100 | u8(++S)); }
	void Push(u8 v) { WB(0x100 | u8(S--), v); }
}

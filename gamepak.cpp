#include <stdint.h>
#include <signal.h>
#include <assert.h>
#include <cmath>

#include <SDL2/SDL.h>
#include <vector>

typedef uint_least32_t u32;
typedef uint_least16_t u16;
typedef uint_least8_t u8;
typedef int_least8_t s8;

namespace GamePak
{
  std::vector<u8> ROM, URAM(0x2000);
  unsigned mappernum;
  const unsigned UROM_Granularity = 0x0400;
  const unsigned UROM_Pages = 0x2000 / UROM_Granularity;
  unsigned char NRAM[0x1000], PRAM[0x2000];
  unsigned char* banks[ROM_Pages] = {};
  unsigned char* Ubanks[UROM_Pages] = {};
  unsigned char* Nta[4] = { NRAM+0x0000, NRAM+0x0400, NRAM+0x0000, NRAM+0x0400 };
  void Init()
  {
  }
}

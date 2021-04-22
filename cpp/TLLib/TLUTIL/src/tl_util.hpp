#pragma once

#include <stdint.h>

const uint32_t abcSHA256[8] = {
  0xba7816bf, 0x8f01cfea, 0x414140de, 0x5dae2223,
  0xb00361a3, 0x96177a9c, 0xb410ff61, 0xf20015ad
};

const uint32_t abcSHA256_next[8] {
  0x4f8b42c2, 0x2dd3729b, 0x519ba6f6, 0x8d2da7cc, 
  0x5b2d606d, 0x05daed5a, 0xd5128cc0, 0x3e6c6358
};

void weakRandomInit(uint32_t* const data, const int n);
void weakRandomInit256(uint32_t* const data, const int n);

void print256(const uint32_t* const data);
#include <tl_util.hpp>

#include <stdlib.h>
#include <stdio.h>

void weakRandomInit(uint32_t* const data, const int n) {
    for (int i = 0; i < n; ++i) {
        data[i] = rand();
    }
}

void weakRandomInit256(uint32_t* const data, const int n) {
  const int num32 = n * 8; // 32 * 8 = 256 bits;
  for (int i = 0; i < num32; ++i) {
      data[i] = rand();
  }
}

void print256(const uint32_t* const data) {
    printf("Hex: ");
    for (int i = 0; i < 8; i++) {
        const uint32_t v = data[i];
        printf("%08x ", v);
    }
    printf("\n");
}
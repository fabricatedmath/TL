#include <tl_util.hpp>

#include <stdlib.h>
#include <stdio.h>

#include <sodium.h>

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

void strongRandomHash(uint32_t* const data) {
    const size_t size = 32;
    randombytes_buf(data, size);
}

void strongRandomHashes(uint32_t* const data, const int n) {
    const size_t size = 32 * n;
    randombytes_buf(data, size);
}

void printHash(const uint32_t* const data) {
    printf("Hex: ");
    for (int i = 0; i < 8; i++) {
        const uint32_t v = data[i];
        printf("%08x ", v);
    }
    printf("\n");
}

void printHashes(const uint32_t* const data, const int n) {
    const size_t offset = 8;
    for (int i = 0; i < n; i++) {
        printHash(data + offset*i);
    }
}
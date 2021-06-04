#pragma once

#include <stdio.h>
#include <stdint.h>

class HashFinder {
private:
  static const int CHUNK_SIZE = 4096;
  static const int HASH_SIZE_BYTES = 32;

  unsigned char buf_in[CHUNK_SIZE];
  int buf_in_read_len;
  int buf_in_read_loc;

  uint8_t hex_buf[64];
  int hex_buf_write_loc;
  int hex_buf_contiguous_hexes_written;

  FILE* fp_s;

public:
  HashFinder();

  int initialize(const char* fp);

  int readHexes(const int numHashesBufferSize, uint8_t* hashStrings, int &numHashesWritten);

  int deinitialize();

  ~HashFinder();
};

extern "C" {
  HashFinder* newHashFinder();
  int initializeHashFinder(HashFinder* hf, const char* fp);
  int readHexesHashFinder(HashFinder* hf, const int numHashesBufferSize, uint8_t* hashStrings, int &numHashesWritten);
  int deinitializeHashFinder(HashFinder* hf);
  void deleteHashFinder(HashFinder* hf);
}
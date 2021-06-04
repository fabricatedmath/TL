#include "hash_finder.hpp"

HashFinder::HashFinder() {
  hex_buf_write_loc = 0;
  hex_buf_contiguous_hexes_written = 0;
  buf_in_read_len = 0;
  buf_in_read_loc = 0;
}

int HashFinder::initialize(const char* fp) {
  fp_s = fopen(fp, "rb");
  if (!fp_s) {
    return -1;
  }
  return 0;
}

static int isHex(unsigned char c) {
  return ('a' <= c && c <= 'z') || ('0' <= c && c <= '9') || ('A' <= c && c <= 'Z');
}

static uint8_t charToHexValue(unsigned char c) {
  if ('0' <= c && c <= '9') {
    return c - '0';
  } else if ('a' <= c && c <= 'z') {
    return 10 + c - 'a';
  } else {
    return 10 + c - 'A';
  }
}

static uint8_t hexValueToBinary (uint8_t c1, uint8_t c2) {
  return c1*16 + c2;
}

// Read possible hashes found in file into buffer, call again to resume
int HashFinder::readHexes(const int numHashesBufferSize, uint8_t* hashStrings, int &numHashesWritten) {
  int numHashes = 0;
  do {
    for (; buf_in_read_loc < buf_in_read_len; buf_in_read_loc++) {
      if (numHashes == numHashesBufferSize) {
        numHashesWritten = numHashes;
        return 0;
      }
      unsigned char c = buf_in[buf_in_read_loc];
      if (!isHex(c)) {
        hex_buf_contiguous_hexes_written = 0;
      } else {
        hex_buf[hex_buf_write_loc] = charToHexValue(c);
        hex_buf_write_loc = (hex_buf_write_loc+1) % 64;
        if (hex_buf_contiguous_hexes_written == 63) {
          for (int j = 0; j < 64; j+=8) {
            uint8_t w1 = hex_buf[(hex_buf_write_loc + j + 0) % 64];
            uint8_t w2 = hex_buf[(hex_buf_write_loc + j + 1) % 64];
            hashStrings[numHashes*HASH_SIZE_BYTES + j/2 + 3] = hexValueToBinary(w1, w2);

            uint8_t w3 = hex_buf[(hex_buf_write_loc + j + 2) % 64];
            uint8_t w4 = hex_buf[(hex_buf_write_loc + j + 3) % 64];
            hashStrings[numHashes*HASH_SIZE_BYTES + j/2 + 2] = hexValueToBinary(w3, w4);

            uint8_t w5 = hex_buf[(hex_buf_write_loc + j + 4) % 64];
            uint8_t w6 = hex_buf[(hex_buf_write_loc + j + 5) % 64];
            hashStrings[numHashes*HASH_SIZE_BYTES + j/2 + 1] = hexValueToBinary(w5, w6);

            uint8_t w7 = hex_buf[(hex_buf_write_loc + j + 6) % 64];
            uint8_t w8 = hex_buf[(hex_buf_write_loc + j + 7) % 64];
            hashStrings[numHashes*HASH_SIZE_BYTES + j/2 + 0] = hexValueToBinary(w7, w8);
          }
          numHashes++;
        } else {
          hex_buf_contiguous_hexes_written++;
        }
      }
    }
    buf_in_read_loc = 0;
    buf_in_read_len = fread(buf_in, 1, CHUNK_SIZE, fp_s);
  } while (buf_in_read_loc != buf_in_read_len || !feof(fp_s));
  numHashesWritten = numHashes;
  return 0;
}

int HashFinder::deinitialize() {
  if (fp_s) {
    return fclose(fp_s);
  }
  return 0;
}

HashFinder::~HashFinder() {}

HashFinder* newHashFinder() {
  return new HashFinder;
}

int initializeHashFinder(HashFinder* hf, const char* fp) {
  return hf->initialize(fp);
}

int readHexesHashFinder(HashFinder* hf, const int numHashesBufferSize, uint8_t* hashStrings, int &numHashesWritten) {
  return hf->readHexes(numHashesBufferSize, hashStrings, numHashesWritten);
}

int deinitializeHashFinder(HashFinder* hf) {
  return hf->deinitialize();
}

void deleteHashFinder(HashFinder* hf) {
  delete hf;
}
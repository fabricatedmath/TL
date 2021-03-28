//
// Copyright 2021 Charles Durham <cpdurham@protonmail.com>, Copyright 2013 Allan MacKinnon <allanmac@alum.mit.edu>
//
// Permission is hereby granted, free of charge, to any person obtaining
// a copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so, subject to
// the following conditions:
//
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
// BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
// ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
// CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
//

#include <cuda.h>
#include <iostream>
using namespace std;

#define WARP_SIZE                    32

#define NUM_WARPS_IN_BLOCK           4 // arbitrarily chosen for now
#define NUM_THREADS_IN_BLOCK         (NUM_WARPS_IN_BLOCK * WARP_SIZE)

#define LAUNCH_BOUNDS                __launch_bounds__(NUM_THREADS_IN_BLOCK,1)
#define DEVICE_FUNCTION_QUALIFIERS   __device__ __forceinline__

//
// SQUASH C++ NAME MANGLING SO WE CAN LOAD AT RUNTIME VIA DRIVER API
//
#define KERNEL_QUALIFIERS            extern "C" __global__

//
//
//

#define STRINGIFY(x)  ""#x
#define COMMA         ,
#define EMPTY

//
// BIG-ENDIAN UNSIGNED 32-bit
//

typedef unsigned int beu32;

//
// 64 WORDS + MAGIC
//
#define W64(notLast,last)                        \
  W(0 ,0  ,0  ,0  ,0 , 0x428a2f98)   notLast     \
  W(1 ,0  ,0  ,0  ,0 , 0x71374491)   notLast     \
  W(2 ,0  ,0  ,0  ,0 , 0xb5c0fbcf)   notLast     \
  W(3 ,0  ,0  ,0  ,0 , 0xe9b5dba5)   notLast     \
  W(4 ,0  ,0  ,0  ,0 , 0x3956c25b)   notLast     \
  W(5 ,0  ,0  ,0  ,0 , 0x59f111f1)   notLast     \
  W(6 ,0  ,0  ,0  ,0 , 0x923f82a4)   notLast     \
  W(7 ,0  ,0  ,0  ,0 , 0xab1c5ed5)   notLast     \
  W(8 ,0  ,0  ,0  ,0 , 0xd807aa98)   notLast     \
  W(9 ,0  ,0  ,0  ,0 , 0x12835b01)   notLast     \
  W(10,0  ,0  ,0  ,0 , 0x243185be)   notLast     \
  W(11,0  ,0  ,0  ,0 , 0x550c7dc3)   notLast     \
  W(12,0  ,0  ,0  ,0 , 0x72be5d74)   notLast     \
  W(13,0  ,0  ,0  ,0 , 0x80deb1fe)   notLast     \
  W(14,0  ,0  ,0  ,0 , 0x9bdc06a7)   notLast     \
  W(15,0  ,0  ,0  ,0 , 0xc19bf174)   notLast     \
  W(16,0  ,1  ,9  ,14, 0xe49b69c1)   notLast     \
  W(17,1  ,2  ,10 ,15, 0xefbe4786)   notLast     \
  W(18,2  ,3  ,11 ,16, 0x0fc19dc6)   notLast     \
  W(19,3  ,4  ,12 ,17, 0x240ca1cc)   notLast     \
  W(20,4  ,5  ,13 ,18, 0x2de92c6f)   notLast     \
  W(21,5  ,6  ,14 ,19, 0x4a7484aa)   notLast     \
  W(22,6  ,7  ,15 ,20, 0x5cb0a9dc)   notLast     \
  W(23,7  ,8  ,16 ,21, 0x76f988da)   notLast     \
  W(24,8  ,9  ,17 ,22, 0x983e5152)   notLast     \
  W(25,9  ,10 ,18 ,23, 0xa831c66d)   notLast     \
  W(26,10 ,11 ,19 ,24, 0xb00327c8)   notLast     \
  W(27,11 ,12 ,20 ,25, 0xbf597fc7)   notLast     \
  W(28,12 ,13 ,21 ,26, 0xc6e00bf3)   notLast     \
  W(29,13 ,14 ,22 ,27, 0xd5a79147)   notLast     \
  W(30,14 ,15 ,23 ,28, 0x06ca6351)   notLast     \
  W(31,15 ,16 ,24 ,29, 0x14292967)   notLast     \
  W(32,16 ,17 ,25 ,30, 0x27b70a85)   notLast     \
  W(33,17 ,18 ,26 ,31, 0x2e1b2138)   notLast     \
  W(34,18 ,19 ,27 ,32, 0x4d2c6dfc)   notLast     \
  W(35,19 ,20 ,28 ,33, 0x53380d13)   notLast     \
  W(36,20 ,21 ,29 ,34, 0x650a7354)   notLast     \
  W(37,21 ,22 ,30 ,35, 0x766a0abb)   notLast     \
  W(38,22 ,23 ,31 ,36, 0x81c2c92e)   notLast     \
  W(39,23 ,24 ,32 ,37, 0x92722c85)   notLast     \
  W(40,24 ,25 ,33 ,38, 0xa2bfe8a1)   notLast     \
  W(41,25 ,26 ,34 ,39, 0xa81a664b)   notLast     \
  W(42,26 ,27 ,35 ,40, 0xc24b8b70)   notLast     \
  W(43,27 ,28 ,36 ,41, 0xc76c51a3)   notLast     \
  W(44,28 ,29 ,37 ,42, 0xd192e819)   notLast     \
  W(45,29 ,30 ,38 ,43, 0xd6990624)   notLast     \
  W(46,30 ,31 ,39 ,44, 0xf40e3585)   notLast     \
  W(47,31 ,32 ,40 ,45, 0x106aa070)   notLast     \
  W(48,32 ,33 ,41 ,46, 0x19a4c116)   notLast     \
  W(49,33 ,34 ,42 ,47, 0x1e376c08)   notLast     \
  W(50,34 ,35 ,43 ,48, 0x2748774c)   notLast     \
  W(51,35 ,36 ,44 ,49, 0x34b0bcb5)   notLast     \
  W(52,36 ,37 ,45 ,50, 0x391c0cb3)   notLast     \
  W(53,37 ,38 ,46 ,51, 0x4ed8aa4a)   notLast     \
  W(54,38 ,39 ,47 ,52, 0x5b9cca4f)   notLast     \
  W(55,39 ,40 ,48 ,53, 0x682e6ff3)   notLast     \
  W(56,40 ,41 ,49 ,54, 0x748f82ee)   notLast     \
  W(57,41 ,42 ,50 ,55, 0x78a5636f)   notLast     \
  W(58,42 ,43 ,51 ,56, 0x84c87814)   notLast     \
  W(59,43 ,44 ,52 ,57, 0x8cc70208)   notLast     \
  W(60,44 ,45 ,53 ,58, 0x90befffa)   notLast     \
  W(61,45 ,46 ,54 ,59, 0xa4506ceb)   notLast     \
  W(62,46 ,47 ,55 ,60, 0xbef9a3f7)   notLast     \
  W(63,47 ,48 ,56 ,61, 0xc67178f2)   last

//
// HASH 8
//
#define H8(notLast,last)                        \
  H(0, a, 0x6a09e667)   notLast                 \
  H(1, b, 0xbb67ae85)   notLast                 \
  H(2, c, 0x3c6ef372)   notLast                 \
  H(3, d, 0xa54ff53a)   notLast                 \
  H(4, e, 0x510e527f)   notLast                 \
  H(5, f, 0x9b05688c)   notLast                 \
  H(6, g, 0x1f83d9ab)   notLast                 \
  H(7, h, 0x5be0cd19)   last

//
// MIX 8
//
#define M8(notLast,last)                      \
  M(h, g)   notLast                           \
  M(g, f)   notLast                           \
  M(f, e)   notLast                           \
  M(e, d)   notLast                           \
  M(d, c)   notLast                           \
  M(c, b)   notLast                           \
  M(b, a)   notLast                           \
  M(a, t)   last


//
// CHUNK 16
//
#define C16(notLast,last)                     \
  C(0 )   notLast                             \
  C(1 )   notLast                             \
  C(2 )   notLast                             \
  C(3 )   notLast                             \
  C(4 )   notLast                             \
  C(5 )   notLast                             \
  C(6 )   notLast                             \
  C(7 )   notLast                             \
  C(8 )   notLast                             \
  C(9 )   notLast                             \
  C(10)   notLast                             \
  C(11)   notLast                             \
  C(12)   notLast                             \
  C(13)   notLast                             \
  C(14)   notLast                             \
  C(15)   last

//
// W TOP 8 INPUT
//
#define T8(notLast,last)                      \
  T(0 )   notLast                             \
  T(1 )   notLast                             \
  T(2 )   notLast                             \
  T(3 )   notLast                             \
  T(4 )   notLast                             \
  T(5 )   notLast                             \
  T(6 )   notLast                             \
  T(7 )   last                                \


//
// W BOTTOM 8 INPUT
//
#define B8(notLast,last)                      \
  B(8 , 0x80000000)   notLast                 \
  B(9 , 0x00000000)   notLast                 \
  B(10, 0x00000000)   notLast                 \
  B(11, 0x00000000)   notLast                 \
  B(12, 0x00000000)   notLast                 \
  B(13, 0x00000000)   notLast                 \
  B(14, 0x00000000)   notLast                 \
  B(15, 0x00000100)   last                    \

//
// W REST 48 INPUT
//
#define R48(notLast,last)                        \
  R(16,0  ,1  ,9  ,14, 0xe49b69c1)   notLast     \
  R(17,1  ,2  ,10 ,15, 0xefbe4786)   notLast     \
  R(18,2  ,3  ,11 ,16, 0x0fc19dc6)   notLast     \
  R(19,3  ,4  ,12 ,17, 0x240ca1cc)   notLast     \
  R(20,4  ,5  ,13 ,18, 0x2de92c6f)   notLast     \
  R(21,5  ,6  ,14 ,19, 0x4a7484aa)   notLast     \
  R(22,6  ,7  ,15 ,20, 0x5cb0a9dc)   notLast     \
  R(23,7  ,8  ,16 ,21, 0x76f988da)   notLast     \
  R(24,8  ,9  ,17 ,22, 0x983e5152)   notLast     \
  R(25,9  ,10 ,18 ,23, 0xa831c66d)   notLast     \
  R(26,10 ,11 ,19 ,24, 0xb00327c8)   notLast     \
  R(27,11 ,12 ,20 ,25, 0xbf597fc7)   notLast     \
  R(28,12 ,13 ,21 ,26, 0xc6e00bf3)   notLast     \
  R(29,13 ,14 ,22 ,27, 0xd5a79147)   notLast     \
  R(30,14 ,15 ,23 ,28, 0x06ca6351)   notLast     \
  R(31,15 ,16 ,24 ,29, 0x14292967)   notLast     \
  R(32,16 ,17 ,25 ,30, 0x27b70a85)   notLast     \
  R(33,17 ,18 ,26 ,31, 0x2e1b2138)   notLast     \
  R(34,18 ,19 ,27 ,32, 0x4d2c6dfc)   notLast     \
  R(35,19 ,20 ,28 ,33, 0x53380d13)   notLast     \
  R(36,20 ,21 ,29 ,34, 0x650a7354)   notLast     \
  R(37,21 ,22 ,30 ,35, 0x766a0abb)   notLast     \
  R(38,22 ,23 ,31 ,36, 0x81c2c92e)   notLast     \
  R(39,23 ,24 ,32 ,37, 0x92722c85)   notLast     \
  R(40,24 ,25 ,33 ,38, 0xa2bfe8a1)   notLast     \
  R(41,25 ,26 ,34 ,39, 0xa81a664b)   notLast     \
  R(42,26 ,27 ,35 ,40, 0xc24b8b70)   notLast     \
  R(43,27 ,28 ,36 ,41, 0xc76c51a3)   notLast     \
  R(44,28 ,29 ,37 ,42, 0xd192e819)   notLast     \
  R(45,29 ,30 ,38 ,43, 0xd6990624)   notLast     \
  R(46,30 ,31 ,39 ,44, 0xf40e3585)   notLast     \
  R(47,31 ,32 ,40 ,45, 0x106aa070)   notLast     \
  R(48,32 ,33 ,41 ,46, 0x19a4c116)   notLast     \
  R(49,33 ,34 ,42 ,47, 0x1e376c08)   notLast     \
  R(50,34 ,35 ,43 ,48, 0x2748774c)   notLast     \
  R(51,35 ,36 ,44 ,49, 0x34b0bcb5)   notLast     \
  R(52,36 ,37 ,45 ,50, 0x391c0cb3)   notLast     \
  R(53,37 ,38 ,46 ,51, 0x4ed8aa4a)   notLast     \
  R(54,38 ,39 ,47 ,52, 0x5b9cca4f)   notLast     \
  R(55,39 ,40 ,48 ,53, 0x682e6ff3)   notLast     \
  R(56,40 ,41 ,49 ,54, 0x748f82ee)   notLast     \
  R(57,41 ,42 ,50 ,55, 0x78a5636f)   notLast     \
  R(58,42 ,43 ,51 ,56, 0x84c87814)   notLast     \
  R(59,43 ,44 ,52 ,57, 0x8cc70208)   notLast     \
  R(60,44 ,45 ,53 ,58, 0x90befffa)   notLast     \
  R(61,45 ,46 ,54 ,59, 0xa4506ceb)   notLast     \
  R(62,46 ,47 ,55 ,60, 0xbef9a3f7)   notLast     \
  R(63,47 ,48 ,56 ,61, 0xc67178f2)   last

//
// NOT AND
//
DEVICE_FUNCTION_QUALIFIERS
beu32
notand(beu32 a, const beu32 b)
{
#if __CUDA_ARCH__ >= 100
  beu32 d;
  asm("not.b32  %1, %1;     \n\t"
      "and.b32  %0, %1, %2; \n\t"
      : "=r"(d), "+r"(a) : "r"(b));
  return d;
#else
  return ~a & b;
#endif
}

//
// ROTATE RIGHT
//
DEVICE_FUNCTION_QUALIFIERS
beu32
ror(const beu32 a, const unsigned int n)
{
#if __CUDA_ARCH__ >= 350 // BEWARE THIS CRASHES NVCC/CICC 5.0 -- BUG REPORTED
  beu32 d;
  asm("shf.r.clamp.b32 %0, %1, %2, %3;" : "=r"(d) : "r"(a), "r"(a), "r"(n));
  return d;
#else
  return (a >> n) | (a << (32 - n));
#endif
}

//
// SHIFT RIGHT
//
DEVICE_FUNCTION_QUALIFIERS
beu32
shr(const beu32 a, const unsigned int n)
{
#if __CUDA_ARCH__ >= 999 // 200 -- DISABLED
  beu32 d;
  asm("vshr.u32.u32.u32.clamp %0, %1, %2;" : "=r"(d) : "r"(a), "r"(n));
  return d;
#else
  return a >> n;
#endif
}

//
// ADD 3
//
DEVICE_FUNCTION_QUALIFIERS
beu32
add3(const beu32 a, const beu32 b, const beu32 c)
{
#if __CUDA_ARCH__ >= 999 // 200 -- DISABLED
  beu32 d;
  asm("vadd.u32.u32.u32.add %0, %1, %2, %3;" : "=r"(d) : "r"(a), "r"(b), "r"(c));
  return d;
#else
  return a + b + c;
#endif
}

//
// MIX ALPHAS
//
DEVICE_FUNCTION_QUALIFIERS
void
hmix(beu32* a,
     beu32* b,
     beu32* c,
     beu32* d,
     beu32* e,
     beu32* f,
     beu32* g,
     beu32* h,
     beu32* t)
{
#undef  M
#define M(alpha,mix) *alpha = *mix;

  M8(EMPTY,EMPTY);
}

////////////////////////////////////////////////////////////////////////
//
// SHA-256 CHUNK FUNC
//
#undef  C
#define C(i)              const beu32 c##i

#undef  H
#define H(i,alpha,magic)  const beu32 hin##i, beu32* hout##i

DEVICE_FUNCTION_QUALIFIERS
void
sha256_chunk(C16(COMMA,EMPTY),
             H8(COMMA,EMPTY))
{
  //
  // DECLARE 'W' REGISTERS
  //
#undef  W
#define W(i,m16,m15,m7,m2,magic)  beu32 w##i;

  W64(EMPTY,EMPTY);

  //
  // INIT W REGISTERS 0-15 OFF OF CHUNK REGISTERS
  //
#undef  C
#define C(i)  w##i = c##i;

  C16(EMPTY,EMPTY);

  //
  // INIT W REGISTERS 16-63
  //
#undef  W
#define W(i,m16,m15,m7,m2,magic)                                \
  if (i >= 16) {                                                \
    w##i = w##m16 +                                             \
      add3(w##m7,                                               \
           (ror(w##m15, 7) ^ ror(w##m15,18) ^ shr(w##m15, 3)),  \
           (ror(w##m2, 17) ^ ror(w##m2, 19) ^ shr(w##m2, 10))); \
  }

  W64(EMPTY,EMPTY);

  //
  // INIT H REGISTERS
  //
#undef  H
#define H(i,alpha,magic)  beu32 alpha = hin##i;

  H8(EMPTY,EMPTY);

  //
  // MAIN LOOP
  //
#undef  W
#define W(i,m16,m15,m7,m2,magic)                        \
  {                                                     \
    beu32 t = add3(add3(h,w##i,magic),                  \
                   (ror(e,6) ^ ror(e,11) ^ ror(e,25)),  \
                   ((e & f) ^ notand(e,g)));            \
                                                        \
    d += t;                                             \
                                                        \
    t = add3(t,                                         \
             (ror(a,2) ^ ror(a,13) ^ ror(a,22)),        \
             ((a & (b ^ c)) ^ (b & c)));                \
                                                        \
    hmix(&a,&b,&c,&d,&e,&f,&g,&h,&t);                   \
  }

  W64(EMPTY,EMPTY);

  //
  // ADD H MAGIC TO ALPHAS
  //
#undef  H
#define H(i,alpha,magic)  *hout##i = hin##i + alpha;

  H8(EMPTY,EMPTY);
}

////////////////////////////////////////////////////////////////////////
//
// CHUNK 0 IS KICKSTARTED WITH CONSTANT HASH INPUTS
//
#undef  C
#define C(i)              const beu32 c##i

#undef  H
#define H(i,alpha,magic)  beu32* hout##i

DEVICE_FUNCTION_QUALIFIERS
void
sha256_chunk0(C16(COMMA,EMPTY),H8(COMMA,EMPTY))
{
#undef  C
#define C(i)              c##i

#undef  H
#define H(i,alpha,magic)  magic,hout##i

  sha256_chunk(C16(COMMA,EMPTY),H8(COMMA,EMPTY));
}

////////////////////////////////////////////////////////////////////////
//
// SPECIALIZED ITERATE HASHING
//
// See https://en.wikipedia.org/wiki/SHA-2#Pseudocode
//
KERNEL_QUALIFIERS
LAUNCH_BOUNDS
void
sha256_iter(const int iter, const beu32* const d_in, beu32* const d_out)
{
  const int offset = 0;
  
  // Don't need initial hash values h0 through h7, results stored directly in bytes 0-7 of w

  #undef T
  #define T(i) beu32 w##i = d_in[offset+i];

  T8(EMPTY,EMPTY);

  // Bytes 8-15 of w can be constants because they encode padding for the 
  // fixed size of 256 bytes instead of the full 512 byte chunks
  
  #undef B
  #define B(i,hex) const beu32 w##i = hex;

  B8(EMPTY,EMPTY);
  
  // Storing hash back into w to iterate again directly

  for(int count = 0; count < iter; count++) 
  {
    //
    // INIT W REGISTERS 16-63
    //
    #undef  R
    #define R(i,m16,m15,m7,m2,magic)                                \
        const beu32 w##i = w##m16 +                                 \
          add3(w##m7,                                               \
              (ror(w##m15, 7) ^ ror(w##m15,18) ^ shr(w##m15, 3)),  \
              (ror(w##m2, 17) ^ ror(w##m2, 19) ^ shr(w##m2, 10))); \

      R48(EMPTY,EMPTY);

    // Initialize working variables from hash constants directly
    
    #undef H
    #define H(i,alpha,magic) beu32 alpha = magic;

    H8(EMPTY,EMPTY);

    //
    // MAIN LOOP
    //
    #undef  W
    #define W(i,m16,m15,m7,m2,magic)                        \
      {                                                     \
        beu32 t = add3(add3(h,w##i,magic),                  \
                      (ror(e,6) ^ ror(e,11) ^ ror(e,25)),  \
                      ((e & f) ^ notand(e,g)));            \
                                                            \
        d += t;                                             \
                                                            \
        t = add3(t,                                         \
                (ror(a,2) ^ ror(a,13) ^ ror(a,22)),        \
                ((a & (b ^ c)) ^ (b & c)));                \
                                                            \
        hmix(&a,&b,&c,&d,&e,&f,&g,&h,&t);                   \
      }

      W64(EMPTY,EMPTY);
      
      // Store working values directly into w because we only have 1 chunk

      #undef H
      #define H(i,alpha,magic) w##i = magic + alpha;

      H8(EMPTY,EMPTY);
  }

  #undef T
  #define T(i) d_out[offset+i] = w##i;

  T8(EMPTY,EMPTY);
}

////////////////////////////////////////////////////////////////////////
//
// TEST KERNEL
//

#define PXL_SHA256_KERNEL_MAIN
#ifdef  PXL_SHA256_KERNEL_MAIN

//
//
//

#undef  C
#define C(i)  const beu32 c##i

KERNEL_QUALIFIERS
LAUNCH_BOUNDS
void
sha256TestKernel(beu32* const hash, C16(COMMA,EMPTY))
{
#undef  H
#define H(i,alpha,magic)  beu32 hout##i;

  H8(EMPTY,EMPTY);

#undef  C
#define C(i)              c##i

#undef  H
#define H(i,alpha,magic)  &hout##i

  sha256_chunk0(C16(COMMA,EMPTY),H8(COMMA,EMPTY));

  //
  // SAVE H'S FOR NOW JUST SO NVCC DOESN'T OPTIMIZE EVERYTHING AWAY
  //
#undef  H
#define H(i,alpha,magic)  hash[i] = hout##i;

  H8(EMPTY,EMPTY);
}

//
//
//

#include <stdio.h>

int main(int argc, char** argv)
{
  cudaError_t err;
  int         device = (argc == 1) ? 0 : atoi(argv[1]);

  cudaDeviceProp props;
  err = cudaGetDeviceProperties(&props,device);

  if (err)
    return -1;

  printf("%s (%2d)\n",props.name,props.multiProcessorCount);

  cudaSetDevice(device);

  //
  // LAUNCH KERNEL
  //

  const int size = sizeof(beu32)*8;

  beu32* h_in = (beu32*)malloc(size);
  beu32* d_in;
  cudaMalloc(&d_in,sizeof(beu32)*8);

  beu32* h_out = (beu32*)malloc(size);
  beu32* d_out;
  cudaMalloc(&d_out,sizeof(beu32)*8);
  
  h_in[0] = 0xba7816bf;
  h_in[1] = 0x8f01cfea;
  h_in[2] = 0x414140de;
  h_in[3] = 0x5dae2223;
  h_in[4] = 0xb00361a3;
  h_in[5] = 0x96177a9c;
  h_in[6] = 0xb410ff61;
  h_in[7] = 0xf20015ad;

  cudaMemcpy(d_in,h_in,size,cudaMemcpyHostToDevice);
  sha256_iter<<<1,1>>>(1000000,d_in,d_out);
  //
  // FROM "FIPS 180-2, Secure Hash Standard, with Change Notice 1"
  //
  // B.1 SHA-256 Example (One-Block Message)
  //
  // Let the message, M, be the 24-bit (l = 24) ASCII string "abc ",
  // which is equivalent to the following binary string:
  //
  /*
  sha256TestKernel<<<1,1>>>(d_hash,
                            0x61626380,
                            0x00000000,
                            0x00000000,
                            0x00000000,
                            0x00000000,
                            0x00000000,
                            0x00000000,
                            0x00000000,
                            0x00000000,
                            0x00000000,
                            0x00000000,
                            0x00000000,
                            0x00000000,
                            0x00000000,
                            0x00000000,
                            0x00000018);
                            */
                            err = cudaDeviceSynchronize();

                            if (err) {
                              printf("Err = %d\n",err);
                              exit(err);
                            }
                          
                            //
                            //
                            //
                          
                            cudaMemcpy(h_out,d_out,sizeof(beu32)*8,cudaMemcpyDeviceToHost);
                          
                            printf("gold: %08x %08x %08x %08x %08x %08x %08x %08x\n",
                                   0xba7816bf,0x8f01cfea,0x414140de,0x5dae2223,
                                   0xb00361a3,0x96177a9c,0xb410ff61,0xf20015ad);
                          
                            printf("cuda: %08x %08x %08x %08x %08x %08x %08x %08x\n",
                              h_out[0],h_out[1],h_out[2],h_out[3],h_out[4],h_out[5],h_out[6],h_out[7]);
 /*                         
sha256TestKernel<<<1,1>>>(d_out,
                          0xba7816bf,
                          0x8f01cfea,
                          0x414140de,
                          0x5dae2223,
                          0xb00361a3,
                          0x96177a9c,
                          0xb410ff61,
                          0xf20015ad,
                          0x80000000,
                          0x00000000,
                          0x00000000,
                          0x00000000,
                          0x00000000,
                          0x00000000,
                          0x00000000,
                          0x00000100);                            

  err = cudaDeviceSynchronize();

  if (err) {
    printf("Err = %d\n",err);
    exit(err);
  }

  //
  //
  //

  cudaMemcpy(h_out,d_out,sizeof(beu32)*8,cudaMemcpyDeviceToHost);

  printf("gold: %08x %08x %08x %08x %08x %08x %08x %08x\n",
         0xba7816bf,0x8f01cfea,0x414140de,0x5dae2223,
         0xb00361a3,0x96177a9c,0xb410ff61,0xf20015ad);

  printf("cuda: %08x %08x %08x %08x %08x %08x %08x %08x\n",
    h_out[0],h_out[1],h_out[2],h_out[3],h_out[4],h_out[5],h_out[6],h_out[7]);
*/
  //
  //
  //

  cudaFree(d_in);
  cudaFree(d_out);
  free(h_in);
  free(h_out);

  cudaDeviceReset();

  return 0;
}

//
//
//

#endif

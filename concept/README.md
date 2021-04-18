# concept

Can build chains and solve them (See app/chain/Main.hs)

# TODO

* Differing chain lengths schedule
* Output final hash for use
* Output ending tower hashes for intermediate use
* verbose mode for printing chains
* verify chains
* set max number of cores
* logging tower creation and progress

* SSE/AVX Per thread Parallel version

* cpu speed benchmarking / parallel (compare against others in pool)

* test case to compare calculated num bytes with encoded bytes 
* test vectors for xchacha
    - https://github.com/orion-rs/orion/blob/master/tests/stream/rfc_xchacha20.rs

* test cuda-haskell driver api without toolkit

* can write a gpu shader implementation for many different platform support (amd, intel integrated, even nvidia)

* windows support


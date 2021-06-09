# concept

# TODO

* Output final hash for use
* Output ending tower hashes for intermediate use
* verify chains
* set max number of cores

* SSE/AVX Per thread Parallel version

* test case to compare calculated num bytes with encoded bytes 
* test vectors for xchacha
    - https://github.com/orion-rs/orion/blob/master/tests/stream/rfc_xchacha20.rs

* can write a gpu shader implementation for many different platform support (amd, intel integrated, even nvidia)

* windows support

* store original filename in TLA file for option to use it for decryption

* Instead of loading all hashes from file, send hashes from chain to check against file in O(n) maybe?

can do `stack build` and then `stack exec ghci`
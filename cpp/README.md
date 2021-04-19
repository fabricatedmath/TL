TODO:
* Serialization for TLA files
* Commandline flags for creating and solving chains
* Changing between different implementations (Generic, x86 SSE4.1 + SHA, CUDA, arm)
* implement generic sha
* implement optimized arm sha
* Chains in progress
  * Storing chains in progress
  * Properly handling sigint for running chain creation 
  * XOR hashes with a password if stored on disk. (Prevents pulling starting hashes off of disk image, even after deletion)
  * XOR starting hashes on disk with file SHA256 if actively encrypting a file
  * Sanity checksum for XOR starting hash read off of disk

* CUDA: need to cubin/ptx for driver api only...
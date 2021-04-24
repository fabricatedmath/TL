# TL
Time-lock encryption tools. Offline. Inspired by https://www.gwern.net/Self-decrypting-files

As described in the Gwern blog post above:

The idea is to generate N random numbers of 256 bits in length. We then, in parallel, successively hash each of these numbers for many iterations, using the output of one iteration for the input of the next. After 'i' iterations (what we will call a "hash tower") we have N*i total hashes done. We then "stitch" these towers of hashes together. We align these towers into a row, and for the first tower only, we write it's starting hash to a file. We then set the second tower as the current tower and repeat the following for every tower in the row. We take the previous tower's ending hash and xor it with the current tower's beginning hash and write the result to a file, we will call this result the "encrypted hash".

The contents of the file is now a type of treasure map. The treasure is the calculated hash of the final tower in the row, however finding this treasure requires a serial computation of N*i hashes in order to find. An explorer must start with the first tower's starting hash published in the file, hash for 'i' iterations to find the ending hash. They must then xor that hash with the published "encrypted hash" of the second tower in the file. This provides them with the original starting hash of the second tower. They must then repeat this process until the final tower's ending hash is found.

This "treasure key" can be used for many different things, such as using the final hash as a key to encrpyting a file or storing a bitcoin wallet address. The novel part of this algorithm is that the decrypting "time capsule" portion of the process is completely offline and does not require the good faith of an entity to divulge a decryption key at a future date. Another advantage is that there is great computational asymmetry involved in the parallel construction of these towers and the serial solving of these towers.

A checksum for each tower may also be generated to give assurance that there hasn't been an error in the computational steps in solving each tower. An obvious checksum may be calulated by incrementing the ending hash of each tower (with bit carry add) and then hashing the result and publishing it to a file. An entity solving the tower can now perform the identical steps to verify that their checksum and the published checksum match.

Obviously this whole process can be extended in many different ways. One can use each tower's ending hash as a key to encrypt a sort of breadcrump payload. Examples include bitcoin wallet addresses to incentivize the solving of the chain, or intermediate files as well.

Another interesting augmentation could be to provide human level puzzles (like a CAPTCHA) in order to derive the xor result of the ending tower's hash. This would necessitate human involvement after each tower is solved and perhaps a speedbump and bottleneck to solving many different chains at once. A more difficult puzzle could also be provided to encourage collaboration.

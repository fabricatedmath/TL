#include <iostream>
#include <thread>
#include <chrono>
using namespace std::chrono;

#include <cuda_sha.hpp>
#include <x86exts_sha.hpp>
#include <arm_sha.hpp>
#include <sha256.hpp>

#include <mutex>
#include <boost/asio/post.hpp>
#include <boost/asio/thread_pool.hpp>
#include <boost/program_options.hpp>
#include <boost/thread.hpp>

#include <sodium.h>

#include <tl_util.hpp>

#include <test.hpp>

namespace po = boost::program_options;

using namespace std;
/*
void myTask() {
    auto id = boost::this_thread::get_id();
    cout << "My id is: " << id << endl;

    uint32_t data[8];
    data[0] = 0xba7816bf;
    data[1] = 0x8f01cfea;
    data[2] = 0x414140de;
    data[3] = 0x5dae2223;
    data[4] = 0xb00361a3;
    data[5] = 0x96177a9c;
    data[6] = 0xb410ff61;
    data[7] = 0xf20015ad;

    print256(data);
    sha256_iter(1000000000, data);
    print256(data);
}*/

const uint32_t initialstate[8] = {
    0xba7816bf, 0x8f01cfea, 0x414140de, 0x5dae2223,
    0xb00361a3, 0x96177a9c, 0xb410ff61, 0xf20015ad
};

void doHash(Hashable* hashable) {
    hashable->hash();
}

int main(int argc, char** argv) {

    cout << "x86: " << X86ExtsSHA::is_available() << endl;
    cout << "arm: " << ARMSHA::is_available() << endl;

    uint32_t initialABC[8] = {
        0xba7816bf, 0x8f01cfea, 0x414140de, 0x5dae2223,
        0xb00361a3, 0x96177a9c, 0xb410ff61, 0xf20015ad
    };

    printHash(initialABC);

    ARMSHA::iterateHash(1, initialABC);

    printHash(initialABC);
    
    return 0;
/*
    Hashable* hashableCPU = new CPU();
    hashableCPU->hash();

    Hashable* hashableGPU = new GPU();
    hashableGPU->hash();

    doHash(new GPU());
    doHash(new CPU());
*/


    X86ExtsSHA::iterateHash(1, initialABC);

    printHash(initialABC);


/*
    CudaSHA::check_availablity();
    CudaSHA* cudaSHA = new CudaSHA;
    cudaSHA->init();
    cudaSHA->createChains(1,1,initialABC,initialABC);
    delete cudaSHA;

    printHash(initialABC);*/
    return 0;


    {
        auto t1 = high_resolution_clock::now();
        X86ExtsSHA::iterateHash(2, initialABC);
        auto t2 = high_resolution_clock::now();
        auto ms_int = duration_cast<milliseconds>(t2 - t1);

        /* Getting number of milliseconds as a double. */
        duration<double, std::milli> ms_double = t2 - t1;

        std::cout << ms_int.count() << "ms\n";
        std::cout << ms_double.count() << "ms\n";
    }

    uint32_t initialABC2[8] = {
        0xba7816bf, 0x8f01cfea, 0x414140de, 0x5dae2223,
        0xb00361a3, 0x96177a9c, 0xb410ff61, 0xf20015ad
    };

    printHash(initialABC);
    printHash(initialABC2);

    return 0;


    if (sodium_init() < 0) {
        /* panic! the library couldn't be initialized, it is not safe to use */
        return 1; 
    }

    #define MESSAGE ((const unsigned char *) "abc")
    #define MESSAGE_LEN 3

    unsigned char out[crypto_hash_sha256_BYTES];

    crypto_hash_sha256(out, MESSAGE, MESSAGE_LEN);

    printHash(reinterpret_cast<uint32_t*>(out));

    uint32_t hash[16];

    strongRandomHashes(hash, 2);
    printHashes(hash, 2);


    printHash(abcSHA256);
    printHash(abcSHA256_next);

    if (X86ExtsSHA::is_available()) {
        cout << "x86 sse4.1 and sha extensions are available" << endl;
    } else {
         cout << "x86 sse4.1 and/or sha extensions are not available" << endl;
    }
    
    const CudaSHA::Availability availability = CudaSHA::check_availablity();
    if(availability == CudaSHA::Available) {
        cout << "Cuda is available" << endl;
        CudaSHA cudaSHA;// = new CudaSHA();
        int result = cudaSHA.init();
        if (result != 0) {
            cout << "Failed to init" << endl;
            return -1;
        }

        result = cudaSHA.createChains(10,10,NULL, NULL);
        if (result != 0) {
            cout << "Failed to launch" << endl;
            return -1;
        }

        //delete cudaSHA;
    } else {
        cout << "Cuda is not available: " << CudaSHA::getAvailabilityString(availability) << endl;
    }

    return 0;

    if (sodium_init() < 0) {
        /* panic! the library couldn't be initialized, it is not safe to use */
        return 1; 
    } else {
        uint32_t startHash[8];
        randombytes_buf(startHash, 32);
        printHash(startHash);
    }

    po::options_description desc("Usage");
    desc.add_options()
        ("help", "produce help message")
        ("multi", "run the simple multi-gpu test")
        ("double,D", "Use Doubles (default Floats)")
        ("testing,T", "Run in testing mode")
        ("hash", po::value<int>(), "number of iterations")
        ;

    po::variables_map opts;
    po::store(po::parse_command_line(argc, argv, desc), opts);
    if (opts.count("help")) {
        cout << desc << "\n";
        return 1;
    }

    if (opts.count("hash")) {
        cout << "hash level was set to " 
            << opts["hash"].as<int>() << ".\n";
    } else {
        cout << "hash level was not set.\n";
    }
/*
    unsigned int n = std::thread::hardware_concurrency();
    std::cout << n << " concurrent threads are supported.\n";

    boost::asio::thread_pool workers(n);

    for (int i = 0; i < n; i++) {
        boost::asio::post(workers, myTask);
    }

    workers.join();
    */
}
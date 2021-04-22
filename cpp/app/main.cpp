#include <iostream>
#include <thread>

#include <cuda_sha.hpp>
#include <x86exts_sha.hpp>

#include <mutex>
#include <boost/asio/post.hpp>
#include <boost/asio/thread_pool.hpp>
#include <boost/program_options.hpp>
#include <boost/thread.hpp>

#include <sodium.h>

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

void print256This(uint32_t* data) {
    printf("Hex: ");
    for (int i = 0; i < 8; i++) {
        const uint32_t v = data[i];
        printf("%08x ", v);
    }
    printf("\n");
}

const uint32_t initialstate[8] = {
    0xba7816bf, 0x8f01cfea, 0x414140de, 0x5dae2223,
    0xb00361a3, 0x96177a9c, 0xb410ff61, 0xf20015ad
};

int main(int argc, char** argv) {

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

    uint32_t testState[8] = {
        0xba7816bf, 0x8f01cfea, 0x414140de, 0x5dae2223,
        0xb00361a3, 0x96177a9c, 0xb410ff61, 0xf20015ad
    };

    return 0;
/*
#ifdef CUDACOMPILED
    print256This(testState);

    const int errorCode = sha256_iter_cuda(1, 1, testState);
    if (errorCode != 0) {
        cout << getErrorString(errorCode) << endl;
        exit(EXIT_FAILURE);
    }

    print256This(testState);
#endif //CUDACOMPILED
*/
    return 0;

    if (sodium_init() < 0) {
        /* panic! the library couldn't be initialized, it is not safe to use */
        return 1; 
    } else {
        uint32_t startHash[8];
        randombytes_buf(startHash, 32);
        print256This(startHash);
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
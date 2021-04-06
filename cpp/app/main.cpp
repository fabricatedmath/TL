#include <iostream>
extern "C" {
#include <sha256-sse41-sha-x86.h>
}

#include <boost/program_options.hpp>

namespace po = boost::program_options;

using namespace std;

int main(const int argc, const char* const argv[]) {
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
}
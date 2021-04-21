#include <iostream>
using namespace std;

#include <cuda_sha.hpp>

int main() {
    cout << "dogs" << endl;
    cout << CudaSHA::is_available() << endl;
}
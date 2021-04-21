#include <iostream>
using namespace std;

#include <cuda_sha.hpp>

int main() {
    cout << "dogs" << endl;
    cout << CudaSHA::check_availablity() << endl;
}
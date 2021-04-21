#include <iostream>
using namespace std;

#include <cuda_sha.hpp>

int main() {
    CudaSHA::Availability availability = CudaSHA::check_availablity();
    cout <<  CudaSHA::getAvailabilityString(availability) << endl;
}
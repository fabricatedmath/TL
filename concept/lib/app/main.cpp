#include <iostream>
using namespace std;

#include <x86_sha.hpp>

int main() {
  cout << "dogs" << endl;
  cout << X86Sha::is_available() << endl;
}
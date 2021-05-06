#include <iostream>
using namespace std;

#include <x86exts_sha.hpp>

int main() {
  cout << "dogs" << endl;
  cout << X86ExtsSHA::is_available() << endl;
}
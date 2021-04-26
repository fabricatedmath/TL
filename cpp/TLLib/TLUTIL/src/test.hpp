#pragma once

#include <iostream>
using namespace std;

class Hashable {
public:
  virtual void hash() {};
};

class CPU : public Hashable {
public:
  void hash() {
    cout << "cpu" << endl;
  }
};

class GPU : public Hashable {
public:
  void hash() {
    cout << "gpu" << endl;
  }
};
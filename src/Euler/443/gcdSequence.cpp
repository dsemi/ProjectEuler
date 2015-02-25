#include <iostream>

using namespace std;

unsigned long gcd(unsigned long a, unsigned long b) {
  unsigned long a1, b1, tmp;
  if (a > b) {
    a1 = a; b1 = b;
  }
  else {
    a1 = b; b1 = a;
  }
  tmp = 0;
  while ((tmp = a1 % b1) != 0) {
    a1 = b1;
    b1 = tmp;
  }
  return b1;
}

unsigned long _g(unsigned long n, unsigned long p, unsigned long s) {
  while (n < s) {
    p += gcd(n,p);
    n++;
  }
  return p + gcd(n,p);
}

unsigned long g(unsigned long n) {
  if (n == 4)
    return 13;
  else
    return _g(5L,13L,n);
}

int main() {
  cout << g(1000000000) << endl;
}


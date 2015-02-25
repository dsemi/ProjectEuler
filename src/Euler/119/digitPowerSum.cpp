#include <iostream>
#include <algorithm>
#include <vector>

using namespace std;

typedef unsigned long long BigInt;

int sumOfDigits(BigInt);

int main() {
  const BigInt LIMIT = (BigInt)-1 /2;
  vector<BigInt> specialNums;

  for(BigInt sum = 2; sum < 100; sum++) {
    for(BigInt power = sum * sum; power <= LIMIT && power != 0; power *= sum) {
      if(sumOfDigits(power) == sum) {
        specialNums.push_back(power);
      }
    }
  }
  sort(specialNums.begin(), specialNums.end());
  int sum = 0;
  for (int i=0; i<30; i++) {
      sum += specialNums[i];
  }
  cout << specialNums[29] << endl;
}

int sumOfDigits(BigInt num) {
  int sum = 0;
  while (num > 0) {
    sum += num % 10;
    num /= 10;
  }
  return sum;
}

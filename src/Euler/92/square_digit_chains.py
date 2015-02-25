#!/usr/bin/python

from itertools import permutations

others = {}
others[1] = 1
others[89] = 89

def nextNum(num):
    x = sum([int(i)**2 for i in str(num)])
    return others.get(x,x)


def main():
    count = 0
    for i in range(1,1000000):
        if i % 1000 == 0:
            print(i)
        current_chain = []
        current_num = i
        while current_num != 1 and current_num != 89:
            current_chain.append(current_num)
            current_num = nextNum(current_num)
        if current_num == 89:
            count += 1
        for i in current_chain:
            # if i <= 567:
            for num in set(permutations(str(i))):
                others[int(''.join(num))] = current_num

    print('There are %d numbers that end at 89' % count)

if __name__ == '__main__':
    main()

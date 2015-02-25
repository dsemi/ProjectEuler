#!/usr/bin/python

import os
import re
import stat
import os.path as op
from glob import glob
from docopt import docopt
from operator import itemgetter
from subprocess import Popen, PIPE
from colorama import init, Fore, Back, Style
from signal import signal, SIGPIPE, SIG_DFL
signal(SIGPIPE,SIG_DFL) 

init(autoreset=True)
basedir = op.dirname(op.realpath(__file__))
executable = stat.S_IEXEC | stat.S_IXGRP | stat.S_IXOTH
with open('../.gitignore') as f:
    ignores = set(s.strip() for s in f.readlines())
new_exes = set()

def memoize(f):
    class memodict(dict):
        def __missing__(self, key):
            ret = self[key] = f(key)
            return ret
    return memodict().__getitem__

def find_missing(num_problems):
    for i in range(1,num_problems+1):
        d = op.join(basedir, str(i))
        if not op.isdir(d) or not any(os.stat(f).st_mode & executable for f in glob(op.join(d,'*'))):
            yield op.basename(d)

@memoize
def run(problem_no):
    result = None
    d = op.join(basedir, str(problem_no))
    if op.isdir(d):
        os.chdir(d)
        for f in glob(op.join(d,'*')):
            if os.stat(f).st_mode & executable:
                new_exes.add(re.search(r'[^/]+/[^/]+$', f).group(0))
                p = Popen(['/usr/bin/time','-f"%e"',f], stdout=PIPE, stderr=PIPE)
                results = p.communicate()
                result = {'num': problem_no, 'result': results[0].decode('utf-8').strip(), 'time': float(results[1].decode('utf-8').strip(' \t\r\n"'))}
                break
        os.chdir(basedir)
    return result

def format_output(ans_dict, noans=False):
    def f(n):
        if n < 0.5:
            return Fore.GREEN + "%.3f" % n + Style.RESET_ALL
        elif n < 1:
            return Fore.YELLOW + "%.3f" % n + Style.RESET_ALL
        else:
            return Fore.RED + "%.3f" % n + Style.RESET_ALL
    if noans:
        return "Problem {0:>3}:                               Elapsed time: {1} seconds".format(ans_dict['num'],f(ans_dict['time']))
    else:
        return "Problem {0:>3}: {1:>28}  Elapsed time: {2} seconds".format(ans_dict['num'],ans_dict['result'],f(ans_dict['time']))

__doc__ = """
Usage: run_all.py missing
       run_all.py [analyze] [--noans] [<problems>...]
       run_all.py --help

Options:
  analyze
      Analyzes the time elapsed for each problem and outputs the sorted times in decreasing order

  missing
      Prints out the missing problems

  <problems>...
      A list of the problem numbers to run

  -h, --help
      Show this screen

  -n, --noans
      Don't print answers

"""

def main():
    os.chdir(basedir)
    num_problems = 500
    args = docopt(__doc__)
    problems = range(1,num_problems+1)
    if args['missing']:
        for prob in find_missing(num_problems):
            print(prob)
    else:
        if args['<problems>']:
            problems = []
            for p in args['<problems>']:
                rng = re.match(r'(\d+)\-(\d+)',p)
                if rng:
                    for i in range(int(rng.group(1)), int(rng.group(2))+1):
                        problems.append(i)
                else:
                    problems.append(p)
        if args['analyze']:
            probs = sorted([run(i) for i in problems if run(i) is not None], key=itemgetter('time'), reverse=True)
            for p in probs:
                print(format_output(p, args['--noans']))
        else:
            count = time = 0
            for i in problems:
                ans = run(i)
                if ans is not None:
                    count += 1
                    time += ans['time']
                    print(format_output(ans, args['--noans']))
            print("Total   {0:>3}: {1:>49.3f} seconds".format(count, time))

    with open('.gitignore', 'a') as f:
        for e in (new_exes - ignores):
            f.write('%s\n' % e)

if __name__ == '__main__':
    main()

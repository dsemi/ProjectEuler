#!/usr/bin/python

import os, re, sys, time
import os.path as op
from subprocess import Popen, PIPE
from colorama import init, Fore, Back, Style
from signal import signal, SIGPIPE, SIG_DFL

signal(SIGPIPE,SIG_DFL)
init(autoreset=True)
basedir = op.dirname(op.realpath(__file__))

problem_pattern = re.compile(r'Problem\s+[0-9]+:')
end_pattern = re.compile(r'Total\s+[0-9]')
total_time = 0

def format_time(elapsed_time):
    global total_time
    def f(n):
        if n < 0.5:
            return Fore.GREEN + "%.3f" % n + Style.RESET_ALL
        elif n < 1:
            return Fore.YELLOW + "%.3f" % n + Style.RESET_ALL
        else:
            return Fore.RED + "%.3f" % n + Style.RESET_ALL
    total_time += elapsed_time
    return "  Elapsed time: {} seconds".format(f(elapsed_time))

def get_time_data(problem_line, elapsed_time):
    if problem_pattern.match(problem_line):
        return problem_line + format_time(elapsed_time)
    elif end_pattern.match(problem_line):
        return problem_line + ": {:>49.3f} seconds".format(total_time)
    else:
        return problem_line

def main():
    os.chdir(basedir)
    p = Popen(['./dist/build/ProjectEuler/ProjectEuler'] + sys.argv[1:], stdout=PIPE)
    while True:
        start = time.time()
        line = p.stdout.readline().decode('utf-8').strip()
        end = time.time()
        line = get_time_data(line, end - start)
        if line == '' and p.poll() is not None:
            break
        print(line)
    

if __name__ == '__main__':
    main()

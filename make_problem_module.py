#!/usr/bin/python

from glob import glob

template = '\n'.join([
    'module Euler',
    '( {}',
    ') where',
    '{}'
])

def main():
    print('Creating Euler module')
    exported_functions = []
    imported_modules = []
    for m in sorted(glob('src/Euler/Problem???.hs')):
        imported_modules.append('import ' + m[4:-3].replace('/', '.'))
        exported_functions.append('problem%d' % int(m[17:20]))
    output = template.format('\n, '.join(exported_functions),
                             '\n'.join(imported_modules))
    with open('src/Euler.hs', 'r+') as f:
        if f.read() != output:
            f.seek(0)
            f.write(output)
            f.truncate()

if __name__ == '__main__':
    main()

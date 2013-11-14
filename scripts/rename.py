from __future__ import print_function, unicode_literals

import os
import os.path
import shutil
import sys

extensions = {'jpg'}


def do_rename(prefix, path):
    for root, dirs, files in os.walk(path):
        for file in files:
            srcpath = os.path.join(path, file)
            destpath = os.path.join(path, prefix + file)
            try:
                shutil.move(srcpath, destpath)
            finally:
                print('{} copied to {}.'.format(srcpath, destpath))
    print('Done!')


def main(argv):
    import argparse

    parser = argparse.ArgumentParser()
    parser.add_argument('-p', '--prefix', required=True)
    parser.add_argument('-d', '--dir', default=os.curdir)
    args = parser.parse_args(args=argv)

    do_rename(args.prefix, args.dir)


if __name__ == '__main__':
    main(sys.argv[1:])

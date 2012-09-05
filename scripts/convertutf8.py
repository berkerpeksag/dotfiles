#!/usr/bin/env python
# coding: utf-8

from sys import argv

import os
import re

extensions = ('srt', 'sub')

SOURCE_ENCODING = 'iso-8859-9'
TARGET_ENCODING = 'utf-8'

_slugify = lambda _str: re.sub(r'\W+', '-', _str.lower())


def _convert_utf8(source, dir_path):
    source_name, source_ext = os.path.splitext(source)
    target_name = _slugify(source_name) + '_utf8' + source_ext
    source_file = open(dir_path + source)
    target_file = open(dir_path + target_name, 'w')
    target_file.write(unicode(source_file.read(),
                              SOURCE_ENCODING).encode(TARGET_ENCODING))
    os.remove(dir_path + source)


def main(dir_path):
    files = os.listdir(dir_path)
    for _file in files:
        if _file.endswith(extensions):
            _convert_utf8(_file, dir_path)


def usage():
    exit('Usage: python {0} <DIR>'.format(__file__))

if __name__ == '__main__':
    if len(argv) != 2 or not isinstance(argv[1], basestring):
        usage()
    main(argv[1])

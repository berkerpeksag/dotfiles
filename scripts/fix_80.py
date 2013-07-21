# coding: utf-8

"""
TODOs
-----

* \n hesaplaması daha tutarlı olmalı
* LINE_LENGTH konsoldan ayarlanabilir olmalı
* Çıktı dosyası konsoldan verilebilmeli: -o hede.md
* argparse kullan
* Düzgün USAGE metni

"""

from __future__ import print_function

import textwrap

LINE_LENGTH = 80


def main(file):
    wrapped_text = []
    with open(file, 'r') as f:
        for line in f:
            if len(line) > LINE_LENGTH:
                wrapped = '\n'.join(textwrap.wrap(line, LINE_LENGTH))
                wrapped_text.append(wrapped)
            else:
                wrapped_text.append(line)
    with open(file, 'w+') as f:
        f.write(''.join(wrapped_text))

if __name__ == '__main__':
    import sys

    main(sys.argv[1])

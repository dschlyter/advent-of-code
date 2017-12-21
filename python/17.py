#!/usr/bin/python

import argparse
import subprocess
import sys
import re
import json
from pprint import pprint
from tempfile import TemporaryFile

step = 382
# it = 2017
it = 50000000

def main():
    pos = 0
    size = 1
    first = -1

    # right ans for p1: 41

    while size < it+1:
        if size % 1000000 == 0:
            print "size", size
        pos = (pos + step) % size + 1
        if pos == 1:
            first = size
        size += 1

    print first

main()

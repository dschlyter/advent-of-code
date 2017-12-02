#!/usr/bin/python

import argparse
import subprocess
import sys
import re
import json
from pprint import pprint
from tempfile import TemporaryFile

description='TODO describe script'

def parse_args():
    parser = argparse.ArgumentParser(description=description)
    parser.add_argument('-c', '--counter', dest='counter', default=14, type=int,
            help='a simple counter')
    parser.add_argument('-f', '--flag', dest='flag', action='store_true',
            help='a simple flag')
    parser.add_argument('first',
            help='required first arg')
    parser.add_argument('second', nargs='?', default=42,
            help='optional trailing arg')
    return parser.parse_args()
input_sample = """
5 1 9 5
7 5 3
2 4 6 8
"""

input_sample2 = """
5 9 2 8
9 4 7 3
3 8 6 5
"""

input = """
1136    1129    184 452 788 1215    355 1109    224 1358    1278    176 1302    186 128 1148
242 53  252 62  40  55  265 283 38  157 259 226 322 48  324 299
2330    448 268 2703    1695    2010    3930    3923    179 3607    217 3632    1252    231 286 3689
89  92  903 156 924 364 80  992 599 998 751 827 110 969 979 734
100 304 797 81  249 1050    90  127 675 1038    154 715 79  1116    723 990
1377    353 3635    99  118 1030    3186    3385    1921    2821    492 3082    2295    139 125 2819
3102    213 2462    116 701 2985    265 165 248 680 3147    1362    1026    1447    106 2769
5294    295 6266    3966    2549    701 2581    6418    5617    292 5835    209 2109    3211    241 5753
158 955 995 51  89  875 38  793 969 63  440 202 245 58  965 74
62  47  1268    553 45  60  650 1247    1140    776 1286    200 604 399 42  572
267 395 171 261 79  66  428 371 257 284 65  25  374 70  389 51
3162    3236    1598    4680    2258    563 1389    3313    501 230 195 4107    224 225 4242    4581
807 918 51  1055    732 518 826 806 58  394 632 36  53  119 667 60
839 253 1680    108 349 1603    1724    172 140 167 181 38  1758    1577    748 1011
1165    1251    702 282 1178    834 211 1298    382 1339    67  914 1273    76  81  71
6151    5857    4865    437 6210    237 37  410 544 214 233 6532    2114    207 5643    6852
"""

def main():
    sum = 0
    inp = input
    for line in inp.strip().split("\n"):
        print "line", line
        elems = line.split(" ")
        for elem1 in elems:
            for elem2 in elems:
                if not elem1 or not elem2 or elem1 == elem2:
                    continue
                e1 = int(elem1)
                e2 = int(elem2)
                if e1 % e2 == 0:
                    print e1, e2
                    sum += e1 / e2

    print sum

def prob1():
    sum = 0
    for line in input.strip().split("\n"):
        print line
        b, s = 0, 999999
        for elem in line.split(" "):
            if not elem:
                continue
            i = int(elem)
            b = max(b, i)
            s = min(s, i)
        sum += b - s
    print sum

main()

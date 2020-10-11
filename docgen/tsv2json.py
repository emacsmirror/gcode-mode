#!/usr/bin/env python3
import argparse
import json


def parse_tsv(path, flavour):
    ret = []
    for line in open(path):
        line = line.rstrip()
        if len(line) == 0 or line.startswith('#'):
            continue
        code, title = line.split('\t')

        # Convert to the same structure as Marlin2
        ret.append({'data': {'flavour': flavour,
                             'title': title,
                             'codes': [code],
                             'parameters': []}})

    return ret


def main():
    ap = argparse.ArgumentParser(description='Convert a 2-column TSV file to JSON')
    ap.add_argument('--flavour', default='Generic', help='G-Code flavour')
    ap.add_argument('file', nargs='+', help='documentation file')
    args = ap.parse_args()

    ret = []
    for path in args.file:
        data = parse_tsv(path, args.flavour)
        ret.extend(data)

    print(json.dumps(ret))


if __name__ == '__main__':
    exit(main())

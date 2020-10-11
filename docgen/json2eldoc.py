#!/usr/bin/env python3
import collections
import argparse
import json


def normalize_json(data):
    ret = []
    buf = {'title': data['data']['title'], 'params': {}}

    # extract parameters
    for param in data['data']['parameters']:
        buf['params'][param['tag']] = param.get('description')

    # unbundle compound documentation
    for code in data['data']['codes']:
        tmp = buf.copy()
        tmp['code'] = code
        ret.append(tmp)

    return ret


def escape_estr(string):
    return '"' + string.replace('\\', '\\\\').replace('"', '\\"') + '"'

def output_eldoc(entries):
    print(''';;; gcode-mode-doc.el --- G-Code documentation entries  -*- lexical-binding: t -*-
;; WARNING! This file is automatically generated using scripts in docgen/!

(defun gcode-mode--doc-entries () '(''')
    for entry in entries:
        line = '  ('
        line += escape_estr(entry['code'])
        line += ' . '
        line += escape_estr(entry['title'])
        line += ')'
        print(line)
    print('))')


def main():
    ap = argparse.ArgumentParser(description='Convert JSON entries to G-Code EL entries')
    ap.add_argument('file', nargs='+', help='JSON documentation entries (merged)')
    ap.add_argument('--override', nargs='+', default=[], help='JSON override entries')
    ap.add_argument('--fallback', nargs='+', default=[], help='JSON fallback entries')
    args = ap.parse_args()

    codes = collections.defaultdict(list)
    entries = []

    # load main entries
    for path in args.file:
        for data in json.loads(open(path).read()):
            for entry in normalize_json(data):
                entries.append(entry)
                codes[entry['code']].append(entry)

    # override entries
    overridden = set()
    for path in args.override:
        for data in json.loads(open(path).read()):
            for entry in normalize_json(data):
                # remove all existing entries
                code = entry['code']
                if code not in overridden:
                    for old in codes[code]:
                        entries.remove(old)
                    overridden.add(code)
                # add overrides
                entries.append(entry)

    # fallback entries
    for path in args.fallback:
        for data in json.loads(open(path).read()):
            for entry in normalize_json(data):
                # append only if missing
                if entry['code'] not in codes:
                    entries.append(entry)

    # sort entries to be more VCS friendly
    entries = sorted(entries, key=lambda x: (x['code'][0], float(x['code'][1:]), x['title']))
    output_eldoc(entries)


if __name__ == '__main__':
    exit(main())

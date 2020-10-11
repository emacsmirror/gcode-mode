#!/usr/bin/env python3
import argparse
import mistune
import html2text
import yaml
import json
import sys
import re


## HTML/Markdown handling

def ht2text(html):
    text = html2text.html2text(html)

    # html2text bugs?
    text = text.replace('&lt;', '<')
    text = text.replace('&gt;', '>')
    text = text.replace('&quot;', '"')
    text = text.replace('&amp;', '&')

    return text


class PlainTextRenderer(mistune.AstRenderer):
    def __init__(self, strip_link_pair=['/docs/gcode/', '.html']):
        super(PlainTextRenderer, self).__init__()
        self.strip_link_pair = strip_link_pair

    def text(self, text):
        return ''.join(text)

    def paragraph(self, text):
        return ''.join(text)

    def inline_html(self, text):
        return ht2text(''.join(text))

    def block_text(self, text):
        return ''.join(text)

    def block_code(self, text):
        return ''.join(text)

    def block_quote(self, text):
        return ''.join(text)

    def codespan(self, text):
        return ''.join(text)

    def newline(self):
        return '\n'

    def linebreak(self):
        return '\n'

    def emphasis(self, text):
        return '_' + ''.join(text) + '_'

    def strong(self, text):
        return '*' + ''.join(text) + '*'

    def link(self, link, children=None, title=None):
        if title is not None:
            return title
        else:
            prefix, suffix = self.strip_link_pair
            if link.startswith(prefix) and link.endswith(suffix):
                link = link[len(prefix):-len(suffix)]
            return link

    def heading(self, children, level):
        return '#' * level + ' ' + ''.join(children)

    def list_item(self, children, level):
        return '' * level + ''.join(children)

    def list(self, children, ordered, level, start=None):
        n = 0
        res = []
        for child in children:
            pref = '-' if not ordered else str(n)
            res.append(pref + ' ' + child)
        return '\n'.join(res)


def md2text(text):
    markdown = mistune.Markdown(renderer=PlainTextRenderer())
    ret = markdown(text)
    return '\n\n'.join(ret)


def md2text_deep(data):
    if isinstance(data, dict):
        for k, v in data.items():
            if isinstance(v, (dict, list)):
                data[k] = md2text_deep(v)
            elif isinstance(v, str):
                data[k] = md2text(v)
    elif isinstance(data, list):
        for i, e in enumerate(data):
            if isinstance(e, (dict, list)):
                data[i] = md2text_deep(e)
            elif isinstance(e, str):
                data[i] = md2text(e)
    return data


## Marlin documentation entry

def parse_entry(path):
    # split off data and description
    data = []
    desc = []
    block = 0
    for line in open(path):
        if line == '---\n':
            block += 1
        elif block == 1:
            data.append(line)
        elif block == 2:
            desc.append(line)
    data = ''.join(data).strip()
    desc = ''.join(desc).strip()

    # parse the YAML data block
    ret = yaml.safe_load(data)
    ret = md2text_deep(ret)

    # fixup parameters to always be a list
    if ret.get('parameters') is None:
        ret['parameters'] = []
    else:
        # check for empty parameters
        filtered = list(filter(lambda x: x is not None, ret['parameters']))
        if len(filtered) != len(ret['parameters']):
            print('warning: {} containts empty parameter node'.format(path), file=sys.stderr)
            ret['parameters'] = filtered

        for param in filtered:
            # fixup tag/s to always be a list like 'codes'
            if isinstance(param['tag'], str):
                tag = param['tag']
                tags = list(map(lambda x: x.strip(), re.split(r'[\s,]+', tag)))
                if len(tags) != 1:
                    print('warning: {} unpacking multiple parameters "{}" into {}'.format(path, tag, tags), file=sys.stderr)
                param['tag'] = tags

            # check tag lenght
            for tag in param['tag']:
                if len(tag) != 1:
                    print('warning: {} unsupported parameter "{}", ignoring'.format(path, tag), file=sys.stderr)
                    param['tag'].remove(tag)

            # check tag case
            for i, e in enumerate(param['tag']):
                e = e.upper()
                if e != param['tag'][i]:
                    print('warning: {} fixing lower-case parameter {}'.format(path, e), file=sys.stderr)
                    param['tag'][i] = e

    # kill codes with parameters
    for code in ret['codes']:
        if not re.match(r'^[A-Z]-?0*\d+(?:\.\d+)?$', code):
            print('warning: {} contains unsupported code "{}", ignoring'.format(path, code), file=sys.stderr)
            ret['codes'].remove(code)
    if len(ret['codes']) == 0:
        return None

    # add some custom tags
    if 'flavor' not in ret:
        ret['flavor'] = 'Marlin'

    return {'data': ret,
            'desc': md2text(desc)}


def main():
    ap = argparse.ArgumentParser(description='Convert a Marlin2 documentation entry to JSON')
    ap.add_argument('file', nargs='+', help='md documentation file')
    args = ap.parse_args()

    ret = []
    for path in args.file:
        data = parse_entry(path)
        if data is not None:
            ret.append(data)

    print(json.dumps(ret))


if __name__ == '__main__':
    exit(main())

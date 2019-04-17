#!/usr/bin/env python3

"""Spider Roskilde Program and produce json output.

(for now)

"""

import datetime
import dateutil.parser
import json
import os
import sys
import urllib

import lxml.etree
import requests

BASEURL = "https://www.roskilde-festival.dk/en/line-up/"
HOSTURL = urllib.parse.urlunsplit(
    (lambda u: (u[0], u[1], '', '', ''))(urllib.parse.urlsplit(BASEURL)))


def main(argv):
    ret = json.dumps(get_main(),
                     default=lambda x: x.isoformat(),
                     indent=2)
    print(ret)
    return ret


def get_main():
    overview = get_parsed(BASEURL)
    return bandlist(overview)


def bandlist(overview):
    bands = dict(complete_item(i)
                 for i in overview.xpath('.//div[@class="item-inner"]'))
    return bands


class session(object):

    SESSION = None

    def __init__(self, *args, **kwargs):
        if session.SESSION is None:
            session.SESSION = requests.Session(*args, **kwargs)
            self.__dict__.update(self.SESSION.__dict__)


def get_parsed(url):
    return lxml.etree.fromstring(
        session().get(url).text,
        lxml.etree.HTMLParser())


def complete_item(item):
    key, parsed_item = parse_main_item(item)
    page = parse_act_page(get_parsed(HOSTURL + '/' + parsed_item['link']))
    parsed_item.update(page)
    return key, parsed_item


def parse_main_item(item):
    a = item.xpath('a')[0]
    key = a.text.strip()
    return key, {
        'link': a.attrib['href'],
        'country': a.xpath(
            'div[@class="item-meta"]/div[@class="country"]')[0].text,
        'data-filters': get_data_filters(item)}


def get_data_filters(item):
    words = {
        '1595': 'Music',
        '2685': 'Arts & Activism'
    }
    items = item.xpath('..')[0].attrib['data-filters'].split()
    ret = []
    for k in items:
        if k in words:
            ret.append(words[k])
    return ret


def parse_act_page(item):
    blocks = item.xpath('.//div[@class="info"]/div[@class="block"]')
    ret = {
        'stage': blocks[0].xpath('text()')[0],
        'date': dateutil.parser.parse(
            blocks[1]
            .xpath('*//text()')
            [1]).date()
    }
    if len(blocks) > 2:
        ret['links'] = {
            a.text: a.attrib['href']
            for a in blocks[2].findall('a')
        }
    header = item.xpath(
        './/div[@class="TextModule"]'
        '/div[@class="inner"]'
        '/div[@class="copy"]'
        '/h6')
    if header:
        ret['tagline'] = header[0].xpath('text()|*//text()')[0]
    ret['article'] = get_article(item)
    return ret


def get_article(item):
    return ''.join(
        lxml.etree.tostring(x).decode('utf-8')
        for x in item.xpath(
                '//div[contains(@class, "TextModule")]'
                '|//div[contains(@class, "SpotifyModule")]'
                '|//div[contains(@class, "MediaModule")]'))


if __name__ == "__main__":
    sys.exit(main(sys.argv))

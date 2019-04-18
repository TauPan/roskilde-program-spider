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


def main(argv):
    ret = json.dumps(get_main(),
                     default=lambda x: x.isoformat(),
                     indent=2)
    print(ret)
    return ret


def get_main():
    overview = get_parsed(BASEURL)
    return bandlist(overview)


def get_parsed(url):
    return lxml.etree.fromstring(
        session().get(url).text,
        lxml.etree.HTMLParser())


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

    def get(self, *args, **kwargs):
        return session.SESSION.get(*args, **kwargs)


def complete_item(item):
    return BandListItem(item).parse()


class BandListItem(object):
    def __init__(self, item):
        self.item = item


    def parse(self):
        self.key, self.parsed_item = self.parse_main_item()
        HOSTURL = urllib.parse.urlunsplit(
            (lambda u: (u[0], u[1], '', '', ''))(urllib.parse.urlsplit(BASEURL)))
        page = parse_act_page(get_parsed(HOSTURL
                                         + '/'
                                         + self.parsed_item['link']))
        self.parsed_item.update(page)
        return self.key, self.parsed_item


    def parse_main_item(self):
        a = self.item.xpath('a')[0]
        key = a.text.strip()
        return key, {
            'link': a.attrib['href'],
            'country': a.xpath(
                'div[@class="item-meta"]/div[@class="country"]')[0].text,
            'data-filters': get_data_filters(self.item)}


def parse_act_page(item):
    blocks = item.xpath('.//div[@class="info"]/div[@class="block"]')
    ret = {
        'stage': blocks[0].xpath('text()')[0],
        'date': get_date(blocks)
    }
    set_tagline(ret, item)
    set_links(ret, blocks)
    ret['article'] = get_article(item)
    return ret


def get_date(blocks):
    return dateutil.parser.parse(
        blocks[1]
        .xpath('*//text()')
        [1]).date()

def set_links(dct, blocks):
    if len(blocks) > 2:
        dct['links'] = {
            a.text: a.attrib['href']
            for a in blocks[2].findall('a')
        }


def set_tagline(dct, item):
    header = item.xpath(
        './/div[@class="TextModule"]'
        '/div[@class="inner"]'
        '/div[@class="copy"]'
        '/h6')
    if header:
        dct['tagline'] = header[0].xpath('text()|*//text()')[0]


def get_data_filters(item):
    words = {
        '1595': 'Music',
        '2685': 'Arts & Activism'
    }
    items = item.xpath('..')[0].attrib['data-filters'].split()
    return [words[k]
            for k in items
            if k in words]


def get_article(item):
    return ''.join(
        lxml.etree.tostring(x).decode('utf-8')
        for x in item.xpath(
                '//div[contains(@class, "TextModule")]'
                '|//div[contains(@class, "SpotifyModule")]'
                '|//div[contains(@class, "MediaModule")]'))


if __name__ == "__main__":
    sys.exit(main(sys.argv))

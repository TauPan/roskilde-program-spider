#!/usr/bin/env python3

"""Spider Roskilde Program and produce json output.

(for now)

"""

import dateutil.parser
import json
import sys
import urllib

from cached_property import cached_property
import lxml.etree
import requests

BASEURL = "https://www.roskilde-festival.dk/en/line-up/"


def main(argv):
    ret = json.dumps(get_data(),
                     default=lambda x: x.isoformat(),
                     indent=2)
    print(ret)
    return ret


def get_data():
    overview = get_parsed(BASEURL)
    return bandlist(overview)


def get_parsed(url):
    return lxml.etree.fromstring(
        session().get(url).text,
        lxml.etree.HTMLParser())


def bandlist(overview):
    bands = dict(BandListItem(i).parse()
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


class BandListItem(object):
    def __init__(self, item):
        self.item = item

    def parse(self):
        self.parsed_item = dict(self.parsed_main_item)
        HOSTURL = urllib.parse.urlunsplit(
            (lambda u: (u[0], u[1], '', '', ''))
            (urllib.parse.urlsplit(BASEURL)))
        page = ActPage(
            get_parsed(HOSTURL
                       + '/'
                       + self.link)).parse()
        self.parsed_item.update(page)
        return self.key, self.parsed_item

    @cached_property
    def a(self):
        return self.item.xpath('a')[0]

    @cached_property
    def key(self):
        return self.a.text.strip()

    @cached_property
    def link(self):
        return self.a.attrib['href']

    @cached_property
    def country(self):
        return self.a.xpath(
            'div[@class="item-meta"]/div[@class="country"]')[0].text

    @cached_property
    def parsed_main_item(self):
        return {
            'link': self.link,
            'country': self.country,
            'data-filters': self.data_filters}

    @cached_property
    def data_filters(self):
        words = {
            '1595': 'Music',
            '2685': 'Arts & Activism'
        }
        items = self.item.xpath('..')[0].attrib['data-filters'].split()
        return [words[k]
                for k in items
                if k in words]


class ActPage(object):

    def __init__(self, item):
        self.item = item

    def parse(self):
        ret = {
            'stage': self.blocks[0].xpath('text()')[0],
            'date': self.get_date()
        }
        self.set_tagline(ret)
        self.set_links(ret)
        ret['article'] = self.get_article()
        return ret

    @cached_property
    def blocks(self):
        return self.item.xpath(
            './/div[@class="info"]/div[@class="block"]')

    def get_date(self):
        return dateutil.parser.parse(
            self.blocks[1]
            .xpath('*//text()')
            [1]).date()

    def set_tagline(self, dct):
        header = self.item.xpath(
            './/div[@class="TextModule"]'
            '/div[@class="inner"]'
            '/div[@class="copy"]'
            '/h6')
        if header:
            dct['tagline'] = header[0].xpath('text()|*//text()')[0]

    def set_links(self, dct):
        if len(self.blocks) > 2:
            dct['links'] = {
                a.text: a.attrib['href']
                for a in self.blocks[2].findall('a')
            }

    def get_article(self):
        return ''.join(
            lxml.etree.tostring(x).decode('utf-8')
            for x in self.item.xpath(
                    '//div[contains(@class, "TextModule")]'
                    '|//div[contains(@class, "SpotifyModule")]'
                    '|//div[contains(@class, "MediaModule")]'))


if __name__ == "__main__":
    sys.exit(main(sys.argv))

#!/usr/bin/env python3

"""Spider Roskilde Program and produce json output.

(for now)

"""

import dateutil.parser
import json
import sys
import urllib.parse

from cached_property import cached_property  # type: ignore
import lxml.etree  # type: ignore
import requests

BASEURL = "https://www.roskilde-festival.dk/en/line-up/"


def main(argv):
    ret = json.dumps(get_data(),
                     default=lambda x: x.isoformat(),
                     indent=2)
    print(ret)
    return ret


def get_data():
    line_up_tree = get_parsed(BASEURL)
    return bandlist(line_up_tree)


def get_parsed(url):
    return lxml.etree.fromstring(
        session().get(url).text,
        lxml.etree.HTMLParser())


def bandlist(line_up_tree):
    bands = dict(BandListItem(i).parse()
                 for i in line_up_tree.xpath('.//div[@class="item-inner"]'))
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
    def __init__(self, item: lxml.etree.ElementBase):
        self.item = item

    def parse(self):
        return self.key, {
            'link': self.link,
            'country': self.country,
            'data-filters': self.data_filters,
            **self.parsed_act_page}

    @cached_property
    def _a(self):
        return self.item.xpath('a')[0]

    @property
    def key(self):
        return self._a.text.strip()

    @property
    def link(self):
        return self._a.attrib['href']

    @property
    def country(self):
        return self._a.xpath(
            'div[@class="item-meta"]/div[@class="country"]')[0].text

    @property
    def data_filters(self):
        words = {
            '1595': 'Music',
            '2685': 'Arts & Activism'
        }
        items = self.item.xpath('..')[0].attrib['data-filters'].split()
        return [words[k]
                for k in items
                if k in words]

    HOSTURL = urllib.parse.urlunsplit(
        (lambda u: (u[0], u[1], '', '', ''))
        (urllib.parse.urlsplit(BASEURL)))

    @property
    def parsed_act_page(self):
        return ActPage(
            get_parsed(self.HOSTURL
                       + '/'
                       + self.link)).parse()


class ActPage(object):

    def __init__(self, item):
        self.item = item

    def parse(self):
        return {
            'stage': self.stage,
            'date': self.date,
            'tagline': self.tagline,
            'links': self.links,
            'article': self.article
        }

    @property
    def stage(self):
        return self._blocks[0].xpath('text()')[0]

    @cached_property
    def _blocks(self):
        return self.item.xpath(
            './/div[@class="info"]/div[@class="block"]')

    @property
    def date(self):
        return dateutil.parser.parse(
            self._blocks[1]
            .xpath('*//text()')
            [1]).date()

    @property
    def tagline(self):
        header = self.item.xpath(
            './/div[@class="TextModule"]'
            '/div[@class="inner"]'
            '/div[@class="copy"]'
            '/h6')
        if header:
            return header[0].xpath('text()|*//text()')[0]
        else:
            return None

    @property
    def links(self):
        if len(self._blocks) > 2:
            return {
                a.text: a.attrib['href']
                for a in self._blocks[2].findall('a')
            }
        else:
            return []

    @property
    def article(self):
        return ''.join(
            lxml.etree.tostring(x).decode('utf-8')
            for x in self.item.xpath(
                '//div[contains(@class, "TextModule")]'
                '|//div[contains(@class, "SpotifyModule")]'
                '|//div[contains(@class, "MediaModule")]'))


if __name__ == "__main__":
    sys.exit(main(sys.argv))

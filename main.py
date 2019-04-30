#!/usr/bin/env python3

"""Spider Roskilde Program and produce json output.

(for now)

"""

import dateutil.parser
import datetime
import json
import sys
import urllib.parse
from typing import (Any, Dict, List, Tuple, Union)

from cached_property import cached_property  # type: ignore
import lxml.etree  # type: ignore
import requests

BASEURL = "https://www.roskilde-festival.dk/en/line-up/"


def main(argv: List[str]) -> str:
    ret = json.dumps(get_data(),
                     default=lambda x: x.isoformat(),
                     indent=2)
    print(ret)
    return ret


def get_data() -> Dict[str, Dict[str, Any]]:
    line_up_tree = get_parsed(BASEURL)
    return bandlist(line_up_tree)


def get_parsed(url: str) -> List[lxml.etree._Element]:
    return lxml.etree.fromstring(
        session().get(url).text,
        lxml.etree.HTMLParser())


ActPageValType = Union[str,
                       datetime.date,
                       Dict[str, str]]


BandlistValType = Union[ActPageValType,
                        List[str]]


def bandlist(line_up_tree: lxml.etree._Element) -> Dict[str,
                                                        Dict[str,
                                                             BandlistValType]]:
    return dict(BandListItem(i).parse()
                for i in line_up_tree.xpath('.//div[@class="item-inner"]'))


class session(object):

    SESSION = None

    def __init__(self) -> None:
        if session.SESSION is None:  # pragma: nobranch
            session.SESSION = requests.Session()
            self.__dict__.update(self.SESSION.__dict__)

    def get(self,
            *args: Union[str, bytes],
            **kwargs: Union[str, bytes]) -> requests.Response:
        assert session.SESSION is not None
        return session.SESSION.get(*args, **kwargs)


class BandListItem(object):

    def __init__(self, item: lxml.etree._Element) -> None:
        self.item = item

    def parse(self) -> Tuple[str,
                             Dict[str, BandlistValType]]:
        return self.key, {
            'link': self.link,
            'country': self.country,
            'data-filters': self.data_filters,
            **self.parsed_act_page}

    @cached_property
    def _a(self) -> lxml.etree._Element:
        return self.item.xpath('a')[0]

    @property
    def key(self) -> str:
        return self._a.text.strip()

    @property
    def link(self) -> str:
        return self._a.attrib['href']

    @property
    def country(self) -> str:
        return self._a.xpath(
            'div[@class="item-meta"]/div[@class="country"]')[0].text

    @property
    def data_filters(self) -> List[str]:
        words = {
            '1595': 'Music',
            '2685': 'Arts & Activism'
        }
        items = self.item.xpath('..')[0].attrib['data-filters'].split()
        return [words[k]
                for k in items
                if k in words]

    HOSTURL: str = urllib.parse.urlunsplit(
        (lambda u: (u[0], u[1], '', '', ''))
        (urllib.parse.urlsplit(BASEURL)))

    @property
    def parsed_act_page(self) -> Dict[str, ActPageValType]:
        return ActPage(
            get_parsed(self.HOSTURL
                       + '/'
                       + self.link)).parse()


class ActPage(object):

    def __init__(self, item: lxml.etree._Element):
        self.item = item

    def parse(self) -> Dict[str, ActPageValType]:
        return {
            'stage': self.stage,
            'date': self.date,
            'tagline': self.tagline,
            'links': self.links,
            'article': self.article
        }

    @property
    def stage(self) -> str:
        return self._blocks[0].xpath('text()')[0]

    @cached_property
    def _blocks(self) -> List[lxml.etree._Element]:
        return self.item.xpath(
            './/div[@class="info"]/div[@class="block"]')

    @property
    def date(self) -> datetime.datetime:
        return dateutil.parser.parse(
            ' '.join(self._blocks[1]
                     .xpath('*//text()')
                     [1:]))

    @property
    def tagline(self) -> str:
        header = self.item.xpath(
            './/div[@class="TextModule"]'
            '/div[@class="inner"]'
            '/div[@class="copy"]'
            '/h6')
        return header[0].xpath('text()|*//text()')[0]

    @property
    def links(self) -> Dict[str, str]:
        if len(self._blocks) > 2:
            return {
                a.text: a.attrib['href']
                for a in self._blocks[2].findall('a')
            }
        else:
            return {}

    @property
    def article(self) -> str:
        return ''.join(
            lxml.etree.tostring(x).decode('utf-8')
            for x in self.item.xpath(
                '//div[contains(@class, "TextModule")]'
                '|//div[contains(@class, "SpotifyModule")]'
                '|//div[contains(@class, "MediaModule")]'))


if __name__ == "__main__":  # pragma: nocover
    sys.exit(main(sys.argv))

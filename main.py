#!/usr/bin/env python3

"""Spider Roskilde Program and produce json output.

(for now)

"""

import os
import sys

import lxml.cssselect
import lxml.etree
import requests

BASEURL = "https://www.roskilde-festival.dk/en/line-up/"

def main(argv):
    return '[true]'

def bandlist(url=BASEURL):
    getbands = lxml.cssselect.CSSSelector('div[class="item-inner"]')
    with requests.Session() as session:
        overview = lxml.etree.fromstring(session.get(BASEURL).content.decode('utf-8'),
                                         lxml.etree.HTMLParser())
        bands = dict(parse_main_item(i)
                     for i in getbands(overview))
    return bands

def parse_main_item(item):
    getlink = lxml.cssselect.CSSSelector('a')
    a = getlink(item)[0]
    key = a.text.strip()
    getcountry = lxml.cssselect.CSSSelector(
        'div[class="item-meta"] > div[class="country"]')
    return key, {'link': a.attrib['href'],
                 'country': getcountry(a)[0].text}

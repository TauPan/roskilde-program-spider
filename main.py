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
    getbands = lxml.cssselect.CSSSelector('body > div.app > div.pages > div > div.PosterViewModule.week > div > div > div > div > div')
    getlink = lxml.cssselect.CSSSelector('a')
    with requests.Session() as session:
        overview = lxml.etree.fromstring(session.get(BASEURL).content.decode('utf-8'),
                                         lxml.etree.HTMLParser())
        bands = getbands(overview)
    return bands

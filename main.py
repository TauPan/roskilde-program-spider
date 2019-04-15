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

import lxml.cssselect
import lxml.etree
import requests

BASEURL = "https://www.roskilde-festival.dk/en/line-up/"
HOSTURL = urllib.parse.urlunsplit(
    (lambda u: (u[0], u[1], '', '', ''))(urllib.parse.urlsplit(BASEURL)))

SESSION = None

def main(argv):
    ret = json.dumps(get_main(),
                     default=lambda x: x.isoformat(),
                     indent=2)
    print(ret)
    return ret

def get_main():
    with requests.Session() as session:
        SESSION = session
        overview = get_parsed(BASEURL)
        return bandlist(overview)

def bandlist(overview):
    getbands = lxml.cssselect.CSSSelector('div[class="item-inner"]')
    bands = dict(complete_item(i)
                 for i in getbands(overview))
    return bands

def get_session():
    global SESSION
    if SESSION is None:
        SESSION = requests.Session()
    return SESSION

def get_parsed(url):
    session = get_session()
    return lxml.etree.fromstring(
        session.get(url).text,
        lxml.etree.HTMLParser())

def complete_item(item):
    key, parsed_item = parse_main_item(item)
    page = parse_act_page(get_parsed(HOSTURL + '/' + parsed_item['link']))
    parsed_item.update(page)
    return key, parsed_item

def parse_main_item(item):
    getlink = lxml.cssselect.CSSSelector('a')
    a = getlink(item)[0]
    key = a.text.strip()
    getcountry = lxml.cssselect.CSSSelector(
        'div[class="item-meta"] > div[class="country"]')
    return key, {'link': a.attrib['href'],
                 'country': getcountry(a)[0].text}


def parse_act_page(item):
    getinfo = lxml.cssselect.CSSSelector('div[class="info"]')
    getblocks = lxml.cssselect.CSSSelector('div[class="block"]')
    blocks = getblocks(getinfo(item)[0])
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
    getheader = lxml.cssselect.CSSSelector(
        'div.TextModule div.inner div.copy h6')
    header = getheader(item)
    if header:
        ret['tagline'] = header[0].xpath('text()|*//text()')[0]
    return ret

if __name__ == "__main__":
    sys.exit(main(sys.argv))

#!/usr/bin/env python3

"""Spider Roskilde Program and produce json output.

(for now)

"""

import os
import sys

from lxml.cssselect import CSSSelector
import requests

BASEURL = "https://www.roskilde-festival.dk/en/line-up/"

def main(argv):
    return '[true]'

def bandlist(url):
    return {'BOB DYLAN WITH HIS BAND': {}}

import json
import lxml.etree
import pytest

import main

class TestMain(object):

    def test_returns_string(self):
        ret = main.main([])
        assert type(ret) == str
        data = json.loads(ret)
        assert data

class TestBandlist(object):

    def test_returns_bandlist(self):
        ret = main.bandlist()
        # assuming we have a dictionary by band name
        bob = 'BOB DYLAN WITH HIS BAND'
        assert bob in ret
        # first attempts only went until "Zeitkratzer" omitting the
        # warm-up acts
        assert len(ret) > 150

class TestParseMainItem(object):

    bob = lxml.etree.fromstring(b'<div class="item-inner">\n<a href="/en/years/2019/acts/bob-dylan-with-his-band/" class="name OutlineAnchorPosterComp">\nBOB DYLAN WITH HIS BAND\n<div class="item-meta">\n<div class="country">US</div>\n<div class="favorit" data-artist-id="2018002790"/>\n</div>\n</a>\n</div>\n')

    def test_has_key(self):
        key, val = main.parse_main_item(self.bob)
        assert key == 'BOB DYLAN WITH HIS BAND'

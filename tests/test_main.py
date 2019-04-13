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

    bob = lxml.etree.fromstring(
        b'''<div class="item-inner">
<a href="/en/years/2019/acts/bob-dylan-with-his-band/" class="name OutlineAnchorPosterComp">
BOB DYLAN WITH HIS BAND
<div class="item-meta">
<div class="country">US</div>
<div class="favorit" data-artist-id="2018002790"/>
</div>
</a>
</div>''')

    def test_has_key(self):
        key, val = main.parse_main_item(self.bob)
        assert key == 'BOB DYLAN WITH HIS BAND'

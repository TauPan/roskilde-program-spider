import datetime
import json
import lxml.etree
import os
import pytest

import main

BOBKEY = 'BOB DYLAN WITH HIS BAND'

class TestMain(object):

    def test_returns_string(self):
        ret = main.main([])
        assert type(ret) == str
        data = json.loads(ret)
        assert data

class TestBandlist(object):

    @pytest.fixture
    def bandlist(self):
        return lxml.etree.fromstring(
            open(os.path.dirname(__file__) + '/line-up-2019-04-13.html', 'r').read(),
            lxml.etree.HTMLParser()
        )

    @pytest.fixture
    def parsed_bandlist(self, bandlist):
        return main.bandlist(bandlist)

    def test_returns_bandlist(self, parsed_bandlist):
        ret = parsed_bandlist
        # assuming we have a dictionary by band name
        bob = BOBKEY
        assert bob in ret
        # first attempts only went until "Zeitkratzer" omitting the
        # warm-up acts
        assert len(ret) > 150
        assert 'ZAAR' in ret
        assert 'ZUSA' in ret

    def test_bob_properties(self, parsed_bandlist):
        bob = parsed_bandlist[BOBKEY]
        assert bob['stage'] == 'Orange'
        assert bob['date'] == datetime.date(2019, 7, 3)

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
        assert key == BOBKEY
        assert val['link'] == '/en/years/2019/acts/bob-dylan-with-his-band/'
        assert val['country'] == 'US'


class TestParseActPAge(object):

    bobpage = lxml.etree.fromstring(
        open(os.path.dirname(__file__) + '/bob-dylan-2019-04-13.html', 'r').read(),
        lxml.etree.HTMLParser()
    )


    @pytest.fixture
    def parsed_bob(self):
        return main.parse_act_page(self.bobpage)

    def test_has_stage(self, parsed_bob):
        assert parsed_bob['stage'] == 'Orange'

    def test_has_date(self, parsed_bob):
        assert parsed_bob['date'] == datetime.date(2019, 7, 3)

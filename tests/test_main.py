import datetime
import json
import lxml.etree
import os
import pytest
import re

import main


@pytest.fixture(autouse=True)
def prevent_requests(mocker):
    mocker.patch('main.requests')


@pytest.fixture
def get_parsed(mocker, request):
    """Takes as parameters a dictionary of

    - mappings: A dictionary of regexes mapped to content files to
      parse as html content. All files are searched underneath the
      directory this test file resides in.

    - default: A filename for the default content if no mapping is found

    """
    basedir = os.path.dirname(__file__)
    default_default = 'bob-dylan-2019-04-13.html'
    default_mappings = {
        r'/line-up/$': 'line-up-2019-04-13.html',
        r'/acts/bob-dylan-with-his-band/$': 'bob-dylan-2019-04-13.html',
        r'/acts/shambs-x-farli-x-b-wood-x-bracy-doll/$': 'shambs-2019-04-15.html',
        r'/acts/zusa/$': 'zusa-2019-04-16.html',
    }
    config = getattr(request, 'param', {})
    config.setdefault('default', default_default)
    config.setdefault('mappings', default_mappings)
    def _get(url):
        file = config['default']
        for k in config['mappings']:
            if re.search(k, url):
                file = config['mappings'][k]
                break
        file = '{}/{}'.format(basedir, file)
        return lxml.etree.fromstring(
            open(file, 'r').read(),
            lxml.etree.HTMLParser()
        )
    mocker.patch('main.get_parsed', side_effect=_get)


BOBKEY = 'BOB DYLAN WITH HIS BAND'


def _assert_bob(bob):
    """Common assertions about "A strange young man called Dylan"
    """
    assert bob['stage'] == 'Orange'
    assert bob['link'] == '/en/years/2019/acts/bob-dylan-with-his-band/'
    assert bob['country'] == 'US'
    assert 'links' in bob
    links = bob['links']
    assert set(links.keys()) == {
        'Spotify', 'Facebook', 'Twitter', 'Instagram', 'Youtube', 'Website'
    }
    assert (
        links['Spotify']
        == 'https://open.spotify.com/track/3AhXZa8sUQht0UEdBJgpGc?si=FIvHuowIQVWS-memeplwHA')
    assert (
        links['Facebook']
        == 'https://www.facebook.com/bobdylan/')
    assert (
        links['Twitter']
        == 'https://twitter.com/bobdylan')
    assert (
        links['Instagram']
        == 'https://www.instagram.com/bobdylan/')
    assert (
        links['Youtube']
        == 'https://www.youtube.com/channel/UCnRI0ay61tY-fKYzzB3fCnw')
    assert (
        links['Website']
        == 'https://www.bobdylan.com/')
    _date_assert(bob['date'], '2019-07-03')
    assert (bob['tagline']
            == 'The iconic singer-songwriter, who changed the world, '
            'will play Roskilde Festival 2019')
    assert bob['data-filters'] == ['Music']

def _date_assert(dat, isostr):
    assert _date_iso(dat) == isostr

def _date_iso(dat):
    if hasattr(dat, 'isoformat'):
        return dat.isoformat()
    else:
        return dat

SHAMBSKEY = "SHAMBS X FARLI' X B WOOD$ X BRACY DOLL"


def _assert_shambs(shambs):
    """Common assertions about "SHAMBS X FARLI' X B WOOD$ X BRACY DOLL"
    """
    assert shambs['stage'] == 'Countdown'
    assert shambs['link'] == '/en/years/2019/acts/shambs-x-farli-x-b-wood-x-bracy-doll/'
    assert shambs['country'] == 'DK'
    assert 'links' not in shambs
    dat = shambs['date']
    _date_assert(dat, '2019-07-01')
    assert (shambs['tagline']
            == 'Street trap rap with four sharp representatives. Expect chaos!')
    assert shambs['data-filters'] == ['Music']


ZUSAKEY = "ZUSA"


def _assert_zusa(zusa):
    """Common assertions about "ZUSA"
"""
    comp = dict(zusa)
    comp['date'] = _date_iso(comp['date'])
    comp['article'] = lxml.etree.fromstring(comp['article'])
    expected = {
        'stage': 'Art Zone',
        'date': '2019-07-03',
        'tagline': 'Meet up at ZUSA and become part of the community',
        'country': 'DK',
        'link': '/en/years/2019/acts/zusa/',
        'data-filters': ['Arts & Activism'],
        'links': {
            'Facebook': 'https://www.facebook.com/zusa.street',
            'Instagram': 'https://www.instagram.com/zusastreet/',
            'Website': 'http://zusastreet.dk/'
        },
        'article': lxml.etree.fromstring(
            open(os.path.dirname(__file__)
                 + '/zusa-2019-04-16-article.html', 'r').read(),
            lxml.etree.HTMLParser())
    }
    assert comp == expected

class TestMain(object):

    def test_returns_string(self, get_parsed):
        ret = main.main([])
        assert type(ret) == str
        data = json.loads(ret)
        assert data
        bob = data[BOBKEY]
        _assert_bob(bob)
        _assert_shambs(data[SHAMBSKEY])
        _assert_zusa(data[ZUSAKEY])


class TestGetMain(object):

    def test_returns_data(self, get_parsed):
        data = main.get_main()
        assert data
        bob = data[BOBKEY]
        _assert_bob(bob)
        _assert_shambs(data[SHAMBSKEY])
        _assert_zusa(data[ZUSAKEY])


class TestBandlist(object):

    @pytest.fixture
    def bandlist(self):
        return lxml.etree.fromstring(
            open(os.path.dirname(__file__) + '/line-up-2019-04-13.html', 'r').read(),
            lxml.etree.HTMLParser()
        )

    @pytest.fixture
    def parsed_bandlist(self, bandlist, get_parsed):
        return main.bandlist(bandlist)

    def test_returns_bandlist(self, parsed_bandlist):
        ret = parsed_bandlist
        # assuming we have a dictionary by band name
        assert BOBKEY in ret
        _assert_bob(ret[BOBKEY])
        # first attempts only went until "Zeitkratzer" omitting the
        # warm-up acts
        assert len(ret) > 150
        assert 'ZAAR' in ret
        assert 'ZUSA' in ret

    def test_bob_properties(self, parsed_bandlist):
        bob = parsed_bandlist[BOBKEY]
        _assert_bob(bob)

    def test_shambs_properties(self, parsed_bandlist):
        _assert_shambs(parsed_bandlist[SHAMBSKEY])

    def test_zusa_properties(self, parsed_bandlist):
        _assert_zusa(parsed_bandlist[ZUSAKEY])


class WithBob(object):
    bobbase = b'''<div class="PosterViewModuleItem item p1" data-sorting="1" data-hover="/media/2499/bob-dylan.png" data-artist="2018002790" data-filters="0 1595">
<div class="item-inner">
<a href="/en/years/2019/acts/bob-dylan-with-his-band/" class="name OutlineAnchorPosterComp">
BOB DYLAN WITH HIS BAND
<div class="item-meta">
<div class="country">US</div>
<div class="favorit" data-artist-id="2018002790"></div>
</div>
</a>
</div>
</div>'''
    bob = lxml.etree.fromstring(bobbase).xpath('div[@class="item-inner"]')[0]


class TestParseMainItem(WithBob):

    def test_has_key(self):
        key, bob = main.parse_main_item(self.bob)
        assert key == BOBKEY
        assert bob['link'] == '/en/years/2019/acts/bob-dylan-with-his-band/'
        assert bob['country'] == 'US'

class WithBobPage(WithBob):
    bobpage = lxml.etree.fromstring(
        open(os.path.dirname(__file__) + '/bob-dylan-2019-04-13.html', 'r').read(),
        lxml.etree.HTMLParser()
    )

    @pytest.fixture
    def parsed_bob(self):
        return main.parse_act_page(self.bobpage)



class TestParseActPage(WithBobPage):

    def test_has_stage(self, parsed_bob):
        assert parsed_bob['stage'] == 'Orange'

    def test_has_date(self, parsed_bob):
        assert parsed_bob['date'] == datetime.date(2019, 7, 3)


class TestCompleteItem(WithBobPage):

    def test_complete_bob(self, mocker, get_parsed):
        key, bob = main.complete_item(self.bob)
        assert key == BOBKEY
        _assert_bob(bob)

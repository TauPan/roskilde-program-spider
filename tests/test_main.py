import datetime
import json
import lxml.etree  # type: ignore
import os
import pytest  # type: ignore
import re

import main


@pytest.fixture(autouse=True)
def prevent_requests(mocker):
    mocker.patch('main.requests')


@pytest.fixture
def get_parsed(mocker):
    """Patches main.get_parsed() to load a file from:

    - mappings: A dictionary of regexes mapped to content files to
      parse as html content. All files are searched underneath
      `TEST_DIRECTORY`, the directory this test file resides in.

    - default: A filename for the default content if no mapping is
      found.

    """
    def _get(url):
        return parse_file(_file_match(url))

    def _file_match(url):
        mappings = {
            r'/line-up/$': 'line-up-2019-04-30.html',
            r'/acts/bob-dylan-with-his-band/$': 'bob-dylan-2019-04-30.html',
            r'/acts/shambs-x-farli-x-b-wood-x-bracy-doll/$': (
                'shambs-2019-04-30.html'),
            r'/acts/zusa/$': 'zusa-2019-04-30.html',
        }
        default = 'dummy_act.html'
        match = next((mappings[k] for k in mappings if re.search(k, url)),
                     default)
        return filename_here(match)

    mocker.patch('main.get_parsed', side_effect=_get)


def parse_file(fil):
    return lxml.etree.fromstring(
        open(fil, 'r').read(),
        lxml.etree.HTMLParser()
    )


def filename_here(fil):
    return '{}/{}'.format(os.path.dirname(__file__), fil)


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
        == ('https://open.spotify.com/track/'
            '3AhXZa8sUQht0UEdBJgpGc?si=FIvHuowIQVWS-memeplwHA'))
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
    _date_assert(bob['date'], '2019-07-03T20:00:00')
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
    assert shambs['link'] == ('/en/years/2019/acts/'
                              'shambs-x-farli-x-b-wood-x-bracy-doll/')
    assert shambs['country'] == 'DK'
    assert shambs['links'] == {}
    _date_assert(shambs['date'], '2019-07-01T23:00:00')
    assert (shambs['tagline']
            == ('Street trap rap with four sharp '
                'representatives. Expect chaos!'))
    assert shambs['data-filters'] == ['Music']


ZUSAKEY = "ZUSA"


def _assert_zusa(zusa):
    """Common assertions about "ZUSA"
"""
    comp = dict(zusa)
    comp['date'] = _date_iso(comp['date'])
    comp['article'] = _normalize_html(comp['article'])
    expected = {
        'stage': 'Art Zone',
        'date': '2019-07-03T00:00:00',
        'tagline': 'Meet up at ZUSA and become part of the community',
        'country': 'DK',
        'link': '/en/years/2019/acts/zusa/',
        'data-filters': ['Arts & Activism'],
        'links': {
            'Facebook': 'https://www.facebook.com/zusa.street',
            'Instagram': 'https://www.instagram.com/zusastreet/',
            'Website': 'http://zusastreet.dk/'
        },
        'article': _normalize_html(
            open(filename_here('/zusa-2019-04-16-article.html'),
                 'r').read())
    }
    assert comp == expected


def _normalize_html(s):
    return lxml.etree.tostring(
        lxml.etree.fromstring(s,
                              lxml.etree.HTMLParser())).decode('utf-8')


class TestGetData(object):

    def test_returns_data(self, get_parsed):
        data = main.get_data()
        assert data
        bob = data[BOBKEY]
        _assert_bob(bob)
        _assert_shambs(data[SHAMBSKEY])
        _assert_zusa(data[ZUSAKEY])


class TestGetParsed(object):

    def test_returns_etree(self, mocker):
        mocker.stopall()

        class Response(object):
            text = '<html><body><p>OK</p></body></html>'

        mocker.patch('main.requests.Session.get',
                     return_value=Response())
        ret = main.get_parsed('http://foo')
        assert ''.join(ret.xpath('//p//text()')) == 'OK'


class TestBandlist(object):

    @pytest.fixture
    def bandlist(self):
        return parse_file(filename_here('/line-up-2019-04-30.html'))

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
</div>'''  # noqa
    bob = lxml.etree.fromstring(bobbase).xpath('div[@class="item-inner"]')[0]


class TestParseMainItem(WithBob):

    def test_has_properties(self):
        item = main.BandListItem(self.bob)
        assert item.key == BOBKEY
        assert item.link == '/en/years/2019/acts/bob-dylan-with-his-band/'
        assert item.country == 'US'
        assert item.data_filters == ['Music']


class WithBobPage(WithBob):
    bobpage = parse_file(filename_here('/bob-dylan-2019-04-30.html'))

    @pytest.fixture
    def parsed_bob(self):
        return main.ActPage(self.bobpage).parse()


class TestParseActPage(WithBobPage):

    def test_has_stage(self, parsed_bob):
        assert parsed_bob['stage'] == 'Orange'

    def test_has_date(self, parsed_bob):
        assert parsed_bob['date'] == datetime.datetime(2019, 7, 3, 20, 0)


class TestBandListItem(WithBobPage):

    def test_complete_bob(self, mocker, get_parsed):
        key, bob = main.BandListItem(self.bob).parse()
        assert key == BOBKEY
        _assert_bob(bob)

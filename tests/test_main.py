import json
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

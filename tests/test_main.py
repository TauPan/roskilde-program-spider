import pytest

import main

class TestMain(object):

    def test_returns_string(self):
        ret = main.main([])
        assert type(ret) == str

# roskilde-program-spider

[![Build Status](https://travis-ci.com/TauPan/roskilde-program-spider.svg?branch=master)](https://travis-ci.com/TauPan/roskilde-program-spider)

I've been spidering the program of roskilde festival in denmark since
2005. These are my past and current efforts.

## Installation

 - Install pyenv, see https://github.com/pyenv/pyenv#installation
 - Install pipenv, see https://pipenv.readthedocs.io/en/latest/#install-pipenv-today

Set up the environment with

 - `pipenv sync --dev`

(You can also set-up your own virtual environment with `pyenv` and
`virtualenvwrapper` before running pipenv, which I prefer, but this is
out of the scope of this simple README file.)

## Run the script

`pipenv run main.py` will generate the JSON output to stdout.


## Run tests

The Pipfile defines a couple of scripts:

 - `pipenv run test` runs pytest, once

 - `pipenv run test_watch` will rerun pytest after every change.

If you don't have my exact ubuntu installation, you can customize the
environment variables `PASS_COMMAND`, `FAIL_COMMAND`, `PASS_ICON` and
`FAIL_ICON` via pipenv's `.env` file to make notifications for
`test_watch` work on your system.

 - `pipenv run mypy` runs mypy to check type annotations.

 - `pipenv run mypy_watch` runs mypy after each source change, using
   the same notification settings as `test_watch`.

 - `pipenv run flake8` will check the sources with flake8. This is not
   very useful if you use elpy on emacs *or* pycharm or another setup
   that automatically runs flake8 or another static checker.

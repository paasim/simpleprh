# simpleprh

[![Build Status](https://travis-ci.org/paasim/simpleprh.svg?branch=master)](https://travis-ci.org/paasim/simpleprh)
[![codecov](https://codecov.io/gh/paasim/simpleprh/branch/master/graphs/badge.svg)](https://codecov.io/gh/paasim/simpleprh)

An R package for downloading data from the [PRH open data API](https://avoindata.prh.fi/index_en.html) in a tidy format for personal use. Currently returns the company name, form, business line and registration date, although more information available through the API might be added in the future.

Installation
------------

    devtools::install_github("paasim/simpleprh")


Usage
-----

    library(simpleprh)

    # Download information related to Nokia Oyj
    bis_dl("0112038-9", lang = "EN")


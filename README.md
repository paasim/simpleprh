# simpleprh

[![R build status](https://github.com/paasim/simpleprh/workflows/R-CMD-check/badge.svg)](https://github.com/paasim/simpleprh/actions)

An R package for downloading data from the [PRH open data API](https://avoindata.prh.fi/index_en.html) in a tidy format for personal use.
Currently contains two functions `bis_dl`, and `bis_lookup`. The first can be used for downloading information related to a single business id and the latter for querying a business ID and official company name given a name as a search parameter. 
More functionality might be added in the future.

Installation
------------

    devtools::install_github("paasim/simpleprh")


Usage
-----

    library(simpleprh)

    # Look up the business ID of Nokia Oyj
    nokia_bid <- bis_lookup("Nokia Oyj")
    
    # Get all the data related to Nokia from the API
    nokia_data <- bis_dl(nokia_bid$business_id)



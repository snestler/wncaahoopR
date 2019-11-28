# wncaahoopR
An R package for working with women's NCAA Basketball play-by-play data

This package relies heavily on the work done by Luke Benz (lbenz430) in his package
`ncaahoopR`, designed for working with men's NCAA basketball play-by-play data.

`wncaahoopR` also scrapes data from ESPN, but differs in that it does not have scraping provided in only function, choosing to only scan in the data once and then
make use of that object within R to produce win-probability and game flow charts,
as well as assist networks.

## Installation
You can install `wncaahoopR` from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("snestler/wncaahoopR")
```

## Functions
Several functions use ESPN game_ids. You can find the game_id in the URL for the game summary, as shown below in the URL for the summary of the Notre Dame - Michigan game played on Nov. 23, 2019.
![game_id](figures/espn.png)



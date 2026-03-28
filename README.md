
<!-- README.md is generated from README.Rmd. Please edit that file -->

# webportal

<!-- badges: start -->

<!-- badges: end -->

The webportal package provides a wrapper for the [Aquatic
Informatics](https://aquaticinformatics.com/) [Web Portal
API](https://github.com/AquaticInformatics/examples?tab=readme-ov-file).
The

## Installation

You can install the development version of webportal from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("jasonelaw/webportal")
```

## Examples

The webportal package can be used to most endpoints. In order to use the
package, you’ll have to make your credentials available via environment
variables. I recommend setting the environmental variables by editiong
your
[`.Renviron`](https://docs.posit.co/ide/user/ide/guide/environments/r/managing-r.html)
file to include:

    AQUARIUS_WEBPORTAL_URL  = "https://www.example.com/webportal"
    AQUARIUS_WEBPORTAL_USER = "user"
    AQUARIUS_WEBPORTAL_PW   = "password"

Afterwards, you should be able to use the package without thinking about
credentials. For example, to retrieve a list of all locations stored in
the system:

``` r
library(webportal)
get_locations()
```

A `dataset` which is either a timeseries or readings collected during
field visits can be retrieved using `get_dataset`:

``` r
get_dataset("Parameter.Label@Location")
```

Several entities can be retrieved as geojson objects. webportal converts
these to [`sf`](https://r-spatial.github.io/sf/) objects. Functions are
available to retrieve locations, datasets (by parameter), latest
statistics (by parameter and statistic), and periodic statistics (by
parameter, statistic, interval, and date):

``` r
get_map_locations()

get_map_datasets(parameter = "Precip Increm")

get_map_latest_stat(parameter = "Precip Increm", statistic = "CALENDAR_1DAY")
```

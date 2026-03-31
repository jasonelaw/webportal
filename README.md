
<!-- README.md is generated from README.Rmd. Please edit that file -->

# webportal

<!-- badges: start -->

<!-- badges: end -->

The webportal package provides a wrapper for the [Aquatic
Informatics](https://aquaticinformatics.com/) [Web Portal
API](https://github.com/AquaticInformatics/examples?tab=readme-ov-file).
Accessing the API requires that you have an account with privileges to
access the API for deployed Web Portal. Speak to an administrator for
the Web Portal server that you’re interested in if you don’t have these.

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
variables. I recommend setting the environmental variables by editing
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
#> # A tibble: 328 × 22
#>    locationId id    name     description folder type  elevation elevationUnit_id
#>         <int> <chr> <chr>    <chr>       <chr>  <chr>     <dbl> <chr>           
#>  1          2 VNB   N Vanco… ""          All L… Surf…         0 m               
#>  2          3 158   NE 158t… ""          All L… Surf…         0 m               
#>  3          4 21B   NE 21st… ""          All L… Surf…         0 m               
#>  4          5 92B   NE 92nd… ""          All L… Surf…         0 m               
#>  5          8 P0016 Kelley … ""          All L… Surf…         0 m               
#>  6         11 P0208 Tryon C… ""          All L… Surf…         0 m               
#>  7         13 P0080 Upper C… ""          All L… Surf…         0 m               
#>  8         14 P0060 Veteran… ""          All L… Surf…         0 m               
#>  9         16 P0250 Balch C… ""          All L… Surf…         0 m               
#> 10         19 P0272 Johnson… ""          All L… Surf…         0 m               
#> # ℹ 318 more rows
#> # ℹ 14 more variables: elevationUnit_symbol <chr>,
#> #   elevationUnit_singularName <chr>, elevationUnit_pluralName <chr>,
#> #   latitude <dbl>, longitude <dbl>, srid <int>, easting <dbl>, northing <dbl>,
#> #   wkid <int>, utcOffset <int>, lastUpdated <chr>, active <lgl>, tags <list>,
#> #   .req_id <chr>
```

A `dataset` which is either a timeseries or readings collected during
field visits can be retrieved using `get_dataset`:

``` r
get_dataset("Parameter.Label@Location")
```

Several entities can be retrieved as geojson objects. webportal converts
these to [`sf`](https://r-spatial.github.io/sf/) objects automatically.
Functions are available to retrieve locations, datasets (by parameter),
latest statistics (by parameter and statistic), and periodic statistics
(by parameter, statistic, interval, and date):

``` r
get_map_locations()

get_map_datasets(parameter = "Precip Increm")

get_map_latest_stat(parameter = "Precip Increm", statistic = "CALENDAR_1DAY")
```

Datasets, periodic statistics, and seasonal statistics can be retrieved
using the export endpoint via `export_dataset`, `export_periodic_stat`,
and `export_seasonal_stat`. These functions are all vectorized over
their arguments. Vectors of length 1 are recycled via R recycling rules,
but otherwise the length of arguments must match. So, for example, the
following is possible:

``` r
library(lubridate)
datasets <- c(
  "Precip Increm.Primary@Location1", 
  "Precip Increm.Primary@Location2"
)
export_dataset(
  dataset = datasets, 
  startTime = "2026-01-01", 
  endTime = "2026-03-01"
)
```

The vectorized functions above issue a single request for each dataset
requested. These requests will be issued in parallel if the option
`webportal.parallel` is set to `TRUE`:

``` r
options("webportal.parallel" = TRUE)
export_dataset(
  dataset = datasets, 
  startTime = "2026-01-01", 
  endTime = "2026-03-01"
)
```

Parallel requests may be faster.

The Web Portal API also has two endpoints for “bulk” exports where
multiple datasets can be requested with the same API call. The
`export_bulk` and `export_time_aligned` functions use these endpoints.
They differ in that the latter function returns a “time-aligned” result
where each time series subsequent to the first is interpolated to the
timesteps of the first time series. This allows a user to easily create
datasets sampled at the same time interval when they are measured using
different intervals. For example, this will linearly interpolate the
hourly precipitation totals in the second time series to the 5 minute
measurements present in the first:

``` r
datasets <- c(
  "Precip Increm.Primary@Location1", 
  "Precip Increm.1Hour@Location1"
)
export_time_aligned(
  dataset = datasets, 
  startTime = "2026-01-01", 
  endTime = "2026-03-01"
)
```

The `export_bulk` call here is equivalent to the example above. The only
difference being that the example below consolidates all of the
requested data into a single API call, whereas the example above will
issue a single request for each dataset (and may be performed in
parallel).

``` r
datasets <- c(
  "Precip Increm.Primary@Location1", 
  "Precip Increm.1Hour@Location1"
)
export_bulk(
  dataset = datasets, 
  startTime = "2026-01-01", 
  endTime = "2026-03-01"
)
```

Finally, there are several options that can be set:

    options(
    webportal.verbose = FALSE,
    webportal.rename = FALSE,
    webportal.parallel = FALSE
    )

All three are `FALSE` by default. The `verbose` option will print
detailed information about each request issued. The `rename` option will
rename formatted output to snake case. I’ve tried to make sure nested
output is also renamed but please create an issue if you notice
inconsistent naming. Finally, for functions that are vectorized and
issue several requests for each vector argument, the `parallel` option
will perform requests in parallel.

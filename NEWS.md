# webportal 0.1.1

# webportal 0.1.0

* Initial release - supports most API operations except location parameter
range values, parameter range definitions, and tag security. There are several
resources that allow access through nested paths like /statistics/latest,
statistics/latest/{parameter}, and /statistics/latest/{parameter}/{statistic}.
In general, webportal only implements the top level as /statistics/latest also
allows query parameters for parameter and statistics.

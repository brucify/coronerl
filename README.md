coronerl
=====

A Cowboy OTP application.  Backend for http://covid19curves.nu

Build
-----

    $ rebar3 compile

Release
-----

Edit version number `X.Y.Z` in `rebar.config`:

    {relx, [{release, {"coronerl", "X.Y.Z"}, [coronerl]},

Build the release into tar

    make release

Deploys and runs the release `X.Y.Z` on prod: 

    make deploy X.Y.Z

Data Sources
-----

`/global`

https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series

`/usa`

https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series


`/poland`

https://github.com/dtandev/coronavirus/tree/master/data
# Overview of shakerr

[![Build Status](https://travis-ci.org/sdbecker/shakerr.svg?branch=master)](https://travis-ci.org/sdbecker/shakerr)

The package is used to clean data retrieved from the:
[NOAA Significant Earthquake Database](www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1).
The package is used to plot the clean data as  a time line of earthquakes 
together with their magnitude or on a map showing the epicentres of the
earthquakes.

## Getting Started

The package source can be forked from Github and built using the devtools
package in your own console or application.

### Prerequisites

Ensure you are running the at least version 3.1 of R and should have
the packages set out in DESCRIPTION installed in your library, if not
they will be installed on building the package.

Example of installing two of the packages:
```
utils::install.packages("dplyr")
utils::install.package("ggplot2")
```

### Installing

Once the package has been forked and downloaded to your console, ensure
that the devtools package is installed.

```
utils::install.package("devtools")
```

Once you have opened the project then you can build and reload it.

```
devtools::build()
devtools::reload()
```

## Running the tests

You can run the devtools::test() function to run the tests for the package
to see if everything is running as expected.

### Break down into end to end tests

Explain what these tests test and why

```
Give an example
```

### And coding style tests

Explain what these tests test and why

```
Give an example
```

## Deployment

Add additional notes about how to deploy this on a live system

## Built With

* [RStudio](https://www.rstudio.com) - The IDE application

## Versioning

We use [Git](https://git-scm.com) for versioning. For the versions available,
see the [this repository](https://github.com/sdbecker/shakerr). 

## Authors

* **Steven Becker** - *Initial work* 

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details


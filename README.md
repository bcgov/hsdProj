
[![img](https://img.shields.io/badge/Lifecycle-Stable-97ca00)](https://github.com/bcgov/repomountie/blob/master/doc/lifecycle-badges.md)

hsdProj
============================

A dashboard for presenting B.C. sub-provincial household estimates and projections, both number of households and average number of people per household, for development (economic) regions, regional districts (census divisions), and municipalities (census subdivisions).

Code is also provided for the data cleaning process.

### Data

Estimates: BC Stats releases annual household estimates for sub-provincial regions as of July 1st of every year. These estimates are calculated using a parametric model adjusted from Census data and the annual population estimates by BC Stats.

Projections: BC Stats applies the same parametric model used for the household estimates to the population projections produced annually by BC Stats to produce household projections. The projections are produced for every region type described above. More information can be found on [BC Stats’ Household Projections page](https://www2.gov.bc.ca/gov/content/data/statistics/people-population-community/population/household-projections).

The underlying data is available through the [BC Data Catalogue](https://catalogue.data.gov.bc.ca/dataset/bc-sub-provincial-household-estimates-and-projections-).

### Usage

The app.R code is used to create a shiny dashboard which is hosted on [shinyapps.io](https://bcstats.shinyapps.io/hsdProjApp). Data are sourced and tidied in the get_data.R script. 

### Project Status

Stable

### Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an [issue](https://github.com/bcgov/popApp/issues/).

### How to Contribute

If you would like to contribute, please see our [CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

### License

```
Copyright 2023 Province of British Columbia

Licensed under the Apache License, Version 2.0 (the &quot;License&quot;);
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an &quot;AS IS&quot; BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and limitations under the License.
```
---



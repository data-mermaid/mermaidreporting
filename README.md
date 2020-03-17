
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mermaidreporting

<!-- badges: start -->

<!-- badges: end -->

The goal of mermaidreporting is to provide utility functions for
summarising and visualizing MERMAID data.

## Installation

You can install the development version of mermaidreporting with:

``` r
# install.packages("devtools")
devtools::install_github("data-mermaid/mermaidreporting@development")
```

## Data

Data is available from the
[`mermaidr`](https://github.com/data-mermaid/mermaidr/tree/package)
package. There are details there on authorization, projects, and
endpoints.

Most functions in `mermaidreporting` use aggregated sample events data.
For example, to query the aggregated belt fish data for a project:

``` r
library(mermaidr)

xpdc_sample_events <- mermaid_search_projects(name = "XPDC Kei Kecil 2018") %>%
  mermaid_get_project_endpoint(endpoint = "beltfishes/sampleevents", limit = 25)
```

## Usage

The following demonstrates functions that are available to generate
maps, figures, and summary tables. All functions in `mermaidreporting`
(and `mermaidr`) are of the form `mermaid_*()`, to take advantage of
RStudio auto-completion and make them easier to find when you have other
packages loaded.

## Maps

You can generate map of site locations using
`mermaid_map_sites_static()` and `mermaid_map_sites_interactive()`,
which produce static and interactive maps, respectively:

``` r
library(mermaidreporting)

mermaid_map_sites_static(xpdc_sample_events)
```

``` r
mermaid_map_sites_interactive(xpdc_sample_events)
```

Note that this is just a screenshot of the interacive plot, but you can
try it out in an interactive R session by running the above code\!

### Mapping by a variable

You can also map sites by the value of a variable. For example, to map
sites by mean total biomass:

``` r
mermaid_map_sites_static(xpdc_sample_events, biomass_kgha_avg)
```

Or by reef exposure:

``` r
mermaid_map_sites_static(xpdc_sample_events, reef_exposure)
```

## Figures

### Plot benthic category percent cover

WIP, once new endpoint is available

### Plot fish belt biomass

You can also plot fish belt biomass, split by a grouping variable (such
as trophic group) and compared by another variable (such as reef
exposure):

``` r
xpdc_sample_events %>%
  mermaid_plot_fish_belt_biomass(biomass_kgha_by_trophic_group_avg, reef_exposure)
```

Or by reef zone:

``` r
xpdc_sample_events %>%
  mermaid_plot_fish_belt_biomass(biomass_kgha_by_trophic_group_avg, reef_zone)
```

## Summary Table

WIP, once new endpoints are available

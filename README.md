
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
devtools::install_github("data-mermaid/mermaidreporting@development", upgrade = "never")
```

## Data

Data is available from the
[`mermaidr`](https://github.com/data-mermaid/mermaidr/tree/package)
package. There are details there on authorization and accessing data.

## Usage

All functions in `mermaidreporting` (and `mermaidr`) are of the form
`mermaid_*()`, to take advantage of RStudio auto-completion and make
them easier to find when you have other packages loaded.

### Data

First, we read in data using `mermaidr`:

``` r
library(mermaidr)
library(dplyr)

my_projects <- mermaid_get_my_projects() 

xpdc <- my_projects %>%
  filter(name == "XPDC Kei Kecil 2018")

xpdc_fishbelt <- xpdc %>%
  mermaid_get_project_data(method = "fishbelt", data = "sampleevents")
```

## Cleaning data

### Data frame columns

The data from `mermaidr` comes in a form that may seem a little
unfriendly to work with\!

For example, in `xpdc_fishbelt`, the mean total biomass data is
contained in a *data frame column*:

``` r
xpdc_fishbelt %>%
  select(biomass_kgha_by_trophic_group_avg)
#> # A tibble: 46 x 1
#>    biomass_kgha_by… $piscivore $planktivore $`invertivore-m… $`herbivore-det…
#>               <dbl>      <dbl>        <dbl>            <dbl>            <dbl>
#>  1            30.2       43.6         280.              16.8            34.9 
#>  2            NA         27.1          26.5             NA               1.38
#>  3            19.7       32.4         642.             384.             48.4 
#>  4             5.81      22.7          15.2             33.1            22.7 
#>  5             7.46      26.2         233.              44.5           105.  
#>  6            42.7       13.4         772.              12.1            35.3 
#>  7             6.42      16.0         213.              38.1            25.3 
#>  8             2.49      NA           159.              23.7            25.1 
#>  9            17.3       41.8         780.              72.7            23.7 
#> 10            NA          2.52         96.9              1.6            NA   
#> # … with 36 more rows, and 3 more variables: $`invertivore-sessile` <dbl>,
#> #   $`herbivore-macroalgae` <dbl>, $other <dbl>
```

Use `mermaid_clean_columns()` to clean this up and split each trophic
group into its own column:

``` r
library(mermaidreporting)

xpdc_fishbelt %>%
  select(biomass_kgha_by_trophic_group_avg) %>%
  mermaid_clean_columns()
#> # A tibble: 46 x 8
#>    omnivore piscivore planktivore invertivore_mob… herbivore_detri…
#>       <dbl>     <dbl>       <dbl>            <dbl>            <dbl>
#>  1    30.2      43.6        280.              16.8            34.9 
#>  2    NA        27.1         26.5             NA               1.38
#>  3    19.7      32.4        642.             384.             48.4 
#>  4     5.81     22.7         15.2             33.1            22.7 
#>  5     7.46     26.2        233.              44.5           105.  
#>  6    42.7      13.4        772.              12.1            35.3 
#>  7     6.42     16.0        213.              38.1            25.3 
#>  8     2.49     NA          159.              23.7            25.1 
#>  9    17.3      41.8        780.              72.7            23.7 
#> 10    NA         2.52        96.9              1.6            NA   
#> # … with 36 more rows, and 3 more variables: invertivore_sessile <dbl>,
#> #   herbivore_macroalgae <dbl>, other <dbl>
```

This works on Benthic PIT data, as well, and auto-detects any columns in
this format to clean\!

``` r
xpdc_benthicpit <- xpdc %>%
  mermaid_get_project_data(method = "benthicpit", data = "sampleevents")

xpdc_benthicpit %>%
  select(percent_cover_by_benthic_category_avg) %>%
  mermaid_clean_columns()
#> # A tibble: 38 x 8
#>     sand rubble hard_coral macroalgae soft_coral turf_algae other_invertebr…
#>    <dbl>  <dbl>      <dbl>      <dbl>      <dbl>      <dbl>            <dbl>
#>  1  5      30         32        11.7       18          1.33             3.67
#>  2  5       2.5       52         9         26.7        6.33             3   
#>  3  1      31.3       51.3      11.7        1.5        3.5              6   
#>  4 33      34         17.3       8.33       5.33       1.5              2   
#>  5 NA      15.7       73.7       9.33      NA          2                1   
#>  6 25      39.3       20         1.67       2.67       4.67             4.67
#>  7 22.7    21         36.7      13          5.33      NA                1.5 
#>  8 50      20.3       13.7       5.67       6.33       3.33             2   
#>  9 14.7    34.3       37         8          2.67       1                2.67
#> 10  4.33   17.3       55.3       6         16         NA                1.5 
#> # … with 28 more rows, and 1 more variable: bare_substrate <dbl>
```

### Management rules

Default data from `mermaidr` has specific details about management
rules:

``` r
north_sulawesi_fishbelt <- my_projects %>%
  filter(name == "North Sulawesi (North Minahasa, Sitaro, Tatoareng MPA) Ecological Survey") %>%
  mermaid_get_project_data("fishbelt", "sampleevents")

north_sulawesi_fishbelt %>%
  distinct(management_rules)
#> # A tibble: 5 x 1
#>   management_rules                                                      
#>   <chr>                                                                 
#> 1 "no take"                                                             
#> 2 "gear restriction; size limits; species restriction"                  
#> 3 "open access"                                                         
#> 4 "gear restriction; periodic closure; size limits; species restriction"
#> 5 ""
```

If you want to look at management rules more broadly, you can clean up
the data and group together Partial Restrictions using
`mermaid_clean_management_rules`:

``` r
north_sulawesi_fishbelt %>%
  distinct(management_rules) %>%
  mermaid_clean_management_rules()
#> # A tibble: 5 x 1
#>   management_rules    
#>   <chr>               
#> 1 No Take             
#> 2 Partial Restrictions
#> 3 Open Access         
#> 4 Partial Restrictions
#> 5 <NA>
```

This groups together the Partial Restrictions rules, tidies up the
names, and converts any empty strings to `NA`s for better missing-value
handling\!

If you want to have a different value, instead of `NA`, you can change
it via `.missing_value`:

``` r
north_sulawesi_fishbelt %>%
  distinct(management_rules) %>%
  mermaid_clean_management_rules(.missing_value = "Not Specified")
#> # A tibble: 5 x 1
#>   management_rules    
#>   <chr>               
#> 1 No Take             
#> 2 Partial Restrictions
#> 3 Open Access         
#> 4 Partial Restrictions
#> 5 Not Specified
```

## Maps

You can generate map of site locations using
`mermaid_map_sites_static()` and `mermaid_map_sites_interactive()`,
which produce static and interactive maps, respectively.

``` r
library(mermaidreporting)

xpdc_sites <- xpdc %>%
  mermaid_get_project_sites()

mermaid_map_sites_static(xpdc_sites)
```

``` r
mermaid_map_sites_interactive(xpdc_sites)
```

Note that this is just a screenshot of the interacive plot, but you can
try it out in an interactive R session by running the above code\!

### Mapping by a variable

You can also map sites by the value of a variable. For example, to map
sites by mean total biomass use `mermaid_map_sites_static()` with
`plot_var` set to `biomass_kgha_avg`:

``` r
mermaid_map_sites_static(xpdc_fishbelt, biomass_kgha_avg)
```

Or by management rules:

``` r
xpdc_fishbelt %>%
  mermaid_clean_management_rules() %>%
  mermaid_map_sites_static(management_rules)
```

## Plotting methods data

### Fish Belt biomass

You can plot Fish Belt biomass, split by a grouping variable (such as
trophic group) and compared by another variable (such as reef exposure).

``` r
xpdc_fishbelt %>%
  mermaid_plot_fish_belt_biomass(biomass_kgha_by_trophic_group_avg, reef_exposure)
```

Or by reef zone:

``` r
xpdc_fishbelt %>%
  mermaid_plot_fish_belt_biomass(biomass_kgha_by_trophic_group_avg, reef_zone)
```

### Benthic PIT percent cover

Similarly, you can plot Benthic PIT percent cover, split by a grouping
variable (such as benthic category) and compared by another variable
(such as management rules):

``` r
xpdc_benthicpit %>%
  mermaid_clean_management_rules(.missing_value = "Not Specified") %>%
  mermaid_plot_benthic_pit_percent_cover(percent_cover_by_benthic_category_avg, management_rules)
```

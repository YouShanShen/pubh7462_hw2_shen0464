PubH_7462_HW2_Shen0464
================
You Shan Shen
2022/2/6

``` r
library(knitr)
knitr::opts_knit$set(echo = TRUE, warning = FALSE, results = "hide", error = FALSE, message = FALSE, include = FALSE,
root.dir = rprojroot::find_rstudio_root_file())
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.6     v dplyr   1.0.7
    ## v tidyr   1.1.4     v stringr 1.4.0
    ## v readr   2.1.1     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
theme_set(theme(plot.title = element_text(hjust = 0.5), legend.position = "right"))
```

3.1 Data Exploration & Cleaning (10pts)

3.2 Data Description (10pts)

``` r
number_row <- brfss_smart_df %>% nrow()
number_col <- brfss_smart_df %>% ncol()
year_level <- length(unique(brfss_smart_df$year))
response_level <- length(unique(brfss_smart_df$response))
number_state <- length(unique(brfss_smart_df$state))
number_county <- length(unique(brfss_smart_df$county))
```

2002-2010. BRFSS SMART County Prevalence contains 10625 observations and
6 variables. Each observation is defined as a data about a U.S. resident
regarding their health-related risk behaviors, chronic health
conditions, and use of preventive services.

The definition of variables in 3.1:

Year: The survey conduct in 9 different numbers of year.

Response: There are 5 levels response from an observation.

State: Observations are from 51 different states in US.

County: Observations are from 51 different counties in US.

Sample size: the number of sample from a specific year, response, state,
and county.

Proportion of response: the proportion of sample from a specific level
of response according to year, state, and county in percentage.

3.3 Do Data Science (50pts) 3.3.1 In the year 2004, which states were
observed at 6 locations? (10pts)

``` r
devtools::install_github("rstudio/gt")
```

    ## WARNING: Rtools is required to build R packages, but is not currently installed.
    ## 
    ## Please download and install Rtools 4.0 from https://cran.r-project.org/bin/windows/Rtools/.

    ## Skipping install of 'gt' from a github remote, the SHA1 (3e0a1acf) has not changed since last install.
    ##   Use `force = TRUE` to force installation

``` r
library(gt)
library(webshot)

  brfss_smart_df %>% 
  filter(year %in% '2004') %>% 
  dplyr::select( state, county ) %>%
  group_by( state) %>% 
  distinct( county , .keep_all = TRUE) %>%
  summarize(n = n() ) %>%
  filter(n == 6) %>%
  gt() 
```

<div id="egbuihbsjf" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#egbuihbsjf .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#egbuihbsjf .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#egbuihbsjf .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#egbuihbsjf .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#egbuihbsjf .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#egbuihbsjf .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#egbuihbsjf .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#egbuihbsjf .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#egbuihbsjf .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#egbuihbsjf .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#egbuihbsjf .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#egbuihbsjf .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#egbuihbsjf .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#egbuihbsjf .gt_from_md > :first-child {
  margin-top: 0;
}

#egbuihbsjf .gt_from_md > :last-child {
  margin-bottom: 0;
}

#egbuihbsjf .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#egbuihbsjf .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#egbuihbsjf .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
  vertical-align: top;
}

#egbuihbsjf .gt_row_group_first td {
  border-top-width: 2px;
}

#egbuihbsjf .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#egbuihbsjf .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#egbuihbsjf .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#egbuihbsjf .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#egbuihbsjf .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#egbuihbsjf .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#egbuihbsjf .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#egbuihbsjf .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#egbuihbsjf .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#egbuihbsjf .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#egbuihbsjf .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#egbuihbsjf .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#egbuihbsjf .gt_left {
  text-align: left;
}

#egbuihbsjf .gt_center {
  text-align: center;
}

#egbuihbsjf .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#egbuihbsjf .gt_font_normal {
  font-weight: normal;
}

#egbuihbsjf .gt_font_bold {
  font-weight: bold;
}

#egbuihbsjf .gt_font_italic {
  font-style: italic;
}

#egbuihbsjf .gt_super {
  font-size: 65%;
}

#egbuihbsjf .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#egbuihbsjf .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#egbuihbsjf .gt_slash_mark {
  font-size: 0.7em;
  line-height: 0.7em;
  vertical-align: 0.15em;
}

#egbuihbsjf .gt_fraction_numerator {
  font-size: 0.6em;
  line-height: 0.6em;
  vertical-align: 0.45em;
}

#egbuihbsjf .gt_fraction_denominator {
  font-size: 0.6em;
  line-height: 0.6em;
  vertical-align: -0.05em;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">state</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">n</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_center">CO</td>
<td class="gt_row gt_right">6</td></tr>
    <tr><td class="gt_row gt_center">CT</td>
<td class="gt_row gt_right">6</td></tr>
    <tr><td class="gt_row gt_center">MD</td>
<td class="gt_row gt_right">6</td></tr>
    <tr><td class="gt_row gt_center">NM</td>
<td class="gt_row gt_right">6</td></tr>
    <tr><td class="gt_row gt_center">SC</td>
<td class="gt_row gt_right">6</td></tr>
    <tr><td class="gt_row gt_center">TX</td>
<td class="gt_row gt_right">6</td></tr>
    <tr><td class="gt_row gt_center">UT</td>
<td class="gt_row gt_right">6</td></tr>
    <tr><td class="gt_row gt_center">VT</td>
<td class="gt_row gt_right">6</td></tr>
  </tbody>
  
  
</table>
</div>

3.3.2 Make a “spaghetti plot” that shows the number of observed
locations in each state from 2002 to 2010. Which state has the highest
mean number of locations over this period? (10pts)

``` r
library(ggplot2)
library(viridis)
```

    ## 載入需要的套件：viridisLite

``` r
library(forcats)

brfss_smart_spa <- brfss_smart_df %>%
  group_by(state, year) %>%
  distinct(state, year, county) %>%
  summarize(number = n() ) 
```

    ## `summarise()` has grouped output by 'state'. You can override using the
    ## `.groups` argument.

``` r
brfss_smart_spa$state1 <- fct_reorder(brfss_smart_spa$state,brfss_smart_spa$number,mean ,  na.rm = TRUE, .desc = TRUE)

brfss_smart_spaggplot <- brfss_smart_spa  %>%
   ggplot(aes(x = year, y = number, colour = state1)) +
   geom_line(aes(group = state)) +
   scale_color_viridis_d( "State" )

brfss_smart_spaggplot
```

![](PubH_7462_HW2_shen0464_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
ggsave("./figures/brfss_smart_spaggplot.png", brfss_smart_spaggplot)
```

    ## Saving 7 x 5 in image

3.3.3 Make a table showing, for the years 2002, 2006, and 2010, the mean
and standard deviation of sample size and proportion of Excellent, Good,
and Poor responses across locations in MN. (15pts)

``` r
library(gt)
MN_brfss_smart <- 
  brfss_smart_df %>%
  filter(state %in% 'MN') %>%
  filter(year %in% c('2002', '2006', '2010')) %>%
  filter(response %in% c('Excellent', 'Good', 'Poor')) %>%
  dplyr::select( year, response, `sample size`, `prop. of responses` )


  summary_MN_brfss_smart <-
  MN_brfss_smart %>%
  group_by(year, response) %>%
  summarise( across(c(`sample size`, `prop. of responses`),
                    list(mean = mean, sd = sd),
                    na.rm = TRUE, 
                    .names = "{.col}_{.fn}"))
```

    ## `summarise()` has grouped output by 'year'. You can override using the `.groups`
    ## argument.

``` r
  summary_MN_brfss_smart %>%
    mutate(across(where(is.numeric), round, 3)) %>%
   gt() %>%
  tab_header("Mean and SD of Sample Size, Prop. Response from 2002, 2006,and 2010 in MN ") 
```

<div id="bftqwmzfti" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#bftqwmzfti .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#bftqwmzfti .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#bftqwmzfti .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#bftqwmzfti .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#bftqwmzfti .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#bftqwmzfti .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#bftqwmzfti .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#bftqwmzfti .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#bftqwmzfti .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#bftqwmzfti .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#bftqwmzfti .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#bftqwmzfti .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#bftqwmzfti .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#bftqwmzfti .gt_from_md > :first-child {
  margin-top: 0;
}

#bftqwmzfti .gt_from_md > :last-child {
  margin-bottom: 0;
}

#bftqwmzfti .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#bftqwmzfti .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#bftqwmzfti .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
  vertical-align: top;
}

#bftqwmzfti .gt_row_group_first td {
  border-top-width: 2px;
}

#bftqwmzfti .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#bftqwmzfti .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#bftqwmzfti .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#bftqwmzfti .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#bftqwmzfti .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#bftqwmzfti .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#bftqwmzfti .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#bftqwmzfti .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#bftqwmzfti .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#bftqwmzfti .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#bftqwmzfti .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#bftqwmzfti .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#bftqwmzfti .gt_left {
  text-align: left;
}

#bftqwmzfti .gt_center {
  text-align: center;
}

#bftqwmzfti .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#bftqwmzfti .gt_font_normal {
  font-weight: normal;
}

#bftqwmzfti .gt_font_bold {
  font-weight: bold;
}

#bftqwmzfti .gt_font_italic {
  font-style: italic;
}

#bftqwmzfti .gt_super {
  font-size: 65%;
}

#bftqwmzfti .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#bftqwmzfti .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#bftqwmzfti .gt_slash_mark {
  font-size: 0.7em;
  line-height: 0.7em;
  vertical-align: 0.15em;
}

#bftqwmzfti .gt_fraction_numerator {
  font-size: 0.6em;
  line-height: 0.6em;
  vertical-align: 0.45em;
}

#bftqwmzfti .gt_fraction_denominator {
  font-size: 0.6em;
  line-height: 0.6em;
  vertical-align: -0.05em;
}
</style>
<table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="5" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Mean and SD of Sample Size, Prop. Response from 2002, 2006,and 2010 in MN </th>
    </tr>
    
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">response</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">sample size_mean</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">sample size_sd</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">prop. of responses_mean</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">prop. of responses_sd</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr class="gt_group_heading_row">
      <td colspan="5" class="gt_group_heading">2002</td>
    </tr>
    <tr class="gt_row_group_first"><td class="gt_row gt_center">Excellent</td>
<td class="gt_row gt_right">116.000</td>
<td class="gt_row gt_right">83.275</td>
<td class="gt_row gt_right">24.150</td>
<td class="gt_row gt_right">3.541</td></tr>
    <tr><td class="gt_row gt_center">Good</td>
<td class="gt_row gt_right">123.750</td>
<td class="gt_row gt_right">84.263</td>
<td class="gt_row gt_right">23.950</td>
<td class="gt_row gt_right">1.047</td></tr>
    <tr><td class="gt_row gt_center">Poor</td>
<td class="gt_row gt_right">13.750</td>
<td class="gt_row gt_right">9.570</td>
<td class="gt_row gt_right">2.400</td>
<td class="gt_row gt_right">1.169</td></tr>
    <tr class="gt_group_heading_row">
      <td colspan="5" class="gt_group_heading">2006</td>
    </tr>
    <tr class="gt_row_group_first"><td class="gt_row gt_center">Excellent</td>
<td class="gt_row gt_right">122.333</td>
<td class="gt_row gt_right">72.625</td>
<td class="gt_row gt_right">23.833</td>
<td class="gt_row gt_right">2.987</td></tr>
    <tr><td class="gt_row gt_center">Good</td>
<td class="gt_row gt_right">137.333</td>
<td class="gt_row gt_right">85.816</td>
<td class="gt_row gt_right">26.367</td>
<td class="gt_row gt_right">0.451</td></tr>
    <tr><td class="gt_row gt_center">Poor</td>
<td class="gt_row gt_right">15.000</td>
<td class="gt_row gt_right">6.928</td>
<td class="gt_row gt_right">2.300</td>
<td class="gt_row gt_right">0.954</td></tr>
    <tr class="gt_group_heading_row">
      <td colspan="5" class="gt_group_heading">2010</td>
    </tr>
    <tr class="gt_row_group_first"><td class="gt_row gt_center">Excellent</td>
<td class="gt_row gt_right">203.800</td>
<td class="gt_row gt_right">190.598</td>
<td class="gt_row gt_right">25.440</td>
<td class="gt_row gt_right">5.278</td></tr>
    <tr><td class="gt_row gt_center">Good</td>
<td class="gt_row gt_right">220.000</td>
<td class="gt_row gt_right">196.099</td>
<td class="gt_row gt_right">26.040</td>
<td class="gt_row gt_right">3.547</td></tr>
    <tr><td class="gt_row gt_center">Poor</td>
<td class="gt_row gt_right">27.400</td>
<td class="gt_row gt_right">27.318</td>
<td class="gt_row gt_right">2.360</td>
<td class="gt_row gt_right">0.770</td></tr>
  </tbody>
  
  
</table>
</div>

3.3.4 Create a ggplot that communicates the results/trends from the
table above and stands on its own (15pts)

``` r
library(stringr)

tidy_brfss_smart <-
summary_MN_brfss_smart %>%
pivot_longer(cols = starts_with("sample"),
names_to = "sample_size_type", 
values_to = "sample_values") %>%
mutate(
sample_size_type = stringr::str_remove(sample_size_type, "sample size_") %>%
str_to_upper() %>% 
as.factor()
) %>%
pivot_longer(cols = starts_with("prop."), 
names_to = "prop._type",
values_to = "prop_values") %>%
mutate(
prop._type = stringr::str_remove(prop._type, "prop. of responses_") %>%
str_to_upper() %>% 
as.factor())



base.gg <- 
  tidy_brfss_smart %>%
  mutate(year= as.factor(year)) 

year_ggplot <-  base.gg %>%
ggplot(aes(x = year, y = `sample_values`, colour = response, fill = response))+
geom_col()  +
ggtitle( "Mean of Sample Size for Response from 2002, 2006,and 2010 in MN" ) +
xlab( "Year" ) + 
ylab( "Mean of Sample Size Values" )

year_ggplot
```

![](PubH_7462_HW2_shen0464_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
response_type_ggplot <-
  year_ggplot  + 
  facet_wrap(~ response, scales = "free_x") +
ggtitle( "Mean of Sample Size for Response from 2002, 2006,and 2010 in MN" ) +
xlab( "Year" ) + 
ylab( " Mean of Sample Size Values" )

response_type_ggplot
```

![](PubH_7462_HW2_shen0464_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

``` r
dir.create("./figures", showWarnings = FALSE)
ggsave("./figures/year_ggplot.png", year_ggplot)
```

    ## Saving 7 x 5 in image

``` r
ggsave("./figures/response_type_ggplot.png", response_type_ggplot)
```

    ## Saving 7 x 5 in image

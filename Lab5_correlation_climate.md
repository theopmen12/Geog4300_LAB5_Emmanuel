# Lab 5: Correlation, National Climate Policy, and Country Characteristics
Your Name
2026-11-13

- [<span class="toc-section-number">1</span>
  Introduction](#introduction)
  - [<span class="toc-section-number">1.1</span> Learning
    objectives](#learning-objectives)
- [<span class="toc-section-number">2</span> 1. Load packages and
  inspect the data](#1-load-packages-and-inspect-the-data)
  - [<span class="toc-section-number">2.1</span> Variables in the
    dataset](#variables-in-the-dataset)
- [<span class="toc-section-number">3</span> 2. Explore the
  distributions of the
  variables](#2-explore-the-distributions-of-the-variables)
- [<span class="toc-section-number">4</span> 3. Calculate correlations
  for 2020](#3-calculate-correlations-for-2020)
- [<span class="toc-section-number">5</span> 4. Compare correlations
  between 2000 and 2020](#4-compare-correlations-between-2000-and-2020)
- [<span class="toc-section-number">6</span> 5. Examine changes within
  countries](#5-examine-changes-within-countries)
- [<span class="toc-section-number">7</span> Challenge!](#challenge)
  - [<span class="toc-section-number">7.1</span> Final submission
    stuff](#final-submission-stuff)

## Introduction

How are national climate policies related to countries’ political
institutions, economic development, and carbon emissions? Do these
relationships look different in 2000 and 2020?

In this lab, you will use data from the OECD, the World Development
Indicators (WDI), and the Varieties of Democracy (V-Dem) project to
investigate these questions. You will use descriptive statistics,
visualizations, correlation coefficients, and statistical tests to
explore patterns across 34 countries.

Correlation can help identify relationships between variables, but it
cannot establish that one variable causes another. For example, an
association between democracy and climate-policy stringency does not
demonstrate that democratic institutions cause stronger climate
policies.

### Learning objectives

This lab assesses the following learning standards:

10. Derive new variables from existing ones through calculated
    transformations, including unit conversions, differences across time
    periods, and categorical dummy variables.
11. Assess the normality of a variable and use those results to select
    between parametric and non-parametric statistical tests.
12. Conduct correlation analysis across multiple variable pairs and
    evaluate results with respect to direction, magnitude, and
    significance, including changes in correlation structure across time
    periods or subgroups.

## 1. Load packages and inspect the data

We will use `tidyverse` for data manipulation and visualization, `Hmisc`
to calculate correlation matrices, and `corrplot` to visualize those
matrices.

``` r
library(tidyverse)
```

    ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ✔ dplyr     1.2.1     ✔ readr     2.2.0
    ✔ forcats   1.0.1     ✔ stringr   1.6.0
    ✔ ggplot2   4.0.3     ✔ tibble    3.3.1
    ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
    ✔ purrr     1.2.2     
    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter() masks stats::filter()
    ✖ dplyr::lag()    masks stats::lag()
    ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(Hmisc)
```


    Attaching package: 'Hmisc'

    The following objects are masked from 'package:dplyr':

        src, summarize

    The following objects are masked from 'package:base':

        format.pval, units

``` r
library(corrplot)
```

    corrplot 0.95 loaded

``` r
library(knitr)

all_vars<-read_csv("data/climpolicy_combined.csv")
```

    Rows: 68 Columns: 8
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: ","
    chr (2): country_text_id, country_name
    dbl (6): year, oecd_climpol_stringent, co2_emissions, gdp, tot_population, v...

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
dim(all_vars)   
```

    [1] 68  8

``` r
head(all_vars)  
```

    # A tibble: 6 × 8
      country_text_id country_name  year oecd_climpol_stringent co2_emissions
      <chr>           <chr>        <dbl>                  <dbl>         <dbl>
    1 ISL             Iceland       2000                  0              2.85
    2 ISL             Iceland       2020                  1              3.07
    3 IRL             Ireland       2020                  3             34.3 
    4 DNK             Denmark       2000                  2             53.1 
    5 AUS             Australia     2000                  0.833        354.  
    6 AUS             Australia     2020                  2.92         393.  
    # ℹ 3 more variables: gdp <dbl>, tot_population <dbl>, v2x_polyarchy <dbl>

The combined dataset, `all_vars`, contains observations for 34 countries
in 2000 and 2020. Each row represents a country in a particular year.

### Variables in the dataset

| Variable | Description |
|:---|:---|
| `country_name` | Country name |
| `country_text_id` | Country identifier |
| `year` | Observation year |
| `oecd_climpol_stringent` | OECD-derived climate-policy stringency measure |
| `co2_emissions` | Total CO₂ emissions measure |
| `gdp` | Gross domestic product (constant 2015 US dollars) |
| `tot_population` | Total population |
| `v2x_polyarchy` | V-Dem electoral democracy index |

We will also calculate GDP per capita and CO₂ emissions per capita.

``` r
all_vars <- all_vars %>%
  mutate(
    gdp_per_capita = gdp / tot_population,
    co2_per_capita = co2_emissions * 1e6 / tot_population
  )
```

This calculation assumes `co2_emissions` is in million tonnes and
population is measured in persons. The resulting `co2_per_capita`
measure is tonnes per person. GDP per capita is in 2015 US dollars,
divided by population.

Inspect the new variables.

``` r
all_vars %>%
  select(
    country_name, year, oecd_climpol_stringent,
    co2_per_capita, gdp_per_capita, v2x_polyarchy
  ) %>%
  slice_head(n = 10) %>%
  kable(digits = 2)
```

| country_name | year | oecd_climpol_stringent | co2_per_capita | gdp_per_capita | v2x_polyarchy |
|:---|---:|---:|---:|---:|---:|
| Iceland | 2000 | 0.00 | 10.14 | 43472.34 | 0.87 |
| Iceland | 2020 | 1.00 | 8.38 | 52464.31 | 0.86 |
| Ireland | 2020 | 3.00 | 6.80 | 80927.94 | 0.89 |
| Denmark | 2000 | 2.00 | 9.95 | 49266.99 | 0.91 |
| Australia | 2000 | 0.83 | 18.60 | 45979.08 | 0.89 |
| Australia | 2020 | 2.92 | 15.30 | 58377.77 | 0.84 |
| Austria | 2000 | 1.89 | 8.32 | 38571.09 | 0.86 |
| Austria | 2020 | 3.31 | 7.06 | 43428.70 | 0.86 |
| Belgium | 2000 | 1.56 | 12.18 | 35878.32 | 0.88 |
| Belgium | 2020 | 3.44 | 7.97 | 40769.02 | 0.90 |

**Task 1** *Why might GDP per capita and CO₂ emissions per capita be
more useful than total GDP and total emissions when comparing
countries?*

{GDP per capita and CO₂ emissions per capita are more useful because
countries have different population sizes. A country with a large
population may have a higher total GDP and more CO₂ emissions simply
because more people live there. By calculating these values per person,
we can compare countries more fairly. GDP per capita helps us understand
the average economic output per person, while CO₂ emissions per capita
show how much carbon dioxide is emitted per person. This makes it easier
to compare countries regardless of their population sizes.}

## 2. Explore the distributions of the variables

Before calculating correlations, examine the distributions of the
variables. Correlation coefficients summarize relationships between
variables, and their interpretation can be affected by skewed
distributions and unusual observations.

We will focus on four measures:

- Climate-policy stringency
- CO₂ emissions per capita
- GDP per capita
- Electoral democracy

Create a subset for 2020.

``` r
data_2020 <- all_vars %>%
  filter(year == 2020)
```

We can then create histograms to examine the distributions for each of
the four variables of interest.

``` r
data_2020 %>%
  select(
    oecd_climpol_stringent,
    co2_per_capita,
    gdp_per_capita,
    v2x_polyarchy
  ) %>%
  pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "value"
  ) %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 12, color = "white") +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  labs(
    title = "Distributions of National Characteristics, 2020",
    x = NULL,
    y = "Number of countries"
  )
```

![](Lab5_correlation_climate_files/figure-commonmark/unnamed-chunk-4-1.png)

The facets use different horizontal scales because the variables have
different units.

You can also examine normal Q-Q plots.

``` r
data_2020 %>%
  select(
    oecd_climpol_stringent,
    co2_per_capita,
    gdp_per_capita,
    v2x_polyarchy
  ) %>%
  pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "value"
  ) %>%
  ggplot(aes(sample = value)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  labs(
    title = "Normal Q-Q Plots, 2020",
    x = "Theoretical quantiles",
    y = "Sample quantiles"
  )
```

![](Lab5_correlation_climate_files/figure-commonmark/unnamed-chunk-5-1.png)

**Task 2** *Which variables appear most skewed? Pick one of these and
identify some of the countries that are outliers for this variable.*

{Based on the histograms and Q-Q plots, GDP per capita and CO₂ emissions
per capita appear to be the most right-skewed variables. This means most
countries have lower or moderate values, while a few countries have much
higher values. Electoral democracy is also left-skewed, with most
countries having high democracy scores.For GDP per capita, Luxembourg
stands out as an outlier, with approximately \$105,274 per person in
2020. Switzerland (\$86,294), Ireland (\$80,928), and Norway (\$78,387)
also have relatively high GDP per capita compared to most other
countries. These high values contribute to the right-skewed
distribution.}

## 3. Calculate correlations for 2020

Correlation coefficients measure the direction and strength of
relationships between two numerical variables. Pearson’s correlation
coefficient is typically used when variables are approximately normally
distributed and exhibit a linear relationship, whereas Spearman’s rank
correlation coefficient is appropriate for non-normal or ordinal data.

**Task 3** *Select just the four variables of interest and create a
correlation matrix using the appropriate measure of correlation. Print
this table once you’ve created it.*

``` r
#select the four variables.
vars_2020 <- data_2020 %>%
  select(oecd_climpol_stringent,
         co2_per_capita,
         gdp_per_capita,
         v2x_polyarchy)

# Spearman is suitable here because normality is questionable.the rcorr() calculates both the correlations and their p-values.
cor_2020 <- rcorr(as.matrix(vars_2020), type = "spearman")
cor_2020
```

                           oecd_climpol_stringent co2_per_capita gdp_per_capita
    oecd_climpol_stringent                   1.00           0.20           0.57
    co2_per_capita                           0.20           1.00           0.42
    gdp_per_capita                           0.57           0.42           1.00
    v2x_polyarchy                            0.50           0.13           0.72
                           v2x_polyarchy
    oecd_climpol_stringent          0.50
    co2_per_capita                  0.13
    gdp_per_capita                  0.72
    v2x_polyarchy                   1.00

    n= 34 


    P
                           oecd_climpol_stringent co2_per_capita gdp_per_capita
    oecd_climpol_stringent                        0.2652         0.0004        
    co2_per_capita         0.2652                                0.0145        
    gdp_per_capita         0.0004                 0.0145                       
    v2x_polyarchy          0.0026                 0.4473         0.0000        
                           v2x_polyarchy
    oecd_climpol_stringent 0.0026       
    co2_per_capita         0.4473       
    gdp_per_capita         0.0000       
    v2x_polyarchy                       

``` r
#Print the matrix of correlation coefficients.
kable(round(cor_2020$r, 2))
```

|  | oecd_climpol_stringent | co2_per_capita | gdp_per_capita | v2x_polyarchy |
|:---|---:|---:|---:|---:|
| oecd_climpol_stringent | 1.00 | 0.20 | 0.57 | 0.50 |
| co2_per_capita | 0.20 | 1.00 | 0.42 | 0.13 |
| gdp_per_capita | 0.57 | 0.42 | 1.00 | 0.72 |
| v2x_polyarchy | 0.50 | 0.13 | 0.72 | 1.00 |

``` r
# p-values for interpreting statistical significance.
kable(round(cor_2020$P, 3))
```

|  | oecd_climpol_stringent | co2_per_capita | gdp_per_capita | v2x_polyarchy |
|:---|---:|---:|---:|---:|
| oecd_climpol_stringent | NA | 0.265 | 0.000 | 0.003 |
| co2_per_capita | 0.265 | NA | 0.014 | 0.447 |
| gdp_per_capita | 0.000 | 0.014 | NA | 0.000 |
| v2x_polyarchy | 0.003 | 0.447 | 0.000 | NA |

**Task 4** *Create a correlation plot from this matrix using `corrplot`
and the parameters of your choosing*

``` r
corrplot.mixed(cor_2020$r,
               lower = "square",
               upper = "circle",
               tl.pos = "lt",
               diag = "n")
```

![](Lab5_correlation_climate_files/figure-commonmark/Task4-1.png)

**Task 5** Based on your correlation matrix and correlation plot, is
climate-policy stringency associated with electoral democracy, GDP per
capita, or CO₂ emissions per capita? Describe the direction and strength
of each relationship. Do the results support your expectations?

The correlation matrix and plot show that climate-policy stringency has
a moderate positive relationship with GDP per capita (r = 0.57) and with
electoral democracy (r = 0.50). Both are statistically significant (p \<
0.05). Countries with higher GDP per capita and stronger democracy tend
to have stricter climate policies. CO₂ emissions per capita have only a
weak positive relationship with stringency (r = 0.20), and it is not
statistically significant (p = 0.265).The results mostly match what I
expected. I thought wealthier, more democratic countries would have
stronger climate policies, and they do. I also expected countries with
higher CO₂ emissions to have stricter policies, but the results show no
strong relationship there.

## 4. Compare correlations between 2000 and 2020

Next, examine whether relationships between climate policy and national
characteristics differ across the two years. First, we can create a new
dataset for the observations in 2000.

``` r
data_2000 <- all_vars %>%
  filter(year == 2000)
```

**Task 6** *Create a new correlation matrix for 2000 and print it, just
as you did for the 2020 data. Then print both matrices so they can be
compared side-by-side.*

``` r
vars_2000 <- data_2000 %>%
  select(oecd_climpol_stringent,
         co2_per_capita,
         gdp_per_capita,
         v2x_polyarchy)

cor_2000 <- rcorr(as.matrix(vars_2000), type = "spearman")
```

**Task 7** How did the correlation between climate-policy stringency and
electoral democracy change between 2000 and 2020? What about the
relationship between policy stringency and CO₂ emissions per capita?

{}

## 5. Examine changes within countries

Remember that these matrices describe relationships across countries
within each year. A change in a correlation across years is not the same
as a change experienced by an individual country. To examine changes
over time, we can reshape the dataset so that each country’s 2000 and
2020 observations appear in the same row.

``` r
country_changes <- all_vars %>%
  select(
    country_text_id,
    country_name,
    year,
    oecd_climpol_stringent,
    co2_per_capita,
    gdp_per_capita,
    v2x_polyarchy
  ) %>%
  pivot_wider(
    names_from = year,
    values_from = c(
      oecd_climpol_stringent,
      co2_per_capita,
      gdp_per_capita,
      v2x_polyarchy
    ),
    names_sep = "_"
  ) 
```

**Task 8** *Calculate the change in each variable between 2000 and 2020
for every country.*

**Task 9** *To assess these changes, create a correlation matrix for
these four new variables. This will show *correlation in changes* across
countries.*

**Task 10** Identify the five countries with the largest increases and
the five countries with the largest decreases in climate-policy
stringency. Repeat this analysis for CO₂ emissions per capita.

{Your response here}

**Task 11** How does this within-country change analysis differ from
correlating climate-policy stringency and emissions across countries in
2020? Why might the two analyses produce different results?

{Your response here}

## Challenge!

Let’s map those variables showing change. The code below downloads
country boundaries from Natural Earth
(https://www.naturalearthdata.com/). Join your final dataset to these
boundaries and use `tmap` to create maps of each of the four variables
showing change from 2000-2020. What patterns do you see? How does it add
to/change your findings from the main part of the lab?

``` r
# install.packages("rnaturalearth")
library(rnaturalearth)
```

    Warning: package 'rnaturalearth' was built under R version 4.6.2

``` r
countries<-ne_download(type="admin_0_countries")
```

    Reading 'ne_110m_admin_0_countries.zip' from naturalearth...

### Final submission stuff

**Disclosure of assistance:** Besides class materials, what other
sources of assistance did you use while completing this lab? These can
include input from classmates, relevant material identified through web
searches (e.g., Stack Overflow), or assistance from ChatGPT or other AI
tools. How did these sources support your own learning in completing
this lab?

{Response here.}

**Lab reflection:** How do you feel about the work you did on this lab?
Was it easy, moderate, or hard? What are the biggest things you learned
by completing it?

{Your response here}

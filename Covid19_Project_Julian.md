COVID19\_Project
================
Julian, Nabih, Nathan, and Yehya
02-10-2020

  - [Getting the Data](#getting-the-data)
  - [Join the Data](#join-the-data)
  - [Work on the Data](#work-on-the-data)
  - [Conclusion](#conclusion)

``` r
library(tidyverse)
```

    ## ── Attaching packages ────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.3     ✓ dplyr   1.0.2
    ## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.5.0

    ## ── Conflicts ───────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

*Background*:

# Getting the Data

<!------------------------------------------------------>

\#\#The GDP data can be substituted for GapMinder

``` r
url_covid <- "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv"

## Set the filenames of the data to download
filename_covid <- "./data/covid_countries.csv"
filename_gdp <- "./data/gdp_countries.csv"

## Download the data locally
curl::curl_download(
        url_covid,
        destfile = filename_covid
)

## Loads the downloaded csv
df_covid_temp <- read_csv(filename_covid) %>% 
  select(iso_code, continent, date, total_cases, total_deaths, total_cases_per_million, total_deaths_per_million, population)
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   iso_code = col_character(),
    ##   continent = col_character(),
    ##   location = col_character(),
    ##   date = col_date(format = ""),
    ##   total_tests = col_logical(),
    ##   new_tests = col_logical(),
    ##   total_tests_per_thousand = col_logical(),
    ##   new_tests_per_thousand = col_logical(),
    ##   new_tests_smoothed = col_logical(),
    ##   new_tests_smoothed_per_thousand = col_logical(),
    ##   tests_per_case = col_logical(),
    ##   positive_rate = col_logical(),
    ##   tests_units = col_logical()
    ## )

    ## See spec(...) for full column specifications.

    ## Warning: 185173 parsing failures.
    ##  row                      col           expected          actual                         file
    ## 2544 total_tests              1/0/T/F/TRUE/FALSE 143056.0        './data/covid_countries.csv'
    ## 2544 total_tests_per_thousand 1/0/T/F/TRUE/FALSE 5.61            './data/covid_countries.csv'
    ## 2544 tests_units              1/0/T/F/TRUE/FALSE tests performed './data/covid_countries.csv'
    ## 2545 tests_units              1/0/T/F/TRUE/FALSE tests performed './data/covid_countries.csv'
    ## 2546 tests_units              1/0/T/F/TRUE/FALSE tests performed './data/covid_countries.csv'
    ## .... ........................ .................. ............... ............................
    ## See problems(...) for more details.

``` r
df_gdp_temp <- read_csv(filename_gdp, skip=3) %>% 
  select("Country Name", "2019","Country Code")
```

    ## Warning: Missing column names filled in: 'X66' [66]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Country Name` = col_character(),
    ##   `Country Code` = col_character(),
    ##   `Indicator Name` = col_character(),
    ##   `Indicator Code` = col_character(),
    ##   `2020` = col_logical(),
    ##   X66 = col_logical()
    ## )
    ## See spec(...) for full column specifications.

``` r
##Rename columns:
df_gdp <- df_gdp_temp %>% 
  rename(
    iso_code = "Country Code",
    country = "Country Name",
    gdp = "2019"
    )
##Rename columns:
df_covid <- df_covid_temp %>% 
  rename(
    "cases" = total_cases,
    "deaths" = total_deaths,
    "cases_per1M" = total_cases_per_million,
    "deaths_per1M" = total_deaths_per_million
    )

## Summaries to check info is okay
summary(df_covid)
```

    ##    iso_code          continent              date                cases         
    ##  Length:52222       Length:52222       Min.   :2019-12-31   Min.   :       1  
    ##  Class :character   Class :character   1st Qu.:2020-04-22   1st Qu.:     145  
    ##  Mode  :character   Mode  :character   Median :2020-06-23   Median :    1745  
    ##                                        Mean   :2020-06-20   Mean   :  137472  
    ##                                        3rd Qu.:2020-08-24   3rd Qu.:   17248  
    ##                                        Max.   :2020-10-24   Max.   :42279076  
    ##                                                             NA's   :3607      
    ##      deaths         cases_per1M        deaths_per1M        population       
    ##  Min.   :      1   Min.   :    0.00   Min.   :   0.000   Min.   :8.090e+02  
    ##  1st Qu.:     11   1st Qu.:   73.56   1st Qu.:   3.384   1st Qu.:1.399e+06  
    ##  Median :     72   Median :  486.96   Median :  17.733   Median :8.279e+06  
    ##  Mean   :   6166   Mean   : 2530.65   Mean   :  85.732   Mean   :8.690e+07  
    ##  3rd Qu.:    616   3rd Qu.: 2823.75   3rd Qu.:  72.257   3rd Qu.:2.983e+07  
    ##  Max.   :1145314   Max.   :52261.70   Max.   :1237.551   Max.   :7.795e+09  
    ##  NA's   :12472     NA's   :3870       NA's   :12720      NA's   :299

``` r
summary(df_gdp)
```

    ##    country               gdp              iso_code        
    ##  Length:264         Min.   :4.727e+07   Length:264        
    ##  Class :character   1st Qu.:1.397e+10   Class :character  
    ##  Mode  :character   Median :7.609e+10   Mode  :character  
    ##                     Mean   :3.237e+12                     
    ##                     3rd Qu.:7.761e+11                     
    ##                     Max.   :8.770e+13                     
    ##                     NA's   :41

# Join the Data

<!-- -------------------------------------------------- -->

\#\#Hopefully well be able to join the data using the ISO code, if not
we can use the country name and see what problems that may bring

``` r
df_data_raw <-df_gdp %>%
  ## Join both databases we have
            left_join(
              df_covid,
              by = "iso_code",
              na.rm = TRUE
            ) %>% 
  ## Whenever teh value of GDP is NA or contintne is NA, we drop the country because we cant compare it to others. If teh value of a case/death count is NA, we set it to 0
              drop_na(gdp) %>% 
                drop_na(continent) %>% 
                  replace_na(list("cases" = 0, "deaths" = 0, "cases_per1M" = 0,  "deaths_per1M" = 0))

df_data_raw
```

    ## # A tibble: 42,817 x 10
    ##    country     gdp iso_code continent date       cases deaths cases_per1M
    ##    <chr>     <dbl> <chr>    <chr>     <date>     <dbl>  <dbl>       <dbl>
    ##  1 Afghan… 1.91e10 AFG      Asia      2019-12-31     0      0           0
    ##  2 Afghan… 1.91e10 AFG      Asia      2020-01-01     0      0           0
    ##  3 Afghan… 1.91e10 AFG      Asia      2020-01-02     0      0           0
    ##  4 Afghan… 1.91e10 AFG      Asia      2020-01-03     0      0           0
    ##  5 Afghan… 1.91e10 AFG      Asia      2020-01-04     0      0           0
    ##  6 Afghan… 1.91e10 AFG      Asia      2020-01-05     0      0           0
    ##  7 Afghan… 1.91e10 AFG      Asia      2020-01-06     0      0           0
    ##  8 Afghan… 1.91e10 AFG      Asia      2020-01-07     0      0           0
    ##  9 Afghan… 1.91e10 AFG      Asia      2020-01-08     0      0           0
    ## 10 Afghan… 1.91e10 AFG      Asia      2020-01-09     0      0           0
    ## # … with 42,807 more rows, and 2 more variables: deaths_per1M <dbl>,
    ## #   population <dbl>

# Work on the Data

<!-- -------------------------------------------------- -->

We are going to calculate GDP per capita so that our measurements to
compare the countries are standardized, and we can get more “honest”
measurements and comparisons.

``` r
df_data_norm <- df_data_raw %>% 
                  mutate(gdpPerCap = (gdp/population), na.rm = TRUE) %>% 
                    select(continent, country, gdpPerCap, date, cases_per1M, deaths_per1M)
df_data_norm
```

    ## # A tibble: 42,817 x 6
    ##    continent country     gdpPerCap date       cases_per1M deaths_per1M
    ##    <chr>     <chr>           <dbl> <date>           <dbl>        <dbl>
    ##  1 Asia      Afghanistan      491. 2019-12-31           0            0
    ##  2 Asia      Afghanistan      491. 2020-01-01           0            0
    ##  3 Asia      Afghanistan      491. 2020-01-02           0            0
    ##  4 Asia      Afghanistan      491. 2020-01-03           0            0
    ##  5 Asia      Afghanistan      491. 2020-01-04           0            0
    ##  6 Asia      Afghanistan      491. 2020-01-05           0            0
    ##  7 Asia      Afghanistan      491. 2020-01-06           0            0
    ##  8 Asia      Afghanistan      491. 2020-01-07           0            0
    ##  9 Asia      Afghanistan      491. 2020-01-08           0            0
    ## 10 Asia      Afghanistan      491. 2020-01-09           0            0
    ## # … with 42,807 more rows

``` r
df_data_norm %>%
  filter(date == max(date)) %>% 
  ggplot(
    aes(gdpPerCap, cases_per1M)
  ) +
  geom_point() +
  stat_smooth(method="lm", se=TRUE, fill=NA,
                formula=y ~ poly(x, 1, raw=TRUE),colour="red") +
  facet_wrap(~ continent) +
  theme_minimal() +
  labs(
    x = "GDP Per Capita",
    y = "Cases (per 1,000,000 persons)"
  )
```

![](Covid19_Project_Julian_files/figure-gfm/SA-plot-latest-1.png)<!-- -->

``` r
df_data_norm %>%
  filter(date == max(date)) %>% 
  ggplot(
    aes(gdpPerCap, cases_per1M)
  ) +
  geom_point() +
  stat_smooth(method="lm", se=TRUE, fill=NA,
                formula=y ~ poly(x, 1, raw=TRUE),colour="red") +
  facet_wrap(~ continent, scale = "free") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45)
  ) +
  labs(
    x = "GDP Per Capita",
    y = "Cases (per 1,000,000 persons)"
  )
```

![](Covid19_Project_Julian_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

# Conclusion

<!-- -------------------------------------------------- -->

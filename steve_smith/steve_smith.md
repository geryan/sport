Steve Smith
================

Load in packages

``` r
library(readr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

``` r
library(ggplot2)
```

Read in and clean the data. Downloaded from [CricInfo](http://stats.espncricinfo.com/ci/engine/player/267192.html?class=1;template=results;type=allround;view=innings) on 2019-09-06

``` r
ssdata <- read_csv(file = "steve_smith_stats.csv", skip = 1)
```

    ## Warning: Missing column names filled in: 'X12' [12]

    ## Parsed with column specification:
    ## cols(
    ##   innings = col_double(),
    ##   score = col_character(),
    ##   overs = col_character(),
    ##   conceded = col_character(),
    ##   wickets = col_character(),
    ##   catches = col_character(),
    ##   stumpings = col_character(),
    ##   opposition = col_character(),
    ##   ground = col_character(),
    ##   start_date = col_character(),
    ##   test_no = col_character(),
    ##   X12 = col_logical()
    ## )

``` r
Encoding(ssdata$opposition) <- "UTF-8"

ssdata %<>%
  mutate(start_date = dmy(start_date),
         opposition = iconv(opposition, "UTF-8", "UTF-8", sub = ""),
         opposition = sub("^v", "", opposition),
         not_out = grepl("\\*$", score),
         score = sub("\\*$", "", score),
         score = as.integer(score),
         overs = ifelse(overs == "-", NA, overs),
         inningstype = ifelse(is.na(overs), "bat", "bowl"),
         conceded = as.integer(conceded),
         wickets = as.integer(wickets),
         catches = as.integer(catches),
         stumpings = as.integer(stumpings),
         oversi = as.integer(overs),
         year = year(start_date)) %>%
  select(-X12)
```

    ## Warning: NAs introduced by coercion

    ## Warning: NAs introduced by coercion

    ## Warning: NAs introduced by coercion

    ## Warning: NAs introduced by coercion

    ## Warning: NAs introduced by coercion

    ## Warning: NAs introduced by coercion

``` r
ssdata
```

    ## # A tibble: 252 x 15
    ##    innings score overs conceded wickets catches stumpings opposition ground
    ##      <dbl> <int> <chr>    <int>   <int>   <int>     <int> <chr>      <chr> 
    ##  1       1     1 <NA>        NA      NA      NA        NA Pakistan   Lord's
    ##  2       2    NA DNB         NA      NA       1         0 Pakistan   Lord's
    ##  3       3    12 <NA>        NA      NA      NA        NA Pakistan   Lord's
    ##  4       4    NA 21          51       3       0         0 Pakistan   Lord's
    ##  5       1    10 <NA>        NA      NA      NA        NA Pakistan   Leeds 
    ##  6       2    NA 1            7       0       0         0 Pakistan   Leeds 
    ##  7       3    77 <NA>        NA      NA      NA        NA Pakistan   Leeds 
    ##  8       4    NA 9           24       0       0         0 Pakistan   Leeds 
    ##  9       1     7 <NA>        NA      NA      NA        NA England    Perth 
    ## 10       2    NA DNB         NA      NA       0         0 England    Perth 
    ## # ... with 242 more rows, and 6 more variables: start_date <date>,
    ## #   test_no <chr>, not_out <lgl>, inningstype <chr>, oversi <int>,
    ## #   year <dbl>

Separate out the batting data

``` r
ss_bat <- ssdata %>%
  filter(inningstype == "bat") %>%
  filter(!is.na(score))

ss_bat
```

    ## # A tibble: 121 x 15
    ##    innings score overs conceded wickets catches stumpings opposition ground
    ##      <dbl> <int> <chr>    <int>   <int>   <int>     <int> <chr>      <chr> 
    ##  1       1     1 <NA>        NA      NA      NA        NA Pakistan   Lord's
    ##  2       3    12 <NA>        NA      NA      NA        NA Pakistan   Lord's
    ##  3       1    10 <NA>        NA      NA      NA        NA Pakistan   Leeds 
    ##  4       3    77 <NA>        NA      NA      NA        NA Pakistan   Leeds 
    ##  5       1     7 <NA>        NA      NA      NA        NA England    Perth 
    ##  6       3    36 <NA>        NA      NA      NA        NA England    Perth 
    ##  7       1     6 <NA>        NA      NA      NA        NA England    Melbo~
    ##  8       3    38 <NA>        NA      NA      NA        NA England    Melbo~
    ##  9       1    18 <NA>        NA      NA      NA        NA England    Sydney
    ## 10       3    54 <NA>        NA      NA      NA        NA England    Sydney
    ## # ... with 111 more rows, and 6 more variables: start_date <date>,
    ## #   test_no <chr>, not_out <lgl>, inningstype <chr>, oversi <int>,
    ## #   year <dbl>

Get his batting average in each year

``` r
ss_ba <- ss_bat %>%
  group_by(year) %>%
  summarise(runs = sum(score),
            ninnings = n(),
            notouts = sum(not_out),
            avg = sum(score)/(n() - sum(not_out)))

ss_ba
```

    ## # A tibble: 9 x 5
    ##    year  runs ninnings notouts   avg
    ##   <dbl> <int>    <int>   <int> <dbl>
    ## 1  2010   187        8       0  23.4
    ## 2  2011    72        2       1  72  
    ## 3  2013   711       21       2  37.4
    ## 4  2014  1146       17       3  81.9
    ## 5  2015  1474       24       4  73.7
    ## 6  2016  1079       18       3  71.9
    ## 7  2017  1305       20       3  76.8
    ## 8  2018   225        7       0  32.1
    ## 9  2019   589        4       0 147.

And career average

``` r
ss_bat %>%
  summarise(avg = sum(score)/(n() - sum(not_out)))
```

    ## # A tibble: 1 x 1
    ##     avg
    ##   <dbl>
    ## 1  64.6

What does it look like if we exclude the early years...

Here's his career average excluding 2010

``` r
ss_bat %>%
  filter(year != 2010) %>%
  summarise(avg = sum(score)/(n() - sum(not_out)))
```

    ## # A tibble: 1 x 1
    ##     avg
    ##   <dbl>
    ## 1  68.1

Excluding 2010-11

``` r
ss_bat %>%
  filter(year != 2010) %>%
  filter(year != 2011) %>%
  summarise(avg = sum(score)/(n() - sum(not_out)))
```

    ## # A tibble: 1 x 1
    ##     avg
    ##   <dbl>
    ## 1  68.0

And excluding up to 2013, an inconveniently lean year

``` r
ss_bat %>%
  filter(year != 2010) %>%
  filter(year != 2011) %>%
  filter(year != 2013) %>%
  summarise(avg = sum(score)/(n() - sum(not_out)))
```

    ## # A tibble: 1 x 1
    ##     avg
    ##   <dbl>
    ## 1  75.6

What is the overall progression of Steve Smith's yearly average? Line is smoothed average, points are yearly average, and colour scale is number of innings that year.

``` r
ggplot(ss_ba) +
  geom_smooth(aes(x = year, y = avg), col = "black") +
  geom_point(aes(x = year, y = avg, col = ninnings), size = 3) +
  scale_colour_viridis_c(guide = guide_colourbar(title = "No.\ninnings")) +
  theme_classic() +
  labs(x = "Year", y = "Yearly batting average")
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](steve_smith_files/figure-markdown_github/plot%20smooth-1.png)

**There's clearly an unequivocal upward trend, so let's make some egregious predictions about how good his average will be at the end of his career.**

So, we fit a simple exponential model, and a linear model, and make predictions from the model fits.

``` r
sslm <- lm(avg ~ year, data = ss_ba)
ssem <- lm(log(avg) ~ year, data = ss_ba)

lin.preds <- predict(sslm, list(year = 2010:2030))
exp.preds <- exp(predict(ssem, list(year = 2010:2030)))

ss_lin_preds <- tibble(year = 2010:2030, avg = lin.preds)
ss_exp_preds <- tibble(year = 2010:2030, avg = exp.preds)
```

And what do the results show?

As above with points showing annual batting averages, black lines are Steve Smith's predicted annual average, and the red line is the Don's career average of 99.94.

``` r
sspredplot <- ggplot() +
  geom_point(data = ss_ba, aes(x = year, y = avg, col = ninnings), size = 3) +
  geom_line(data = ss_lin_preds, aes(x = year, y = avg), size = 1) +
  geom_line(data = ss_exp_preds, aes(x = year, y = avg), size = 1) +
  geom_hline(yintercept = 99.94, col = "red") +
  scale_colour_viridis_c(guide = guide_colourbar(title = "No.\ninnings")) +
  theme_classic() +
  labs(x = "Year", y = "Yearly batting average")

sspredplot
```

![](steve_smith_files/figure-markdown_github/preds%20plot-1.png)

This is predicts that Steve Smith will have an average in each year higher than the Don's career average every year from around 2020.

*Oh my gosh it's already higher than that this year, he's even better than these models predict.*

**Ahhhh he's going to average over 100 each year until he retires at 41 and might get as high as 250!!!**

The facts don't lie.

And here are the predicted yearly averages for the linear model

``` r
ss_lin_preds
```

    ## # A tibble: 21 x 2
    ##     year   avg
    ##    <int> <dbl>
    ##  1  2010  37.7
    ##  2  2011  44.2
    ##  3  2012  50.6
    ##  4  2013  57.1
    ##  5  2014  63.5
    ##  6  2015  69.9
    ##  7  2016  76.4
    ##  8  2017  82.8
    ##  9  2018  89.2
    ## 10  2019  95.7
    ## # ... with 11 more rows

... and the exponential model

``` r
ss_exp_preds
```

    ## # A tibble: 21 x 2
    ##     year   avg
    ##    <int> <dbl>
    ##  1  2010  38.5
    ##  2  2011  42.2
    ##  3  2012  46.3
    ##  4  2013  50.8
    ##  5  2014  55.7
    ##  6  2015  61.1
    ##  7  2016  67.1
    ##  8  2017  73.6
    ##  9  2018  80.8
    ## 10  2019  88.6
    ## # ... with 11 more rows

``` r
png(res = 300, height = 1000, width = 2000)

sspredplot

dev.off()
```

    ## png 
    ##   2

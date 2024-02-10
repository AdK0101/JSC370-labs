Lab 05 - Data Wrangling
================

# Learning goals

- Use the `merge()` function to join two datasets.
- Deal with missings and impute data.
- Identify relevant observations using `quantile()`.
- Practice your GitHub skills.

# Lab description

For this lab we will be dealing with the meteorological dataset `met`.
In this case, we will use `data.table` to answer some questions
regarding the `met` dataset, while at the same time practice your
Git+GitHub skills for this project.

This markdown document should be rendered using `github_document`
document.

# Part 1: Setup a Git project and the GitHub repository

1.  Go to wherever you are planning to store the data on your computer,
    and create a folder for this project

2.  In that folder, save [this
    template](https://github.com/JSC370/JSC370-2024/blob/main/labs/lab05/lab05-wrangling-gam.Rmd)
    as “README.Rmd”. This will be the markdown file where all the magic
    will happen.

3.  Go to your GitHub account and create a new repository of the same
    name that your local folder has, e.g., “JSC370-labs”.

4.  Initialize the Git project, add the “README.Rmd” file, and make your
    first commit.

5.  Add the repo you just created on GitHub.com to the list of remotes,
    and push your commit to origin while setting the upstream.

Most of the steps can be done using command line:

``` sh
# Step 1
cd ~/Documents
mkdir JSC370-labs
cd JSC370-labs

# Step 2
wget https://raw.githubusercontent.com/JSC370/JSC370-2024/main/labs/lab05/lab05-wrangling-gam.Rmd
mv lab05-wrangling-gam.Rmd README.Rmd
# if wget is not available,
curl https://raw.githubusercontent.com/JSC370/JSC370-2024/main/labs/lab05/lab05-wrangling-gam.Rmd --output README.Rmd

# Step 3
# Happens on github

# Step 4
git init
git add README.Rmd
git commit -m "First commit"

# Step 5
git remote add origin git@github.com:[username]/JSC370-labs
git push -u origin master
```

You can also complete the steps in R (replace with your paths/username
when needed)

``` r
# Step 1
setwd("~/Documents")
dir.create("JSC370-labs")
setwd("JSC370-labs")

# Step 2
download.file(
  "https://raw.githubusercontent.com/JSC370/JSC370-2024/main/labs/lab05/lab05-wrangling-gam.Rmd",
  destfile = "README.Rmd"
  )

# Step 3: Happens on Github

# Step 4
system("git init && git add README.Rmd")
system('git commit -m "First commit"')

# Step 5
system("git remote add origin git@github.com:[username]/JSC370-labs")
system("git push -u origin master")
```

Once you are done setting up the project, you can now start working with
the MET data.

## Setup in R

1.  Load the `data.table` (and the `dtplyr` and `dplyr` packages),
    `mgcv`, `ggplot2`, `leaflet`, `kableExtra`.

``` r
library(data.table)
library(dtplyr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:data.table':
    ## 
    ##     between, first, last

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(mgcv)
```

    ## Loading required package: nlme

    ## 
    ## Attaching package: 'nlme'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     collapse

    ## This is mgcv 1.9-0. For overview type 'help("mgcv-package")'.

``` r
library(ggplot2)
library(leaflet)
library(kableExtra)
```

    ## 
    ## Attaching package: 'kableExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     group_rows

``` r
fn <- "https://raw.githubusercontent.com/JSC370/JSC370-2024/main/data/met_all_2023.gz"
if (!file.exists("met_all_2023.gz"))
  download.file(fn, destfile = "met_all_2023.gz")
met <- data.table::fread("met_all_2023.gz")
```

2.  Load the met data from
    <https://github.com/JSC370/JSC370-2024/main/data/met_all_2023.gz> or
    (Use
    <https://raw.githubusercontent.com/JSC370/JSC370-2024/main/data/met_all_2023.gz>
    to download programmatically), and also the station data. For the
    latter, you can use the code we used during lecture to pre-process
    the stations data:

``` r
# Download the data
stations <- fread("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv")
stations[, USAF := as.integer(USAF)]
```

    ## Warning in eval(jsub, SDenv, parent.frame()): NAs introduced by coercion

``` r
# Dealing with NAs and 999999
stations[, USAF   := fifelse(USAF == 999999, NA_integer_, USAF)]
stations[, CTRY   := fifelse(CTRY == "", NA_character_, CTRY)]
stations[, STATE  := fifelse(STATE == "", NA_character_, STATE)]

# Selecting the three relevant columns, and keeping unique records
stations <- unique(stations[, list(USAF, CTRY, STATE, LAT, LON)])

# Dropping NAs
stations <- stations[!is.na(USAF)]

# Removing duplicates
stations[, n := 1:.N, by = .(USAF)]
stations <- stations[n == 1,][, n := NULL]

# Read in the met data and fix lat, lon, temp
# Read in the met data and fix lat, lon, temp
met$lat <- met$lat/1000
met$lon <- met$lon/1000
met$wind.sp <- met$wind.sp/10
met$temp <- met$temp/10
met$dew.point <- met$dew.point/10
met$atm.press <- met$atm.press/10
```

3.  Merge the data as we did during the lecture. Use the `merge()` code
    and you can also try the tidy way with `left_join()`

``` r
met_dt <- merge(
  # Data
  x     = met,      
  y     = stations, 
  # List of variables to match
  by.x  = "USAFID",
  by.y  = "USAF", 
  # Which obs to keep?
  all.x = TRUE,      
  all.y = FALSE
)
```

## Question 1: Identifying Representative Stations

Across all weather stations, which stations have the median values of
temperature, wind speed, and atmospheric pressure? Using the
`quantile()` function, identify these three stations. Do they coincide?

``` r
med_temp <- quantile(met_dt$temp, 0.5, na.rm = TRUE)
med_ws <- quantile(met_dt$wind.sp, 0.5, na.rm = TRUE)
med_atp <- quantile(met_dt$atm.press, 0.5, na.rm = TRUE)

med_temp
```

    ##  50% 
    ## 21.7

``` r
med_atp
```

    ##    50% 
    ## 1011.7

``` r
med_ws
```

    ## 50% 
    ## 3.1

Next identify the stations have these median values.

``` r
med_met <- met_dt[, .(
  temp = median(temp, na.rm = TRUE),
  atm.press = median(atm.press, na.rm = TRUE),
  wind.sp = median(wind.sp, na.rm = TRUE)
), by = c("USAFID", "STATE")]


med_met[, temp_d:= abs(temp-med_temp)]
med_temp_stat <- med_met[temp_d==0]

med_met[, ws_d:= abs(wind.sp-med_ws)]
med_ws_stat <- med_met[ws_d==0]

med_met[, atp_d:= abs(atm.press-med_atp)]
med_atp_stat <- med_met[atp_d==0]

print("The stations that attain the median temperature are:")
```

    ## [1] "The stations that attain the median temperature are:"

``` r
unique(med_temp_stat)
```

    ##     USAFID STATE temp atm.press wind.sp temp_d
    ##  1: 720263    GA 21.7        NA    2.60      0
    ##  2: 720312    IA 21.7        NA    2.60      0
    ##  3: 720327    WI 21.7        NA    4.60      0
    ##  4: 722076    IL 21.7        NA    3.60      0
    ##  5: 722180    GA 21.7   1010.70    3.10      0
    ##  6: 722196    GA 21.7   1010.60    2.85      0
    ##  7: 722197    GA 21.7   1011.60    2.60      0
    ##  8: 723075    VA 21.7   1011.40    4.10      0
    ##  9: 723086    VA 21.7   1011.50    3.60      0
    ## 10: 723110    GA 21.7   1011.20    2.60      0
    ## 11: 723119    SC 21.7   1011.70    3.10      0
    ## 12: 723190    SC 21.7   1011.10    3.10      0
    ## 13: 723194    NC 21.7   1011.95    3.10      0
    ## 14: 723200    GA 21.7   1010.60    2.10      0
    ## 15: 723658    NM 21.7   1011.70    3.60      0
    ## 16: 723895    CA 21.7   1011.80    3.10      0
    ## 17: 724010    VA 21.7   1011.70    3.60      0
    ## 18: 724345    MO 21.7   1011.30    3.10      0
    ## 19: 724356    IN 21.7   1011.90    3.60      0
    ## 20: 724365    IN 21.7   1012.00    3.10      0
    ## 21: 724373    IN 21.7   1012.10    3.10      0
    ## 22: 724380    IN 21.7   1012.20    4.10      0
    ## 23: 724397    IL 21.7   1012.70    4.10      0
    ## 24: 724454    MO 21.7   1013.20    3.10      0
    ## 25: 724517    KS 21.7   1011.40    4.10      0
    ## 26: 724585    KS 21.7   1010.80    4.10      0
    ## 27: 724815    CA 21.7   1011.60    3.10      0
    ## 28: 724838    CA 21.7   1011.90    3.60      0
    ## 29: 725116    PA 21.7   1010.30    3.10      0
    ## 30: 725317    IL 21.7   1012.00    3.60      0
    ## 31: 725326    IL 21.7    999.70    3.10      0
    ## 32: 725340    IL 21.7   1012.20    4.10      0
    ## 33: 725450    IA 21.7   1012.35    3.60      0
    ## 34: 725472    IA 21.7   1012.00    3.60      0
    ## 35: 725473    IA 21.7   1013.40    3.60      0
    ## 36: 725480    IA 21.7   1012.20    3.10      0
    ## 37: 725499    IA 21.7   1012.00    2.60      0
    ## 38: 725513    NE 21.7        NA    3.10      0
    ## 39: 725720    UT 21.7   1009.60    3.60      0
    ## 40: 726515    SD 21.7   1013.10    4.10      0
    ## 41: 726525    SD 21.7   1012.80    3.60      0
    ## 42: 726546    SD 21.7   1011.30    3.85      0
    ## 43: 726556    MN 21.7   1012.15    3.60      0
    ## 44: 726560    SD 21.7   1010.35    4.10      0
    ## 45: 727570    ND 21.7   1011.45    3.60      0
    ## 46: 727845    WA 21.7   1012.40    3.10      0
    ## 47: 727900    WA 21.7   1012.00    3.60      0
    ## 48: 745046    CA 21.7   1011.80    3.60      0
    ## 49: 746410    OK 21.7   1010.10    4.10      0
    ## 50: 747808    GA 21.7   1012.60    2.60      0
    ##     USAFID STATE temp atm.press wind.sp temp_d

``` r
print("The stations that attain the median atm pressure are:")
```

    ## [1] "The stations that attain the median atm pressure are:"

``` r
unique(med_atp_stat)
```

    ##     USAFID STATE temp atm.press wind.sp temp_d ws_d atp_d
    ##  1: 720394    AR 23.9    1011.7     2.1    2.2  1.0     0
    ##  2: 722085    SC 24.4    1011.7     3.1    2.7  0.0     0
    ##  3: 722348    MS 24.4    1011.7     2.1    2.7  1.0     0
    ##  4: 723119    SC 21.7    1011.7     3.1    0.0  0.0     0
    ##  5: 723124    SC 21.1    1011.7     2.6    0.6  0.5     0
    ##  6: 723270    TN 23.9    1011.7     3.1    2.2  0.0     0
    ##  7: 723658    NM 21.7    1011.7     3.6    0.0  0.5     0
    ##  8: 724010    VA 21.7    1011.7     3.6    0.0  0.5     0
    ##  9: 724035    VA 22.2    1011.7     3.6    0.5  0.5     0
    ## 10: 724100    VA 19.4    1011.7     3.1    2.3  0.0     0
    ## 11: 724235    KY 21.1    1011.7     3.1    0.6  0.0     0
    ## 12: 724280    OH 20.6    1011.7     3.6    1.1  0.5     0
    ## 13: 724336    IL 23.3    1011.7     3.1    1.6  0.0     0
    ## 14: 724926    CA 20.6    1011.7     4.1    1.1  1.0     0
    ## 15: 725126    PA 18.3    1011.7     3.1    3.4  0.0     0
    ## 16: 725266    PA 16.1    1011.7     3.1    5.6  0.0     0
    ## 17: 725510    NE 23.9    1011.7     3.6    2.2  0.5     0
    ## 18: 725570    IA 22.2    1011.7     3.1    0.5  0.0     0
    ## 19: 725620    NE 20.6    1011.7     3.6    1.1  0.5     0
    ## 20: 725845    CA 14.4    1011.7     2.6    7.3  0.5     0
    ## 21: 726690    WY 13.3    1011.7     4.6    8.4  1.5     0
    ## 22: 726810    ID 20.0    1011.7     3.1    1.7  0.0     0
    ##     USAFID STATE temp atm.press wind.sp temp_d ws_d atp_d

``` r
print("The stations that attain the median wind speed are:")
```

    ## [1] "The stations that attain the median wind speed are:"

``` r
unique(med_ws_stat)
```

    ##      USAFID STATE temp atm.press wind.sp temp_d ws_d
    ##   1: 720110    TX 28.0        NA     3.1    6.3    0
    ##   2: 720113    MI 20.0        NA     3.1    1.7    0
    ##   3: 720258    MN 18.0        NA     3.1    3.7    0
    ##   4: 720261    TX 27.7        NA     3.1    6.0    0
    ##   5: 720266    IN 18.5        NA     3.1    3.2    0
    ##  ---                                                
    ## 573: 747680    MS 25.0    1011.4     3.1    3.3    0
    ## 574: 747760    FL 23.9    1011.4     3.1    2.2    0
    ## 575: 747809    GA 22.0        NA     3.1    0.3    0
    ## 576: 747900    SC 22.8    1011.1     3.1    1.1    0
    ## 577: 747918    SC 23.0        NA     3.1    1.3    0

``` r
all_measures <- med_met[temp_d== 0 & ws_d==0 & atp_d==0]
print("The stations that coincide at all three are:")
```

    ## [1] "The stations that coincide at all three are:"

``` r
unique(all_measures)
```

    ##    USAFID STATE temp atm.press wind.sp temp_d ws_d atp_d
    ## 1: 723119    SC 21.7    1011.7     3.1      0    0     0

As as can see above, the values all coincide for the station with
USAFID: 723119.

Knit the document, commit your changes, and save it on GitHub. Don’t
forget to add `README.md` to the tree, the first time you render it.

## Question 2: Identifying Representative Stations per State

Now let’s find the weather stations by state with closest temperature
and wind speed based on the euclidean distance from these medians.

``` r
state_weather <- met_dt[, .(
  state_temp = median(temp, na.rm = TRUE),
  state_wind.sp = median(wind.sp, na.rm = TRUE)
), by = c("STATE")]

euclidean <- merge(state_weather, met_dt, by = "STATE")
euclidean[, distance:= sqrt((temp-state_temp)^2 + (wind.sp-state_wind.sp)^2)]
euclidean <- euclidean[!is.na(distance)]
state_reps <- euclidean[, .(min_distance = min(distance), USAFID = USAFID[which.min(distance)]), by = c("STATE")]
state_reps[, .(STATE, USAFID)]
```

    ##     STATE USAFID
    ##  1:    AL 720265
    ##  2:    AR 720377
    ##  3:    AZ 722720
    ##  4:    CA 720267
    ##  5:    CO 720531
    ##  6:    CT 725027
    ##  7:    DE 724088
    ##  8:    FL 720383
    ##  9:    GA 720257
    ## 10:    IA 720293
    ## 11:    ID 720322
    ## 12:    IL 720137
    ## 13:    IN 720266
    ## 14:    KS 720422
    ## 15:    KY 720447
    ## 16:    LA 720467
    ## 17:    MA 725059
    ## 18:    MD 720384
    ## 19:    ME 726060
    ## 20:    MI 720198
    ## 21:    MN 720258
    ## 22:    MO 720869
    ## 23:    MS 720708
    ## 24:    MT 726676
    ## 25:    NC 720274
    ## 26:    ND 720491
    ## 27:    NE 720405
    ## 28:    NH 726050
    ## 29:    NJ 720581
    ## 30:    NM 722695
    ## 31:    NV 720549
    ## 32:    NY 722098
    ## 33:    OH 720397
    ## 34:    OK 720342
    ## 35:    OR 720202
    ## 36:    PA 720304
    ## 37:    RI 722151
    ## 38:    SC 720120
    ## 39:    SD 726510
    ## 40:    TN 720974
    ## 41:    TX 720261
    ## 42:    UT 720567
    ## 43:    VA 720501
    ## 44:    VT 720493
    ## 45:    WA 720254
    ## 46:    WI 720343
    ## 47:    WV 724120
    ## 48:    WY 720345
    ##     STATE USAFID

Knit the doc and save it on GitHub.

## Question 3: In the Geographic Center?

For each state, identify which station is closest to the geographic
mid-point (median) of the state. Combining these with the stations you
identified in the previous question, use `leaflet()` to visualize all
~100 points in the same figure, applying different colors for the
geographic median and the temperature and wind speed median.

``` r
med_stat <- met_dt[, .(
  mlon = median(lon, na.rm = TRUE),
  mlat = median(lat, na.rm = TRUE)
), by = c("STATE")]

med_stat.info <- merge(met_dt[, c("USAFID", "STATE", "lat", "lon")], med_stat, by.x = "STATE", by.y = "STATE")
med_stat.info[, mid_dist:=sqrt((lon-mlon)^2+(lat-mlat)^2)]
med_stat <- med_stat.info[ , .SD[which.min(mid_dist)], by = c("STATE")]

met_dt.filtered <- met_dt[met_dt$USAFID %in% med_stat$USAFID | met_dt$USAFID %in% state_reps$USAFID]
met_dt.filtered$state_rep <- ifelse(met_dt.filtered$USAFID %in% state_reps$USAFID, 1, 0)
met_dt.filtered$median_stat <- ifelse(met_dt.filtered$USAFID %in% med_stat$USAFID, 1, 0)

met_dt.filtered <- met_dt.filtered[, .(
  lon = mean(lon, na.rm = TRUE),
  lat = mean(lat, na.rm = TRUE),
  state_rep = first(state_rep),
  median_stat = first(median_stat)
), by = c("USAFID")]

leaflet() %>%
  addTiles() %>%
  addCircles(data = met_dt.filtered, 
                   lat = ~lat, 
                   lng = ~lon, 
                   radius = 5, 
                   color = ~ifelse(state_rep == 1, "red", 
                                   ifelse(median_stat == 1, "blue", "green")))
```

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
#install.packages("webshot")
webshot::install_phantomjs()
```

    ## It seems that the version of `phantomjs` installed is greater than or equal to the requested version.To install the requested version or downgrade to another version, use `force = TRUE`.

``` r
#install.packages("webshot2")
```

Knit the doc and save it on GitHub.

## Question 4: Summary Table with `kableExtra`

Generate a summary table using `kable` where the rows are each state and
the columns represent average temperature broken down by low, median,
and high elevation stations.

Use the following breakdown for elevation:

- Low: elev \< 93
- Mid: elev \>= 93 and elev \< 401
- High: elev \>= 401

Knit the document, commit your changes, and push them to GitHub.

## Question 5: Advanced Regression

Let’s practice running regression models with smooth functions on X. We
need the `mgcv` package and `gam()` function to do this.

- using your data with the median values per station, first create a
  lazy table. Filter out values of atmospheric pressure outside of the
  range 1000 to 1020. Examine the association between temperature (y)
  and atmospheric pressure (x). Create a scatterplot of the two
  variables using ggplot2. Add both a linear regression line and a
  smooth line.

- fit both a linear model and a spline model (use `gam()` with a cubic
  regression spline on wind speed). Summarize and plot the results from
  the models and interpret which model is the best fit and why.

## Deliverables

- .Rmd file (this file)

- link to the .md file (with all outputs) in your GitHub repository

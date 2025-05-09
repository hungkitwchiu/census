# census

Some of this code is embeded in another project, see [Census data](https://hungkitwchiu.github.io/doc.Census.html) for a discussion of some of the functions, including a complete, fully worked example of crosswalking 2020s block group variables to 2010s tracts. To call, <br />

```r
source("https://raw.githubusercontent.com/hungkitwchiu.github.io/r-data-wrangling/main/functions.R")
```
To use the census related functions, make sure you load `tidycensus` and `tidyverse` and make sure that you have your census API key stored the environment otherwise you will be limited to 500 calls per day. API key can be obtained for free from Census.

# Functions

`get.census()` wrapper for `get_acs()` or `get_decennial()` to get multiple years at once

-   `state` : chr such as "CA" or "06" (NOTE: a numeric 6 will not work)

-   `county` : chr such as "San Francisco" or "075"

-   `geography` : chr such as "tract", "block group" or "county subdivision"

-   `years` : numeric or vector of integers

-   `variables` : chr variables as in load_variables(year, survey)

-   `geometry` = FALSE : get shape data if TRUE

-   `survey` = "acs5" : default survey variance set to "acs5"

-   `acs` = TRUE : default call get_acs, if FALSE, call get_decennial

-   `years.id` = "year" : chr column name assigned to year column

`function` `get.census.list()` wrapper for `get.census()` get multiple state-county at once

-   `state.county.list`: nested list of states and counties in the form of `list(list(state1, list(county1, county2)), list(state2, list(county1)))` and so on

`function` `census.crosswalk()` crosswalk data using weights from [NHGIS](https://www.nhgis.org/geographic-crosswalks)

-   `crosswalk.file` : data.frame or data.table of the correct crosswalk file from NHGIS

-   `col.start` : chr column name of the originating GEOID

-   `col.end` : chr column name of the target GEOID

-   `col.weight` : chr column name of the interpolation weight

-   `data.walk` : data.frame or data.table of the census data that needs to be crosswalked

-   `cols.walk` : chr column name of the variable to be crosswalked; can be a vector

-   `col.year` = NULL : chr column name of the year column, if any; note that only data from the same decade can be fed into the function

`function` `get.geometry()` function for getting Census geographies for a given data set

*WARNING*: This function treats `(0,0)` coordinates as `NA`

-  `data.interest` : data with rows of GPS location that requires mapping to Census geographies, assumed to be `ESPG:4326` (the standard GPS longitude and latitude)

-  `coords.name` : character vector of 2 elements, with first one being column name of longitude (X), and second one being column name of latitude (Y)

-  `data.shape` : relevant Census data with geography information (TIGER lines)

-  `parallel` : if `TRUE`, run code in parallel using multiple cores

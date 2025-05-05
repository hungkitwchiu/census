**AS OF APR 3, 2025, `geometry` DATA IS DISABLED FROM CENSUS' END, NO MORE `geometry = TRUE`**

Load `tidycensus` and `tidyverse` and make sure that you have your census API key loaded into the environment.

`function` `get.census()` wrapper for `get_acs()` or `get_decennial()`

-   `state` : chr such as "CA" or "06" (NOTE: a numeric 6 will not work)

-   `county` : chr such as "San Francisco" or "075"

-   `geography` : chr such as "tract", "block group" or "county subdivision"

-   `years` : numeric or vector of integers

-   `variables` : chr variables as in load_variables(year, survey)

-   `geometry` = FALSE : get shape data if TRUE

-   `survey` = "acs5" : default survey variance set to "acs5"

-   `acs` = TRUE : default call get_acs, if FALSE, call get_decennial

-   `years.id` = "year" : chr column name assigned to year column

`function` `census.crosswalk()` crosswalk data using weights from [NHGIS](https://www.nhgis.org/geographic-crosswalks)

-   `data.crosswalk` : data.frame or data.table of the correct crosswalk data from NHGIS

-   `col.start` : chr column name of the originating GEOID

-   `col.target` : chr column name of the target GEOID

-   `col.weight` : chr column name of the interpolation weight

-   `data.var` : data.frame or data.table of the census data

-   `col.estimate` : chr column name of the variable to be crosswalked

-   `col.year` = NULL : chr column name of the year column, if any; note that only data from the same decade can be fed into the function

`function` `get.geometry()` function for getting Census geographies for a given data set

*WARNING*: This function treats `(0,0)` coordinates as `NA`

-  `data.interest` : data with rows of GPS location that requires mapping to Census geographies, assumed to be `ESPG:4326` (the standard GPS longitude and latitude)

-  `coords.name` : character vector of 2 elements, with first one being column name of longitude (X), and second one being column name of latitude (Y)

-  `data.shape` : relevant Census data with geography information (TIGER lines)

-  `parallel` : if `TRUE`, run code in parallel using multiple cores

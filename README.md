Load `tidycensus` and `tidyverse` and make sure that you have your census API key loaded into the environment.

`function` `get.census()` wrapper for `get_acs()` or `get_decennial()`

-   `state` : chr such as "CA" or "06"

-   `county` : chr such as "San Francisco" or "075"

-   `geography` : chr such as "tract", "block group" or "county subdivision"

-   `years` : numeric or vector of integers

-   `variables` : chr variables as in load_variables(year, survey)

-   `geometry` = TRUE : get shape data

-   `survey` = "acs5" : default survey variance set to "acs5"

-   `acs` = TRUE : default call get_acs, if FALSE, call get_decennial

`function` `census.crosswalk()` crosswalk data using weights from [NHGIS](https://www.nhgis.org/geographic-crosswalks)

-   `data.crosswalk` : data.frame or data.table of the correct crosswalk data from NHGIS

-   `col.start` : chr column name of the originating GEOID

-   `col.target` : chr column name of the target GEOID

-   `col.weight` : chr column name of the interpolation weight

-   `data.var` : data.frame or data.table of the census data

-   `col.estimate` : chr column name of the variable to be crosswalked

-   `col.year` = NULL : chr column name of the year column, if any; note that only data from the same decade can be fed into the function

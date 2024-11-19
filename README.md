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

-   

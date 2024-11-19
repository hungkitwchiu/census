acs.vars <- load_variables(2010, "acs5")
test<- load_variables(2005, "acs1")
temp<-acs5_geography

get.census <- function(state, county, geography, years, variables, geometry = TRUE, survey = "acs5", acs = TRUE){
  if (acs){
    temp <- map_dfr(
      years,
      ~ get_acs(
        geography = geography, # "tract" or "block group" or "county subdivision"
        variables = variables, # B01003_001 for population
        state = state, # all of CA: c("Alameda", "Contra Costa", "Marin", "San Francisco", "San Mateo", "Santa Clara", "Solano")
        county = county,
        year = .x,
        survey = "acs5",
        geometry = geometry
      ),
      .id = "year"  # when combining results, add id var (name of list item)
    ) %>%
      arrange(variable, GEOID) %>% 
      mutate(year = as.numeric(year) + min(years)-1)
  } else { # 2000, 2010, and 2020 only
    temp <- get_decennial(
      geography = geography,
      variables = variables, # P001001 for population
      state = state,
      county = county,
      year = year,
      geometry = geometry
    )
  }
  return(temp)
}

acs5.2010 <- get_census("CA", "San Francisco", "tract", 2010, "B01003_001") 
# ==================================================== Population
# 2009 is when the earliest "acs5" becomes available (tract level estimates)
# 2005 is when the earliest "acs1" becomes available (limited to geographies with populations of 65k or greater)
# keep everything in 2010 acs5 tracts, thus no adjustment needed for 2010-2019, need to crosswalk 2009, 2020-2022 (2023 not available as of Nov 16, 2024)
# get large area data in 2005-2008 from acs1, adjust to acs5 number, then impute using 2010 acs5 share

geography = "block group"
variables = "B01003_001"
state = "CA"
county = "San Francisco"
years = 2020

# call, from acs5, population by census tracts from 2009 to 2022
acs5.test <- get.census(state, county, "tract", 2010, variables) %>%
  filter(GEOID != "06075980401")


tract00.to.tract10 <- fread("nhgis_tr2000_tr2010/nhgis_tr2000_tr2010.csv",
                               keepLeadingZeros = getOption("datatable.keepLeadingZeros", TRUE))

block20.to.tract10 <- fread("nhgis_bg2020_tr2010/nhgis_bg2020_tr2010.csv",
                            keepLeadingZeros = getOption("datatable.keepLeadingZeros", TRUE))

data.crosswalk <- block20.to.tract10


# currently supports single variable
census.crosswalk <- function(data.crosswalk, col.start, col.target, col.weight, data.var, col.estimate, col.year = NULL){
  data.crosswalk <- as.data.table(data.crosswalk)
  data.var <- as.data.table(data.var)
  # check data integrity
  if (!("GEOID" %in% colnames(data.var))){stop("GEOID not found in data.var")}
  if (!all(unique(data.var$GEOID) %in% data.crosswalk[, get(col.start)])){
    stop("GEOID does not exist in data.crosswalk, check if you have the correct crosswalk for your geography")}
  if (!any(unique(data.var$GEOID) %in% data.crosswalk[, get(col.target)])){
    print("Note that none of the GEOID is in col.target (this is expected for crosswalks between different levels of geography)")}
  if (!is.null(col.year)){
    years.mod = sort(unique(data.var[, get(col.year)])) %% 10
    if (!all(sort(years.mod) == years.mod)){print("Warning: Years are not from the same decade")}
  }
  
  cols.group <- c(eval(col.year), eval(col.target))

  temp <- data.crosswalk %>% 
    filter(!!rlang::sym(col.start) %in% data.var$GEOID) %>%
    select(!!rlang::sym(col.start), !!rlang::sym(col.target), !!rlang::sym(col.weight)) %>%
    right_join(data.var, by = join_by(!!rlang::sym(col.start) == GEOID), keep = TRUE, relationship = "many-to-many") %>% # look up estimates of corresponding col.start
    mutate(target.estimate = get(col.estimate) * get(col.weight)) %>%
    group_by(across(any_of(cols.group))) %>%
    dplyr::summarise(estimate = as.integer(sum(target.estimate))) %>%
    rename_with(~ "GEOID", !!rlang::sym(col.target))
  
  #temp <- data.var %>%
  #  left_join(data.crosswalk, by = join_by(GEOID == !!rlang::sym(col.start)), keep = TRUE) %>% # look up estimates of corresponding col.start
  #  mutate(target.estimate = get(col.estimate) * get(col.weight))
  
  return(temp)
}

data.crosswalk <- block20.to.tract10
col.start = "bg2020ge"
col.target = "tr2010ge"
col.weight = "wt_pop"
data.var = acs5.2020

test <- census.crosswalk(tract00.to.tract10, "tr2000ge", "tr2010ge", "wt_pop", acs5.2009, "estimate", "year")
test <- census.crosswalk(block20.to.tract10, "bg2020ge", "tr2010ge", "wt_pop", acs5.2020, "estimate", "year")

sum((test %>% filter(year == 2020))$estimate)
sum(acs5.2020.single$estimate)

mapview(acs5.2009, zcol = "estimate") # 2009 data, 2009 tract
sum(acs5.2009$estimate)
mapview.with.shape.data(test %>% filter(year == 2020), acs5.2010, "estimate", "GEOID") # 2009 data, 2010 tract
sum(test$estimate)
mapview(acs5.2010, zcol = "estimate") # 2010 data, 2010 tract


acs5.2010 <- acs5.2010 %>%
  filter(GEOID != "06075980401")

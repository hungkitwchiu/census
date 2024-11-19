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

# testing get.census
geography = "block group"
variables = "B01003_001"
state = "CA"
county = "San Francisco"
years = 2020

# call, from acs5, population by census tracts in 2020
acs5.test <- get.census(state, county, geography, years, variables) %>%
  filter(GEOID != "06075980401")


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
  
  return(temp)
}

# test
# test <- census.crosswalk(tract00.to.tract10, "tr2000ge", "tr2010ge", "wt_pop", acs5.2009, "estimate", "year")
test <- census.crosswalk(block20.to.tract10, "bg2020ge", "tr2010ge", "wt_pop", acs5.2020, "estimate", "year")

sum((test %>% filter(year == 2020))$estimate)
sum(acs5.2020.single$estimate)

mapview(acs5.2009, zcol = "estimate") # 2009 data, 2009 tract
sum(acs5.2009$estimate)
mapview.with.shape.data(test %>% filter(year == 2020), acs5.2010, "estimate", "GEOID") # 2009 data, 2010 tract
sum(test$estimate)
mapview(acs5.2010, zcol = "estimate") # 2010 data, 2010 tract

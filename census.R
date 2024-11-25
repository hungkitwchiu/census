library(tidycensus)
library(tidyverse)

# check that you have your census API key loaded
# census_api_key("YOUR API KEY GOES HERE")

get.census <- function(state.county, geography, years, variables, geometry = FALSE, survey = "acs5", acs = TRUE, years.id = "year"){
  if (acs){
    temp <- map_dfr(
      years,
      ~ get_acs(
        geography = geography, # "tract" or "block group" or "county subdivision"
        variables = variables, # B01003_001 for population
        state = state.county[[1]], # all of CA: c("Alameda", "Contra Costa", "Marin", "San Francisco", "San Mateo", "Santa Clara", "Solano")
        county = state.county[[2]],
        year = .x,
        survey = "acs5",
        geometry = geometry
      ),
      .id = eval(years.id)  # when combining results, add id var (name of list item)
    ) %>%
      arrange(variable, GEOID) %>% 
      mutate(year = as.numeric(year) + min(years)-1)
  } else { # 2000, 2010, and 2020 only
    temp <- get_decennial(
      geography = geography,
      variables = variables, # P001001 for population
      state = state,
      county = county,
      year = years,
      geometry = geometry
    )
  }
  return(temp)
}

# testing get.census
# geography = "block group"
# variables = "B01003_001"
# state = "CA"
# county = "San Francisco"
# years = 2020

# call, from acs5, population of San Francisco, by block group in 2020
#acs5.test <- get.census(state, county, geography, years, variables) %>%
#  filter(GEOID != "06075980401")

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

# test
# test <- census.crosswalk(tract00.to.tract10, "tr2000ge", "tr2010ge", "wt_pop", acs5.2009, "estimate", "year")
# test <- census.crosswalk(block20.to.tract10, "bg2020ge", "tr2010ge", "wt_pop", acs5.2020, "estimate", "year")

# sum((test %>% filter(year == 2020))$estimate)
# sum(acs5.2020.single$estimate)

# mapview(acs5.2009, zcol = "estimate") # 2009 data, 2009 tract
# sum(acs5.2009$estimate)
# mapview.with.shape.data(test %>% filter(year == 2020), acs5.2010, "estimate", "GEOID") # 2009 data, 2010 tract
# sum(test$estimate)
# mapview(acs5.2010, zcol = "estimate") # 2010 data, 2010 tract


get.geometry <- function(data.interest, coords.name, data.shape, parallel = FALSE){
  #geo.within <- function(x){return(sf::st_within(x, data.shape))}
  #cl <- makeCluster(getOption("cl.cores", detectCores(logical = FALSE)-2))
  #clusterExport(cl, varlist = list("data.shape", "geo.within"), envir = environment())
  
  data.interest <- data.interest %>%
    filter(!!rlang::sym(coords.name[1]) != "") %>% # omit if longitude is empty
    filter(!is.na(!!rlang::sym(coords.name[1]))) # omit if longitude is NA
  
  data.interest$Geometry <- st_as_sf(
    as.data.frame(data.interest %>% select(all_of(coords.name))),
    coords = coords.name,
    crs = st_crs(data.shape)
  )
  
  if (parallel){data.interest$block <- parallel::parLapplyLB(cl, list(data.interest$Geometry), geo.within)
  }else{ data.interest <- data.interest %>% mutate(block = st_within(Geometry, data.shape))}

  in.none = sum(data.interest$block %>% lengths == 0)
  in.multiple = sum(data.interest$block %>% lengths > 1)
  
  data.interest <- data.interest %>%
    filter(block %>% lengths > 0) %>% # get rid of (empty) in Sparse geometry binary predicate (sgbp) list
    filter(block %>% lengths < 2) %>% # also get rid of points within multiple shapes
    mutate(GEOID = data.shape$GEOID[as.numeric(unlist(block))])

  if (in.none > 0){print(paste("Removed ", in.none, " rows with unmatched geometry"))}
  if (in.multiple > 0){print(paste("Removed ", in.multiple, " rows with multiple matched blocks"))}

  return(data.interest)
}

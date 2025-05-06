library(tidycensus)
library(tidyverse)
library(parallel)

# check that you have your census API key loaded
# census_api_key("YOUR API KEY GOES HERE")

get.census <- function(state.county, geography, years, variables, geometry = FALSE, survey = "acs5", 
                       acs = TRUE, years.id = "year"){
  if (acs){
    temp <- map_dfr(
      years,
      ~ get_acs(
        # "tract" or "block group" or "county subdivision", etc.
        geography = geography,
        # "B01003_001" for population
        variables = variables,
        state = state.county[[1]],
        county = state.county[[2]],
        year = .x,
        survey = "acs5",
        geometry = geometry,
        cache_table = TRUE
      ),
      .id = eval(years.id)  # when combining results, add id var (name of list item)
    ) %>%
      arrange(variable, GEOID) %>% 
      mutate(year = as.numeric(year) + min(years)-1)
  } else { # 2000, 2010, or 2020 only for decennial
    temp <- get_decennial(
      geography = geography,
      variables = variables, # P001001 for population
      state = state.county[[1]],
      county = state.county[[2]],
      year = years,
      geometry = geometry
    )
  }
  return(temp)
}

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
    dplyr::select(!!rlang::sym(col.start), !!rlang::sym(col.target), !!rlang::sym(col.weight)) %>%
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

get.geometry <- function(data.interest, coords.name, data.shape, parallel = TRUE, crs = "EPSG:4326"){
  nrow.c <- nrow(data.interest)
  data.interest <- data.interest %>%
    filter(!(!!rlang::sym(coords.name[1]) %in% c("","NA"))) %>% # omit if latitude is empty or "NA"
    filter(!is.na(!!rlang::sym(coords.name[1]))) %>% # omit if latitude is NA
    filter(!is.na(!!rlang::sym(coords.name[2]))) %>% # omit if longitude is NA
    filter(!(abs(!!rlang::sym(coords.name[1])) < 1e-6)) %>%
    filter(!(abs(!!rlang::sym(coords.name[2])) < 1e-6))
  nrow.d <- nrow(data.interest)

  if (nrow.c != nrow.d){cat("Dropped", nrow.c - nrow.d, "rows out of", nrow.c, "due to unknown coordinates \n")}
  
  data.interest$Geometry <- st_as_sf(
    as.data.frame(data.interest %>% dplyr::select(all_of(coords.name))),
    coords = coords.name,
    crs = st_crs(crs) # assuming coords come in WGS84, specifically EPSG:4326
  ) %>%
    st_transform(crs = st_crs(data.shape)) # convert to crs of shape file
  
  if (parallel){
    cl <- makeCluster(detectCores(logical = FALSE)-1, type = "PSOCK")
    clusterExport(cl, varlist = c("data.shape"), envir = environment())
    # make sure a list is passed in 2nd argument, can be data.interest[, "Geometry"]
    # but not data.interest[, `Geometry`] or data.interest[, Geometry]
    data.interest$block <- parLapplyLB(cl, list(data.interest$Geometry), st_within, data.shape)
    stopCluster(cl)
    gc()
  }else{data.interest <- data.interest %>% mutate(block = st_within(Geometry, data.shape))}

  in.none = sum(data.interest$block %>% lengths == 0)
  in.multiple = sum(data.interest$block %>% lengths > 1)
  
  data.interest <- data.interest %>%
    filter(block %>% lengths > 0) %>% # get rid of (empty) in Sparse geometry binary predicate (sgbp) list
    filter(block %>% lengths < 2) %>% # also get rid of points within multiple shapes
    mutate(GEOID = data.shape$GEOID[as.numeric(unlist(block))]) # make sure GEOID is in data.shape
  
  if (in.none > 0){cat("Removed", in.none, "rows with unmatched geometry", "\n")}
  if (in.multiple > 0){cat("Removed", in.multiple, "rows with multiple matched blocks", "\n")}
  
  return(data.interest)
}

library(tidycensus)
library(tidyverse)
library(parallel)
library(furrr)

# check that you have your census API key loaded
# census_api_key("YOUR API KEY GOES HERE")

get.census <- function(state.county,
                       geography,
                       years,
                       variables,
                       geometry = FALSE,
                       survey = "acs5",
                       acs = TRUE,
                       years.id = "year") {
  variables <- unname(variables)
  if (acs) {
    temp <- future_map_dfr(
      years,
      ~ get_acs(
        geography = geography,
        variables = variables,
        state = state.county[[1]],
        county = state.county[[2]],
        year = .x,
        survey = "acs5",
        geometry = geometry,
        cache_table = TRUE
      ),
      .id = eval(years.id) # add id var (name of list item) when combining
    ) %>%
      arrange(variable, GEOID) %>%
      mutate(year = as.numeric(year) + min(years) - 1)
  } else {
    # 2000, 2010, or 2020 only
    temp <- get_decennial(
      geography = geography,
      variables = variables,
      state = state.county[[1]],
      county = state.county[[2]],
      year = years,
      geometry = geometry
    )
  }
  return(temp)
}

get.census.list <- function(s.c.list,
                            geography,
                            years,
                            variables,
                            geometry = FALSE) {
  variables <- unname(variables)
  plan(multisession, workers = parallelly::availableCores())
  tic()
  data.list <- lapply(s.c.list, function(x) {
    print(x)
    temp <- get.census(x, geography, years, variables, geometry = geometry) %>%
      dplyr::select(year, GEOID, variable, estimate)
    temp <- dcast(setDT(temp), year + GEOID ~ variable, value.var = "estimate")
  })
  toc()
  return(rbindlist(data.list))
}

census.crosswalk <- function(crosswalk.file,
                             col.start,
                             col.end,
                             col.weight,
                             data.walk,
                             cols.walk,
                             col.year = NULL) {
  crosswalk.file <- as.data.table(crosswalk.file)
  data.walk <- as.data.table(data.walk)
  cols.walk <- unname(cols.walk) # just in case a named vector is passed
  
  # check data integrity
  if (any(!cols.walk %in% names(data.walk))) {
    stop("Missing crosswalk columns!")}
  if (!("GEOID" %in% colnames(data.walk))) {
    stop("GEOID not found in data.walk")}
  if (!all(unique(data.walk$GEOID) %in% crosswalk.file[, get(col.start)])) {
    stop("GEOID does not exist in crosswalk.file, check if crosswalk file is correct")}
  if (!any(unique(data.walk$GEOID) %in% crosswalk.file[, get(col.end)])) {
    cat("Note: Zero GEOID in col.end (expected for crosswalks across levels) \n")}
  if (!is.null(col.year)) {
    years.mod = sort(unique(data.walk[, get(col.year)])) %% 10
    if (!all(sort(years.mod) == years.mod)) {
      cat("Warning: Years not from same decade \n")}}
  
  # get desired group_by keys; year could be NULL
  cols.group <- c(eval(col.year), eval(col.end))
  
  temp <- crosswalk.file %>%
    # filter only relevant GEOID
    filter(!!rlang::sym(col.start) %in% data.walk$GEOID) %>%
    # select starting GEOID, ending GEOID, and weight columns
    dplyr::select(!!rlang::sym(col.start),
                  !!rlang::sym(col.end),
                  !!rlang::sym(col.weight)) %>%
    # right join data.walk and look up estimates of corresponding col.start
    right_join(
      data.walk,
      by = join_by(!!rlang::sym(col.start) == GEOID),
      keep = TRUE,
      relationship = "many-to-many"
    ) %>%
    # mutate/crosswalked variables; basically calculating a weighted average
    mutate_at(cols.walk, ~ .x * get(col.weight)) %>%
    # aggregate back to a single row per GEOID per year
    group_by(across(any_of(cols.group))) %>%
    dplyr::summarise_at(cols.walk, ~ round(sum(.x, na.rm = TRUE))) %>%
    # rename GEOID column to "GEOID"
    rename_with( ~ "GEOID", !!rlang::sym(col.end))
  
  return(temp)
}

get.geometry <- function(data.interest, coords.name, data.shape, parallel = TRUE, crs = "EPSG:4326") {
  nrow.c <- nrow(data.interest)
  data.interest <- data.interest %>%
    filter(!(!!rlang::sym(coords.name[1]) %in% c("", "NA"))) %>%
    # don't refactor this using any(), you tried; omit if latitude or longitude is NA
    filter(!is.na(!!rlang::sym(coords.name[1]))) %>%
    filter(!is.na(!!rlang::sym(coords.name[2]))) %>%
    # omit if latitude or longitude is (floating point) zero
    filter(!abs(!!rlang::sym(coords.name[1])) < 1e-6) %>%
    filter(!abs(!!rlang::sym(coords.name[2])) < 1e-6)
  nrow.d <- nrow(data.interest)
  
  if (nrow.d < nrow.c) {cat("Dropped", nrow.c - nrow.d, "rows out of", nrow.c, "with no coordinates \n")}
  
  # get unique subset for mapping
  data.interest <- data.interest %>%
    mutate(tempID = as.factor(paste(!!rlang::sym(coords.name[1]), !!rlang::sym(coords.name[2]))))
  unique.coords <- data.interest %>%
    dplyr::select(tempID, !!rlang::sym(coords.name[1]), !!rlang::sym(coords.name[2])) %>%
    unique(by = "tempID")
  
  unique.coords$Geometry <- st_as_sf(
    as.data.frame(unique.coords %>% dplyr::select(all_of(coords.name))),
    coords = coords.name, crs = st_crs(crs)
  ) %>% # assuming coords come in WGS84, i.e., EPSG:4326
    st_transform(crs = st_crs(data.shape)) # convert to crs of shape file
  
  if (parallel) {
    cl <- makeCluster(detectCores(logical = FALSE) - 1, type = "PSOCK")
    clusterExport(cl, varlist = c("data.shape"), envir = environment())
    # make sure a list is passed in 2nd argument, can be unique.coords[, "Geometry"]
    # but not unique.coords[, `Geometry`] or unique.coords[, Geometry]
    unique.coords$block <- parLapplyLB(cl, list(unique.coords$Geometry), st_within, data.shape)
    stopCluster(cl)
    # gc() # un-comment garbage collection gc() if you are tight on ram
  } else { unique.coords <- unique.coords %>% mutate(block = st_within(Geometry, data.shape)) }
  
  # do filtering here, before joining
  in.none = sum(unique.coords$block %>% lengths == 0)
  in.multiple = sum(unique.coords$block %>% lengths > 1)
  if (in.none > 0) { cat("Removed", in.none, "coordinates with unmatched geometry", "\n") }
  if (in.multiple > 0) { cat("Removed", in.multiple, "coordinates with multiple matched blocks", "\n") }
  
  data.interest <- data.interest %>%
    left_join(unique.coords[, .SD, .SDcols = !c(coords.name)], by = "tempID") %>%
    # call outs are for *unique coordinates*, number of rows removed may be higher
    filter(block %>% lengths > 0) %>% # remove (empty) in Sparse geometry binary predicate (sgbp) list
    filter(block %>% lengths < 2) %>% # also remove points within multiple shapes
    mutate(GEOID = data.shape$GEOID[as.numeric(unlist(block))]) %>%
    dplyr::select(-tempID)
  
  return(data.interest)
}

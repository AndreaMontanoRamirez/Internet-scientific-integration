# =========================
# packages and functions.R  (single file)
# How to use:
#   source("utilities.R")        # once per session (or add to .Rprofile)
#   # now call: read_clean_csv(), scale01(), theme_nolegend(), etc.
# =========================
# R/packages.R
required_pkgs <- c(
  "sf",
  "tigris",
  "bibliometrix",
  "brainGraph",
  "broom",
  "car",
  "ggprism",
  "forcats",
  "scales",
  "data.table",
  "dplyr",
  "forecast",
  "ggplot2",
  "hrbrthemes",
  "igraph",
  "janitor",
  "lubridate",
  "netUtils",
  "officer",
  "openxlsx",
  "plotly",
  "purrr",
  "patchwork",
  "qs",
  "readr",
  "readxl",
  "stringr",
  "tibble",
  "tidyr",
  "tseries",
  "sandwich",
  "tidygeocoder",
  "tidyverse",
  "viridis",
  "geodist",
  "lmtest",
  "ggfortify"
)

to_install <- setdiff(required_pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, dependencies = TRUE)

invisible(lapply(required_pkgs, require, character.only = TRUE))

# Optional but handy: set global options
options(stringsAsFactors = FALSE, scipen = 999)


#------------------------
# Functions
#------------------------
#---------------
# General
#---------------
limpiar <- function(x){
  return(sapply(chartr('ÁÉÍÓÚÜÑáéíóúüñ','AEIOUUNaeiouun',x),
                FUN=function(x) trimws(toupper(x),which='both')))
}
clean_na=function(data){
  data1=na.omit(data)
  return(data=data1)
} #Remove rows with NaN (math error)
#------------------------
# Script 1: Combine WoS data from 142 folders
#------------------------
# 1. Read folders with .txt files
# Null-coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x

# Split folder name into 3 parts: State, Code, University
# If there are more than 3 tokens, 1=State, 2=Code, the rest = University (joined with "_")
split_folder_meta <- function(folder_path) {
  fname <- basename(folder_path)
  parts <- unlist(strsplit(fname, "_", fixed = TRUE))
  if (length(parts) >= 3) {
    state      <- parts[1]
    state_code <- parts[2]
    university <- paste(parts[3:length(parts)], collapse = "_")
  } else {
    state      <- parts[1] %||% NA_character_
    state_code <- parts[2] %||% NA_character_
    university <- if (length(parts) >= 3) parts[3] else NA_character_
  }
  tibble(
    folder_name = fname,
    State       = state,
    StateCode   = state_code,
    University  = gsub("_", " ", university) # cosmetic: turn "_" into spaces
  )
}

# Read and combine all WoS .txt files in a given folder (including subfolders)
# Returns a tibble with bibliometrix fields + folder metadata
read_wos_folder <- function(folder_path) {
  # 1) Collect ALL .txt files (including subfolders)
  txt_files <- list.files(folder_path, pattern = "\\.txt$", full.names = TRUE, recursive = TRUE)
  if (length(txt_files) == 0) return(NULL)
  
  # 2) Build folder-level metadata (from the root folder name)
  meta <- split_folder_meta(folder_path)
  
  # 3) Try converting all files at once (fast path)
  out <- tryCatch({
    convert2df(file = txt_files, dbsource = "wos", format = "plaintext")
  }, error = function(e) {
    message(sprintf("⚠️  Bulk convert failed for '%s' (%s). Falling back to per-file convert...",
                    basename(folder_path), e$message))
    return(NULL)
  })
  
  # 4) Fallback: per-file conversion if bulk failed
  if (is.null(out)) {
    safe_convert <- purrr::safely(function(f) {
      convert2df(file = f, dbsource = "wos", format = "plaintext")
    })
    res_list <- purrr::map(txt_files, safe_convert)
    ok   <- purrr::map(res_list, "result")
    errs <- purrr::map(res_list, "error")
    
    # Warn about problematic files
    bad_idx <- which(!purrr::map_lgl(errs, is.null))
    if (length(bad_idx) > 0) {
      message("⚠️  Files with errors (skipped):")
      for (i in bad_idx) message(sprintf(" - %s | %s", basename(txt_files[i]), errs[[i]]$message))
    }
    
    # Bind only successful results
    ok <- ok[purrr::map_lgl(ok, ~ !is.null(.x) && nrow(.x) > 0)]
    if (length(ok) == 0) return(NULL)
    out <- dplyr::bind_rows(ok)
  }
  
  if (is.null(out) || nrow(out) == 0) return(NULL)
  
  # 5) Attach folder metadata to every row
  out <- out %>%
    dplyr::mutate(
      folder_name = meta$folder_name,
      State       = meta$State,
      StateCode   = meta$StateCode,
      University  = meta$University
    )
  
  # Optional: remove exact duplicates, if needed
  # out <- dplyr::distinct(out)
  
  out
}

# Helper to read a list of folders and combine them
read_all_folders <- function(folder_vec) {
  if (length(folder_vec) == 0) return(tibble())
  folder_vec %>%
    purrr::map(read_wos_folder) %>%
    purrr::compact() %>%
    dplyr::bind_rows()
}

# Combine variable types
coerce_export_types <- function(df) {
  # Keep only expected columns (missing ones will be added as NA)
  missing <- setdiff(cols_keep, names(df))
  if (length(missing)) df[missing] <- NA
  
  df %>%
    dplyr::select(all_of(cols_keep)) %>%
    dplyr::mutate(
      State          = as.character(State),
      University     = as.character(University),
      University_Code= suppressWarnings(as.numeric(University_Code)),
      DI             = as.character(DI),        # DOI
      EM             = as.character(EM),        # Emails
      AU             = as.character(AU),        # Authors
      PY             = suppressWarnings(as.integer(PY))  # Publication year
    )
}


#------------------------
# Script 2: Get first instance email for each institution and distances in km
#------------------------
# Split names in data distances
split_uni_pairs <- function(df,
                            col_from   = "uni1",
                            col_to     = "uni2",
                            col_dist   = "distance_km",   # puede ser NULL si no existe
                            sep        = "\\|",           # separador (regex). Para "|" usar "\\|"
                            keep_original = FALSE) {
  stopifnot(col_from %in% names(df), col_to %in% names(df))
  if (!is.null(col_dist)) stopifnot(col_dist %in% names(df))
  
  # Armar regex según separador
  #   ^(.*?)<sep>([^<sep>]+)<sep>(.*)$
  rgx <- paste0("^(.*?)", sep, "([^", sep, "]+)", sep, "(.*)$")
  
  df2 <- df %>%
    mutate(across(all_of(c(col_from, col_to)), as.character)) %>%
    # rename to stable temp names to avoid tidy-eval hassles
    rename(.from = all_of(col_from),
           .to   = all_of(col_to))
  
  out <- df2 %>%
    # split FROM
    tidyr::extract(
      .from,
      into  = c("university_from", "university_code_from", "state_from"),
      regex = rgx,
      remove = !keep_original
    ) %>%
    # split TO
    tidyr::extract(
      .to,
      into  = c("university_to", "university_code_to", "state_to"),
      regex = rgx,
      remove = !keep_original
    ) %>%
    mutate(
      across(c(university_from, state_from, university_to, state_to), ~ str_squish(.)),
      university_code_from = suppressWarnings(parse_number(university_code_from)),
      university_code_to   = suppressWarnings(parse_number(university_code_to))
    )
  
  
  # final columns
  final_cols <- c("university_from", "university_code_from", "state_from",
                  "university_to",   "university_code_to",   "state_to")
  if (!is.null(col_dist) && col_dist %in% names(out)) {
    final_cols <- c(final_cols, col_dist)
  }
  out %>% select(all_of(final_cols), everything())
}

#------------------------
# Script 3: Combine Data Nij from SciSciNet and get JSI matrices
#------------------------
#---------------------------
# Step 0: 
# Combine data frames
#---------------------------
combine_dataframes <- function(temp, names_data,root_dir="C:/Users/OMEN/Box/Andrea_Universities/Project_InternetEffect/DataOut_CoOccur_1980-2020_NX_142_KX_10/") {
  data <- NULL
  d_years <- NULL
  
  for (i in temp) {
    # Leer el archivo con fread
    df <- fread(paste0(root_dir,i), col.names = paste0("V", seq_along(names_data)))
    df <- data.frame(df)
    
    # Extraer el año de los últimos 4 dígitos antes de la extensión
    year <- as.numeric(sub(".*(\\d{4})\\..*$", "\\1", basename(i)))
    
    # Añadir columnas
    colnames(df) <- names_data
    df$from <- names_data
    df$year <- year
    
    # Acumular
    data <- bind_rows(data, df)
  }
  
  x <- length(temp)
  return(list(data = data, x = x))
}

#------------------------------------
# Step 1: 
# Compute the Jaccard index matrixes
#------------------------------------
make_symmetric <- function(mat) {
  if (!is.matrix(mat)) stop("Input must be a matrix.")
  if (!all(mat[lower.tri(mat)] == 0, na.rm = TRUE)) stop("Matrix is not upper triangular.")
  
  # Copy upper triangle to lower triangle
  mat[lower.tri(mat)] <- t(mat)[lower.tri(mat)]
  
  return(mat)
}
compute_jsi <- function(a) {
  # Initialize an empty data frame
  JSI <- data.frame(from = character(), to = character(), weight = numeric(), stringsAsFactors = FALSE)
  
  # Get all column and row names
  univs <- colnames(a)
  
  # Use vectorized outer to compute all combinations of indices
  indices <- expand.grid(w = 1:ncol(a), z = 1:ncol(a))
  
  # Compute `ji` for all pairs using vectorized operations
  ji_values <- mapply(function(w, z) {
    a[w, z] / (a[w, w] + a[z, z] - a[w, z])
  }, indices$w, indices$z)
  
  # Build the result data frame
  JSI <- data.frame(
    from = univs[indices$z],
    to = univs[indices$w],
    weight = as.double(ji_values)
  )
  
  return(JSI)
}
JSI_matrix=function(data){
  #data=data_USA142aff
  stopifnot(is.data.frame(data))
  stopifnot(c("from","year")%in%colnames(data))
  stopifnot(sum(is.na(data))==0)
  
  JSI_comp=NULL
  for (i in unique(data$year)) {
    #i=unique(data$year)[2]
    eje=as.matrix(data%>%filter(year==i)%>%select(-c(from,year)))
    
    a=make_symmetric(eje)
    JSI=compute_jsi(a)
    JSI$Year=i
    JSI_comp=bind_rows(JSI_comp,JSI)
    print(i)
    
  }
  return(JSI_comp)
}
#---------------------------------------------------------------
# Step 1.5: 
# Intermedia filtering at least X publications in a given year
#---------------------------------------------------------------
# --- helper: keep rows with tot >= threshold in that year ---
filter_npub <- function(dataset, threshold) {
  stopifnot(is.data.frame(dataset), is.numeric(threshold))
  if (!all(c("from","tot","year") %in% names(dataset)))
    stop("dataset must have columns: from, tot, year")
  dataset %>%
    mutate(keep = as.integer(tot >= threshold)) %>%
    filter(keep == 1)
}
#------------------------
# Script 4: Get Edge page rank and frontiers
#------------------------
# Computing centrality measures
compute_centrality_by_year=function(df){
  #df=JSI_Nij_1985_2005 %>% clean_names()
  # Get unique years
  unique_years <- unique(df$year)
  
  # Initialize an empty dataframe to store results
  final_results <- NULL
  
  # Iterate over each year
  for (yr in unique_years) {
    #yr=unique_years[1]
    # Filter data for the current year and remove zero-weight edges
    example_network <- df %>%
      filter(year == yr&from!=to & weight > 0) %>%
      select(c("from", "to", "nij", "edge_code"))
    
    # Create the graph
    g <- graph_from_data_frame(example_network, directed = FALSE)
    
    # Assign weights
    E(g)$weight <- example_network$nij
    E(g)$name <- example_network$edge_code
    
    # Compute node PageRank (with weights if available)
    node_page_rank <- page_rank(g, weights = E(g)$weight)$vector
    
    # Compute edge-level PageRank as the average PageRank of its two connected nodes
    edge_page_rank <- apply(ends(g, E(g)), 1, function(x) mean(node_page_rank[x]))
    
    # Transform weights so larger values mean stronger links
    inv_weights <- 1 / E(g)$weight 
    
    # Compute Edge Centrality Metrics
    edge_betweenness <- edge_betweenness(g, directed = FALSE, weights = inv_weights)
    
    # Compute Node-Level Closeness Centrality
    node_closeness <- closeness(g, mode = "all", weights = inv_weights)
    
    # Compute Edge Closeness Centrality as the mean closeness of its connected nodes
    edge_closeness <- apply(ends(g, E(g)), 1, function(x) mean(node_closeness[x]))
    
    # Compute Edge Strength (Weighted Degree)
    edge_strength <- E(g)$weight
    
    # Compute Edge Degree Centrality
    edge_degree <- apply(ends(g, E(g)), 1, function(x) sum(degree(g, x)))
    
    # Compute Node Eigenvector Centrality
    node_eigen <- eigen_centrality(g, weights = E(g)$weight)$vector
    
    # Compute Edge Eigenvector Centrality as the mean eigenvector of its two connected nodes
    edge_eigen <- apply(ends(g, E(g)), 1, function(x) mean(node_eigen[x]))
    
    
    # Create a DataFrame with results
    df_centrality <- data.frame(
      edge = E(g)$name,
      edge_page_rank=edge_page_rank,
      betweenness = edge_betweenness,
      closeness = edge_closeness,
      strength = edge_strength,
      degree = edge_degree,
      edge_eigenvector = edge_eigen,
      year = yr  # Add the year column
    )
    print(yr)
    # Append to the final results dataframe
    final_results <- final_results%>%bind_rows(df_centrality)
    
  }
  
  return(final_results)
}
compute_centrality_by_year_old=function(df){
  #df=data_nij_USA142aff
  # Get unique years
  unique_years <- unique(df$year)
  
  # Initialize an empty dataframe to store results
  final_results <- NULL
  
  # Iterate over each year
  for (yr in unique_years) {
    #yr=unique_years[1]
    # Filter data for the current year and remove zero-weight edges
    example_network <- df %>%
      filter(year == yr&from!=to & weight > 0) %>%
      select(c("from", "to", "nij", "edge_code"))
    
    # Create the graph
    g <- graph_from_data_frame(example_network, directed = FALSE)
    
    # Assign weights
    E(g)$weight <- example_network$nij
    E(g)$name <- example_network$edge_code
    
    # Compute node PageRank (with weights if available)
    node_page_rank <- page_rank(g, weights = E(g)$weight)$vector
    
    # Compute edge-level PageRank as the average PageRank of its two connected nodes
    edge_page_rank <- apply(ends(g, E(g)), 1, function(x) mean(node_page_rank[x]))
    
    # Transform weights so larger values mean stronger links
    inv_weights <- 1 / E(g)$weight 
    
    # Compute Edge Centrality Metrics
    edge_betweenness <- edge_betweenness(g, directed = FALSE, weights = inv_weights)
    
    # Compute Node-Level Closeness Centrality
    node_closeness <- closeness(g, mode = "all", weights = inv_weights)
    
    # Compute Edge Closeness Centrality as the mean closeness of its connected nodes
    edge_closeness <- apply(ends(g, E(g)), 1, function(x) mean(node_closeness[x]))
    
    # Compute Edge Strength (Weighted Degree)
    edge_strength <- E(g)$weight
    
    # Compute Edge Degree Centrality
    edge_degree <- apply(ends(g, E(g)), 1, function(x) sum(degree(g, x)))
    
    # Compute Node Eigenvector Centrality
    node_eigen <- eigen_centrality(g, weights = E(g)$weight)$vector
    
    # Compute Edge Eigenvector Centrality as the mean eigenvector of its two connected nodes
    edge_eigen <- apply(ends(g, E(g)), 1, function(x) mean(node_eigen[x]))
    
    
    # Create a DataFrame with results
    df_centrality <- data.frame(
      edge = E(g)$name,
      edge_page_rank=edge_page_rank,
      betweenness = edge_betweenness,
      closeness = edge_closeness,
      strength = edge_strength,
      degree = edge_degree,
      edge_eigenvector = edge_eigen,
      year = yr  # Add the year column
    )
    print(yr)
    # Append to the final results dataframe
    final_results <- final_results%>%bind_rows(df_centrality)
    
  }
  
  return(final_results)
}
# Trying some different thresholds on the minimum number of publications and start date
# --- main: compute and plot for multiple thresholds ---
plot_edgepagerank_and_links_by_thresholds <- function(
    data_tot_npub_USA142aff,
    data_Nij_USA142aff,
    JSI_comp_USA142aff,
    thresholds,
    min_values     = NULL,   # vector of starting years (e.g., c(1980, 1983, 1985))
    show_legend    = FALSE
) {
  data_tot <- janitor::clean_names(data_tot_npub_USA142aff)
  data_nij <- janitor::clean_names(data_Nij_USA142aff)
  jsi_comp <- janitor::clean_names(JSI_comp_USA142aff)
  
  req_tot <- c("from","tot","year")
  req_nij <- c("from","to","year","code_min","code_max")
  req_jsi <- c("year","code_min","code_max","weight")
  if (!all(req_tot %in% names(data_tot))) stop("data_tot_npub_USA142aff must have: ", paste(req_tot, collapse=", "))
  if (!all(req_nij %in% names(data_nij))) stop("data_Nij_USA142aff must have: ", paste(req_nij, collapse=", "))
  if (!all(req_jsi %in% names(jsi_comp))) stop("JSI_comp_USA142aff must have: ", paste(req_jsi, collapse=", "))
  
  # --- define year range combinations ---
  min_year_global <- min(jsi_comp$year, na.rm = TRUE)
  max_year_global <- max(jsi_comp$year, na.rm = TRUE)
  if (is.null(min_values)) min_values <- min_year_global
  
  # build all combinations: threshold × starting year
  combos <- expand.grid(threshold = thresholds, start_year = min_values, stringsAsFactors = FALSE)
  
  # iterate through all combos
  per_combo <- purrr::map(seq_len(nrow(combos)), function(i) {
    thr <- combos$threshold[i]
    start_yr <- combos$start_year[i]
    
    message("Processing threshold = ", thr, " | start year = ", start_yr)
    
    # 1) filter tot_npub dataset to years >= start_yr
    tot_filtered <- data_tot %>% filter(year >= start_yr)
    
    # 2) filter unis with tot >= thr every year from start_yr to max
    univ_keep <- tot_filtered %>%
      filter(tot >= thr) %>%
      group_by(from) %>%
      summarise(years = n_distinct(year), .groups = "drop") %>%
      filter(years == (max_year_global - start_yr + 1)) %>%
      pull(from)
    
    if (length(univ_keep) == 0) return(NULL)
    
    # 3) filter Nij for these unis in that year range
    nij_base <- data_nij %>%
      filter(from %in% univ_keep, to %in% univ_keep,year <= max_year_global) %>%
      filter(from != to)
    
    nij_for_weights <- nij_base %>%
      distinct(year, code_min, code_max, .keep_all = TRUE) %>%
      mutate(edge_code = paste0(code_min, "_", code_max)) %>%
      select(-code_min, -code_max) %>%
      left_join(
        jsi_comp %>%
          distinct(year, code_min, code_max, .keep_all = TRUE) %>%
          mutate(edge_code = paste0(code_min, "_", code_max)) %>%
          select(weight, edge_code, year),
        by = c("edge_code", "year")
      )
    
    # 4) compute centrality by year
    centrality <- compute_centrality_by_year(nij_for_weights) %>%
      mutate(year = as.integer(year),
             threshold = thr,
             start_year = start_yr)
    
    # 5) compute n_links/total_links by from
    links_summary <- nij_base %>%
      mutate(edge_code = paste0(code_min, "_", code_max)) %>%
      left_join(
        nij_for_weights %>% select(weight, edge_code, year),
        by = c("edge_code", "year")
      ) %>%
      filter(weight > 0) %>%
      group_by(year, from) %>%
      summarise(
        n_links     = n_distinct(to),
        avg_JSI     = mean(weight, na.rm = TRUE),
        total_links = length(univ_keep) - 1L,
        .groups     = "drop"
      ) %>%
      mutate(
        year = as.integer(year),
        threshold = thr,
        start_year = start_yr,
        ratio_links = n_links / total_links
      )
    
    list(centrality = centrality, links_summary = links_summary)
  })
  
  # combine results
  per_combo <- compact(per_combo) # drop NULL combos
  centrality_all    <- bind_rows(lapply(per_combo, `[[`, "centrality"))
  links_summary_all <- bind_rows(lapply(per_combo, `[[`, "links_summary"))
  
  # --- plots: now faceted by threshold *and* start_year ---
  p_centrality <- centrality_all %>%
    mutate(
      start_year = factor(start_year, levels = sort(unique(start_year))),
      threshold  = factor(threshold,  levels = sort(unique(threshold)))
    ) %>%
    ggplot(aes(x = year, y = edge_page_rank, group = edge, color = edge)) +
    geom_line(linewidth = 0.6, alpha = 0.9, show.legend = show_legend) +
    facet_wrap(vars(start_year, threshold), nrow = 4, ncol = 6, scales = "free") +
    labs(x = "year", y = "Edge PageRank",
         title = "Edge PageRank by threshold × starting year") +
    theme_minimal(base_size = 12) +
    theme(legend.position = if (show_legend) "right" else "none")
  
  p_JSI <- links_summary_all %>%
    arrange(from, year) %>%
    mutate(
      start_year = factor(start_year, levels = sort(unique(start_year))),
      threshold  = factor(threshold,  levels = sort(unique(threshold)))
    ) %>%
    ggplot(aes(x = year, y = avg_JSI, group = from, color = from)) +
    geom_line(linewidth = 0.7, alpha = 0.9, show.legend = show_legend) +
    labs(x = "Year", y = "AVG(JSI)") +
    facet_wrap(vars(start_year, threshold), nrow = 4, ncol = 6, scales = "free") +
    theme_minimal(base_size = 12) +
    theme(legend.position = if (show_legend) "right" else "none")
    
  p_connectivity <- links_summary_all %>%
    mutate(
      start_year = factor(start_year, levels = sort(unique(start_year))),
      threshold  = factor(threshold,  levels = sort(unique(threshold)))
    ) %>%
    ggplot(aes(x = year, y = ratio_links, group = from, color = from)) +
    geom_line(linewidth = 0.7, alpha = 0.9, show.legend = show_legend) +
    facet_wrap(vars(start_year, threshold), nrow = 4, ncol = 6, scales = "free") +
    labs(x = "year", y = "n_links / total_links",
         title = "Connectivity ratio by threshold × starting year") +
    theme_minimal(base_size = 12) +
    theme(legend.position = if (show_legend) "right" else "none")
  
  list(
    plot_edge_pagerank = p_centrality,
    plot_links_ratio   = p_connectivity,
    plot_jsi=p_JSI,
    links_summary      = links_summary_all,
    centrality         = centrality_all
  )
}

# General script test autocorrelation and find zeries non statistically differen from 0
# base data frame: data_tot_npub_USA142aff (Nii)
find_not_significant_from <- function(df=data_tot_npub_USA142aff,
                                      from_col  = "from",
                                      year_col  = "year",
                                      value_col = "tot",
                                      year_min  = -Inf,
                                      year_max  =  Inf,
                                      fdr_level = 0.05) {
  #--------------------------------------------------------------
  # Identify 'from' units whose time series are NOT significantly
  # different from zero, accounting for temporal correlation.
  #
  # Logic replicates your manual pipeline:
  #   1. Ljung–Box test for autocorrelation.
  #   2. Split into two groups:
  #        a) autocorrelated  -> test on differenced series
  #        b) non-autocorrelated -> test on raw series
  #   3. Run t-tests vs 0 for both groups.
  #   4. Adjust p-values (BH/FDR).
  #   5. Return 'from' with p_adj ≥ fdr_level.
  #--------------------------------------------------------------
  
  # Standardize column names
  dat <- df %>%
    rename(from = !!sym(from_col),
           year = !!sym(year_col),
           val  = !!sym(value_col)) %>%
    mutate(year = as.numeric(year)) %>%
    filter(year >= year_min, year <= year_max) %>%
    arrange(from, year)
  
  # 1. Autocorrelation test
  temp_test <- dat %>%
    group_by(from) %>%
    nest() %>%
    mutate(
      acf1 = map_dbl(data, ~ acf(.x$val, plot = FALSE)$acf[2]),
      lb_p = map_dbl(data, ~ Box.test(.x$val, lag = 1, type = "Ljung-Box")$p.value),
      result = ifelse(lb_p < 0.05,
                      "Reject H0: Temporal correlation",
                      "No temporal correlation")
    ) %>%
    select(from, result)
  
  no_temporal_from <- temp_test %>%
    filter(result == "No temporal correlation") %>%
    pull(from)
  
  # Helper function for the t-test logic
  test_vs_zero <- function(df_sub) {
    df_sub %>%
      group_by(from) %>%
      summarise(
        n = n(),
        mean_diff = mean(diff(val), na.rm = TRUE),
        sd_diff = sd(diff(val), na.rm = TRUE),
        ttest = list(t.test(diff(val), mu = 0)),
        .groups = "drop"
      ) %>%
      mutate(tt_tidy = map(ttest, broom::tidy)) %>%
      unnest(tt_tidy) %>%
      mutate(p_adj = p.adjust(p.value, method = "BH")) %>%
      filter(p_adj >= fdr_level) %>%
      pull(from)
  }
  
  # 2a. For autocorrelated series: use differenced data
  autocorrelated <- dat %>%
    filter(!from %in% no_temporal_from)
  res_auto <- test_vs_zero(autocorrelated)
  
  # 2b. For non-autocorrelated series: test raw data (no differencing)
  #non_auto=
  res_non_auto <- test_vs_zero(dat %>%
                                 filter(from %in% no_temporal_from)
  )
  
  
  # 3. Combine both
  res_all <- unique(c(res_auto, res_non_auto))
  
  return(res_all)
}

# Helper: most frequent value (mode) per state
mode_value <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  names(sort(table(x), decreasing = TRUE))[1]
}

run_did2s_analysis <- function(data_raw,
                               y_col,
                               treatment_col,
                               unit_col,
                               time_col,
                               real_time_col,
                               min_year=NULL,
                               truncate = FALSE,
                               max_year = NULL,
                               y_lim,
                               x_lim,
                               controls = FALSE,
                               time_indep_controls = NULL,
                               time_dep_controls = NULL) {
  library(dplyr)
  library(did2s)
  library(fixest)
  library(ggplot2)
  
  # ---------------------------
  # STEP 1: Prepare panel data
  # ---------------------------
  if (truncate) {
    if (is.null(max_year)) {
      stop("If truncate = TRUE, you must provide max_year")
    }
    
    message(paste("Filtering data with", min_year,"<= Year <=", max_year))
    
    # Lista base de columnas a seleccionar
    base_vars <- c(time_col, unit_col, y_col, treatment_col)
    
    # Agregar controles si se desea
    if (controls) {
      if (is.null(time_indep_controls) & is.null(time_dep_controls)) {
        stop("If controls = TRUE, provide at least one control variable (time_indep_controls or time_dep_controls).")
      }
      control_vars <- c(time_indep_controls, time_dep_controls)
    } else {
      control_vars <- NULL 
    }
    
    # Preparar panel balanceado
    panel_data_balanced <- data_raw %>%
      filter(.data[[time_col]] <= max_year,
             .data[[time_col]] >= min_year) %>%
      rename(!!unit_col := edge_code) %>%
      dplyr::select(all_of(c(base_vars, control_vars))) %>%
      left_join(
        data_raw %>%
          filter(.data[[time_col]] >= min_year,
                 .data[[treatment_col]] == 1) %>%
          rename(!!unit_col := edge_code) %>%
          group_by(.data[[unit_col]]) %>%
          summarise(min_year = min(.data[[time_col]])),
        by = unit_col
      ) %>%
      mutate(real_time = ifelse(
        is.na(.data[[time_col]] - min_year),
        Inf,
        ifelse(min_year <= max_year, .data[[time_col]] - min_year, Inf)
      ))
  } else {
    panel_data_balanced <- data_raw
  }
  
  # ---------------------------
  # STEP 2: Construir fórmulas
  # ---------------------------
  # Fixed effects
  if (controls) {
    f_first_stage <- as.formula(paste0("~0+ ", paste(control_vars, collapse = " + "), " | ",unit_col, " + ", time_col))
    
  }else{
    f_first_stage <- as.formula(paste0("~0 | ",unit_col, " + ", time_col))
  }
  
  
  # Segundo paso con o sin controles dependientes del tiempo
  second_stage_static <- paste0("i(", treatment_col, ", ref = FALSE)")
  second_stage_event <- paste0("i(", real_time_col, ", ref = c(-1, Inf))")
  
  
  f_second_static <- as.formula(paste0("~ ", second_stage_static))
  f_second_event <- as.formula(paste0("~ ", second_stage_event))
  
  # ---------------------------
  # STEP 3: Static DiD Model
  # ---------------------------
  
  did2s_model <- did2s(
    data = panel_data_balanced,
    yname = y_col,
    treatment = treatment_col,
    first_stage = f_first_stage,#~0 + Frontiers + IS+Scaled_km+same_language+edge_page_rank| Links + Year ,#~0 + IS + same_language + Frontiers + Scaled_km + edge_page_rank | Links + Year,# ,
    second_stage = f_second_static,
    cluster_var = unit_col,
    verbose = FALSE
  )
  
  summary(did2s_model)
  
  #cor(panel_data_balanced[, c("IS", "Frontiers", "same_language", "Scaled_km", "edge_page_rank")], use = "complete.obs")
  static_summary <- summary(did2s_model)
  
  est_table <- fixest::etable(
    did2s_model,
    fitstat = c("n"),
    title = "Estimate of Static TWFE Model",
    notes = "Estimated using Two-Stage DiD per Gardner (2021)."
  )
  
  static_plot <- fixest::iplot(
    did2s_model,
    main = "Event study: Staggered treatment",
    xlab = "Relative time to treatment",
    col = "steelblue",
    ref.line = -0.5,
    plot = FALSE
  )
  
  # ---------------------------
  # STEP 4: Event Study Model
  # ---------------------------
  es_model <- did2s(
    data = panel_data_balanced,
    yname = y_col,
    treatment = treatment_col,
    first_stage = f_first_stage,
    second_stage = f_second_event,
    cluster_var = unit_col,
    verbose = FALSE
  )
  
  event_study_plot <- fixest::iplot(
    es_model,
    main = "Event Study: Staggered Treatment Effect",
    xlab = "Relative Time to Treatment",
    ylab = "Estimated Effect with 95% CI",
    col = "navy",
    pch = 16,
    lwd = 4,
    ci.col = "skyblue",
    ci.lwd = 4,
    ci.alpha = 0.4,
    ref.line = -0.5,
    ref.col = "black",
    ref.lwd = 2,
    grid = TRUE,
    xlim = x_lim,
    ylim = y_lim,
    cex.main = 20,
    legend = TRUE,
    legend.pos = "topleft",
    legend.col = c("navy", "skyblue"),
    legend.pch = c(16, NA),
    legend.lwd = c(3, 2),
    plot = TRUE
  )
  
  # ---------------------------
  # STEP 5: Return Results
  # ---------------------------
  return(list(
    panel_data = panel_data_balanced,
    static_model = did2s_model,
    static_summary = static_summary,
    estimate_table = est_table,
    static_plot = static_plot,
    event_study_model = es_model,
    event_study_plot = event_study_plot
  ))
}
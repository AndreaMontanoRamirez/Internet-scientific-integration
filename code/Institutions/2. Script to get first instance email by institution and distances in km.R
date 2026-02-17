#-----------------------------------
# Get first email instance for each university and get distances in km among them
#-----------------------------------
#--------------------
# ---- Utilities ----
#--------------------
source("Scripts/utilities.R") #Libraries and functions
#--------------------
# Read data 
#--------------------
# Combined WoS Data 142 institutions
combined_wos_complete=read_csv(file.path("Outputs/WoS_USA_States_combined complete 14102025_oldcheck.csv"))

# SciSciNet codes
SciSciNet_Affiliations_Univ<- read_delim("Inputs/SciSciNet_Affiliations_AffiliationID_AffiliationName_USStateCode_AIGType_9199.tsv", 
                                         delim = "\t", escape_double = FALSE, 
                                         trim_ws = TRUE,col_names = F) 
colnames(SciSciNet_Affiliations_Univ)=c("university_code","university_name","state_code","type")

#-----------------------------
# Merge with SciScinet datframe to correct names
#-----------------------------
combined_wos_complete=combined_wos_complete %>% 
  clean_names()%>% 
  left_join(SciSciNet_Affiliations_Univ %>% 
              select(-c(type)),by=c("university_code"))# %>% 
  #mutate(university = ifelse(!is.na(university_name) & university != university_name,university_name, university)) %>% 
  #select(-c(university_name))

#--------------------------
# Email analysis
#--------------------------
universities_df <- tribble(
  ~State,    ~University,                                 ~Domain,
  # Alabama
  "Alabama", "University of Alabama at Birmingham",       "UAB.EDU",
  "Alabama", "Auburn University",                         "AUBURN.EDU",
  "Alabama", "University of Alabama",                     "UA.EDU",
  # Alaska
  "Alaska",  "University of Alaska Fairbanks",            "UAF.EDU",
  "Alaska",  "University of Alaska Anchorage",            "UAA.ALASKA.EDU",
  # Arizona
  "Arizona", "Arizona State University",                  "ASU.EDU",
  "Arizona", "University of Arizona",                     "ARIZONA.EDU",
  "Arizona", "Northern Arizona University",               "NAU.EDU",
  # Arkansas
  "Arkansas","University of Central Arkansas",            "UCA.EDU",
  "Arkansas","Arkansas State University",                 "ASTATE.EDU",
  "Arkansas","University of Arkansas",                    "UARK.EDU",
  # California
  "California","University of California, Berkeley",      "BERKELEY.EDU",
  "California","University of California, Los Angeles",   "UCLA.EDU",
  "California","University of Southern California",       "USC.EDU",
  # Colorado
  "Colorado","Colorado State University",                 "COLOSTATE.EDU",
  "Colorado","University of Colorado Boulder",            "COLORADO.EDU",
  "Colorado","University of Colorado Denver",             "CUDENVER.EDU",
  # Connecticut
  "Connecticut","Yale University",                        "YALE.EDU",
  "Connecticut","Central Connecticut State University",   "CCSU.EDU",
  "Connecticut","University of Connecticut",              "UCONN.EDU",
  # Delaware
  "Delaware","Delaware State University",                 "DSC.EDU", #now it changed to "DESU.EDU"
  "Delaware","University of Delaware",                    "UDEL.EDU",
  # District of Columbia
  "District Of Columbia", "George Washington University",                   "GWU.EDU",
  "District Of Columbia", "American University",                            "AMERICAN.EDU",
  "District Of Columbia", "Georgetown University",                          "GEORGETOWN.EDU",
  # Florida
  "Florida","University of Florida",                      "UFL.EDU",
  "Florida","Florida International University",           "FIU.EDU",
  "Florida","University of Central Florida",              "UCF.EDU",
  # Georgia
  "Georgia","University of Georgia",                      "UGA.EDU",
  "Georgia","Kennesaw State University",                  "KENNESAW.EDU",
  "Georgia","Georgia State University",                   "GSU.EDU",
  # Hawaii
  "Hawaii","University of Hawaii at Hilo",                "HAWAII.EDU",
  "Hawaii","University of Hawaii at Manoa",               "HAWAII.EDU",
  # Idaho
  "Idaho","University of Idaho",                          "UIDAHO.EDU",
  "Idaho","Boise State University",                       "BOISESTATE.EDU",
  "Idaho","Idaho State University",                       "ISU.EDU",
  # Illinois
  "Illinois","University of Illinois at Urbana–Champaign","UIUC.EDU",
  "Illinois","Northwestern University",                   "NORTHWESTERN.EDU",
  "Illinois","University of Illinois at Chicago",         "UIC.EDU",
  # Indiana
  "Indiana","Indiana University Indianapolis", "IUPUI.EDU",
  "Indiana","Indiana University Bloomington", "INDIANA.EDU",
  "Indiana","Purdue University",                          "PURDUE.EDU",
  # Iowa
  "Iowa","University of Iowa",                            "UIOWA.EDU",
  "Iowa","University of Northern Iowa",                   "UNI.EDU",
  "Iowa","Iowa State University",                         "IASTATE.EDU",
  # Kansas
  "Kansas","Kansas State University",                     "KSU.EDU",
  "Kansas","University of Kansas",                        "KU.EDU",
  "Kansas","Wichita State University",                    "WICHITA.EDU",
  # Kentucky
  "Kentucky","Western Kentucky University",               "WKU.EDU",
  "Kentucky","University of Kentucky",                    "UKY.EDU",
  "Kentucky","University of Louisville",                  "LOUISVILLE.EDU",
  # Louisiana
  "Louisiana","University of Louisiana at Lafayette",     "LOUISIANA.EDU",
  "Louisiana","Louisiana State University",               "LSU.EDU",
  "Louisiana","Tulane University",                        "TULANE.EDU",
  # Maine
  "Maine","University of Maine",                          "UMAINE.EDU",
  "Maine","University of New England Maine",                    "UNE.EDU",
  "Maine","University of Southern Maine",                 "USM.MAINE.EDU",
  # Maryland
  "Maryland","University of Maryland, College Park",      "UMD.EDU",
  "Maryland","Towson University",                         "TOWSON.EDU",
  "Maryland","Johns Hopkins University",                  "JHU.EDU",
  # Massachusetts
  "Massachusetts","University of Massachusetts Amherst",  "UMASS.EDU",
  "Massachusetts","Boston University",                    "BU.EDU",
  "Massachusetts","Harvard University",                   "HARVARD.EDU",
  # Michigan
  "Michigan","University of Michigan",                    "UMICH.EDU",
  "Michigan","Wayne State University",                    "WAYNE.EDU",
  "Michigan","Michigan State University",                 "MSU.EDU",
  # Minnesota
  "Minnesota","St. Cloud State University",               "STCLOUDSTATE.EDU",
  "Minnesota","University of Minnesota",                  "UMN.EDU",
  "Minnesota","Minnesota State University, Mankato",      "MNSU.EDU",
  # Mississippi
  "Mississippi","Mississippi State University",           "MSSTATE.EDU",
  "Mississippi","University of Mississippi",              "OLEMISS.EDU",
  "Mississippi","University of Southern Mississippi",     "USM.EDU",
  # Missouri
  "Missouri","University of Missouri Columbia",                    "MISSOURI.EDU",
  "Missouri","Missouri State University",                 "SMSU.EDU",
  "Missouri","Saint Louis University",                    "SLU.EDU",
  # Montana
  "Montana","Montana State University Billings",          "MSUBILLINGS.EDU",
  "Montana","Montana State University",                   "MONTANA.EDU",
  "Montana","University of Montana",                      "UMT.EDU",
  # Nebraska
  "Nebraska","University of Nebraska–Lincoln",            "UNL.EDU",
  "Nebraska","University of Nebraska at Kearney",         "UNK.EDU",
  "Nebraska","Creighton University",                      "CREIGHTON.EDU",
  # Nevada
  "Nevada","University of Nevada, Reno",                  "UNR.EDU",
  "Nevada","University of Nevada, Las Vegas",             "UNLV.EDU",
  # New Hampshire
  "New Hampshire","University of New Hampshire",          "UNH.EDU",
  "New Hampshire","Plymouth State University",            "PLYMOUTH.EDU",
  "New Hampshire","Dartmouth College",                    "DARTMOUTH.EDU",
  # New Jersey
  "New Jersey","Rutgers University",                      "RUTGERS.EDU",
  "New Jersey","Montclair State University",              "MONTCLAIR.EDU",
  "New Jersey","Rowan University",                        "ROWAN.EDU",
  # New Mexico
  "New Mexico","Eastern New Mexico University",           "ENMU.EDU",
  "New Mexico","University of New Mexico",                "UNM.EDU",
  "New Mexico","New Mexico State University",             "NMSU.EDU",
  # New York
  "New York","Columbia University",                       "COLUMBIA.EDU",
  "New York","New York University",                       "NYU.EDU",
  "New York","University at Buffalo",                     "BUFFALO.EDU",
  # North Carolina
  "North Carolina","University of North Carolina at Chapel Hill","UNC.EDU",
  "North Carolina","East Carolina University",            "ECU.EDU",
  "North Carolina","North Carolina State University",     "NCSU.EDU",
  # North Dakota
  "North Dakota","University of North Dakota",            "UND.EDU",
  "North Dakota","North Dakota State University Fargo",         "NDSU.EDU",
  # Ohio
  "Ohio","Kent State University",                         "KENT.EDU",
  "Ohio","University of Cincinnati",                      "UC.EDU",
  "Ohio","Ohio State University",                         "OSU.EDU",
  # Oklahoma
  "Oklahoma","Northeastern State University",             "NSUOK.EDU",
  "Oklahoma","University of Central Oklahoma",            "UCOK.EDU",
  "Oklahoma","University of Oklahoma",                    "OU.EDU",
  # Oregon
  "Oregon","University of Oregon",                        "UOREGON.EDU",
  "Oregon","Portland State University",                   "PDX.EDU",
  "Oregon","Oregon State University",                     "OREGONSTATE.EDU",
  # Pennsylvania
  "Pennsylvania","Temple University",                     "TEMPLE.EDU",
  "Pennsylvania","Pennsylvania State University",         "PSU.EDU",
  "Pennsylvania","University of Pittsburgh",              "PITT.EDU",
  # Rhode Island
  "Rhode Island","University of Rhode Island",            "URI.EDU",
  "Rhode Island","Brown University",                      "BROWN.EDU",
  "Rhode Island","Rhode Island College",                  "RIC.EDU",
  # South Carolina
  "South Carolina","University of South Carolina",        "SC.EDU",
  "South Carolina","College of Charleston",               "COFC.EDU",
  "South Carolina","Clemson University",                  "CLEMSON.EDU",
  # South Dakota
  "South Dakota","South Dakota State University",         "SDSTATE.EDU",
  "South Dakota","University of South Dakota",            "USD.EDU",
  # Tennessee
  "Tennessee","University of Tennessee",                  "UTK.EDU",
  "Tennessee","Middle Tennessee State University",        "MTSU.EDU",
  "Tennessee","University of Memphis",                    "MEMPHIS.EDU",
  # Texas
  "Texas","Texas A&M University",                         "TAMU.EDU",
  "Texas","University of Texas at Austin",                "UTEXAS.EDU",
  "Texas","University of Houston",                        "UH.EDU",
  # Utah
  "Utah","Utah State University",                         "USU.EDU",
  "Utah","Brigham Young University",                      "BYU.EDU",
  "Utah","University of Utah",                            "UTAH.EDU",
  # Vermont
  "Vermont","University of Vermont",                      "UVM.EDU",
  # Virginia
  "Virginia","Virginia Tech",                             "VT.EDU",
  "Virginia","Virginia Commonwealth University",          "VCU.EDU",
  "Virginia","George Mason University",                   "GMU.EDU",
  # Washington
  "Washington","Washington State University",             "WSU.EDU",
  "Washington","Western Washington University",           "WWU.EDU",
  "Washington","University of Washington",                "WASHINGTON.EDU",
  # West Virginia
  "West Virginia","Marshall University",                  "MARSHALL.EDU",
  "West Virginia","West Virginia University",             "WVU.EDU",
  # Wisconsin
  "Wisconsin","University of Wisconsin-Madison",          "WISC.EDU",
  "Wisconsin","Marquette University",                     "MARQUETTE.EDU",
  "Wisconsin","University of Wisconsin–Milwaukee",        "UWM.EDU",
  # Wyoming
  "Wyoming","University of Wyoming",                      "UWYO.EDU"
)

universities_df <- universities_df %>%
  mutate(
    University = str_squish(University),
    State = str_to_title(State)
  ) %>% clean_names()

WOS_email_data=
  combined_wos_complete %>%
  mutate(
    university = str_squish(university),
    state = str_to_title(state)
  ) %>% 
  clean_names() %>% 
  left_join(universities_df,by=c("state","university")) %>%
  mutate(
    em     = toupper(em),
    domain = toupper(domain)
  ) %>%
  rowwise() %>%  # by row
  mutate(
    em_un = as.integer(str_detect(em, fixed(domain)))  # búsqueda literal, no regex
  ) %>%
  ungroup()
 
# ---- EDIT your thresholds here (fractions, not %): ----
thresholds <- c(0.005, 0.001, 0.01, 0.05, 0.1,0.15,0.2)
  
# grouping keys
group_vars <- c("state","university","university_name","university_code")
  
# ---- LONG: first year reaching each threshold ----
Univ_WoS_first_year <-
      WOS_email_data %>%
      group_by(across(all_of(group_vars)), py) %>%
      summarise(
        n_rows=n(),
        n_emails=sum(em_un,na.rm = T))%>%
      mutate(n_emails = dplyr::coalesce(n_emails, 0L)) %>%
      arrange(across(all_of(group_vars)), py) %>%
      group_by(across(all_of(group_vars))) %>%
      mutate(
        first_year_email = {
          yrs <- py[n_emails > 0]
          if (length(yrs)) min(yrs) else NA_integer_
        },
        total_emails = sum(n_emails, na.rm = TRUE),
        cum_emails   = cumsum(n_emails),
        pct_emails   = dplyr::if_else(total_emails > 0, cum_emails / n_rows, NA_real_)
      ) %>%
      ungroup() %>%
    select(all_of(group_vars), py, pct_emails, first_year_email) %>%
    tidyr::crossing(threshold = thresholds) %>%                 # add thresholds
    group_by(across(all_of(group_vars)), threshold) %>%
    filter(!is.na(pct_emails) & pct_emails >= threshold) %>%
    summarise(
      year_at_threshold = min(py),
      first_year_email  = dplyr::first(first_year_email),
      .groups = "drop"
    )

Univ_WoS_first_year %>% write.csv(file="Outputs/WoS_USA_States first year email and threshold 10202025.csv")

#------------------------
# Get Distances in KM
#------------------------
nm_uni   <- "university"
nm_state <- "state"
nm_uni_code <- "university_code"

df <- Univ_WoS_first_year %>% select(state,university_code,university,university_name) %>% distinct() |>
  mutate(
    university = .data[[nm_uni]] |> as.character(),
    state      = .data[[nm_state]] |> as.character(),
    university_code = .data[[nm_uni_code]] |> as.character()
  ) |>
  distinct(university, state, .keep_all = TRUE) |>
  mutate(query_addr = paste(university, state, "USA", sep = ", "))

# ==== 2) Geocodificar con OSM y fallback a ArcGIS ====
# Nota: OSM (Nominatim) tiene límites de tasa; el paquete maneja rate limiting básico.
# Si obtienes NA en algunos, probamos ArcGIS como respaldo.

message("Geocodificando con OSM...")
geo_osm <- df |>
  tidygeocoder::geocode(address = query_addr,
                        method = "osm",
                        lat = latitude, long = longitude,
                        limit = 1)  # sin cascade para controlar fallback

# 2) Fallback only for missing rows
by_cols  <- c("state", "university","university_code","university_name") 
faltantes <- geo_osm |> filter(is.na(latitude) | is.na(longitude))
if (nrow(faltantes) > 0) {
  message("Intentando fallback con ArcGIS para faltantes...")
  geo_arc <- faltantes %>%
    tidygeocoder::geocode(
      address = query_addr,
      method  = "arcgis",
      lat     = "lat_arc",
      long    = "lon_arc",
      limit   = 1
    ) %>%
    # keep only the join keys + the new coords
    select(all_of(by_cols), lat_arc, lon_arc)
  
  # Join + coalesce coords (keep all original columns, including University_Code)
  geo <- geo_osm %>%
    left_join(geo_arc, by = setNames(by_cols, by_cols)) %>%
    mutate(
      latitude  = coalesce(latitude,  lat_arc),
      longitude = coalesce(longitude, lon_arc)
    ) %>%
    select(-lat_arc, -lon_arc)
} else {
  geo <- geo_osm
}

# Chequeo final
n_falt <- sum(is.na(geo$latitude) | is.na(geo$longitude))
if (n_falt > 0) {
  warning(sprintf("Quedaron %s instituciones sin coordenadas. Revísalas manualmente.", n_falt))
}
# Tabla limpia con lat/long
geo_clean <- geo |>
  select(all_of(c(nm_uni, nm_state,nm_uni_code)), latitude, longitude) |>
  arrange(.data[[nm_state]], .data[[nm_uni]])

# ==== 3) Distancias por pares (km) ====
# Usamos geodist (haversine); entrada: lon/lat
coord_mat <- geo_clean |>
  select(longitude, latitude) |>
  as.matrix()

# Matriz en metros -> convertir a km
dist_mat_km <- geodist::geodist(coord_mat, measure = "haversine") / 1000

# Poner nombres de filas/columnas con "Universidad (Estado)"
labels_uc <- geo_clean |>
  transmute(label = paste(.data[[nm_uni]],"|",.data[[nm_uni_code]], "|", .data[[nm_state]], sep = "")) |>
  pull()

dimnames(dist_mat_km) <- list(labels_uc, labels_uc)

dist_long<- dist_mat_km |>
  as.dist() |>
  as.matrix() |>
  as.data.frame() |>
  tibble::rownames_to_column("uni1") |>
  tidyr::pivot_longer(-uni1, names_to = "uni2", values_to = "distance_km") |>
  dplyr::arrange(distance_km)


dist_long <- split_uni_pairs(dist_long)



write_csv(geo_clean, "Outputs/list of universities with their geolocation.csv")
write_csv(as.data.frame(dist_mat_km) |>
            rownames_to_column("universidad_estado"),
          "Outputs/university matrix of distances.csv")
write_csv(dist_long, "Outputs/distances by pair of universities in km.csv")

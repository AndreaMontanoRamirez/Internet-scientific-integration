#------------------------------------------
# Script to get figures Draft and robustness check
#------------------------------------------
#rm(list = ls())
options(scipen = 999)
#------------
# Libraries
#------------
# List of required packages
packages <- c(
  "tidyr", "dplyr", "readr", "stringr", "officer",
  "openxlsx", "ggplot2", "scales", "ggprism", "readxl", "did2s",
  "rlang", "rnaturalearth", "rnaturalearthdata", "countrycode",
  "sf", "maps", "wbstats"
)

# Install missing packages
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) {
  install.packages(packages[!installed], dependencies = TRUE)
}

# Load all packages
lapply(packages, library, character.only = TRUE)

#------------------------
# Data
#------------------------
# JSI COMP
JSI_comp_USAKX10_v2 <- read_csv("Data Inputs/JSI_comp_USAKX10_v2.csv") %>% 
  dplyr::select(-c("...1"))

# Neighbors
neighbors=read_excel("Data Inputs/neighbors.xlsx")
neighbors=neighbors%>%mutate(neighbors1=gsub("USA","US",neighbors))
# Internet
all_compliance=read_excel("Data Inputs/Internet access compliance.xlsx")%>%
  mutate(X1=ifelse(X1=="USA","US",X1))%>%
  mutate(X1=ifelse(X5=="Namibia","NA",X1))
#Balanced
d11_BP_Int=read_csv(file="Data Inputs/Final dataframes/Data modeling 2507 links balanced panel d11 full internet thr5 25032025.csv")%>%
  dplyr::select(-c("...1"))
#Unbalanced
d11_UBP_Int=read_csv(file="Data Inputs/Final dataframes/Data modeling 2507 links unbalanced panel d11 full internet thr5 25032025.csv")%>%
  dplyr::select(-c("...1"))
# countries codes and names
country_list <- read_csv("Data Inputs/country-list.csv",col_names = F)%>%
  mutate(X9=ifelse(is.na(X9),"NA",X9))%>%
  dplyr::select(-c("X4","X3","X6","X7","X8"))%>%
  mutate(X1=ifelse(X1=="USA","US",
                   ifelse(X1=="UK","GB",X1)))%>%
  filter(X5!="United_KingdomRAINE")
# Belonging to the core by year
ed3_lc=read.csv("Data Inputs/Final dataframes/Core groups ed3_lc KX10 08022025.csv")%>%dplyr::select(-c(X))
ed3_lc=ed3_lc%>%
  dplyr::select(-c(tot))%>%
  pivot_longer(cols = -State,  # Transform all columns except "State"
               names_to = "Year",
               values_to = "Core") %>%
  mutate(Year=as.numeric(gsub("Y_","",Year)))
# Global South Countries
globalSouthCountries <- read.csv("Data Inputs/global-south-countries-2024.csv",sep = ";")

globalSouthCountries=globalSouthCountries%>%
  left_join(country_list%>%dplyr::select(c(X1,X5)),by=c("country"="X5"))%>%
  mutate(X1=ifelse(is.na(X1)&country=="DR Congo","CD",
                   ifelse(is.na(X1)&country=="Czech Republic","CZ",
                          ifelse(is.na(X1)&country=="Namibia","NA",
                                 ifelse(is.na(X1)&country=="North Macedonia","MK",
                                        ifelse(is.na(X1)&country=="Timor-Leste","TL",
                                               ifelse(is.na(X1)&country=="Eswatini","SZ",
                                                      ifelse(is.na(X1)&country=="Sao Tome and Principe","ST",
                                                             ifelse(X1=="USA","US",
                                                                    ifelse(X1=="UK","GB",X1))))))))))%>%
  bind_rows(data.frame(X1=c("MO","HK"),country=c("Macao","Hong Kong"),globalSouth=c("No","No")))%>%
  bind_rows(data.frame(
    country = c(
      "American Samoa", "Aruba", "Bermuda", "British Virgin Islands", "Cayman Islands",
      "Curacao", "Faroe Islands", "French Polynesia", "Gibraltar", "Greenland",
      "Guam", "Isle of Man", "Kosovo", "Namibia", "New Caledonia",
      "Northern Mariana Islands", "Puerto Rico", "Sint Maarten", "Saint Martin",
      "Turks and Caicos Islands", "U.S. Virgin Islands"
    ),
    globalSouth = c(
      "Yes",  # American Samoa
      "Yes",  # Aruba
      "No",   # Bermuda (high-income UK territory)
      "Yes",  # British Virgin Islands
      "Yes",  # Cayman Islands
      "Yes",  # Curacao
      "No",   # Faroe Islands (Denmark)
      "Yes",  # French Polynesia
      "No",   # Gibraltar (UK)
      "No",   # Greenland (Denmark)
      "Yes",  # Guam
      "No",   # Isle of Man (UK Crown dependency)
      "Yes",  # Kosovo (developing)
      "Yes",  # Namibia
      "Yes",  # New Caledonia
      "Yes",  # Northern Mariana Islands
      "Yes",  # Puerto Rico
      "Yes",  # Sint Maarten
      "Yes",  # Saint Martin
      "Yes",  # Turks and Caicos Islands
      "Yes"   # U.S. Virgin Islands
    ),
    X1 = c(
      "AS", # American Samoa
      "AW", # Aruba
      "BM", # Bermuda
      "VG", # British Virgin Islands
      "KY", # Cayman Islands
      "CW", # Curacao
      "FO", # Faroe Islands
      "PF", # French Polynesia
      "GI", # Gibraltar
      "GL", # Greenland
      "GU", # Guam
      "IM", # Isle of Man
      "XK", # Kosovo (temporary ISO code)
      "NA", # Namibia
      "NC", # New Caledonia
      "MP", # Northern Mariana Islands
      "PR", # Puerto Rico
      "SX", # Sint Maarten
      "MF", # Saint Martin (French part)
      "TC", # Turks and Caicos Islands
      "VI"  # U.S. Virgin Islands
    )))%>%
  distinct()%>%
  mutate(X1=ifelse(country=="Namibia","NA",X1))
#------------------------
# Data extra preparation
#------------------------
# 1. Identifying early and middle adopters
add_adoption_info <- function(d11, compliance_df,
                              from_col, to_col,
                              unit_col, year_col,
                              complies_col, threshold_col,
                              complies_val = 1, threshold_val = 5,
                              adoption_cutoffs = c(5, 11)) {
  
  # Convert column names to symbols
  from_sym <- sym(from_col)
  to_sym <- sym(to_col)
  unit_sym <- sym(unit_col)
  year_sym <- sym(year_col)
  complies_sym <- sym(complies_col)
  threshold_sym <- sym(threshold_col)
  
  # Internal helper function to generate min_year and Adop_Group
  compute_adoption_info <- function(direction) {
    compliance_df %>%
      filter(!!complies_sym == complies_val, !!threshold_sym == threshold_val) %>%
      group_by(!!unit_sym) %>%
      summarise(min_year = min(!!year_sym), .groups = "drop") %>%
      mutate(Adop_Group = case_when(
        min_year <= min(min_year) + adoption_cutoffs[1] ~ "Early Adopter",
        min_year <= min(min_year) + adoption_cutoffs[2] ~ "Middle Adopter",
        TRUE ~ "Late Adopter"
      )) %>%
      rename(!!paste0("min_year_", direction) := min_year,
             !!paste0("Adop_Group_", direction) := Adop_Group)
  }
  
  # Join for "from" side
  adoption_from <- compute_adoption_info("from")
  d11 <- d11 %>%
    left_join(adoption_from, by = setNames(unit_col, from_col))
  
  # Join for "to" side
  adoption_to <- compute_adoption_info("to")
  d11 <- d11 %>%
    left_join(adoption_to, by = setNames(unit_col, to_col))
  
  # New column: Adop_Pair
  d11 <- d11 %>%
    mutate(
      Adop_Pair = case_when(
        is.na(Adop_Group_from) | is.na(Adop_Group_to) ~ NA_character_,
        Adop_Group_from == Adop_Group_to ~ Adop_Group_from,
        TRUE ~ paste(pmin(Adop_Group_from, Adop_Group_to),
                     pmax(Adop_Group_from, Adop_Group_to),
                     sep = " - ")
      )
    )
  
  return(d11)
}

# Balanced
d11_BP_Int_adop <- add_adoption_info(
  d11 = d11_BP_Int,
  compliance_df = all_compliance,
  from_col = "from",
  to_col = "to",
  unit_col = "X1",
  year_col = "year",
  complies_col = "complies",
  threshold_col = "Threshold",
  complies_val = 1,
  threshold_val = 5,
  adoption_cutoffs = c(5, 11)  # Early: +5 años desde el mínimo, Middle: +11
)%>%
  mutate(Frontiers=ifelse(Frontiers=="Neighhbors",1,0),
         IS=ifelse(IS=="Within the IS",1,0))

#Adding Core number variable
d11_BP_Int_adop=d11_BP_Int_adop%>%left_join(
  ed3_lc,by=c("Year"="Year","from"="State"))%>%
  rename("Core_from"="Core")%>%
  left_join(
    ed3_lc,by=c("Year"="Year","to"="State"))%>%
  rename("Core_to"="Core")%>%
  mutate(core_num=Core_from+Core_to)



d11_BP_Int_adop=d11_BP_Int_adop%>%
  filter(from!="KP"&to!="KP")%>%
  left_join(globalSouthCountries,by=c("from"="X1"))%>%
  rename(globalSouth_from=globalSouth,country_from=country)%>%
  left_join(globalSouthCountries,by=c("to"="X1"))%>%
  rename(globalSouth_to=globalSouth,country_to=country)%>%
  mutate(GS_group=ifelse(globalSouth_from==globalSouth_to&globalSouth_to=="Yes","Within Global South",
                         ifelse(globalSouth_from==globalSouth_to&globalSouth_to=="No","Within Global North",
                                ifelse(globalSouth_from!=globalSouth_to,"Between Global South and North",NA))))%>%
  mutate(GN=ifelse(GS_group=="Within Global South",0,
                   ifelse(GS_group=="Between Global South and North",1,
                          ifelse(GS_group=="Within Global North",2,NA))))


# Ading frontiers
d11_BP_Int_adop=d11_BP_Int_adop%>%mutate(Frontiers1=ifelse(paste0(from,"-",to)%in%neighbors$neighbors1,1,0))

#------------
# Unbalanced
d11_UBP_Int_adop <- add_adoption_info(
  d11 = d11_UBP_Int,
  compliance_df = all_compliance,
  from_col = "from",
  to_col = "to",
  unit_col = "X1",
  year_col = "year",
  complies_col = "complies",
  threshold_col = "Threshold",
  complies_val = 1,
  threshold_val = 5,
  adoption_cutoffs = c(5, 11)  # Early: +5 años desde el mínimo, Middle: +11
)%>%
  mutate(Frontiers=ifelse(Frontiers=="Neighhbors",1,0),
         IS=ifelse(IS=="Within the IS",1,0))

#Adding Core number variable
d11_UBP_Int_adop=d11_UBP_Int_adop%>%left_join(
  ed3_lc,by=c("Year"="Year","from"="State"))%>%
  rename("Core_from"="Core")%>%
  left_join(
    ed3_lc,by=c("Year"="Year","to"="State"))%>%
  rename("Core_to"="Core")%>%
  mutate(core_num=Core_from+Core_to)

d11_UBP_Int_adop=d11_UBP_Int_adop%>%
  filter(from!="KP"&to!="KP")%>%
  left_join(globalSouthCountries,by=c("from"="X1"))%>%
  rename(globalSouth_from=globalSouth,country_from=country)%>%
  left_join(globalSouthCountries,by=c("to"="X1"))%>%
  rename(globalSouth_to=globalSouth,country_to=country)%>%
  mutate(GS_group=ifelse(globalSouth_from==globalSouth_to&globalSouth_to=="Yes","Within Global South",
                         ifelse(globalSouth_from==globalSouth_to&globalSouth_to=="No","Within Global North",
                                ifelse(globalSouth_from!=globalSouth_to,"Between Global South and North",NA))))%>%
  mutate(GN=ifelse(GS_group=="Within Global South",0,
                   ifelse(GS_group=="Between Global South and North",1,
                          ifelse(GS_group=="Within Global North",2,NA))))


# Ading frontiers
d11_UBP_Int_adop=d11_UBP_Int_adop%>%mutate(Frontiers1=ifelse(paste0(from,"-",to)%in%neighbors$neighbors1,1,0))




# Adoption groups dataset
adopt_groups=d11_BP_Int_adop%>%dplyr::select(c(from,Adop_Group_from))%>%distinct()%>%
  rename(Adop_Group=Adop_Group_from,country=from)%>%
  bind_rows(d11_BP_Int_adop%>%dplyr::select(c(to,Adop_Group_to))%>%distinct()%>%
              rename(Adop_Group=Adop_Group_to,country=to))%>%distinct()
#---------------------------------------------
# Getting the data for a particular country
#---------------------------------------------
countries=unique(c(d11_BP_Int_adop$from,d11_BP_Int_adop$to))

d11_BP_Int_adop_country=NULL

for (cou in countries) {
  d11_BP_Int_adop_country=d11_BP_Int_adop_country%>%
    bind_rows(d11_BP_Int_adop%>%
                filter(grepl(cou,from)|grepl(cou,to))%>%
                mutate(country=cou)%>%mutate(GS_country=ifelse(country==country_from,globalSouth_from,globalSouth_to)))
  print(cou)
}

countries_data=adopt_groups%>%
  left_join(country_list%>%dplyr::select(-c(X2)),by=c("country"="X1"))%>%
  left_join(globalSouthCountries%>%dplyr::select(-c(country)),by=c("country"="X1"))%>%
  left_join(d11_BP_Int_adop_country%>%group_by(country)%>%summarise(Links=n_distinct(key)),by=c("country"))%>%
  mutate(X9_names = case_when(
    X9 == "AS" ~ "Asia",
    X9 == "EU" ~ "Europe",
    X9 == "NA" ~ "North America",
    X9 == "AF" ~ "Africa",
    X9 == "AN" ~ "Antarctica",
    X9 == "SA" ~ "South America",
    X9 == "OC" ~ "Oceania"
  ))%>%
  left_join(data.frame(X9_names = c("Asia", "Europe", "North America", "Africa", "Antarctica", "South America", "Oceania"),
                       Color = c("#FF3030", "#1E90FF", "#698B22", "#FFD700", "#BF3EFF", "#FF8C00", "cyan1")),by="X9_names")%>%
  filter(!X5%in%c("England","Scotland","Wales","North Ireland"))%>%
  mutate(X5=ifelse(X5=="United Kingdom","Great Britain",X5))
#---------------------------
# Adding WB extra variables
#---------------------------

#------------------------------------------------------
# Adding extra GN-Gs clasification based on latitude and High/Low Income countries using WB Data
#------------------------------------------------------
# Most used R package for latitude and longitude countries
# Download data on populated places (includes capitals)
capital_points <- ne_download(
  scale = "medium",
  type = "populated_places",
  category = "cultural",
  returnclass = "sf"
)

# Filter only national capitals
capital_data <- capital_points %>%
  filter(FEATURECLA == "Admin-0 capital") %>%
  dplyr::select(NAME, ADM0NAME, ISO_A2, LONGITUDE, LATITUDE) %>%
  rename(
    capital = NAME,
    country = ADM0NAME,
    iso2c = ISO_A2,
    capital_long = LONGITUDE,
    capital_lat = LATITUDE
  ) %>%
  st_drop_geometry()

library(wbstats)
# Get country metadata including income levels
wb_countries <- wb_countries()


WB_extra=read.csv("WB Data extra variables 04082025.csv",header = T)%>%
  select(-c(X))%>%
  group_by(country) %>%
  arrange(year, .by_group = TRUE) %>% 
  mutate(
    gdp_pc1 = ifelse(gdp_pc < 0, lead(gdp_pc), gdp_pc)
  ) %>%
  ungroup()
#-------------------------------
# Computing relative_GDP_ij
#-------------------------------
d11_BP_Int_adop=
  d11_BP_Int_adop%>%
    left_join(WB_extra%>%select(c(iso2c,year,gdp_pc1,population)),by=c("from"="iso2c","Year"="year"))%>%
    rename("gdp_pc_from"="gdp_pc1",
           "population_from"="population")%>%
    left_join(WB_extra%>%select(c(iso2c,year,gdp_pc1,population)),by=c("to"="iso2c","Year"="year"))%>%
    rename("gdp_pc_to"="gdp_pc1",
           "population_to"="population")%>%
  mutate(rel_GDP=abs(log(gdp_pc_from/gdp_pc_to)))





#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
# FIGURES
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
#------------------------------------------------------------------------
# Figure 1:
# Descriptive Schematic introducing variables and Staggered treatment
#------------------------------------------------------------------------
# Number of new entries
data_fig1=
  all_compliance%>%
  filter(Threshold==5&X1%in%countries)%>%
  left_join(globalSouthCountries%>%dplyr::select(-country),by=c("X1"))%>%
  filter(complies==1)%>%
  group_by(X9,globalSouth,X1,`Country Name`)%>%
  summarise(min_year=min(year))%>%
  filter(X1%in%unique(c(d11_BP_Int_adop$from,d11_BP_Int_adop$to)))%>%
  mutate(X9_names = case_when(
    X9 == "AS" ~ "Asia",
    X9 == "EU" ~ "Europe",
    X9 == "NA" ~ "North America",
    X9 == "AF" ~ "Africa",
    X9 == "AN" ~ "Antarctica",
    X9 == "SA" ~ "South America",
    X9 == "OC" ~ "Oceania"
  ),
  globalSouth=ifelse(globalSouth=="Yes","Global South",
                     ifelse(globalSouth=="No","Global North",NA)))%>%
  left_join(data.frame(X9_names = c("Asia", "Europe", "North America", "Africa", "Antarctica", "South America", "Oceania"),
                       Color = c("#FF3030", "#1E90FF", "#698B22", "#FFD700", "#BF3EFF", "#FF8C00", "cyan1")),by="X9_names")%>%
  left_join(wb_countries%>%dplyr::select(iso2c,latitude, income_level), by = c("X1" = "iso2c"))%>%
  mutate(Lat_GS=ifelse(latitude>0,"Global North","Global South"),Inc_GS=ifelse(income_level!="High income","Global South","Global North"))

#Original GS-GN clasification
data_fig1_GS=data_fig1%>%
  group_by(X9,X9_names,Color,globalSouth,min_year)%>%
  summarise(count=n())%>%
  group_by(globalSouth) %>%
  arrange(min_year, .by_group = TRUE) %>%
  mutate(
    group_total = sum(count),
    cumulative_number = round(cumsum(count) * 100 / group_total, 2)
  ) %>%
  ungroup()

# Year where GS and Gn reached 80% of countries with 5% of Internet Access
GN_80=min(data_fig1_GS$min_year[data_fig1_GS$globalSouth=="Global North"&data_fig1_GS$cumulative_number>=80])
GS_80=min(data_fig1_GS$min_year[data_fig1_GS$globalSouth=="Global South"&data_fig1_GS$cumulative_number>=80])


vline_data <- data.frame(
  globalSouth = c("Global North", "Global South"),
  year_line = c(GN_80, GS_80)
)
label_data <- vline_data %>%
  mutate(
    label = as.character(year_line),
    y = 0  # puedes ajustarlo si quieres ponerlo más arriba
  )


data_fig1_GS %>%
  ggplot(aes(x = min_year, y = count, fill = X9_names)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = setNames(data_fig1$Color, data_fig1$X9_names)) +
  
  # Línea vertical por grupo
  geom_vline(data = vline_data, aes(xintercept = year_line), 
             linetype = "dashed", color = "black", linewidth = 1) +
  
  # Etiqueta sobre la línea
  geom_text(
    data = label_data,
    aes(x = year_line+3, y = y, label = label),
    inherit.aes = FALSE,
    size = 15,
    vjust = -13.5
  ) +
  labs(
    x = "",
    y = "Number of new countries with Internet Access",
    fill = "Continent"
  ) +
  theme_classic()+
  facet_wrap(~globalSouth,scales="free_x")+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        axis.text.x = element_text(size = 35, face = "bold"), 
        axis.text.y = element_text(size = 35, face = "bold"), 
        axis.title = element_blank(),
        legend.title = element_blank(),
        strip.text = element_text(size = 50, face = "bold"),
        legend.text = element_text(size = 30),
        legend.position = "none",
        plot.margin = margin(t = 1, r = 2, b = 1, l = 1, unit = "cm"),
        panel.spacing = unit(4, "cm"),
        strip.placement = "outside"
  ) +
  scale_x_continuous(guide = "prism_minor", 
                     limits = c(1993, 2015),
                     expand = c(0, 0),
                     minor_breaks = seq(1993, 2015, 1))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 9),breaks = seq(0,9,2))

ggsave(paste0("Figures draft/raw output figures/1. Fig 1 Internet access of countries overtime.pdf"), width =30, height = 20, units = "in",limitsize = FALSE)

#------------------------------------------------------------------------
# Figure 2:
# Descriptive graphs by country (Core groups and adoption groups)
#------------------------------------------------------------------------
countries_desc=c("US","SG","CN","TR")
#cou="CN"
# plot(y=plot_data$weight[plot_data$to=="HK"],x=plot_data$Year[plot_data$to=="HK"],type = "l")
#plot(y=plot_data$weight[plot_data$to=="HK"],x=plot_data$Year[plot_data$to=="HK"])

#----------------------
# Average JSI before and after
# Global south and global north 
fig2_extraGS_GN=d11_BP_Int_adop_country %>%
  # Crear GS_country y min_year
  mutate(
    GS_country = ifelse(from == country, globalSouth_from, globalSouth_to),
    min_year = ifelse(from == country, min_year_from, min_year_to),
    before_after = ifelse(Year < min_year, 0, 1)
  ) %>%
  
  # Promedio por relación antes/después
  group_by(GS_country, country, key, before_after) %>%
  summarise(mean_before = round(100 * mean(weight, na.rm = TRUE), 2), .groups = "drop") %>%
  
  # Extraer country_2 con str_replace usando expresiones regulares
  mutate(
    country_2 = str_replace(key, paste0("^", country, "_|_", country, "$"), "")
  ) %>%
  #View()
  # Unir con grupos de adopción
  
  #View()
  
  # Promediar por grupo final
  group_by(GS_country, country, before_after) %>%
  summarise(mean = mean(mean_before, na.rm = TRUE), .groups = "drop") %>%
  left_join(adopt_groups%>%mutate(Adop_Group=ifelse(Adop_Group=="Early Adopter","Early Adopter","Late Adopter")), by = c("country" = "country")) %>%
  mutate(GS_country=ifelse(GS_country=="Yes","Global South","Global North"))


extra_mean_fig2=
  d11_BP_Int_adop_country%>%filter(country%in%countries_desc)%>%
  mutate(min_year=ifelse(from==country,min_year_from,min_year_to),
         before_after=ifelse(Year<min_year,0,1))%>%
  group_by(country,key,before_after)%>%
  summarise(mean_before=round(100*mean(weight),2))%>%
  mutate(country_2 = str_replace(key, paste0("^", country, "_|_", country, "$"), ""))%>%
  left_join(adopt_groups%>%mutate(Adop_Group=ifelse(Adop_Group=="Early Adopter","Early Adopter","Late Adopter")),by=c("country_2"="country"))%>%
  left_join(globalSouthCountries%>%rename("country_2_name"="country"),by=c("country_2"="X1"))%>%
  left_join(globalSouthCountries%>%rename("country_name"="country")%>%dplyr::select(-globalSouth),by=c("country"="X1"))%>%
  dplyr::select(-c(key,globalSouth))%>%
  group_by(country_name,before_after)%>%
  summarise(mean=mean(mean_before),sd=sd(mean_before))%>%
  bind_rows(fig2_extraGS_GN%>%group_by(GS_country,before_after)%>%summarise(mean1=mean(mean),sd=sd(mean,na.rm = T))%>%
              rename("country_name"="GS_country","mean"="mean1"))%>%
  mutate(country_name = factor(country_name, levels = c("Global North","United States", "Turkey", "Global South","Singapore","China")))


#-------------
# T test
#-------------
t_test_data=fig2_extraGS_GN %>%
  mutate(before_after = ifelse(before_after == 0, "before", "after")) %>%
  pivot_wider(
    names_from = before_after,
    values_from = mean
  )%>%
  mutate(ratio=before/after)

# Perform two-sample t-test on ratio by GS_country
t_test_result <- t.test(ratio ~ GS_country, data = t_test_data)

# View the result
t_test_result

#-------------------------------------
d11_BP_Int_adop_country%>%filter(country%in%countries_desc)%>%
  mutate(min_year=ifelse(from==country,min_year_from,min_year_to),
         before_after=ifelse(Year<min_year,0,1))%>%
  group_by(country,key,before_after)%>%
  summarise(mean_before=round(100*mean(weight),2))%>%
mutate(country_2 = str_replace(key, paste0("^", country, "_|_", country, "$"), ""))%>%
  left_join(adopt_groups%>%mutate(Adop_Group=ifelse(Adop_Group=="Early Adopter","Early Adopter","Late Adopter")),by=c("country_2"="country"))%>%
  left_join(globalSouthCountries%>%rename("country_2_name"="country"),by=c("country_2"="X1"))%>%
  left_join(globalSouthCountries%>%rename("country_name"="country")%>%dplyr::select(-globalSouth),by=c("country"="X1"))%>%
  dplyr::select(-c(key,globalSouth))%>%
  bind_rows(fig2_extraGS_GN%>%rename("country_2"="country",
                                     "country"="GS_country",
                                     "mean_before"="mean")%>%
              mutate(country_name=country,
                     country_2_name=country_2))%>%
  #View()
  mutate(country_name = factor(country_name, levels = c("Global North","United States", "Turkey", "Ukraine","Global South","Singapore","China","India")),
         Adop_Group=factor(Adop_Group,levels = c("Early Adopter","Late Adopter")))%>%
  ggplot(aes(x = before_after, y = mean_before, colour =Adop_Group ,group = country_2)) + #linetype = globalSouth
  geom_line(alpha = 0.4,size=8) +
  geom_point(alpha = 0.8,size=13) +
  geom_line(data = extra_mean_fig2,
            aes(x = before_after, y = mean),
            color = "black",  size = 8, inherit.aes = FALSE) +
  geom_point(data = extra_mean_fig2,
             aes(x = before_after, y = mean),
             color = "black",  size = 13, inherit.aes = FALSE) +
  facet_wrap(~country_name,scales="free_y", nrow = 2, ncol = 3)+
  #scale_linetype_manual(values = c("Yes" = "dashed", "No" = "solid")) + 
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        axis.text.x = element_text(size = 100, face = "bold"), 
        axis.text.y = element_text(size = 100, face = "bold"), 
        axis.title = element_blank(),
        legend.title = element_blank(),
        strip.text = element_text(size = 100, face = "bold"),
        legend.text = element_text(size = 100, face = "bold"),
        legend.position = "none",
        plot.margin = margin(t = 1, r = 2, b = 1, l = 1, unit = "cm"),
  ) +
  scale_x_continuous(
    breaks = c(0, 1),
    labels = c("B", "A")
  )
ggsave(paste0("Figures draft/raw output figures/2. Figure 2 JSI_before_after_selected countries 22072025 Adopters.pdf"), width = 50, height = 50, units = "in",limitsize = FALSE)
# Change with new figure 2
#----------------------------
# Figure 3: modeling
#----------------------------
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
      rename(!!unit_col := key) %>%
      dplyr::select(all_of(c(base_vars, control_vars))) %>%
      left_join(
        data_raw %>%
          filter(.data[[time_col]] >= min_year,
                 .data[[treatment_col]] == 1) %>%
          rename(!!unit_col := key) %>%
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
    ref.line = -0.5#,
    #plot = FALSE
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
    ylab = "Estimated Effect (95% CI)",
    ref.line = -0.5,
    grid = TRUE,
    xlim = x_lim,
    ylim = y_lim
  )
  
  #  event_study_plot <- fixest::iplot(
  #   es_model,
  #  main = "Event Study: Staggered Treatment Effect",
  # xlab = "Relative Time to Treatment",
  #ylab = "Estimated Effect with 95% CI",
  #col = "navy",
  #pch = 16,
  #lwd = 4,
  #ci.col = "skyblue",
  #ci.lwd = 4,
  #ci.alpha = 0.4,
  #ref.line = -0.5,
  #ref.col = "black",
  #ref.lwd = 2,
  #grid = TRUE,
  #xlim = x_lim,
  #ylim = y_lim,
  #cex.main = 20,
  #legend = TRUE,
  #legend.pos = "topleft",
  #legend.col = c("navy", "skyblue"),
  #legend.pch = c(16, NA),
  #legend.lwd = c(3, 2)#,
  #plot = TRUE
  #)
  
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


# Function for all countries
run_did2s_analysis_country <- function(data_raw,
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
                                       GS_group_cou=NULL,
                                       time_indep_controls = NULL,
                                       time_dep_controls = NULL,
                                       adopt_groups=NULL,
                                       country_dataset=NULL,
                                       country_name=NULL
) {
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
      rename(!!unit_col := key) %>%
      dplyr::select(all_of(c(base_vars, control_vars))) %>%
      left_join(
        data_raw %>%
          filter(.data[[time_col]] >= min_year,
                 .data[[treatment_col]] == 1) %>%
          rename(!!unit_col := key) %>%
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
    first_stage = f_first_stage,#~0 +  Scaled_km + same_language +  edge_page_rank | Links + Year,#f_first_stage,#~0 + Frontiers + IS+Scaled_km+same_language+edge_page_rank| Links + Year ,#~0 + IS + same_language + Frontiers + Scaled_km + edge_page_rank | Links + Year,# ,
    second_stage = f_second_static,
    cluster_var = unit_col,
    verbose = FALSE
  )
  
  #summary(did2s_model)
  
  #cor(panel_data_balanced[, c( "weight","Frontiers", "same_language", "Scaled_km","GN", "edge_page_rank")], use = "complete.obs")
  static_summary <- summary(did2s_model)
  
  est_table <- fixest::etable(
    did2s_model,
    fitstat = c("n"),
    title = "Estimate of Static TWFE Model",
    notes = "Estimated using Two-Stage DiD per Gardner (2021)."
  )
  pdf_filename1 <- paste0("Figures draft/Did2s Figures Countries/static_plot_",adopt_groups$Adop_Group[adopt_groups$country==country_name],"_", country_dataset$X5[country_dataset$X1==country_name],"_",GS_group_cou, ".pdf")
  pdf(file = pdf_filename1, width = 8, height = 6)
  static_plot <- fixest::iplot(
    did2s_model,
    main = "Event study: Staggered treatment",
    xlab = "Relative time to treatment",
    col = "steelblue",
    ref.line = -0.5
  )
  dev.off()
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
  pdf_filename <- paste0("Figures draft/Did2s Figures Countries/event_study_",adopt_groups$Adop_Group[adopt_groups$country==country_name],"_",country_dataset$X5[country_dataset$X1==country_name],"_",GS_group_cou, ".pdf")
  pdf(file = pdf_filename, width = 8, height = 6)
  event_study_plot <- fixest::iplot(
    es_model,
    main = "Event Study: Staggered Treatment Effect",
    xlab = "Relative Time to Treatment",
    ylab = "Estimated Effect (95% CI)",
    ref.line = -0.5,
    grid = TRUE,
    xlim = x_lim,
    ylim = y_lim
  )
  
  #  event_study_plot <- fixest::iplot(
  #   es_model,
  #  main = "Event Study: Staggered Treatment Effect",
  # xlab = "Relative Time to Treatment",
  #ylab = "Estimated Effect with 95% CI",
  #col = "navy",
  #pch = 16,
  #lwd = 4,
  #ci.col = "skyblue",
  #ci.lwd = 4,
  #ci.alpha = 0.4,
  #ref.line = -0.5,
  #ref.col = "black",
  #ref.lwd = 2,
  #grid = TRUE,
  #xlim = x_lim,
  #ylim = y_lim,
  #cex.main = 20,
  #legend = TRUE,
  #legend.pos = "topleft",
  #legend.col = c("navy", "skyblue"),
  #legend.pch = c(16, NA),
  #legend.lwd = c(3, 2)#,
  #plot = TRUE
  #)
  dev.off()
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



#---------------
# full dataset and WB Data
#---------------
# all data - page rank - GDP
results <- run_did2s_analysis(
  data_raw = d11_BP_Int_adop%>%
    mutate(GN1=ifelse(GN==2,1,0)),
  y_col = "weight",
  treatment_col = "Internet",
  unit_col = "Links",
  time_col = "Year",
  real_time_col = "real_time",
  truncate = TRUE,
  max_year = 2014,
  min_year = 1990,
  y_lim=c(-0.0001,0.0008),
  x_lim=c(-10,10),
  controls = T,
  time_indep_controls = c("Frontiers","IS","Scaled_km","same_language","GN1"),
  time_dep_controls = c("edge_page_rank","rel_GDP")
)

# Graph for the static plot
results$static_plot
df_plot <- results$static_plot$prms

# Convertimos x a numérico si no lo es (por seguridad)
df_plot <- df_plot %>%
  mutate(x = as.numeric(x))%>%
  filter(x==1)


# Gráfico Static plot
ggplot(df_plot, aes(x = x, y = estimate)) +
  geom_point(color = "blue", size = 5) +  # estimación puntual
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.003, color = "skyblue", size = 2) +  # IC
  # geom_vline(xintercept = 0, linetype = "dashed", color = "black") +  # línea del tratamiento
  geom_hline(yintercept = 0, color = "black") +  # línea de referencia
  labs(
    x = "Relative Time to Treatment",
    y = "Estimated Effect with 95% CI",
    title = "Event Study: Staggered Treatment Effect"
  )+
  #geom_vline(xintercept = 0.5, color = "black") +
  theme_classic()+
  scale_x_continuous(breaks = c( 1))+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        axis.text.x = element_blank(), 
        axis.text.y = element_text(size = 30, face = "bold"), 
        axis.title = element_blank(),
        legend.title = element_blank(),
        plot.margin = margin(t = 1, r = 6, b = 1, l = 6, unit = "cm"),
        #strip.text = element_text(size = 10, face = "bold"),
        #legend.text = element_text(size = 100, face = "bold"),
        #legend.position = "none"
  )

ggsave(paste0("Figures draft/raw output figures/3. Figure 3 bottom-right inset Tau.pdf"), width = 10, height = 10, units = "in",limitsize = FALSE)



#---------------------------------
# Graph for the case event plot
#---------------------------------
results$event_study_plot
df_plot <- results$event_study_plot$prms

# Convertimos x a numérico si no lo es (por seguridad)
df_plot <- df_plot %>%
  mutate(x = as.numeric(x))


# Gráfico estilo event study
df_plot%>%
  mutate(ci_low=ifelse(x==-1,NA,ci_low),
         ci_high=ifelse(x==-1,NA,ci_high))%>%
  ggplot(aes(x = x, y = estimate)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.8, color = "skyblue", size = 2) +  # IC
  geom_point(color = "blue", size = 5) +  # estimación puntual
  # geom_vline(xintercept = 0, linetype = "dashed", color = "black") +  # línea del tratamiento
  geom_hline(yintercept = 0, color = "black") +  # línea de referencia
  labs(
    x = "Relative Time to Treatment",
    y = "Estimated Effect with 95% CI",
    title = "Event Study: Staggered Treatment Effect"
  )+
  geom_vline(xintercept = -0.5, color = "black") +
  theme_classic()+
  scale_x_continuous(breaks = seq(-10, 10, 5), expand = expansion(mult = 0.02)) +
  coord_cartesian(xlim = c(-10, 10), ylim = c(-0.0001, 0.0008))+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        axis.text.x = element_text(size = 30, face = "bold"), 
        axis.text.y = element_text(size = 30, face = "bold"), 
        axis.title = element_blank(),
        legend.title = element_blank(),
        #strip.text = element_text(size = 10, face = "bold"),
        #legend.text = element_text(size = 100, face = "bold"),
        #legend.position = "none"
  )

ggsave(paste0("Figures draft/raw output figures/3. Figure 3A real treatment.pdf"), width = 10, height = 10, units = "in",limitsize = FALSE)


#--------------------
# Placebo
#--------------------
d11_BP_Int_adop_placebo=d11_BP_Int_adop%>%filter(Internet!=1)%>%
  mutate(treat_placebo=complies_from+complies_to)

results <- run_did2s_analysis(
  data_raw = d11_BP_Int_adop_placebo%>%
    mutate(GN1=ifelse(GN==2,1,0)),
  y_col = "weight",
  treatment_col = "treat_placebo",
  unit_col = "Links",
  time_col = "Year",
  real_time_col = "real_time",
  truncate = TRUE,
  max_year = 2014,
  min_year = 1990,
  y_lim=c(-0.0008,0.0003),
  x_lim=c(-10,10),
  controls = T,
  time_indep_controls = c("Frontiers","IS","Scaled_km","same_language","GN1"),
  time_dep_controls = c("edge_page_rank","rel_GDP")
)

#---------------------------------
# Graph for the case event plot
#---------------------------------
results$event_study_plot
df_plot_placebo <- results$event_study_plot$prms


# Convertimos x a numérico si no lo es (por seguridad)
df_plot_placebo <- df_plot_placebo %>%
  mutate(x = as.numeric(x))


# Gráfico estilo event study
df_plot%>%
  filter(x>=-10&x<=10)%>%
  mutate(ci_low=ifelse(x==-1,NA,ci_low),
         ci_high=ifelse(x==-1,NA,ci_high))%>%
  ggplot(aes(x = x, y = estimate)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.8, color = "#1E90FF", size = 4) +  # IC
  geom_point(color = "darkblue", size = 8) +  # estimación puntual
  # Placebo
  geom_errorbar(data = df_plot_placebo%>%
                  filter(x!=-1&x>=-10&x<=10),
                aes(ymin = ci_low, ymax = ci_high), width = 0.8, color = "darkred", size = 2,alpha=0.3) +  # IC
  geom_point(data = df_plot_placebo%>%
               filter(x!=-1&x>=-10&x<=10),
             aes(x = x, y = estimate),color = "darkred", size = 5,alpha=0.45) +  # estimación puntual
  # geom_vline(xintercept = 0, linetype = "dashed", color = "black") +  # línea del tratamiento
  geom_hline(yintercept = 0, color = "black") +  # línea de referencia
  labs(
    x = "Relative Time to Treatment",
    y = "Estimated Effect with 95% CI",
    title = ""
  )+
  geom_vline(xintercept = -0.5, color = "black") +
  theme_classic()+
  scale_x_continuous(breaks = seq(-10, 10, 5)) +
  coord_cartesian(xlim = c(-10, 10), ylim = c(-0.0007, 0.0008))+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        axis.text.x = element_text(size = 50, face = "bold"), 
        axis.text.y = element_text(size = 50, face = "bold"), 
        axis.title = element_blank(),
        legend.title = element_blank(),
        plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "cm"),
        #strip.text = element_text(size = 10, face = "bold"),
        #legend.text = element_text(size = 100, face = "bold"),
        #legend.position = "none"
  )

ggsave("Figures draft/raw output figures/3. Figure 3 A event study plot with placebo.pdf",
       width = 45,                         # Specify the width in inches
       height = 25,                        # Specify the height in inches
       units = "in")



#------------------
# All countries
#------------------
#---------------------------------------------
# Concatenate data for a particular country
#---------------------------------------------
countries=unique(c(d11_BP_Int_adop$from,d11_BP_Int_adop$to))

d11_BP_Int_adop_country=NULL

for (cou in countries) {
  d11_BP_Int_adop_country=d11_BP_Int_adop_country%>%
    bind_rows(d11_BP_Int_adop%>%
                filter(grepl(cou,from)|grepl(cou,to))%>%
                mutate(country=cou)%>%mutate(GS_country=ifelse(country==country_from,globalSouth_from,globalSouth_to)))
  print(cou)
}

# Table with the number of links by country
country_links=d11_BP_Int_adop_country%>%
  filter(weight>0&country!="KE")%>%
  mutate(country2=ifelse(from==country,to,from))%>%
  #left_join(adopt_groups,by="country")%>%
  group_by(country,country2)%>%
  summarise(Count_JSIno0=n_distinct(Year))%>%
  #View()
  mutate(no0=ifelse(Count_JSIno0>=21,1,0))%>%
  group_by(country)%>%
  summarise(Count=sum(no0),n_links=n())%>%
  mutate(prop=round(Count*100/n_links,2))#%>%
#View()

countries20min=country_links$country[country_links$prop>=50]

#------------------
# Running the model in all the countries
country_results <- list()

for (cou in countries20min) {
  tryCatch({
    results <- run_did2s_analysis_country(
      data_raw = d11_BP_Int_adop_country %>%
        filter(country == cou) %>%
        dplyr::select(-country)%>%
        mutate(GN1=ifelse(GN==2,1,0)),
      y_col = "weight",
      treatment_col = "Internet",
      unit_col = "Links",
      time_col = "Year",
      real_time_col = "real_time",
      truncate = TRUE,
      max_year = 2014,
      min_year = 1990,
      GS_group_cou=d11_BP_Int_adop_country$GS_country[d11_BP_Int_adop_country$country==cou],
      country_dataset=country_list,
      y_lim = c(-0.002, 0.006),
      x_lim = c(-15, 15),
      country_name = cou,
      adopt_groups = adopt_groups,
      controls = TRUE,
      time_indep_controls = c("Frontiers","IS","Scaled_km","same_language","GN1"),
      time_dep_controls = c("edge_page_rank","rel_GDP")
    )
    
    country_results[[cou]] <- results
    print(paste("Finished:", cou))
  },
  error = function(e) {
    message(paste("Error in", cou, ":", e$message))
  })
}


library(purrr)
# Extraer y combinar todos los prms
all_prms <- map_dfr(names(country_results), function(cou) {
  result <- country_results[[cou]]
  
  if (!is.null(result$static_plot$prms)) {
    result$static_plot$prms %>%
      mutate(country = cou)  # Agrega el nombre del país
  }
})

prms_x1 <- all_prms %>%
  filter(x == 1)%>%left_join(countries_data,by="country")%>%
  mutate(Adop_Group=factor(Adop_Group,levels = c("Early Adopter","Middle Adopter","Late Adopter")))

library(ggplot2)

ggplot(prms_x1, aes(x = reorder(X5, estimate), y = estimate)) +
  geom_point(aes(shape = globalSouth, color = X9_names), size = 9) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high, color = X9_names), width = 0.8, size = 2) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  scale_color_manual(values = setNames(countries_data$Color, countries_data$X9_names)) +
  #facet_wrap(~Adop_Group, scales = "free_x") +
  labs(
    title = "",
    x = "Country",
    y = "Estimated Effect",
    color = "Continent",
    shape = "Global South"
  )+
  theme_classic()+
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        axis.text.y = element_text(size = 50, face = "bold"), 
        axis.title = element_blank(),
        legend.title = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size = 50, face = "bold"),
        panel.grid.major.x = element_blank(),
        #strip.text = element_text(size = 10, face = "bold"),
        #legend.text = element_text(size = 100, face = "bold"),
        legend.position = "none"
  )


ggsave("Figures draft/raw output figures/3. Figure 3 B static_plot by country.pdf",
       width = 45,                         # Specify the width in inches
       height = 25,                        # Specify the height in inches
       units = "in")


#------------------
# Mini Fig 3b

# Extraer y combinar todas las estimaciones del efecto de tratamiento
library(tibble)

all_coeff <- map_dfr(names(country_results), function(cou) {
  result <- country_results[[cou]]
  
  if (!is.null(result$static_model$coefficients)) {
    tibble(
      country = cou,
      estimate = as.numeric(result$static_model$coefficients[1])
    )
  }
})

all_coeff1 <- all_coeff %>%
  left_join(countries_data,by="country")%>%
  mutate(Adop_Group=factor(Adop_Group,levels = c("Early Adopter","Middle Adopter","Late Adopter")))%>%
  left_join(d11_BP_Int_adop_country%>%filter(country%in%names(country_results))%>%
              mutate(min_year=ifelse(from==country,min_year_from,min_year_to),
                     before_after=ifelse(Year<min_year,0,1))%>%
              filter(before_after==0)%>%
              group_by(country,before_after)%>%
              summarise(mean_before=mean(weight))%>%
              dplyr::select(-c(before_after)),by="country")%>%
  mutate(delta=estimate/mean_before,
         globalSouth1 = ifelse(globalSouth == "Yes", "Global South", "Global North"))

# Calculate mean delta per group
delta_means <- all_coeff1 %>%
  group_by(globalSouth1) %>%
  summarise(mean_delta = mean(delta, na.rm = TRUE))

all_coeff1 %>%
  ggplot(aes(x = delta)) +
  geom_histogram(aes(y = after_stat(density)), fill = "#1E90FF", color = "white", bins = 20, alpha = 0.6) +
  geom_density(color = "darkblue", size = 2, alpha = 0.7) +
  geom_vline(data = delta_means, aes(xintercept = mean_delta), color = "red", linetype = "dashed", linewidth = 2) +
  facet_wrap(~globalSouth1, scales = "free_y") +
  labs(
    title = "",
    x = "Delta (Estimate / Mean Before)",
    y = "Count"
  ) +
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.text.x = element_text(size = 20, face = "bold"),
    axis.text.y = element_text(size = 20, face = "bold"),
    axis.title = element_blank(),
    legend.title = element_text(size = 10, face = "bold"),
    panel.grid.major.x = element_blank(),
    strip.text = element_text(size = 50, face = "bold"),
    legend.position = "none"
  )


#-------------------------------
# Figure 4: Regression model
#-------------------------------
#---------------------
# with categorical variables
#-----------------------
data_regresion_v2=NULL
data_regresion_v2_full=NULL
limits_data_v2=NULL

d11_UBP_Int_adop_categorical=d11_UBP_Int_adop %>%
  mutate(Frontiers=factor(Frontiers,levels = c("0","1")),
         IS=factor(IS,levels = c("0","1")),
         same_language=factor(same_language,levels = c("0","1")),
         GN1=ifelse(GN==2,"Within GN","Else"),
         GN1=factor(GN1,levels = c("Else","Within GN")),
         GN=factor(GN,levels = c("0","1","2")))



for (i in unique(d11_UBP_Int_adop_categorical$Year)[unique(d11_UBP_Int_adop_categorical$Year)<2012]) {
  # Filter data for the current year 'i' and create a key variable
  #i=1990
  d11_full_temp <- d11_UBP_Int_adop_categorical %>%
    filter(Year == i) %>%
    dplyr::select(c("IS", "Frontiers", "edge_page_rank", "Scaled_km", "same_language", "Internet", "weight","GN1"))
  
  # Fit the model
  fit7 <- lm(weight ~ IS + Frontiers + Scaled_km +edge_page_rank+ same_language+Internet+GN1, data = d11_full_temp)
  
  # Collect model statistics
  data_regresion_v2 <- data.frame(
    Estimate = ifelse(is.na(fit7$coefficients), NA, fit7$coefficients),
    coeff_name = rownames(data.frame(fit7$coefficients)),
    Year = ifelse(is.na(fit7$coefficients), NA, i),
    p_value = ifelse(is.na(fit7$coefficients), NA, summary(fit7)$coefficients[, 4]),
    r2 = ifelse(is.na(fit7$coefficients), NA, summary(fit7)$r.squared),
    r2_adj = ifelse(is.na(fit7$coefficients), NA, summary(fit7)$adj.r.squared),
    f_stat = ifelse(is.na(fit7$coefficients), NA, summary(fit7)$fstatistic[1])
  )
  
  data_regresion_v2_full <- data_regresion_v2_full %>%
    bind_rows(data_regresion_v2)
  
  limits_data_v2 <- bind_rows(
    limits_data_v2,
    data.frame(confint.lm(fit7)) %>%
      mutate(coeff_name = rownames(data.frame(confint.lm(fit7))), Year = i) %>%
      rename(linf = "X2.5..", lsup = "X97.5..")
  )
  
  print(i)
}

data_regresion_v2_full=data_regresion_v2_full%>%left_join(limits_data_v2,by=c("coeff_name","Year"))%>%
  mutate(significance=ifelse(p_value<0.05,"Yes","No"))



data_regresion_v2_full%>%ggplot(aes(x=Year,y=r2_adj))+
  geom_line()+theme_classic()
# Find the min and max of Estimate for y-axis scaling
min_estimate <- min(data_regresion_v2_full$Estimate)
max_estimate <- max(data_regresion_v2_full$Estimate)

#Part A edge page rank
data_regresion_v2_full %>%
  filter(coeff_name=="edge_page_rank")%>%
  ggplot(aes(x = Year, y = Estimate, color = coeff_name)) + 
  geom_line(size = 10,color="#d62728") +
  labs(y = "Estimation of model coefficients", color = "Explanatory variables") + 
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        legend.title = element_blank(), # Set legend title size
        legend.text = element_text(size = 20, face = "bold"),
        axis.title.x = element_blank(), # Increase x-axis title size
        axis.title.y = element_blank(), # Increase y-axis title size
        axis.text.x = element_text(size = 70, face = "bold"), 
        axis.text.y = element_text(size = 70, face = "bold"),
        legend.position = "none",
        plot.margin = margin(t = 1, r = 2, b = 1, l = 1, unit = "cm"),
  ) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 1995) +
  scale_x_continuous(#guide = "prism_minor", 
    limits = c(1990, 2012),
    expand = c(0, 0),
    minor_breaks = seq(1990, 2012, 1)) +
  geom_point(data = data_regresion_v2_full %>%
               filter(coeff_name=="edge_page_rank")%>%
               mutate(p05 = ifelse(p_value < 0.05, Estimate, NA)),
             aes(x = Year, y = p05, color = coeff_name),size=18,color="#d62728") +
  #scale_color_manual(values = colors,labels = legend_labels) +  # Apply manual color palette
  theme(axis.title.x = element_blank())   # Remove X-axis label
#scale_y_continuous(breaks = seq(floor(min_estimate - 0.005), ceiling(max_estimate + 0.005), by = 0.005))


ggsave("Figures draft/raw output figures/E1B. Extra Fig 1B model coefficients page edge rank.pdf",
         width = 45,                         # Specify the width in inches
         height = 30,                        # Specify the height in inches
         units = "in")

#Part B link group
# Define the existing colorblind-friendly palette with new colors for additional values
colors <- c(
  "(Intercept)" = "#4a4a4a",                          # Dark Gray
  "IS1" = "#ff7f0e",                       # Orange
  "Frontiers1" = "#2ca02c",                   # Medium Green
  #"GN1" = "#1f77b4",  # Dark Blue
  "GN1Within GN" = "darkblue",  # Dark Blue
  "Scaled_km" = "#9467bd",                               # Medium Purple
  "same_language1" = "#ffcc00",                            # Medium Gold
  "edge_page_rank" = "#d62728",
  "Internet"="cyan"
  
  
)

# Define new legend labels
legend_labels <- c(
  "(Intercept)" = "Intercept",                          # Dark Gray
  "IS1" = "Innovation System (EU)",                       # Orange
  "Frontiers1" = "Common Borders",                   # Medium Green
  #"GN1" = "Global North - Global South",  # Dark Blue
  "GN1Within GN" = "Within Global North",  # Dark Blue
  "Scaled_km" = "Scaled Distance",                               # Medium Purple
  "same_language1" = "Same Language",                            # Medium Gold
  "edge_page_rank" = "Edge Page Rank",
  "Internet"="Internet"
)

data_regresion_v2_full %>%
  mutate(Estimate=ifelse(coeff_name=="Internet"&Year%in%c(1994,1995),NA,Estimate))%>%
  filter(!coeff_name%in%c("edge_page_rank","(Intercept)"))%>%
  ggplot(aes(x = Year, y = Estimate, color = coeff_name)) + 
  geom_line(size = 10) +
  scale_color_manual(values = colors, labels = legend_labels)+
  labs(y = "Estimation of model coefficients", color = "Explanatory variables") + 
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
        legend.title = element_blank(), # Set legend title size
        legend.text = element_text(size = 30, face = "bold"),
        axis.title.x = element_blank(), # Increase x-axis title size
        axis.title.y = element_blank(), # Increase y-axis title size
        axis.text.x = element_text(size = 70, face = "bold"), 
        axis.text.y = element_text(size = 70, face = "bold"),
        legend.position = "none",
        plot.margin = margin(t = 1, r = 2, b = 1, l = 1, unit = "cm"),
        
  ) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 1995) +
  scale_x_continuous(#guide = "prism_minor", 
    limits = c(1990, 2012),
    expand = c(0, 0),
    minor_breaks = seq(1990, 2012, 1)) +
  geom_point(data = data_regresion_v2_full %>%
               mutate(Estimate=ifelse(coeff_name=="Internet"&Year%in%c(1994,1995),NA,Estimate))%>%
               filter(!coeff_name%in%c("edge_page_rank","(Intercept)"))%>%
               mutate(p05 = ifelse(p_value < 0.05, Estimate, NA)),
             aes(x = Year, y = p05, color = coeff_name),size=17) +
  #scale_color_manual(values = colors,labels = legend_labels) +  # Apply manual color palette
  theme(axis.title.x = element_blank())   # Remove X-axis label
#scale_y_continuous(breaks = seq(floor(min_estimate - 0.005), ceiling(max_estimate + 0.005), by = 0.005))


ggsave("Figures draft/raw output figures/E1C. Extra Fig 1C model other coefficients with categorical variables.pdf",
       width = 45,                         # Specify the width in inches
       height = 30,                        # Specify the height in inches
       units = "in")

#-----------------------------------
#-------------------------------
# Figure 4: Regression model with Interaction: Internet*Global North
#---------------------------------------------
data_regresion_v2=NULL
data_regresion_v2_full=NULL
limits_data_v2=NULL

for (i in unique(d11_UBP_Int_adop_categorical$Year)[unique(d11_UBP_Int_adop_categorical$Year)<2012]) {
  # Filter data for the current year 'i' and create a key variable
  #i=2000
  d11_full_temp <- d11_UBP_Int_adop_categorical %>%
    #filter(key!="EG_SA")%>%
    filter(Year == i) %>%
    dplyr::select(c("IS", "Frontiers", "edge_page_rank", "Scaled_km", "same_language", "Internet", "weight","GN","GN1"))
  
  #cor(d11_full_temp$edge_page_rank,d11_full_temp$Internet)
  
  # Fit the model
  fit7 <- lm(weight ~ IS + Frontiers + Scaled_km +edge_page_rank+ same_language+Internet*GN1, data = d11_full_temp)
  
  summary(fit7)
  # Collect model statistics
  data_regresion_v2 <- data.frame(
    Estimate = ifelse(is.na(fit7$coefficients), NA, fit7$coefficients),
    coeff_name = rownames(data.frame(fit7$coefficients)),
    Year = ifelse(is.na(fit7$coefficients), NA, i),
    p_value = ifelse(is.na(fit7$coefficients), NA, summary(fit7)$coefficients[, 4]),
    r2 = ifelse(is.na(fit7$coefficients), NA, summary(fit7)$r.squared),
    r2_adj = ifelse(is.na(fit7$coefficients), NA, summary(fit7)$adj.r.squared),
    f_stat = ifelse(is.na(fit7$coefficients), NA, summary(fit7)$fstatistic[1])
  )
  
  data_regresion_v2_full <- data_regresion_v2_full %>%
    bind_rows(data_regresion_v2)
  
  limits_data_v2 <- bind_rows(
    limits_data_v2,
    data.frame(confint.lm(fit7)) %>%
      mutate(coeff_name = rownames(data.frame(confint.lm(fit7))), Year = i) %>%
      rename(linf = "X2.5..", lsup = "X97.5..")
  )
  
  print(i)
}

data_regresion_v2_full=data_regresion_v2_full%>%left_join(limits_data_v2,by=c("coeff_name","Year"))%>%
  mutate(significance=ifelse(p_value<0.05,"Yes","No"))



data_regresion_v2_full%>%ggplot(aes(x=Year,y=r2_adj))+
  geom_line()+theme_classic()
# Define colors for each variable
library(ggplot2)
library(dplyr)


# Find the min and max of Estimate for y-axis scaling
min_estimate <- min(data_regresion_v2_full$Estimate)
max_estimate <- max(data_regresion_v2_full$Estimate)

#Part B link group
# Define the existing colorblind-friendly palette with new colors for additional values
colors <- c(
  "(Intercept)" = "#4a4a4a",                          # Dark Gray
  "IS1" = "#ff7f0e",                       # Orange
  "Frontiers1" = "#2ca02c",                   # Medium Green
  #"GN1" = "#1f77b4",  # Dark Blue
  "GN1Within GN" = "darkblue",  # Dark Blue
  "Scaled_km" = "#9467bd",                               # Medium Purple
  "same_language1" = "#ffcc00",                            # Medium Gold
  "edge_page_rank" = "#d62728",
  "Internet"="black",
  "Internet:GN1Within GN"="mediumvioletred"
  
  
)

# Define new legend labels
legend_labels <- c(
  "(Intercept)" = "Intercept",                          # Dark Gray
  "IS1" = "Innovation System (EU)",                       # Orange
  "Frontiers1" = "Common Borders",                   # Medium Green
  #"GN1" = "Global North - Global South",  # Dark Blue
  "GN1Within GN" = "Within Global North",  # Dark Blue
  "Scaled_km" = "Scaled Distance",                               # Medium Purple
  "same_language1" = "Same Language",                            # Medium Gold
  "edge_page_rank" = "Edge Page Rank",
  "Internet"="Internet",
  "Internet:GN1Within GN"="Internet Within Global North"
)

data_regresion_v2_full %>%
  filter(coeff_name %in% c("Internet:GN1Within GN")) %>%
  ggplot(aes(x = Year, y = Estimate, color = coeff_name)) + 
  geom_line(size = 10) +
  geom_ribbon(aes(ymin = linf, ymax = lsup), alpha = 0.3, color = NA,fill="mediumvioletred")+
  #geom_errorbar(aes(ymin = linf, ymax = lsup), width = 0.3, linewidth = 1.2) +  # <-- Error bars added here
  scale_color_manual(values = colors, labels = legend_labels) +
  labs(y = "Estimation of model coefficients", color = "Explanatory variables") + 
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    legend.title = element_blank(),
    legend.text = element_text(size = 30, face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 70, face = "bold"), 
    axis.text.y = element_text(size = 70, face = "bold"),
    legend.position = "none",
    plot.margin = margin(t = 1, r = 2, b = 1, l = 1, unit = "cm")
  ) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 1995) +
  scale_x_continuous(
    limits = c(1996, 2005),
    breaks = seq(1996, 2005, 2),
    expand = c(0, 0),
    minor_breaks = seq(1996, 2005, 1)
  ) +
  scale_y_continuous(limits = c(-0.0007,0.0023))+
  geom_line(data = data_regresion_v2_full %>%
               filter(coeff_name %in% c("GN1Within GN","Internet")),
             aes(x = Year, y = Estimate, color = coeff_name), size = 10) +
  geom_point(data = data_regresion_v2_full %>%
               filter(coeff_name %in% c("GN1Within GN","Internet","Internet:GN1Within GN")) %>%
               mutate(p05 = ifelse(p_value < 0.05, Estimate, NA)),
             aes(x = Year, y = p05, color = coeff_name), size = 22) +
  theme(axis.title.x = element_blank())


ggsave("Figures draft/raw output figures/4. Figure 4 model interaction.pdf",
       width = 45,                         # Specify the width in inches
       height = 30,                        # Specify the height in inches
       units = "in")
#-------------------------
# Extra figure JSI
#-------------------------
JSI_comp_USAKX10_v2%>% mutate(key=paste0(from,"_",to)) %>% 
  filter(key%in%d11_BP_Int$key)%>% 
  group_by(Year) %>% 
  summarise(AVGJSI = mean(weight, na.rm = TRUE)) %>% 
  ggplot(aes(x = Year, y = AVGJSI)) +
  geom_line(size = 10, color = "purple4") +
  geom_point(size = 20, color = "purple4") +
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 70, face = "bold"),
    axis.text.y = element_text(size = 70, face = "bold"),
    plot.margin = margin(t = 1, r = 4, b = 1, l = 1, unit = "cm")
  ) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 1995) +
  scale_x_continuous(
    limits = c(1980, 2020),
    breaks = seq(1980, 2020, 5),
    expand = c(0, 0),
    minor_breaks = seq(1980, 2020, 1)
  )
ggsave("Figures draft/Figure Extra JSI Average.pdf",
       width = 45,                         # Specify the width in inches
       height = 45,                        # Specify the height in inches
       units = "in")


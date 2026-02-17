#-----------------------------------
# Combine JSI matrices with WoS Email and extra variables
#----------------------------------
#--------------------
# ---- Utilities ----
#--------------------
source("Scripts/utilities.R") #Libraries and functions
#---------------------
# Read files
#---------------------
# Email
Univ_WoS_first_year=read_csv("Outputs/WoS_USA_States first year email and threshold 10202025.csv") %>% select(-c("...1"))
# Distances
Univ_distances = read_csv("Outputs/distances by pair of universities in km.csv")
# Data JSI
JSI_comp_USA142aff=read_csv("Outputs/JSI USA142aff full 10222025.csv")%>% select(-c("...1"))
# Data Nij filtered symmetric long
Data_USA142_Nij=read_csv(file="Outputs/Data Nij USA142aff full 10222025.csv")%>% select(-c("...1"))
# Data Nii by university and year
data_tot_npub_USA142aff=read_csv(file="Outputs/Data Nii per institution USA142aff full 10222025.csv")%>% select(-c("...1"))

# SciSciNet codes
SciSciNet_Affiliations_Univ<- read_delim("Inputs/SciSciNet_Affiliations_AffiliationID_AffiliationName_USStateCode_AIGType_9199.tsv", 
                                                                                            delim = "\t", escape_double = FALSE, 
                                                                                            trim_ws = TRUE,col_names = F) 
colnames(SciSciNet_Affiliations_Univ)=c("university_code","university_name","state_code","type")
# neighbors
neighbors_list=read_csv(file="Outputs/Data neighbors by state USA142aff full 10232025.csv")%>% select(-c("...1"))
colnames(neighbors_list)=c("frontiers")

# Edge page rank and centrality measures by threshold and start year
Univ_edge_page_rank =read_csv(file="Outputs/Data Edge page rank full 10242025.csv")%>% select(-c("...1"))
# Number of links and JSI average by threshold and start year
Univ_number_links =read_csv(file="Outputs/Data Links and JSI full 10242025.csv")%>% select(-c("...1"))

#-----------------------------------------------------
# Combining data to creating dataframe for modeling
#-----------------------------------------------------
# Dependent varible:
# y: JSI (weight) --> JSI_comp_USA142aff
# Independent variables:
# 1. Internet: First instance and certain thresholds --> Univ_WoS_first_year
# 2. Edge page rank: 6 different thresholds and 4 different start dates --> Univ_edge_page_rank
# 3. Distances in km --> Univ_distances
# 4. Frontiers --> neighbors_list
# 5. Within/Across State --> JSI_comp_USA142aff


# key: edge_code="code_min"_"code_max"

# Initial dataframe
# weight: dependent variable 

# Combining with other dataframes
JSI_comp_USA142aff_Modeling=
  JSI_comp_USA142aff %>% 
  # Step 0: Filter out from==to
  filter(from!=to) %>% 
  # Step 1: remove duplicated rows
  distinct(Year, code_min, code_max, .keep_all = TRUE) %>% 
  # Step 2: create edge_code using code sciscinet
  mutate(edge_code=paste0(code_min,"_",code_max)) %>% 
  # Step 3: Merge with distance data frame using SciSciNet code: --> Add distances in Km by pair of universities 
  left_join(Univ_distances,by=c("university_code_from","university_code_to")) %>% 
  # Step 4: Merge with first email data frame using SciSciNet code: --> Add Min year Email first instance 
  left_join(Univ_WoS_first_year %>% clean_names() %>% select(-c(state,university_name)),
            by=c("university_code_from"="university_code","university_from"="university"),
            relationship = "many-to-many") %>% 
  rename("first_year_email_from"="first_year_email","year_at_threshold_from"="year_at_threshold") %>%
  left_join(Univ_WoS_first_year %>% clean_names() %>% select(-state,-university_name),
            by=c("university_code_to"="university_code","university_to"="university","threshold"),
            relationship = "many-to-many") %>% 
  rename("first_year_email_to"="first_year_email","year_at_threshold_to"="year_at_threshold") %>%
  # Step 5: merge with edge page rank data frame: --> Add edge page rank variable 
  left_join(Univ_edge_page_rank  %>% 
              select(c(edge,edge_page_rank,year,threshold,start_year))%>% 
              rename("Nii_threshold"="threshold"),
            by=c("edge_code"="edge","Year"="year"),
            relationship = "many-to-many") %>% 
  mutate(edge_page_rank=ifelse(is.na(edge_page_rank),0,edge_page_rank)) %>%  # fill with 0 when is NA
  # Step 6: Add Nii from and to
  left_join(data_tot_npub_USA142aff,by=c("from","Year"="year")) %>% 
  rename("Nii_from"="tot") %>%
  left_join(data_tot_npub_USA142aff,by=c("to"="from","Year"="year")) %>% 
  rename("Nii_to"="tot") %>% 
  # Step 7: Add Nij
  left_join(Data_USA142_Nij,by=c("from","to","Year","university_code_from","state_code_from","university_code_to","state_code_to","code_min","code_max"))
  

# Create extra variables
JSI_comp_USA142aff_Modeling=JSI_comp_USA142aff_Modeling %>% 
  # Step 1: create internet variables (using first year and also the different thresholds)
  mutate(Internet_first_instance=ifelse(Year>=pmax(first_year_email_from,first_year_email_to),1,0),
         Internet_by_threshold=ifelse(Year>=pmax(year_at_threshold_from,year_at_threshold_to),1,0)) %>%
  # Step 2: Scale distance
  mutate(scaled_km=distance_km/max(distance_km)) %>% 
  # Step 3: Unify university names
  mutate(from = ifelse(from != university_from, university_from, from),
         to = ifelse(to != university_to, university_to, to)) %>% 
  select(-c(university_from,university_to)) %>% 
  # Step 4: Add frontiers variable
  mutate(frontiers=ifelse(paste0(state_code_from,"_",state_code_to)%in%neighbors_list$frontiers,1,0)) %>% 
  # Step 5: Add Within/Across state variable
  mutate(within_state=ifelse(state_code_from==state_code_to,1,0)) 

#JSI_comp_USA142aff_Modeling %>% write.csv("Outputs/JSI matrix complete for modeling unique 10232025.csv")

qsave(JSI_comp_USA142aff_Modeling, "Outputs/JSI matrix complete for modeling unique 10242025.qs", preset = "high")


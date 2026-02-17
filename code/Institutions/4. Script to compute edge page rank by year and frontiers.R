#-----------------------------------
# Get Edge page rank and frontiers
#-----------------------------------
#--------------------
# ---- Utilities ----
#--------------------
source("Scripts/utilities.R") #Libraries and functions

#---------------------
# Read files
#---------------------
# Data JSI
JSI_comp_USA142aff=read_csv(file="Outputs/JSI USA142aff full 10222025.csv")%>% select(-c("...1"))
# Data Nij as dataframe
data_Nij_USA142aff =read_csv(file="Outputs/Data Nij USA142aff full 10222025.csv")%>% select(-c("...1"))
# Data Nii by university and year
data_tot_npub_USA142aff=read_csv(file="Outputs/Data Nii per institution USA142aff full 10222025.csv")%>% select(-c("...1"))
# SciSciNet codes
SciSciNet_Affiliations_Univ<- read_delim("Inputs/SciSciNet_Affiliations_AffiliationID_AffiliationName_USStateCode_AIGType_9199.tsv", 
                                         delim = "\t", escape_double = FALSE, 
                                         trim_ws = TRUE,col_names = F) 
colnames(SciSciNet_Affiliations_Univ)=c("university_code","university_name","state_code","type")
SciSciNet_Affiliations_Univ=SciSciNet_Affiliations_Univ%>% select(-c(type))

# Frontiers
neighbors=read_csv(file="Inputs/neighbors-states.csv")%>%mutate(StateCode=paste0(StateCode,"-USA"),
                                                                NeighborStateCode=paste0(NeighborStateCode,"-USA"))
#---------
# Get neighbors list
neighbors_list=unique(c(unique(paste0(neighbors$StateCode,"_",neighbors$NeighborStateCode)),unique(paste0(neighbors$NeighborStateCode,"_",neighbors$StateCode))))
neighbors_list%>% write.csv(file="Outputs/Data neighbors by state USA142aff full 10242025.csv")

#----------------------------------
# Trying some different thresholds on the minimum number of publications and start date
#-----------------------------------------------------------------------------
 out <- plot_edgepagerank_and_links_by_thresholds(
   data_tot_npub_USA142aff,
   data_Nij_USA142aff,
   JSI_comp_USA142aff,
   thresholds = c(50,100,200,300,500,1000),
   min_values = c(1980,1983,1985, 1987),
   show_legend = FALSE
 )

ggsave(filename = "Figures/Plot Edge page rank by 6 threshold and 4 start date 10242025.pdf",plot =out$plot_edge_pagerank,
       width = 15, height = 12)
ggsave(filename = "Figures/Plot links proportion by 6 threshold and 4 start date 10242025.pdf",plot =out$plot_links_ratio,
       width = 15, height = 12)
ggsave(filename = "Figures/Plot average JSI by 6 threshold and 4 start date 10242025.pdf",plot =out$plot_jsi,
       width = 15, height = 12)

# Write centrality 
out$centrality %>% write.csv(file="Outputs/Data Edge page rank full 10242025.csv")
# Write links and JSI 
out$links_summary %>% write.csv(file="Outputs/Data Links and JSI full 10242025.csv")




#------------------------------------------
# Specific threshold and start example
#------------------------------------------
#--------------------------------------------------
# Cleaning Nij dataset to compute edge page rank
#--------------------------------------------------
# Steps:
# 1. Remove univ with less than XX publications each year
# Try 500
#---------------------------------------------------------------
data_tot_npub_USA142aff_filtered=filter_npub(data_tot_npub_USA142aff,1000)

univ_keep=data_tot_npub_USA142aff_filtered %>% group_by(from) %>% summarise(years=n_distinct(year)) %>% 
  filter(years==41) %>% select(from) %>% unique() %>% pull()
#23 out of 142 have at least 500 pub every year since 1980
#55 out of 142 have at least 200 pub every year since 1980
#73 out of 142 have at least 100 pub every year since 1980
#------------------------
# Filtering in Data Nij
#------------------------
data_Nij_USA142aff_filtered=
  data_Nij_USA142aff %>% 
  filter(from%in%univ_keep&to%in%univ_keep)
#left_join(data_tot_npub_USA142aff_filtered,by=c("from","Year"="year")) %>% filter(!is.na(keep))

#------------------------------------------------------
# Removing duplicated rows, from!=to and weight > 0
# Merge with SciScinet so we use the codes instead
#------------------------------------------------------
data_Nij_USA142aff_filtered1=data_Nij_USA142aff_filtered %>%
  # Step 1: from!=to and weight >0
  filter(from!=to) %>% 
  # Step 2: Remove duplicated rows
  distinct(Year, code_min, code_max, .keep_all = TRUE) %>% 
  mutate(edge_code=paste0(code_min,"_",code_max)) %>% 
  select(-code_min, -code_max) %>% 
  # Step 3: Add weights merge with JSI
  left_join(JSI_comp_USA142aff %>% 
              distinct(Year, code_min, code_max, .keep_all = TRUE) %>% 
              mutate(edge_code=paste0(code_min,"_",code_max)) %>% 
              select(weight,edge_code,Year),by=c("edge_code","Year"))

data_Nij_USA142aff_filtered1 %>% write.csv("Outputs/Data Nij filtered for modeling USA142aff full 10222025.csv")
#---------------------
# Edge page rank
#--------------------
# Run the function on your dataset
df_centrality_all_years <- compute_centrality_by_year(data_Nij_USA142aff_filtered1 %>% rename("year"="Year"))

df_centrality_all_years %>%
  mutate(
    Year = as.integer(Year)  # uses Year or year
  ) %>%
  arrange(edge, Year) %>%
  ggplot(aes(x = Year, y = edge_page_rank, group = edge, color = edge)) +
  geom_line(linewidth = 0.6, alpha = 0.9, show.legend = FALSE) +
  # optional: points
  # geom_point(size = 0.7, show.legend = FALSE) +
  labs(x = "Year", y = "Edge PageRank") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

df_centrality_all_years %>% write.csv("Outputs/edge page rank by year JSI matrices 10222025.csv")


#-----------------
# number of links weight>0 by university
#------------------------------
data_Nij_USA142aff_filtered %>% 
  mutate(edge_code=paste0(code_min,"_",code_max)) %>%
  left_join(data_Nij_USA142aff_filtered1 %>% 
              select(weight,edge_code,Year),by=c("edge_code","Year")) %>% 
  filter(weight>0) %>% 
  group_by(Year,from) %>% summarise(n_links=n_distinct(to),
                                    avg_JSI=mean(weight),
                                    total_links=length(univ_keep)-1,
                                    prop_links=n_links/total_links) %>% 
  View()



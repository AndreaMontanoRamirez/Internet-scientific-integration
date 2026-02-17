#-----------------------------------
# Combining with JSI data of 142 universities
#-----------------------------------
#--------------------
# ---- Utilities ----
#--------------------
source("Scripts/utilities.R") #Libraries and functions
#%%%%%%%%%%%%%%%%%%%%%%%%
# Data USA States Institutions
#%%%%%%%%%%%%%%%%%%%%%%%%
# Root:
root_dir <- "C:/Users/OMEN/Box/Andrea_Universities/Project_InternetEffect/DataOut_CoOccur_1980-2020_NX_142_KX_10"
# Data Nij by year
USAffilEntity_MatrixOrder_142 <- read_delim(paste0(root_dir,"/USAffilEntity_MatrixOrder_142_AffilID_Name_State_AIG.txt"),col_names = F)
colnames(USAffilEntity_MatrixOrder_142)=c("Code SciSciNet","University Name","State","Type")
temp = list.files(path =root_dir,pattern="^Ac")

# SciSciNet codes
SciSciNet_Affiliations_Univ<- read_delim("Inputs/SciSciNet_Affiliations_AffiliationID_AffiliationName_USStateCode_AIGType_9199.tsv", 
                                         delim = "\t", escape_double = FALSE, 
                                         trim_ws = TRUE,col_names = F) 
colnames(SciSciNet_Affiliations_Univ)=c("university_code","university_name","state_code","type")

SciSciNet_Affiliations_Univ=SciSciNet_Affiliations_Univ%>% select(-c(type))
#-----------------
# Apply functions
#-----------------
# Step 0:
out1=combine_dataframes(temp,names_data = USAffilEntity_MatrixOrder_142$`University Name`) #Output:A single data set with all the input data sets and the number of columns of the data set
#---------------------
# Data Nij
#---------------------
data_USA142aff=out1$data

data_USA142aff%>% write.csv(file="Outputs/Data Nij matrix USA142aff full 10292025.csv")

# Adding Nij to data set and converting from matrix to dataframe and add SciSciNet code
data_USA142aff_sym=NULL
for (yr in unique(data_USA142aff$year)) {
  #yr=unique(data_USA142aff$year)[1]
  data_USA142aff_sym=data_USA142aff_sym %>% bind_rows(
    data_USA142aff %>% filter(year==yr) %>% select(c(from,year)) %>% bind_cols(
      data_USA142aff %>% filter(year==yr) %>%
        { 
          nm <- .$from
          mat <- make_symmetric(as.matrix(select(., -year, -from)))
          out <- as.data.frame(mat)
          colnames(out) <- nm
          rownames(out) <- nm
          out
        }
    )%>% 
      gather(key="to",value="Nij",-c(from,year)) %>% 
      rename("Year"="year") %>% 
      left_join(SciSciNet_Affiliations_Univ,by=c("from"="university_name")) %>% 
      rename("university_code_from"="university_code","state_code_from"="state_code") %>% 
      left_join(SciSciNet_Affiliations_Univ,by=c("to"="university_name")) %>% 
      rename("university_code_to"="university_code","state_code_to"="state_code") %>% 
      mutate(
        code_min = as.character(pmin(university_code_from, university_code_to, na.rm = TRUE)),
        code_max = as.character(pmax(university_code_from, university_code_to, na.rm = TRUE))
      )
  )
}
data_USA142aff_sym %>% write.csv(file="Outputs/Data Nij USA142aff full 10222025.csv")

remove(temp);remove(out1)
#------------------------
# Compute data_tot_npub
#------------------------
data_tot_npub_USA142aff=NULL
for (i in unique(data_USA142aff$year)) {
  d1=data_USA142aff%>%filter(year==i)
  data_tot_npub_USA142aff=data_tot_npub_USA142aff%>%bind_rows(data.frame(from=d1$from,year=d1$year,tot=diag(as.matrix(d1%>% select(-c(from,year))))))
}
remove(d1)

data_tot_npub_USA142aff %>% write.csv(file="Outputs/Data Nii per institution USA142aff full 10222025.csv")

#-----------------
# Step 1
#-----------------
JSI_comp_USA142aff=JSI_matrix(data_USA142aff)

JSI_comp_USA142aff=JSI_comp_USA142aff %>% arrange(Year) %>% mutate(weight=ifelse(is.na(weight),0,weight))

#-----------------
# Step 2: Left join with SciSciNet code
#-----------------
JSI_comp_USA142aff=JSI_comp_USA142aff %>% # Step 2: Merge with SciSciNet codes data frame to get universities codes
  left_join(SciSciNet_Affiliations_Univ,by=c("from"="university_name")) %>% 
  rename("university_code_from"="university_code","state_code_from"="state_code") %>% 
  left_join(SciSciNet_Affiliations_Univ,by=c("to"="university_name")) %>% 
  rename("university_code_to"="university_code","state_code_to"="state_code") %>% 
  mutate(
    code_min = as.character(pmin(university_code_from, university_code_to, na.rm = TRUE)),
    code_max = as.character(pmax(university_code_from, university_code_to, na.rm = TRUE))
  )
write.csv(JSI_comp_USA142aff,file="Outputs/JSI USA142aff full 10222025.csv")


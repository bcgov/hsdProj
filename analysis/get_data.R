# Copyright 2023 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the &quot;License&quot;);
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an &quot;AS IS&quot; BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

options(java.parameters = "-Xmx8g" )  ## run BEFORE loading any libraries else can't open huge .xlsx file

### load libraries  ----
if (!require('here')) install.packages('here')
if (!require('tidyverse')) install.packages('tidyverse')
if (!require('xlsx')) install.packages('xlsx')

library(here)
library(tidyverse)  ## for: dplyr, readr, ...
library(xlsx)


### get data ----
## Function to get sub-provincial estimates data
data.pretty <- function(base_folder, file_name, file_type, mysheet, data_cols, final_cols) {

  ## A. if csv file_type, else xlsx file_type
  if(file_type == "csv"){
    
    ## read in csv data
    df <- readr::read_csv(file = paste0(base_folder, file_name, ".csv"))
    
  } else {
    
    ## read in xlsx data
    df <- xlsx::read.xlsx2(file = paste0(base_folder, file_name, ".xlsx"),
                           sheetName = mysheet, stringsAsFactors = FALSE) %>%
      mutate_at(vars(-c(data_cols[1])), as.numeric)  ## else everything comes in as character class
  }
  
  ## B. change colnames if needed
  if(data_cols[1] != final_cols[1]){ df <- rename(df, !!final_cols[1] := !!data_cols[1]) }  ## "Region.Type"
  if(data_cols[2] != final_cols[2]){ df <- rename(df, !!final_cols[2] := !!data_cols[2]) }  ## "Region"
  if(data_cols[3] != final_cols[3]){ df <- rename(df, !!final_cols[3] := !!data_cols[3]) }  ## Year
  if(data_cols[4] != final_cols[4]){ df <- rename(df, !!final_cols[4] := !!data_cols[4]) }  ## Total

  ## create character Gender var and re-arrange columns
  df <- df %>% select(Region, Region.Type, Year, Total)
  
  ## if analysis/inputs/lookup.csv does not exist, make it (no longer working b/c data changed)
  if(!file.exists(here("analysis", "inputs", "lookup.csv"))){
    ## requires: "analysis/inputs/REGNAMES_from_Access.csv"
    ## made manually by opening Database work/WorkingFile.accdb and copying REGNAMES into Excel
    source(here("analysis", "make_lookup.R"))
  }

  ## open lookup and join in Region.Names
  lookup <- readr::read_csv(here("analysis", "inputs", "lookup.csv"),
                            col_names = TRUE, col_types = "dcc")
  
  df <- left_join(df, lookup, by = c("Region", "Region.Type")) %>%
    select(Region, Region.Name = NAME, everything())

  ## ensure that British Columbia is always Region 0
  ### get any Region.Types with a non-0 BC
  changeBCregion <- df %>% 
    filter(Region.Name == "British Columbia", Region != 0) %>% 
    select(Region.Type) %>% unique() %>% pull()
  
  # regions <- df %>% select(Region.Type) %>% unique() %>% pull()
  for(i in seq_along(changeBCregion)) {
    if(all(df %>% filter(Region.Name == "British Columbia", Region.Type %in% changeBCregion[i]) %>% 
      select(Region) %>% unique() %>% pull() != 0)) {
      df <- df %>% mutate(Region = case_when(Region.Name == "British Columbia" & Region.Type %in% changeBCregion[i] ~ 0,
                                             TRUE ~ Region))
    }
  }

  ## replace Region.Type abbreviations with names
  df <- df %>% mutate(Region.Type = case_when(
    Region.Type == "CF" ~ "Children and Family Development",
    Region.Type == "DR" ~ "Development Region",
    Region.Type == "HA" ~ "Local Health Area",
    Region.Type == "HY" ~ "Health Authority",  ## note that HY 6 is BC, there may be no 0 in data
    Region.Type == "HS" ~ "Health Service Delivery Area",
    Region.Type == "PS" ~ "College Region",
    Region.Type == "RD" ~ "Regional District",
    Region.Type == "SD" ~ "School District",
    Region.Type == "SR" ~ "Special Regions (CMAs and Vancouver Island)",
    Region.Type == "CH" ~ "Community Health Service Area",
    TRUE ~ as.character(Region.Type))
  )
  
  df <- df %>% filter(!is.na(Region.Name))

  ## save as RDS
  saveRDS(df, paste0("app/data/data", ".rds"))
  
  ## save again in Archive folder with date
  saveRDS(df, paste0("data_archive/data", "_", Sys.Date(), ".rds"))
  #saveRDS(df, paste0("data_archive/data", age_var, "_", Sys.Date(), ".rds"))
  
  ## return df
  df
}

## data1 for single-yr intervals, data5 for 5-yr intervals
data1 <- data.pretty(base_folder, file_name, file_type, mysheet, data_cols, final_cols)

## clean up ----
rm(data.pretty, data1)
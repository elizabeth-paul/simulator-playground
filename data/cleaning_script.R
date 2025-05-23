
##### Introduction

### Data Indicator Definitions

# Sparsity: Districts with fewer than 30 students per square mile

# Students per Sq Mi: Calculated the total area of a district using 2019
#                     data from EdBuild. Then divided total ADM from 23-24
#                     by this area to get an updated estimate. Charters are
#                     not eligible for this funding.

# Concentrated Poverty: High Poverty Tier 1 (at least 80% of 
#                        students are econ. disadvantaged)
#                       High Poverty Tier 2 (at least 40% but
#                        less than 80% are econ. disadvantaged)






## Load packages ------
options(scipen = 999)

library(tidyverse)
library(tidycensus)
library(viridis)
library(scales)
library(readxl)


################################################################################
## Functions

# Function to change cases of >95 to 96, <5 to 4
adj_pct_range <- function (pct) {
  case_when(pct == ">95" ~ "96",
            pct == "<5" ~ "4",
            TRUE ~ pct)
}


################################################################################
## ADM Data

# Load raw data files
adm_raw <- read_csv("data/raw/23-24 ADM.csv")


# Clean ADM files
adm_clean <- adm_raw |>
  rename_with(tolower) |>
  rename(district = "lea name",
         adm = "total",
         dist_id = "lea") |>
  select(dist_id, district, adm, charter_status) |>
  filter(!is.na(dist_id))


# Clean environment
rm(adm_raw)

################################################################################
## EC Data

# Load Raw Data files
ec_trad_raw <- read_excel("data/raw/23-24 EC ADM.xlsx",
                     range = "A3:T118")
ec_charter_raw <- read_excel("data/raw/23-24 Charter EC ADM.xlsx",
                          range = "A3:T223")


# Traditional Public 
ec_trad <- ec_trad_raw |>
  rename_with(tolower) |>
  rename(dist_id = "lea",
         district = "lea name",
         swd_total = "total",
         swd_aut = "au",
         swd_dd = "dd",
         swd_ed = "ed",
         swd_hi = "hi",
         swd_id = "idmi",
         swd_md = "mu",
         swd_ohi = "oh",
         swd_oi = "oi",
         swd_sld = "ld",
         swd_sli = "si",
         swd_tbi = "tb",
         swd_vi = "vi",
         swd_db = "db") |>
        # Combine SWD measures as needed
  mutate(swd_id = swd_id + idmo + idse,
         swd_dd = swd_dd + df,
         dist_id = as.character(dist_id),
         # Add 0 to front of lea to match other dataframes
         dist_id = str_pad(dist_id, width = 3, pad = "0"),
         # Add Charter Status
         charter_status = "Traditional") |>
  select(dist_id, swd_total, swd_aut, swd_dd, swd_ed, swd_hi, swd_id, swd_md,
         swd_ohi, swd_oi, swd_sld, swd_sli, swd_tbi, swd_vi, swd_db)

# Charter
ec_charter <- ec_charter_raw |>
  rename_with(tolower) |>
  rename(dist_id = "cs",
         district = "charter school name",
         swd_total = "total",
         swd_aut = "au",
         swd_dd = "dd",
         swd_ed = "ed",
         swd_hi = "hi",
         swd_id = "idmi",
         swd_md = "mu",
         swd_ohi = "oh",
         swd_oi = "oi",
         swd_sld = "ld",
         swd_sli = "si",
         swd_tbi = "tb",
         swd_vi = "vi",
         swd_db = "db") |>
  # Combine SWD measures as needed
  mutate(swd_id = swd_id + idmo + idse,
         swd_dd = swd_dd + df,
         # Add 0 to front of lea to match other dataframes
         dist_id = str_pad(dist_id, width = 3, pad = "0"),
         # Add Charter Status
         charter_status = "Charter") |>
  select(dist_id, swd_total, swd_aut, swd_dd, swd_ed, swd_hi, swd_id, swd_md,
         swd_ohi, swd_oi, swd_sld, swd_sli, swd_tbi, swd_vi, swd_db)

# Combine traditional and charter
ec_clean <- ec_trad |>
  bind_rows(ec_charter)

# Clean environment
rm(ec_trad_raw, ec_charter_raw, ec_trad, ec_charter)



################################################################################
## EDS Data + Concentrated Poverty Indicator

# Load raw data files
eds_raw <- read_excel("data/raw/23-24 EDS_All_Years.xlsx",
                      sheet = "APR 2024")


# Clean EDs files
eds_clean <- eds_raw |>
  group_by(psu_name) |>
  # keep only first instance of psu_name so that we have only district level data
  slice(1) |>
  ungroup() |>
  filter(psu_code != "NC",           # Remove state summary
         psu_code != "298202",       # Remove three Deaf + Blind Schools
         psu_code != "298203",
         psu_code != "298204") |>
  arrange(psu_code) |>
  rename(dist_id = "psu_code",
         district = "psu_name",
         eds_den = "den",
         ed_pct = "pct_eds") |>
  select(dist_id, eds_den, ed_pct) |>
  # Change cases of >95 to 96 and <5 to 4
  mutate(ed_pct = adj_pct_range(ed_pct),
         # Convert to numeric
         eds_den = as.numeric(eds_den),
         ed_pct = as.numeric(ed_pct),
         # Convert percentages to correct format
         ed_pct = ed_pct * .01,
         # Calculate raw enrollment numbers
         ed_adm = eds_den * ed_pct,
         # Calculate High Poverty Eligibility tiers
         high_pov_1 = ifelse(ed_pct >= .8, 1, 0),
         high_pov_2 = case_when(ed_pct >= .8 ~ 0,
                                ed_pct >= .4 ~ 1,
                                ed_pct < .4 ~ 0)) |>
  select(dist_id, ed_adm, ed_pct, high_pov_1, high_pov_2)


# Clean environment
rm(eds_raw)

################################################################################
## EL Data

# Load raw data files
el_raw <- read_excel("data/raw/23-24 EL ADM.xlsx")

# Clean EL Files
el_clean <- el_raw |>
  rename_with(tolower) |>
  rename(dist_id = "psu code",
         district = "psu name",
         el_adm = "count") |>
  filter(dist_id != "298",           # Remove Deaf + Blind schools
         dist_id != "996",           # Remove two DPS Schools
         dist_id != "998") |>
  # Change cases of * to 9
  mutate(el_adm = ifelse(el_adm == "*", "9", el_adm),
         el_adm = as.numeric(el_adm),
         # Add 0 to front of lea to match other dataframes
         dist_id = str_pad(dist_id, width = 3, pad = "0")) |>
  select(-district)

# Clean environment
rm(el_raw)

################################################################################
## AIG Data

# Load raw data files
aig_raw <- read_excel("data/raw/23-24 AIG ADM.xlsx")

# Clean AIG Data
aig_clean <- aig_raw |>
  rename_with(tolower) |>
  filter(exceptionality == "Subtotal") |>
  rename(dist_id = "district id",
         aig_enroll = "total",
         district = "district name") |>
  mutate(aig_enroll = as.numeric(aig_enroll),
         # Add 0 to front of lea to match other dataframes
         dist_id = str_pad(dist_id, width = 3, pad = "0")) |>
  select(dist_id, aig_enroll)

# Clean environment
rm(aig_raw)




################################################################################
## Students per Sq Mile

# Load Raw Data
edbuildr_url = "https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Master/2019/full_data_19_type_exc.csv"
edbuild_fy19 <- read.csv(file = edbuildr_url, stringsAsFactors = FALSE)

# Clean Data
stud_sq_mi_19 <- edbuild_fy19 |> 
  filter(State == "North Carolina") |>
  # Remove military bases
  filter(!is.na(state_id)) |>
  rename_with(tolower) |>
  rename(dist_id = "state_id",
         student_per_sq_mile_19 = "student_per_sq_mile",
         district = "name") |>
  mutate(dist_id = substr(dist_id,4,6)) |>
  select(dist_id, district, enroll, student_per_sq_mile_19) |>
          # Calculate total area
  mutate(sq_mi = enroll/student_per_sq_mile_19) |>
  select(dist_id, sq_mi)

# Clean environment
rm(edbuildr_url, edbuild_fy19)


################################################################################
## Current Funding

# Load raw data files
ppe_raw_traditional <- read_excel("data/raw/23-24 PPE.xlsx",
                                  range = "A4:K119")
ppe_raw_charter <- read_excel("data/raw/23-24 Charter PPE.xlsx",
                              range = "A4:K224")

# Clean Traditional PPE
ppe_clean_traditional <- ppe_raw_traditional |>
  rename_with(tolower) |>
  rename(dist_id = "lea",
         district = "lea name",
         state_ppe = "state ppe",
         federal_ppe = "federal ppe",
         local_ppe = "local ppe",
         total_ppe = "total ppe") |>
  # Convert LEA to charcters to bind with charters
  mutate(dist_id = as.character(dist_id),
         current_pp = state_ppe + local_ppe) |>
  select(dist_id, district, current_pp)

# Clean Charter PPE
ppe_clean_charter <- ppe_raw_charter |>
  rename_with(tolower) |>
  rename(dist_id = "cs",
         district = "charter school name",
         state_ppe = "state ppe",
         federal_ppe = "federal ppe",
         local_ppe = "local ppe",
         total_ppe = "total ppe") |>
  mutate(district = str_to_title(district),
         current_pp = state_ppe + local_ppe) |>
  select(dist_id, district, current_pp)


# Combine charter and traditional public ADM files
ppe_clean <- ppe_clean_traditional |>
  bind_rows(ppe_clean_charter) |>
  select(dist_id, current_pp) |>
  # Add 0 to front of lea to match other dataframes
  mutate(dist_id = str_pad(dist_id, width = 3, pad = "0"))

# Clean environment
rm(ppe_raw_traditional, ppe_raw_charter, ppe_clean_traditional, ppe_clean_charter)


################################################################################
## Combine Data

lea_data_23_24 <- adm_clean |>
  left_join(eds_clean, by = "dist_id") |>
  left_join(el_clean, by = "dist_id") |>
  left_join(ec_clean, by = "dist_id") |>
  left_join(aig_clean, by = "dist_id") |>
  left_join(stud_sq_mi_19, by = "dist_id") |>
  left_join(ppe_clean, by = "dist_id")

# Clean combined data
lea_data_23_24_clean <- lea_data_23_24 |>
  # Change NAs to 0 Enrollment
  mutate(aig_enroll = ifelse(is.na(aig_enroll), 0, aig_enroll),
         swd_total = ifelse(is.na(swd_total), 0, swd_total),
         el_adm = ifelse(is.na(el_adm), 0, el_adm),
         ed_adm = ifelse(is.na(ed_adm), 0, ed_adm)) |>
  # Calculate sparsity for 2023-24 school year and set charters to 0
  mutate(student_per_sq_mi_23 = adm / sq_mi,
         sparsity_23 = ifelse(student_per_sq_mi_23 < 30, 1, 0)) |>
  select(-student_per_sq_mi_23) |>
  mutate(sparsity_23 = ifelse(is.na(sparsity_23), 0, sparsity_23),
    # Calculate EL Percentage
         el_pct = el_adm / adm,
  # Calculate total funding  
         current_total = adm * current_pp)



# Select Traditional Public Schools Only
traditional_lea_23_24 <- lea_data_23_24_clean |>
  filter(charter_status == "Traditional")


# Save data 
write_csv(lea_data_23_24_clean, "data/lea_data_23_24.csv")
saveRDS(lea_data_23_24_clean, "lea_data_23_24_clean.rds")
saveRDS(traditional_lea_23_24, "traditional_lea_23_24.rds")


# Clean environment
rm(adm_clean, aig_clean, ec_clean, eds_clean, el_clean, lea_data_23_24, 
   stud_sq_mi_19, ppe_clean)



# We need to change charters to a weight. So add charter_adm for each district




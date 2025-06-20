# 1 Setup ----------------------------------------------------------------------

library(psych)        # Describe function
library(rcompanion)   # GLM comparison function
library(msme)         # Over-dispersion function
library(sandwich)     # Cluster-robust standard errors (sandwich)
library(tidyverse)    # Data handling and plotting
library(lmtest)       # Significance testing
library(car)          # Assumptions testing (vif)
library(ggpubr)       # Arrange plots
library(stargazer)    # Descriptives/model export
library(DescTools)    # confidence intervals

# Set the working directory to the data folder
getwd()
# setwd("../../data")
setwd("/Users/mn/OneDrive - University of Exeter/Projects/GitHub/campaign-financing-and-climate-change/data")


## 1.1 Load the MoC data ----
df <- read.csv("raw/inference_witnesses_03_10_utterances_witnesses_MoCs_labels_for_plotting.csv")

### 1.1.1 Subset the data to Member of Congress (MoC) utterances ----
df <- df[df$type == "MoC",]

### 1.1.2 Drop irrelevant data columns ----
df <- df %>% select(-contains("witness"))
# Rename last_name to last_name_transcript
df <- df %>% rename(last_name_transcript = last_name)
# Rename id to utterance_id 
df <- df %>% rename(utterance_id = id)

### 1.1.3 Drop the MoC column prefix ----
names(df) <- sub('MoC_', '', names(df))

### 1.1.4 Fill in missing information ----
# Hearing 110hhrg37579 has missing information that will be filled in with 
# bioguide info from another hearing in the same year
df[df$hearing_id=="110hhrg37579" & 
     df$last_name_transcript %in% c("Akin"), 17:39] <- 
  df[df$year==2007 & df$last_name %in% c("Akin"), 17:39][1,]
df[df$hearing_id=="110hhrg37579" & 
     df$last_name_transcript %in% c("Bartlett"), 17:39] <- 
  df[df$year==2007 & df$last_name %in% c("Bartlett"), 17:39][1,]
df[df$hearing_id=="110hhrg37579" & 
     df$last_name_transcript %in% c("Inglis"), 17:39] <- 
  df[df$year==2007 & df$last_name %in% c("Inglis"), 17:39][1,]
df[df$hearing_id=="110hhrg37579" & 
     df$last_name_transcript %in% c("Lampson"), 17:39] <- 
  df[df$year==2007 & df$last_name %in% c("Lampson"), 17:39][1,]
# Check result
df[df$hearing_id=="110hhrg37579" & 
     df$last_name_transcript %in% c("Akin", "Bartlett", "Inglis", "Lampson"),]

# Export corrected MoC data without predictions for CARDS2.0 predictions

write.csv(df %>% select(-c(DataRow.Workflow.Info_taskId:train_val_test)), 
          "processed/committee_member_utterances_03_10.csv")

### 1.1.5 Define all paragraphs with fewer than 10 words ----
# (these are excluded in the claim prediction)
df$word_count_cat <- ifelse(df$word_count<10,"<10","10+")

## 1.2 Load the correction metrics ----

# Precision and recall used to correct the claim predictions
claim <- c("4_1", "4_2", "4_3", "4")

precision_sample  <- c(0.50877193, 0.37735849, 0.18333333, 0.43333333) 
precision_boosted <- c(0.51602896, 0.38448276, 0.22323529, 0.42085294) 
recall_sample     <- c(0.82857143, 0.83333333, 0.78571429, 0.97014925) 
recall_boosted    <- c(0.51407967, 0.49445676, 0.85762712, 0.68791863)

DEM_precision_sample  <- c(0.25925926, 0.14814815, 0.15625   , 0.25333333) 
DEM_precision_boosted <- c(0.27231121, 0.15740741, 0.20430108, 0.28127208) 
DEM_recall_sample     <- c(0.77777778, 0.8       , 0.83333333, 0.95      ) 
DEM_recall_boosted    <- c(0.21996303, 0.5       , 0.9178744 , 0.50895141)

REP_precision_sample  <- c(0.73333333, 0.61538462, 0.21428571, 0.61333333) 
REP_precision_boosted <- c(0.74178645, 0.61458333, 0.24122807, 0.61096975) 
REP_recall_sample     <- c(0.84615385, 0.84210526, 0.75      , 0.9787234 ) 
REP_recall_boosted    <- c(0.7286939 , 0.50862069, 0.78384798, 0.82607034)

PR <- data.frame(claim,
                 precision_sample,
                 precision_boosted,
                 recall_sample,
                 recall_boosted, 
                 DEM_precision_sample,
                 DEM_precision_boosted,
                 DEM_recall_sample,
                 DEM_recall_boosted, 
                 REP_precision_sample,
                 REP_precision_boosted,
                 REP_recall_sample,
                 REP_recall_boosted)

rm(precision_sample,
   precision_boosted,
   recall_sample,
   recall_boosted, 
   DEM_precision_sample,
   DEM_precision_boosted,
   DEM_recall_sample,
   DEM_recall_boosted, 
   REP_precision_sample,
   REP_precision_boosted,
   REP_recall_sample,
   REP_recall_boosted)


# 2 Pre-processing -------------------------------------------------------------

## 2.1 Merge additional data files ----

### 2.1.1 Add the FFI funds data ----

MoC_data <- read.csv("raw/hearings_committee_member_level.csv")

# Remove duplicated data rows for MoCs who's committee status changed in the
# year in which the hearing was held
MoC_data %>% group_by(hearing_id, CID) %>% filter(n()>1) %>% ungroup() %>% 
  select(hearing_id, CID, name, date, date_of_assignment, date_of_termination) %>% 
  mutate(hearing_after_date_of_assignment = ifelse(date_of_assignment<date, T, F))
# >> All changes in assignment have happened before the hearing takes place
# >> Drop the row containing the earlier assignment
# >> Reverse sort the data by date of assignment and keep distrinct rows
# depending on hearing_id and CID only (distinct keeps the first row)
MoC_data <- MoC_data %>% arrange(desc(date_of_assignment)) %>% 
  distinct(hearing_id, CID, .keep_all = TRUE)

# Merge the fossilfuel money information from the MoC_data to the main data
df <- merge(df, MoC_data %>% 
              select(hearing_id, CID, 
                     cc_fossilfuel, cc_fossilfuel_noUSCAP, cc_total, 
                     lobbying_fossilfuel, lobbying_fossilfuel_noUSCAP,
                     lobbying_total),
            by = c("hearing_id", "CID"), all.x = T)

# Fill in missing information for hearing 110hhrg37579 with lobbying /
# campaign contribution info from another hearing in the same year
df[df$hearing_id=="110hhrg37579" &
     df$last_name_transcript %in% c("Akin"), 54:59] <-
  df[df$year==2007 & df$last_name %in% c("Akin"), 54:59][1,]
df[df$hearing_id=="110hhrg37579" &
     df$last_name_transcript %in% c("Bartlett"), 54:59] <-
  df[df$year==2007 & df$last_name %in% c("Bartlett"),54:59][1,]
df[df$hearing_id=="110hhrg37579" &
     df$last_name_transcript %in% c("Inglis"), 54:59] <-
  df[df$year==2007 & df$last_name %in% c("Inglis"), 54:59][1,]
df[df$hearing_id=="110hhrg37579" &
     df$last_name_transcript %in% c("Lampson"), 54:59] <-
  df[df$year==2007 & df$last_name %in% c("Lampson"), 54:59][1,]
# Check result
df[df$hearing_id=="110hhrg37579" &
     df$last_name_transcript %in% c("Akin", "Bartlett", "Inglis", "Lampson"),]

### 2.1.2 Add the MoC data for members with 0 utterances ---- 

# Find all MoCs that are on the committees but don't speak during the hearings
df <- merge(df,
            anti_join(MoC_data %>% 
                        select(c(names(df)[names(df) %in% names(MoC_data)], 
                                 cc_fossilfuel, cc_fossilfuel_noUSCAP, cc_total, 
                                 lobbying_fossilfuel, lobbying_fossilfuel_noUSCAP,
                                 lobbying_total)),
                      df, by=c("hearing_id", "CID")),
            all = T) %>% 
  mutate(word_count_cat = ifelse(is.na(word_count_cat), 
                                 "zero utterances", word_count_cat),
         chamber = toupper(chamber))

### 2.1.3 Add the QCEW employment data ----

QCEW_district <- read.csv("raw/QCEW_congressional_districts_employment.csv") %>% 
  select(!X) %>% 
  rename(emp_total = emp.10) %>% 
  mutate(emp_fossil = rowSums(across(starts_with("emp."))),
         emp_fossil_per = emp_fossil/emp_total*100)

# QCEW_state <- read.csv("raw/QCEW_states_employment.csv") %>% 
#   select(!X) %>% 
#   rename(emp_total = emp.10) %>% 
#   mutate(emp_fossil = rowSums(across(starts_with("emp."))),
#          emp_fossil_per = emp_fossil/emp_total*100)

QCEW_state <- QCEW_district %>% 
  select(-cd_code, -congressionaldistrict, -emp_fossil, -emp_fossil_per) %>% 
  group_by(year, stab, state, congress) %>% 
  summarise_each(funs = sum) %>% 
  ungroup() %>% 
  mutate(emp_fossil = rowSums(across(starts_with("emp."))),
         emp_fossil_per = emp_fossil/emp_total*100)

par(mfrow = c(1, 2))
hist(QCEW_district$emp_fossil_per)
hist(QCEW_state$emp_fossil_per)

# Merge the congressional district data for matching

legislators <- rbind(
  read.csv("raw/legislators-current.csv") %>% 
    select(bioguide_id, district),
  read.csv("raw/legislators-historical.csv") %>% 
    select(bioguide_id, district))

df <- merge(df, legislators, by = "bioguide_id")

# Add missing district information for Representatives 
# (for members that were both Senators and Representatives during their careers 
# and are only listed in their first role in the data)

df %>% filter(chamber == "HOUSE" & is.na(district)) %>% 
  select(bioguide_id) %>% table()

# Find the correct values in the more detailed yaml file
# https://theunitedstates.io/congress-legislators/committees-historical.yaml

df[df$bioguide_id == "B000575", "district"] <- 7
df[df$bioguide_id == "B000944", "district"] <- 13
df[df$bioguide_id == "B001135", "district"] <- 5
df[df$bioguide_id == "B001230", "district"] <- 2
df[df$bioguide_id == "B001236", "district"] <- 3
df[df$bioguide_id == "B001243", "district"] <- 7
df[df$bioguide_id == "C001047", "district"] <- 2
df[df$bioguide_id == "C001075", "district"] <- 6
df[df$bioguide_id == "F000444", "district"] <- 6
df[df$bioguide_id == "H001041", "district"] <- 2
df[df$bioguide_id == "H001042", "district"] <- 2
df[df$bioguide_id == "H001046", "district"] <- 1
df[df$bioguide_id == "L000570", "district"] <- 3
df[df$bioguide_id == "L000571", "district"] <- 0
df[df$bioguide_id == "M000133", "district"] <- 7
df[df$bioguide_id == "M000934", "district"] <- 1
df[df$bioguide_id == "M001169", "district"] <- 5
df[df$bioguide_id == "P000595", "district"] <- 9
df[df$bioguide_id == "S000033", "district"] <- 0
df[df$bioguide_id == "U000038", "district"] <- 2
df[df$bioguide_id == "V000128", "district"] <- 8
df[df$bioguide_id == "W000800", "district"] <- 0
# Correct wrong values
df[df$bioguide_id == "D000399", "district"] <- 10
df[df$bioguide_id == "R000435", "district"] <- 18
df[df$bioguide_id == "D000600", "district"] <- 25
df[df$bioguide_id == "M001142", "district"] <- 2
df[df$bioguide_id == "Y000033", "district"] <- 1

# Merge the QCEW employment data by state / state + district

df <- rbind(
  merge(df[df$chamber == "SENATE",], QCEW_state %>% 
          select(stab, year, emp_fossil, emp_fossil_per),
        by = c("stab", "year")),
  merge(df[df$chamber == "HOUSE",], QCEW_district %>% 
          select(stab, cd_code, year, emp_fossil, emp_fossil_per),
        by.x = c("stab", "district", "year"),
        by.y = c("stab", "cd_code", "year"), all.x = T))

# Check the members that don't match any district
df %>% filter(is.na(emp_fossil) & chamber == "HOUSE") %>%
  select(CID, stab, district) %>% unique()

# These are all states not considered for voting
## >> Territories and Districts with Non-Voting Representatives (98)
# American Samoa
# District of Columbia
# Guam
# Northern Mariana Islands
# Puerto Rico
# U.S. Virgin Islands

# These will be dropped from the data due to employment data missingness

df %>% filter(is.na(emp_fossil)) %>%
  select(CID, stab, district) %>% unique()

rm(QCEW_state, QCEW_district, legislators)

# Create final dataset including only the variables in the analysis in order
# to get proper descriptives including NAs.

df <- df %>% filter(!is.na(emp_fossil))

### 2.1.4 Group independent MoCs with their caucusses ----
# Check MoC party status by Congress
table(df$Party, df$congress)
# All independent MoCs from 2003 to 2010 were part of the Democratic caucus 
# >> recode I to D
df <- df %>% mutate(party = ifelse(Party == "R", "R", "D"))

### 2.1.5 Add who is chairing the hearing ----

df <- df %>% 
  mutate(chairman = ifelse(senior_party_member %in% 11:16, 1, 0),
         seniorminority = ifelse(senior_party_member %in% 21:24, 1, 0))

# Check which hearings have multiple chairmen identified
df %>% group_by(hearing_id, senior_party_member, chairman) %>% unique() %>% 
  filter(chairman != 0) %>% summarise() %>% 
  pivot_wider(hearing_id, values_from = chairman, values_fill = 0,
              names_from = senior_party_member, names_prefix = "ranking_",
              names_sort = TRUE) %>% 
  mutate(chairmen = sum(ranking_11, ranking_12, ranking_13, ranking_16)) %>% 
  filter(chairmen > 1) %>% print() %>% 
  select(hearing_id) %>% deframe() -> hearings_with_multiple_chairmen

# Choose the correct chairman depending on the date of the hearing 

df %>% filter(hearing_id %in% hearings_with_multiple_chairmen) %>% 
  select(hearing_id, CID, date, name, senior_party_member, notes, chairman) %>%
  unique() %>% filter(senior_party_member %in% c(11:16)) %>% 
  arrange(hearing_id, senior_party_member)

df[df$hearing_id == "111hhrg49410" & 
     df$senior_party_member %in% c(13, 16),]$chairman <- 0
df[df$hearing_id == "111hhrg50224" & 
     df$senior_party_member %in% c(13, 16),]$chairman <- 0
df[df$hearing_id == "111hhrg51119" & 
     df$senior_party_member %in% c(13, 16),]$chairman <- 0
df[df$hearing_id == "111hhrg51949" & 
     df$senior_party_member %in% c(13, 16),]$chairman <- 0
df[df$hearing_id == "111shrg56563" & 
     df$senior_party_member == 13,]$chairman <- 0
df[df$hearing_id == "111shrg62715" & 
     df$senior_party_member == 12,]$chairman <- 0

rm(hearings_with_multiple_chairmen)

df <- df %>% 
  mutate(seniority = case_when(
    chairman == 1 ~ "chairman",
    seniorminority == 1 ~ "seniorminority",
    T ~ "base"))

## 2.2 Aggregate data  ----
# Aggregation from the utterance to the hearing-member level

hearings <-
  df %>%
  # Drop predicted claims in paragraphs with fewer than ten words
  mutate(pred_4 = ifelse(word_count<10, 0, pred_4),
         pred_4_1 = ifelse(word_count<10, 0, pred_4_1),
         pred_4_2 = ifelse(word_count<10, 0, pred_4_2),
         pred_4_3 = ifelse(word_count<10, 0, pred_4_3),
         speaking = ifelse(word_count_cat == "zero utterances",
                           "not speaking", "speaking")) %>% 
  group_by(hearing_id, name, CID,  congress, chamber, committee,
           committee_short, year, date, title,
           bioguide_id, govtrack_id, id, 
           maj_min, rank_within_party, party_code,
           senior_party_member, committee_seniority, 
           state_icpsr, cd_code, stab, notes, party,
           last_name, first_name, birthday, gender, 
           chairman, seniorminority, seniority,
           cc_fossilfuel, cc_total, lobbying_fossilfuel, 
           emp_fossil, emp_fossil_per, speaking) %>% 
  summarise(n_paragraphs = n(),
            n_paragraphs_10plus = sum(ifelse(word_count<10, 0, 1)),
            pred_4 = sum(pred_4),
            pred_4_1 = sum(pred_4_1),
            pred_4_2 = sum(pred_4_2),
            pred_4_3 = sum(pred_4_3)) %>%
  mutate(n_paragraphs = ifelse(speaking == "not speaking",
                               0, n_paragraphs)) %>% 
  ungroup() %>% 
  mutate(pred_4_sample = pred_4*PR$precision_sample[4]/PR$recall_sample[4],
         pred_4_boosted = pred_4*PR$precision_boosted[4]/PR$recall_boosted[4],
         pred_4_sample_party = ifelse(
           party == "D",
           pred_4*PR$DEM_precision_sample[4]/PR$DEM_recall_sample[4],
           pred_4*PR$REP_precision_sample[4]/PR$REP_recall_sample[4]),
         pred_4_boosted_party = ifelse(
           party == "D",
           pred_4*PR$DEM_precision_boosted[4]/PR$DEM_recall_boosted[4],
           pred_4*PR$REP_precision_boosted[4]/PR$REP_recall_boosted[4]),
         pred_4_1_boosted_party = ifelse(
           party == "D",
           pred_4_1*PR$DEM_precision_boosted[1]/PR$DEM_recall_boosted[1],
           pred_4_1*PR$REP_precision_boosted[1]/PR$REP_recall_boosted[1]),
         pred_4_2_boosted_party = ifelse(
           party == "D",
           pred_4_2*PR$DEM_precision_boosted[2]/PR$DEM_recall_boosted[2],
           pred_4_2*PR$REP_precision_boosted[2]/PR$REP_recall_boosted[2]),
         pred_4_3_boosted_party = ifelse(
           party == "D",
           pred_4_3*PR$DEM_precision_boosted[3]/PR$DEM_recall_boosted[3],
           pred_4_3*PR$REP_precision_boosted[3]/PR$REP_recall_boosted[3]),
         cc_fossilfuel = ifelse(cc_fossilfuel<0, 0, cc_fossilfuel), # Set negative donations to 0
         cc_fossilfuel = cc_fossilfuel/100000,
         cc_total = cc_total/100000,
         lobbying_fossilfuel = lobbying_fossilfuel/1000000,
         majority =  factor(ifelse(year<2007, "R", "D"), levels  = c("R", "D")),
         age = as.integer(round(difftime(date, birthday, unit = "weeks")/52, 0)),
         speaking = ifelse(n_paragraphs==0, 0, 1),
         party = factor(party, levels  = c("R", "D"))) 

hearings$pred_4 %>% describe()
hearings$pred_4_1 %>% describe()
hearings$pred_4_2 %>% describe()
hearings$pred_4_3 %>% describe()

hearings$cc_fossilfuel %>% describe()
hearings$lobbying_fossilfuel %>% describe()

# 3.2 Descriptives -------------------------------------------------------------

## 3.1 Dataset ----

# Basic descriptive numbers
hearings %>% select(hearing_id) %>% unique() %>% nrow()
# >> 117 hearings
hearings %>% select(CID) %>% unique() %>% nrow()
# >> 477 MoCs on the committees holding the hearings from 2003-2010 
hearings %>% filter(speaking == 1) %>% select(CID) %>% unique() %>% nrow()
# >>> 295 MoCs of the 477 MoCs on the committees have at least once said 
# something at any of the 117 hearing, i.e. 182 have either never attended any 
# of the hearings or have attended but never spoken
hearings %>% filter(speaking == 1) %>% summarise(sum(n_paragraphs))
# >> 37943 paragraphs were spoken by these 295 MoCs
hearings %>% filter(speaking == 1) %>% summarise(sum(n_paragraphs_10plus))
# >> 27203 of these paragraphs contained at least 10 words
hearings %>% nrow()
# >> 3808 MoC_hearings
hearings %>% filter(speaking == 1) %>% nrow()
# >> 1451 MoC_hearings talking
hearings %>% filter(speaking == 0) %>% nrow()
# >> 2357 MoC_hearings not talking

hearings %>% filter(speaking == 1) %>% 
  ggplot(aes(n_paragraphs)) + geom_histogram()
hearings %>% filter(speaking == 1) %>% 
  ggplot(aes(n_paragraphs_10plus)) + geom_histogram()

# Campaign contributions

hearings %>%
  filter(party == "D") %>% 
  select(cc_fossilfuel, cc_total) %>%
  mutate(cc_per = cc_fossilfuel/cc_total*100) %>% 
  distinct() %>% 
  describe()

hearings %>%
  filter(party == "R") %>% 
  select(cc_fossilfuel, cc_total) %>%
  mutate(cc_per = cc_fossilfuel/cc_total*100) %>% 
  distinct() %>% 
  describe()

## 3.2 Variables ----

# Attendance (Mod 1)

hearings %>%
  filter(party == "R") %>% 
  select(cc_fossilfuel, emp_fossil_per, lobbying_fossilfuel,
         age) %>%
  describe()

hearings %>%
  filter(party == "D") %>% 
  select(cc_fossilfuel, emp_fossil_per, lobbying_fossilfuel,
         age) %>%
  describe()

hearings %$%
  table(party) %>% addmargins()

hearings %>%
  select(party, chamber, gender, seniority, majority, speaking) %>%
  gather(variable, value, chamber, gender, seniority, majority, speaking) %>%
  group_by(party, variable, value) %>%
  summarise (n = n()) %>%
  mutate(prop = round(n/sum(n)*100,1)) %>%
  pivot_wider(names_from = party, values_from = c(n, prop)) %>% 
  select(variable, value, n_R, prop_R, n_D, prop_D) %>% 
  as.tibble() %>% 
  stargazer(type = "text", summary = F)

# Involvement and Claim-making (Mod 2 & 3)
hearings %>% filter(speaking == 1) %>%
  filter(party == "R") %>% 
  select(n_paragraphs, pred_4, pred_4_boosted_party,
         cc_fossilfuel, emp_fossil_per, lobbying_fossilfuel,
         age) %>%
  describe()

hearings %>% filter(speaking == 1) %>%
  filter(party == "D") %>% 
  select(n_paragraphs, pred_4, pred_4_boosted_party,
         cc_fossilfuel, emp_fossil_per, lobbying_fossilfuel,
         age) %>%
  describe()

hearings %>% filter(speaking == 1) %$% 
  table(party) %>% addmargins()

hearings %>% filter(speaking == 1) %>%
  select(party, chamber, gender, seniority, majority) %>%
  gather(variable, value, chamber, gender, seniority, majority) %>%
  group_by(party, variable, value) %>%
  summarise (n = n()) %>%
  mutate(prop = round(n/sum(n)*100,1)) %>%
  pivot_wider(names_from = party, values_from = c(n, prop)) %>% 
  select(variable, value, n_R, prop_R, n_D, prop_D) %>% 
  as.tibble() %>% 
  stargazer(type = "text", summary = F)


ggarrange(ggplot(aes(x = pred_4, fill = party), 
                 data = hearings) +
            geom_histogram() +
            facet_wrap( ~ party),
          ggplot(aes(x = pred_4_boosted_party, fill = party), 
                 data = hearings) +
            geom_histogram() +
            facet_wrap( ~ party),
          nrow = 2)

### 3.2.1 Plots: Claims ----

#### 3.2.1.1 Totals per year - hearings, paragraphs, claims ----

total_hearings <- hearings %>%
  select(year, hearing_id) %>% distinct() %>% group_by(year) %>% 
  summarise(n = n()) %>% 
  ggplot() +
  geom_col(aes(x = year, y = n),
           color = "#6c6c6c", fill = "white",) +
  scale_x_continuous(breaks = scales::pretty_breaks(n=8),
                     expand = c(0.01,0)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n=4),
                     limits = c(0,50), expand = c(0.01,0)) +
  labs(x = "Year", y = "Hearings") +
  theme_pubclean() +
  theme(plot.margin = margin(t = 20))

total_attendance <- hearings %>%
  group_by(year, party) %>% 
  summarise(speaking = sum(speaking)) %>%
  ggplot(aes(year, speaking, color = party, fill = party)) +
  geom_col(position=position_dodge(width=.93)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n=8),
                     expand = c(0.01,0)) +
  labs(x = "Year", y = "Attendance", color = "Party", fill = "Party") +
  scale_color_manual(values=c("#FF0000", "#0015BC"),
                     labels = c("Republican", "Democratic")) +
  scale_fill_manual(values=c("#ffe0e2", "#dfdff6"),
                    labels = c("Republican", "Democratic")) +
  theme_pubclean()

total_participation <- hearings %>% filter(speaking == 1) %>%
  group_by(year, party) %>%
  summarise(n_paragraphs = sum(n_paragraphs)) %>%
  ggplot(aes(year, n_paragraphs, color = party, fill = party)) +
  geom_col(position=position_dodge(width=.93)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n=8),
                     expand = c(0.01,0)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n=5),
                     limits = c(0,11000), expand = c(0.01,0)) +
  labs(x = "Year", y = "Participation", color = "Party", fill = "Party") +
  scale_color_manual(values=c("#FF0000", "#0015BC"),
                     labels = c("Republican", "Democratic")) +
  scale_fill_manual(values=c("#ffe0e2", "#dfdff6"),
                    labels = c("Republican", "Democratic")) +
  theme_pubclean()

total_claims <- hearings %>% filter(speaking == 1) %>%
  group_by(year, party) %>%
  summarise(p4_total = sum(pred_4),
            p4bp_total = round(sum(pred_4_boosted_party),0),
            n_paragraphs = sum(n_paragraphs)) %>% 
  ggplot(aes(year, p4bp_total, color = party, fill = party)) +
  geom_col(position="dodge") +
  scale_x_continuous(breaks = scales::pretty_breaks(n=8),
                     expand = c(0.01,0)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n=6),
                     limits = c(0,1400), expand = c(0.01,0)) +
  labs(x = "Year", y = "Claim-making", color = "Party", 
       fill = "Party") +
  scale_color_manual(values=c("#FF0000", "#0015BC"),
                     labels = c("Republican", "Democratic")) +
  scale_fill_manual(values=c("#ffe0e2", "#dfdff6"),
                    labels = c("Republican", "Democratic")) +
  theme_pubclean()

total_cc <- hearings %>%
  select(year, party, CID, cc_fossilfuel) %>% 
  distinct() %>% 
  group_by(year, party) %>% 
  summarise(cc_fossilfuel = sum(cc_fossilfuel)/10) %>% 
  ggplot(aes(x = year, cc_fossilfuel, color = party)) +
  geom_line() +
  theme_bw() +
  scale_x_continuous(breaks = scales::pretty_breaks(n=8),
                     expand = c(.0625,.0625)) +
  labs(x = "Year", y = "Campaign\ncontributions", 
       color = "Party") +
  scale_color_manual(values=c("#FF0000", "#0015BC"),
                     labels = c("Republican", "Democratic")) +
  theme_pubclean()

plot_totals <- 
  ggarrange(total_hearings + theme(axis.title.x = element_blank()),
            total_attendance + theme(axis.title.x = element_blank()),
            total_participation + theme(axis.title.x = element_blank()), 
            total_claims + theme(axis.title.x = element_blank()),
            total_cc,
            ncol = 1, common.legend = T, align = "v",
            heights = c(2,2,2,2,2)); plot_totals

ggsave(filename = file.path("../plots","plot_totals.png"), plot_totals,
       width = 1900, height = 2200, unit = "px", dpi = 300)

#### 3.2.1.2 Average per MoC total vs proportion - boosted by party ----

hearings %>% filter(speaking == 1) %>%
  summarise(n_paragraphs = sum(n_paragraphs))

hearings %>% filter(speaking == 1) %>%
  group_by(party) %>% 
  summarise(per_party = sum(pred_4_boosted_party)/sum(n_paragraphs)*100)

hearings %>% filter(speaking == 1) %>%
  group_by(congress, party) %>%
  summarise(n_paragraphs = sum(n_paragraphs),
            p4bp_count_mean = MeanCI(pred_4_boosted_party)[1],
            p4bp_count_lower_ci = MeanCI(pred_4_boosted_party)[2],
            p4bp_count_upper_ci = MeanCI(pred_4_boosted_party)[3])

hearings %>% filter(speaking == 1) %>%
  mutate(per_pred_4_boosted_party = pred_4_boosted_party/n_paragraphs) %>% 
  group_by(congress, party) %>%
  summarise(n = n(),
            p4bp_mean = mean(per_pred_4_boosted_party),
            p4bp_lower_ci = MeanCI(per_pred_4_boosted_party)[2],
            p4bp_upper_ci = MeanCI(per_pred_4_boosted_party)[3])

plot_p4_boosted_party <-
  hearings %>% filter(speaking == 1) %>%
  mutate(per_pred_4_boosted_party = pred_4_boosted_party/n_paragraphs) %>% 
  group_by(congress, party) %>%
  summarise(p4bp_mean = mean(per_pred_4_boosted_party),
            p4bp_lower_ci = MeanCI(per_pred_4_boosted_party)[2],
            p4bp_upper_ci = MeanCI(per_pred_4_boosted_party)[3]) %>%
  mutate(p4bp_lower_ci = ifelse(p4bp_lower_ci<0, 0,
                                p4bp_lower_ci)) %>% 
  ungroup() %>% 
  ggplot(aes(congress, p4bp_mean, color = party)) + 
  geom_ribbon(aes(ymin=p4bp_lower_ci, ymax=p4bp_upper_ci,
                  fill = party, color = party),
              alpha = .08, linetype=2, size = .08) +
  geom_line() +
  scale_x_continuous(breaks = scales::pretty_breaks(n=4),
                     expand = c(0.01,0)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n=5),
                     labels = scales::percent,
                     limits = c(0,.27)) +
  labs(x="Congress", y="Claim paragraphs:\n Mean proportion", 
       color = "Party", fill = "Party") +
  theme_bw() +
  scale_fill_manual(values=c("#FF0000", "#0015BC"),
                    labels = c("Republican", "Democratic")) +
  scale_color_manual(values=c("#FF0000", "#0015BC"),
                     labels = c("Republican", "Democratic")) +
  theme(legend.position = "top"); plot_p4_boosted_party

ggsave(filename = file.path("../plots","plot_p4_boosted_party.png"), 
       plot_p4_boosted_party,
       width = 1300, height = 1000, unit = "px", dpi = 300)


#### 3.2.1.3 Average per MoC total vs proportion - raw predictions ----

hearings %>% filter(speaking == 1) %>%
  group_by(year, party) %>%
  summarise(n = n(),
            p4_count_mean = MeanCI(pred_4)[1],
            p4_count_lower_ci = MeanCI(pred_4)[2],
            p4_count_upper_ci = MeanCI(pred_4)[3]) 

hearings %>% filter(speaking == 1) %>%
  mutate(per_pred_4 = pred_4/n_paragraphs) %>% 
  group_by(year, party) %>%
  summarise(n = n(),
            p4_mean = mean(per_pred_4),
            p4_lower_ci = MeanCI(per_pred_4)[2],
            p4_upper_ci = MeanCI(per_pred_4)[3])


plot_p4_raw_appendix <-
  hearings %>% filter(speaking == 1) %>%
  mutate(per_pred_4 = pred_4/n_paragraphs) %>% 
  group_by(congress, party) %>%
  summarise(p4_mean = mean(per_pred_4),
            p4_lower_ci = MeanCI(per_pred_4)[2],
            p4_upper_ci = MeanCI(per_pred_4)[3]) %>% 
  mutate(p4_lower_ci = ifelse(p4_lower_ci<0, 0,
                              p4_lower_ci)) %>% 
  ungroup() %>% 
  ggplot(aes(congress, p4_mean, color = party)) + 
  geom_ribbon(aes(ymin=p4_lower_ci, ymax=p4_upper_ci,
                  fill = party, color = party),
              alpha = .08, linetype=2, size = .08) +
  geom_line() +
  scale_x_continuous(breaks = scales::pretty_breaks(n=4),
                     expand = c(0.01,0)) +
  scale_y_continuous(labels = scales::percent) +
  labs(x="Congress", y="Claim paragraphs:\nMean proportion", 
       color = "Party", fill = "Party") +
  theme_bw() +
  scale_fill_manual(values=c("#FF0000", "#0015BC"),
                    labels = c("Republican", "Democratic")) +
  scale_color_manual(values=c("#FF0000", "#0015BC"),
                     labels = c("Republican", "Democratic")) +
  theme(legend.position = "top"); plot_p4_raw_appendix

# NOTE: CIs not symmetrical everywhere as the lower values have been capped at 0

ggsave(filename = file.path("../plots","plot_p4_raw_appendix.png"), 
       plot_p4_raw_appendix,
       width = 1300, height = 1000, unit = "px", dpi = 300)

#### 3.2.1.4 Attendance:Participation:Claim-making - corrected predictions ----

plot_individuals_boxplot <- 
  rbind(hearings %>%
          group_by(congress, party, hearing_id) %>% 
          summarise(speaking_per = sum(speaking)/n()*100) %>%
          select(congress, party, speaking_per) %>% 
          mutate(type = "Attendance (%)") %>% 
          rename(value = speaking_per),
        hearings %>%
          filter(speaking==1) %>% 
          select(congress, party, n_paragraphs) %>% 
          mutate(type = "Participation (N)") %>% 
          rename(value = n_paragraphs),
        hearings %>%
          filter(speaking==1) %>% 
          select(congress, party, pred_4_boosted_party) %>% 
          mutate(type = "Claim-making (N)") %>% 
          rename(value = pred_4_boosted_party),
        hearings %>%
          select(congress, party, CID, cc_fossilfuel) %>% 
          distinct() %>% 
          select(congress, party, cc_fossilfuel) %>% 
          mutate(cc_fossilfuel = cc_fossilfuel*100,
                 type = "Campaign contributions\n(in $1K)") %>% 
          rename(value = cc_fossilfuel)) %>% 
  mutate(congress = as.factor(congress),
         type = factor(type, 
                       levels = c("Attendance (%)",
                                  "Participation (N)",
                                  "Claim-making (N)",
                                  "Campaign contributions\n(in $1K)")),
         party = factor(party, levels  = c("R", "D"))) %>% 
  ggplot(aes(x = congress, y = value, color = party)) +
  geom_boxplot(outlier.alpha = 0.2, width = 4, linewidth=.3) +
  facet_grid(vars(type), vars(party), scales = "free_y",
             labeller = labeller(party = c("D"="Democrats", "R"="Republicans"))) +
  scale_color_manual(values=c("#FF0000","#0015BC")) +
  labs(x = "Congress") +
  theme_bw() +
  theme(legend.position="none", 
        axis.title.y = element_blank()); plot_individuals_boxplot

plot_individuals_mean <- 
  rbind(hearings %>%
          group_by(congress, party, hearing_id) %>% 
          summarise(speaking_per = sum(speaking)/n()*100) %>%
          select(congress, party, speaking_per) %>% 
          mutate(type = "Attendance (%)") %>% 
          rename(value = speaking_per),
        hearings %>%
          filter(speaking==1) %>% 
          select(congress, party, n_paragraphs) %>% 
          mutate(type = "Participation (N)") %>% 
          rename(value = n_paragraphs),
        hearings %>%
          filter(speaking==1) %>% 
          select(congress, party, pred_4_boosted_party) %>% 
          mutate(type = "Claim-making (N)") %>% 
          rename(value = pred_4_boosted_party),
        hearings %>%
          select(congress, party, CID, cc_fossilfuel) %>% 
          distinct() %>% 
          select(congress, party, cc_fossilfuel) %>% 
          mutate(cc_fossilfuel = cc_fossilfuel*100,
                 type = "Campaign contributions\n(in $1K)") %>% 
          rename(value = cc_fossilfuel)) %>% 
  mutate(congress = as.factor(congress),
         type = factor(type, 
                       levels = c("Attendance (%)",
                                  "Participation (N)",
                                  "Claim-making (N)",
                                  "Campaign contributions\n(in $1K)")),
         
         party = factor(party, levels  = c("R", "D"))) %>% 
  group_by(congress, type, party) %>% 
  summarise(mean = MeanCI(value)[1],
            lower_ci = MeanCI(value)[2],
            upper_ci = MeanCI(value)[3],
            median = median(value)) %>% 
  ungroup() %>% 
  ggplot(aes(x = congress, y = mean, color = party, group = type)) +
  geom_ribbon(aes(ymin=lower_ci, ymax=upper_ci, 
                  fill = party, color = party),
              alpha = .08, linetype=0, size = .08) +
  geom_line() +
  geom_line(aes(y = median), linetype = 3, alpha = .6) +
  facet_grid(vars(type), vars(party), scales = "free_y",
             labeller = labeller(party = c("D"="Democrats", "R"="Republicans"))) +
  scale_color_manual(values=c("#FF0000", "#0015BC")) +
  scale_fill_manual(values=c("#FF0000", "#0015BC")) +
  labs(x = "Congress") +
  theme_bw() +
  theme(legend.position="none", 
        axis.title.y = element_blank()); plot_individuals_mean

ggsave(filename = file.path("../plots","plot_individuals_boxplot.png"), 
       plot_individuals_boxplot,
       width = 950, height = 2300, unit = "px", dpi = 300)

ggsave(filename = file.path("../plots","plot_individuals_mean.png"), 
       plot_individuals_mean,
       width = 950, height = 2300, unit = "px", dpi = 300)

# Extreme points

layer_data(plot_individuals_boxplot, i = 1L)[7]$outliers[[15]] %>% min()
hearings %>% 
  filter(congress==110 & party == "D") %>% 
  filter(n_paragraphs>=54) %>% 
  select(chamber, name, seniority, n_paragraphs) %>% 
  arrange(n_paragraphs) %>% 
  print(n=50)


layer_data(plot_individuals_boxplot, i = 1L)[7]$outliers[[16]] %>% min()
hearings %>% 
  filter(congress==111 & party == "D") %>% 
  filter(n_paragraphs>=59) %>% 
  select(chamber, name, seniority, n_paragraphs)%>%
  arrange(n_paragraphs) %>% 
  print(n=50)

# Extreme points
hearings %>%
  filter(n_paragraphs>352) %>% 
  select(title, hearing_id, name, chamber, n_paragraphs)
hearings %>%
  filter(pred_4_boosted_party>26) %>% 
  select(title, hearing_id, name, chamber, pred_4_boosted_party)

hearings %>%
  group_by(congress, party, committee, CID, majority) %>% 
  distinct() %>% 
  group_by(majority, party) %>% 
  summarise(n=n()) %>% 
  pivot_wider(names_from = "party", values_from = "n") %>% 
  mutate(sum = D+R,
         R_per = R/sum*100)


c(layer_data(plot_individuals_boxplot, i = 1L)[7]$outliers[[25]],
  layer_data(plot_individuals_boxplot, i = 1L)[7]$outliers[[26]],
  layer_data(plot_individuals_boxplot, i = 1L)[7]$outliers[[27]],
  layer_data(plot_individuals_boxplot, i = 1L)[7]$outliers[[28]])

c(layer_data(plot_individuals_boxplot, i = 1L)[7]$outliers[[29]],
  layer_data(plot_individuals_boxplot, i = 1L)[7]$outliers[[30]],
  layer_data(plot_individuals_boxplot, i = 1L)[7]$outliers[[31]],
  layer_data(plot_individuals_boxplot, i = 1L)[7]$outliers[[32]]) %>% hist()

library(magrittr)
hearings %>% filter(cc_fossilfuel>=4) %>% 
  select(congress, name, party) %>% distinct() %$% 
  table(party)

### 3.2.2 Plots: Sub-claims ----

#### 3.2.2.1 Average per MoC proportion - boosted by party ----

hearings %>% filter(speaking == 1) %>%
  select(year, party, n_paragraphs, pred_4_1_boosted_party, 
         pred_4_2_boosted_party, pred_4_3_boosted_party) %>% 
  group_by(party) %>%
  summarise(pred_4_1_boosted_party = sum(pred_4_1_boosted_party),
            pred_4_2_boosted_party = sum(pred_4_2_boosted_party),
            pred_4_3_boosted_party = sum(pred_4_3_boosted_party))

plot_p4_boosted_party_sub <- hearings %>% filter(speaking == 1) %>%
  select(year, party, n_paragraphs, pred_4_1_boosted_party, 
         pred_4_2_boosted_party, pred_4_3_boosted_party) %>% 
  pivot_longer(cols = c(pred_4_1_boosted_party,
                        pred_4_2_boosted_party,
                        pred_4_3_boosted_party)) %>% 
  mutate(per = value/n_paragraphs) %>% 
  group_by(year, party, name) %>% 
  summarise(per_mean = mean(per),
            per_lower_ci = MeanCI(per)[2],
            per_upper_ci = MeanCI(per)[3]) %>% 
  mutate(per_lower_ci = ifelse(per_lower_ci<0, 0,
                               per_lower_ci)) %>% 
  ggplot(aes(year, per_mean, color = party, fill = party)) +
  geom_ribbon(aes(ymin=per_lower_ci, ymax=per_upper_ci,
                  fill = party, color = party),
              alpha = .08, linetype=2, size = .08) +
  geom_line() +
  scale_x_continuous(breaks = scales::pretty_breaks(n=8),
                     expand = c(0.01,0)) +
  scale_y_continuous(labels = scales::percent) +
  facet_grid(name~party, labeller = 
               labeller(party = c(D = "Democrats", R = "Republicans"),
                        name = c(pred_4_1_boosted_party = "Sub-claim 4.1:\nClimate policies\nare harmful",
                                 pred_4_2_boosted_party = "Sub-claim 4.2:\nClimate policies\nare ineffective",
                                 pred_4_3_boosted_party = "Sub-claim 4.3:\nPolicy opposition\nfor other reasons"))) +
  scale_fill_manual(values=c("#FF0000", "#0015BC"),
                    labels = c("Republicans", "Democrats")) +
  scale_color_manual(values=c("#FF0000", "#0015BC"),
                     labels = c("Republicans", "Democrats")) +
  labs(x="Year", y="Mean proportion of claim paragraphs", 
       color = "Party", fill = "Party") +
  theme_bw() +
  theme(legend.position="none"); plot_p4_boosted_party_sub

ggsave(filename = file.path("../plots","plot_p4sub_boosted_party.png"), 
       plot_p4_boosted_party_sub,
       width = 1900, height = 1500, unit = "px", dpi = 300)

#### 3.2.1.2 Average per MoC proportion - raw predictions ----

plot_p4sub_raw_appendix <- hearings %>% filter(speaking == 1) %>%
  select(year, party, n_paragraphs, pred_4_1, 
         pred_4_2, pred_4_3) %>% 
  pivot_longer(cols = c(pred_4_1,
                        pred_4_2,
                        pred_4_3)) %>% 
  mutate(per = value/n_paragraphs) %>% 
  group_by(year, party, name) %>% 
  summarise(per_mean = mean(per),
            per_lower_ci = MeanCI(per)[2],
            per_upper_ci = MeanCI(per)[3]) %>% 
  mutate(per_lower_ci = ifelse(per_lower_ci<0, 0,
                               per_lower_ci)) %>% 
  ggplot(aes(year, per_mean, color = party, fill = party)) +
  geom_ribbon(aes(ymin=per_lower_ci, ymax=per_upper_ci,
                  fill = party, color = party),
              alpha = .08, linetype=2, size = .08) +
  geom_line() +
  scale_x_continuous(breaks = scales::pretty_breaks(n=8),
                     expand = c(0.01,0)) +
  scale_y_continuous(labels = scales::percent) +
  facet_grid(name~party, labeller = 
               labeller(party = c(D = "Democrats", R = "Republicans"),
                        name = c(pred_4_1 = "Sub-claim 4.1:\nClimate policies\nare harmful",
                                 pred_4_2 = "Sub-claim 4.2:\nClimate policies\nare ineffective",
                                 pred_4_3 = "Sub-claim 4.3:\nPolicy opposition\nfor other reasons"))) +
  scale_fill_manual(values=c("#FF0000", "#0015BC"),
                    labels = c("Republicans", "Democrats")) +
  scale_color_manual(values=c("#FF0000", "#0015BC"),
                     labels = c("Republicans", "Democrats")) +
  labs(x="Year", y="Mean proportion of claim paragraphs", 
       color = "Party", fill = "Party") +
  theme_bw() +
  theme(legend.position="none"); plot_p4sub_raw_appendix

ggsave(filename = file.path("../plots","plot_p4sub_raw_appendix.png"), 
       plot_p4sub_raw_appendix,
       width = 1900, height = 1500, unit = "px", dpi = 300)

# 4 Analysis -------------------------------------------------------------------

# Plot the count of paragraphs per MoC and hearing
par(mfrow = c(1, 2)) 
hist(hearings$n_paragraphs)
plot(hearings$cc_fossilfuel, hearings$n_paragraphs)

# Plot the predicted count of claims per MoC and hearing
hist(hearings$pred_4)
plot(hearings$cc_fossilfuel, hearings$pred_4)
par(mfrow = c(1, 1)) 

# Plot the number of predicted claims vs. the anbsolute number of 
# paragraphs by party
ggplot(data = hearings,
       aes(pred_4, n_paragraphs, color=party)) + 
  geom_point() +
  theme_minimal()

# Plot the prediction correction by correction type
hearings %>% 
  select(hearing_id, CID, party,
         pred_4, pred_4_sample, pred_4_boosted, pred_4_sample_party,
         pred_4_boosted_party) %>% 
  pivot_longer(!c(hearing_id, CID, party), 
               names_to = "Predictions",
               values_to = "count") %>% 
  mutate(Predictions = ifelse(Predictions == "pred_4",
                              "raw", Predictions),
         Predictions = ifelse(Predictions == "pred_4_sample",
                              "corrected (sample)", Predictions),
         Predictions = ifelse(Predictions == "pred_4_boosted",
                              "corrected (boosted)", Predictions),
         Predictions = ifelse(Predictions == "pred_4_sample_party",
                              "corrected (sample/by party)", Predictions),
         Predictions = ifelse(Predictions == "pred_4_boosted_party",
                              "corrected (boosted/by party)", Predictions),
         Predictions = factor(Predictions, 
                              levels = c("raw",
                                         "corrected (sample)",
                                         "corrected (boosted)",
                                         "corrected (sample/by party)",
                                         "corrected (boosted/by party)"))) %>% 
  data.frame() %>% 
  ggplot(aes(x = Predictions, count)) +
  geom_violin(size = .1, fill = "gray", alpha = .5) +
  geom_boxplot(width = .1) +
  scale_y_continuous(limits = c(0, 50)) +
  labs(x = "Claim", y = "Count per hearing per MoC") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  facet_wrap(vars(party))


## 4.1 Attendance (at) ----
# Whether an MoC was active or not (measured by spoken up)

# Formula:

formula_attendance <- (speaking ~ 
                         cc_fossilfuel + 
                         emp_fossil_per +
                         age +
                         gender +
                         seniority +                   
                         chamber +
                         majority +
                         lobbying_fossilfuel)

M_AT <- list()

### 4.1.1 Logistic regression models ----

summary(M_AT$at_R_log0 <-
          glm(speaking ~ 1,
              data = hearings %>% filter(party == "R"),
              family = binomial()))

summary(M_AT$at_R_log <-
          glm(formula_attendance,
              data = hearings %>% filter(party == "R"),
              family = binomial()))

summary(M_AT$at_D_log0 <-
          glm(speaking ~ 1,
              data = hearings %>% filter(party == "D"),
              family = binomial()))

summary(M_AT$at_D_log <-
          glm(formula_attendance,
              data = hearings %>% filter(party == "D"),
              family = binomial()))

# Model comparison
compareGLM(M_AT$at_R_log0, M_AT$at_R_log)
compareGLM(M_AT$at_D_log0, M_AT$at_D_log)

# Residual plots
par(mfrow = c(2, 2))
plot(M_AT$at_D_log)
plot(M_AT$at_R_log)

#### 4.1.1.1 Cluster-robust standard errors ----

# Clustered within committee & year

# "For nested two-way or multiway clustering one simply clusters at the 
# highest level of aggregation. For example, with individual-level data 
# and clustering on both household and state one should cluster on state"
# Cameron et al. 2011
# DOI: 10.1198/jbes.2010.07136
# https://www.tandfonline.com/doi/epdf/10.1198/jbes.2010.07136?needAccess=true

# committee member and hearing are nested within committee
# >> we will only cluster at the top level, i.e. committee & year

M_AT$at_D_vcov <- vcovCL(M_AT$at_D_log, ~ committee + year,
                         type = "HC0", fix = TRUE)
M_AT$at_D_robustSE <- coeftest(M_AT$at_D_log, M_AT$at_D_vcov)[,] %>%
  as.data.frame()
coeftest(M_AT$at_D_log, M_AT$at_D_vcov)

M_AT$at_R_vcov <- vcovCL(M_AT$at_R_log, ~ committee + year,
                         type = "HC0", fix = TRUE)
M_AT$at_R_robustSE <- coeftest(M_AT$at_R_log, M_AT$at_R_vcov)[,] %>%
  as.data.frame()
coeftest(M_AT$at_R_log, M_AT$at_R_vcov)

#### 4.1.1.2 Assumptions testing ----

## Multicollinearity
vif(M_AT$at_D_log)
vif(M_AT$at_R_log)
# >> No multicollinearity problems


## 4.2 Participation (pa) ------------------------------------------------
# Count of times each MoC spoke per hearing

# Formula:

formula_participation <- (n_paragraphs ~ 
                            cc_fossilfuel + 
                            emp_fossil_per +
                            age +
                            gender +
                            seniority +                   
                            chamber +
                            majority +
                            lobbying_fossilfuel)

M_PA <- list()

### 4.2.1 Poisson regression models ----

summary(M_PA$pa_R_poi0 <- 
          glm(n_paragraphs ~ 1, 
              data = hearings %>% filter(party == "R" & speaking == 1), 
              family = poisson()))


summary(M_PA$pa_R_poi <- 
          glm(formula_participation,
              data = hearings %>% filter(party == "R" & speaking == 1), 
              family = poisson()))

summary(M_PA$pa_D_poi0 <- 
          glm(n_paragraphs ~ 1, 
              data = hearings %>% filter(party == "D" & speaking == 1), 
              family = poisson()))

summary(M_PA$pa_D_poi <- 
          glm(formula_participation, 
              data = hearings %>% filter(party == "D" & speaking == 1), 
              family = poisson()))

# Model comparison
compareGLM(M_PA$pa_R_poi0, M_PA$pa_R_poi)
compareGLM(M_PA$pa_D_poi0, M_PA$pa_D_poi)

# Pearson Chi2 dispersion statistic

P__disp(M_PA$pa_R_poi)
P__disp(M_PA$pa_D_poi)

# >> Over-dispersion >> should be modeled with negative binomial 
#    regression rather than poisson regression

### 4.2.2 Negative binomial regression models ----
summary(M_PA$pa_R_nb0 <- 
          glm.nb(n_paragraphs ~ 1, 
                 data = hearings %>% filter(party == "R" & speaking == 1)))

summary(M_PA$pa_R_nb <- 
          glm.nb(formula_participation, 
                 data = hearings %>% filter(party == "R" & speaking == 1)))

summary(M_PA$pa_D_nb0 <- 
          glm.nb(n_paragraphs ~ 1, 
                 data = hearings %>% filter(party == "D" & speaking == 1)))

summary(M_PA$pa_D_nb <- 
          glm.nb(formula_participation, 
                 data = hearings %>% filter(party == "D" & speaking == 1)))

# Model comparison
compareGLM(M_PA$pa_R_nb0, M_PA$pa_R_nb)
compareGLM(M_PA$pa_D_nb0, M_PA$pa_D_nb)

# Residual plots
par(mfrow = c(2, 2))
plot(M_PA$pa_R_nb)
plot(M_PA$pa_D_nb)

#### 4.2.2.1 Cluster-robust standard errors ----

# Clustered within committee & year

M_PA$pa_R_vcov <- vcovCL(M_PA$pa_R_nb, ~ committee + year, 
                         type = "HC0", fix = TRUE)
M_PA$pa_R_robustSE <- coeftest(M_PA$pa_R_nb, 
                               M_PA$pa_R_vcov)[,] %>% 
  as.data.frame()
coeftest(M_PA$pa_R_nb, M_PA$pa_R_vcov)

M_PA$pa_D_vcov <- vcovCL(M_PA$pa_D_nb, ~ committee + year, 
                         type = "HC0", fix = TRUE)
M_PA$pa_D_robustSE <- coeftest(M_PA$pa_D_nb, 
                               M_PA$pa_D_vcov)[,] %>% 
  as.data.frame()
coeftest(M_PA$pa_D_nb, M_PA$pa_D_vcov)

#### 4.2.2.2 Assumptions testing ----

## Multicollinearity
vif(M_PA$pa_R_nb)
vif(M_PA$pa_D_nb)
# >> No multicollinearity problems


## 4.3 Claim-making (cl) --------------------------------------------------
# Count of paragraphs per MoC containing at least one claim per hearing

# Formulas:

formula_claim_making <- (pred_4 ~ 
                           cc_fossilfuel + 
                           emp_fossil_per +
                           age +
                           gender +
                           seniority +                   
                           chamber +
                           majority +
                           lobbying_fossilfuel)

formula_claim_making_corr <- (round(pred_4_boosted_party, 0) ~ 
                                cc_fossilfuel + 
                                emp_fossil_per +
                                age +
                                gender +
                                seniority +                   
                                chamber +
                                majority +
                                lobbying_fossilfuel)

M_CL <- list()

### 4.3.1 Poisson regression models ----

#### Raw predictions ----
summary(M_CL$cl_R_poi0 <- 
          glm(pred_4 ~ 1, 
              data = hearings %>% filter(party == "R" & speaking == 1)), 
        family = poisson())

summary(M_CL$cl_R_poi <- 
          glm(formula_claim_making, 
              data = hearings %>% filter(party == "R" & speaking == 1)), 
        family = poisson())

summary(M_CL$cl_D_poi0 <- 
          glm(pred_4 ~ 1, 
              data = hearings %>% filter(party == "D" & speaking == 1)), 
        family = poisson())

summary(M_CL$cl_D_poi <- 
          glm(formula_claim_making, 
              data = hearings %>% filter(party == "D" & speaking == 1)), 
        family = poisson())

#### Corrected predictions ----

summary(M_CL$cl_R_poi0_corr <- 
          glm(round(pred_4_boosted_party, 0) ~ 1, 
              data = hearings %>% filter(party == "R" & speaking == 1)), 
        family = poisson())

summary(M_CL$cl_R_poi_corr <- 
          glm(formula_claim_making_corr, 
              data = hearings %>% filter(party == "R" & speaking == 1)), 
        family = poisson())

summary(M_CL$cl_D_poi0_corr <- 
          glm(round(pred_4_boosted_party, 0) ~ 1, 
              data = hearings %>% filter(party == "D" & speaking == 1)), 
        family = poisson())

summary(M_CL$cl_D_poi_corr <- 
          glm(formula_claim_making_corr, 
              data = hearings %>% filter(party == "D" & speaking == 1)), 
        family = poisson())

# Pearson Chi2 dispersion statistic

P__disp(M_CL$cl_R_poi)
P__disp(M_CL$cl_D_poi)
P__disp(M_CL$cl_R_poi_corr)
P__disp(M_CL$cl_D_poi_corr)

# >> Over-dispersion >> should be modeled with negative binomial 
#    regression rather than poisson regression


### 4.3.2 Negative binomial regression models ----

#### Raw predictions ----
summary(M_CL$cl_R_nb0 <- 
          glm.nb(pred_4 ~ 1, 
                 data = hearings %>% filter(party == "R" & speaking == 1)))

summary(M_CL$cl_R_nb <- 
          glm.nb(formula_claim_making, 
                 data = hearings %>% filter(party == "R" & speaking == 1)))

summary(M_CL$cl_D_nb0 <- 
          glm.nb(pred_4 ~ 1, 
                 data = hearings %>% filter(party == "D" & speaking == 1)))

summary(M_CL$cl_D_nb <- 
          glm.nb(formula_claim_making, 
                 data = hearings %>% filter(party == "D" & speaking == 1)))

# Residual plots
par(mfrow = c(2, 2)) 
plot(M_CL$cl_R_nb)
plot(M_CL$cl_D_nb)

#### Corrected predictions ----
summary(M_CL$cl_R_nb0_corr <- 
          glm.nb(round(pred_4_boosted_party, 0) ~ 1, 
                 data = hearings %>% filter(party == "R" & speaking == 1)))

summary(M_CL$cl_R_nb_corr <- 
          glm.nb(formula_claim_making_corr, 
                 data = hearings %>% filter(party == "R" & speaking == 1)))

summary(M_CL$cl_D_nb0_corr <- 
          glm.nb(round(pred_4_boosted_party, 0) ~ 1, 
                 data = hearings %>% filter(party == "D" & speaking == 1)))

summary(M_CL$cl_D_nb_corr <- 
          glm.nb(formula_claim_making_corr, 
                 data = hearings %>% filter(party == "D" & speaking == 1)))

# Residual plots
par(mfrow = c(2, 2)) 
plot(M_CL$cl_R_nb_corr)
plot(M_CL$cl_D_nb_corr)

#### 4.3.2.1 Cluster-robust standard errors ----

# Clustered within committee & year

##### Raw predictions ----

M_CL$cl_R_vcov <- vcovCL(M_CL$cl_R_nb, ~ committee + year, 
                         type = "HC0", fix = TRUE)
M_CL$cl_R_robustSE <- coeftest(M_CL$cl_R_nb, 
                               M_CL$cl_R_vcov)[,] %>% 
  as.data.frame()
coeftest(M_CL$cl_R_nb, M_CL$cl_R_vcov)

M_CL$cl_D_vcov <- vcovCL(M_CL$cl_D_nb, ~ committee + year, 
                         type = "HC0", fix = TRUE)
M_CL$cl_D_robustSE <- coeftest(M_CL$cl_D_nb, 
                               M_CL$cl_D_vcov)[,] %>% 
  as.data.frame()
coeftest(M_CL$cl_D_nb, M_CL$cl_D_vcov)

##### Corrected predictions ----

M_CL$cl_R_corr_vcov <- vcovCL(M_CL$cl_R_nb_corr, ~ committee + year, 
                              type = "HC0", fix = TRUE)
M_CL$cl_R_corr_robustSE <- coeftest(M_CL$cl_R_nb_corr, 
                                    M_CL$cl_R_corr_vcov)[,] %>% 
  as.data.frame()
coeftest(M_CL$cl_R_nb_corr, M_CL$cl_R_corr_vcov)

M_CL$cl_D_corr_vcov <- vcovCL(M_CL$cl_D_nb_corr, ~ committee + year, 
                              type = "HC0", fix = TRUE)
M_CL$cl_D_corr_robustSE <- coeftest(M_CL$cl_D_nb_corr, 
                                    M_CL$cl_D_corr_vcov)[,] %>% 
  as.data.frame()
coeftest(M_CL$cl_D_nb_corr, M_CL$cl_D_corr_vcov)

# -0.9231975 - 1.96*0.9800770
# -0.9231975 + 1.96*0.9800770

ggplot(data = hearings, aes(n_paragraphs, pred_4_boosted_party, color=party)) + 
  geom_point(alpha = .2) + facet_wrap(~party) +
  geom_smooth(method = 'loess')

hearings %>% select(congress, CID, cc_fossilfuel, party) %>% 
  # distinct() %>% 
  group_by(party) %>% 
  summarise(cc_fossilfuel = mean(cc_fossilfuel))

hearings %>% filter(party=="R") %$% 
  range(lobbying_fossilfuel)

#### 4.3.2.2 Assumptions testing ----

## Multicollinearity

vif(M_CL$cl_R_nb)
vif(M_CL$cl_R_nb_corr)

# No multicollinearity problems

### 4.2.5 Inspection of prevalent claim makers

hearings %>% filter(pred_4>30) %>% 
  select(name, party, pred_4, pred_4_boosted_party) %>% 
  arrange(desc(party), desc(pred_4))

# 5. Results plots ----

library(ggeffects)

describe(hearings[hearings$party=="R",]$cc_fossilfuel)
describe(hearings[hearings$party=="D",]$cc_fossilfuel)

options(ggeffects_margin = "marginalmeans")

# Effect plots for independent variable

effect_data <- rbind(
  predict_response(M_AT$at_R_log,
                   terms = "cc_fossilfuel [all]",
                   vcov = M_AT$at_R_vcov) %>% 
    as.data.frame() %>% 
    mutate(party = "Republicans", type = "Attendance (%)"),
  predict_response(M_PA$pa_R_nb,
                   terms = "cc_fossilfuel [all]",
                   vcov = M_PA$pa_R_vcov) %>% 
    as.data.frame() %>% 
    mutate(party = "Republicans", type = "Participation (N)"),
  predict_response(M_CL$cl_R_nb_corr,
                   terms = "cc_fossilfuel [all]",
                   vcov = M_CL$cl_R_corr_vcov) %>% 
    as.data.frame() %>% 
    mutate(party = "Republicans", type = "Claim-making (N)"),
  predict_response(M_AT$at_D_log,
                   terms = "cc_fossilfuel [all]",
                   vcov = M_AT$at_D_vcov) %>% 
    as.data.frame() %>% 
    mutate(party = "Democrats", type = "Attendance (%)"),
  predict_response(M_PA$pa_D_nb,
                   terms = "cc_fossilfuel [all]",
                   vcov = M_PA$pa_D_vcov) %>% 
    as.data.frame() %>% 
    mutate(party = "Democrats", type = "Participation (N)"),
  predict_response(M_CL$cl_D_nb_corr,
                   terms = "cc_fossilfuel [all]",
                   vcov = M_CL$cl_D_corr_vcov) %>% 
    as.data.frame() %>% 
    mutate(party = "Democrats", type = "Claim-making (N)"))  %>% 
  mutate(party = factor(party, levels = c("Republicans", "Democrats")))

plot_attendance <- 
  ggplot(effect_data %>% filter(type == "Attendance (%)"), 
         aes(x=x, y=predicted, color = party, fill = party)) +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), 
              alpha = .08, linetype=0, size = .08) +
  geom_line() +
  facet_grid(~party) +
  scale_y_continuous(labels = scales::percent, 
                     breaks = scales::pretty_breaks(),
                     limits = c(0,1)) +
  labs(x = "FFI campaign contributions (in $100K)", y = "Attendance (%)") +
  scale_color_manual(values=c("#FF0000", "#0015BC")) +
  scale_fill_manual(values=c("#FF0000", "#0015BC")) +
  theme_bw() +
  theme(legend.position="none"); plot_attendance

plot_participation <- 
  ggplot(effect_data %>% filter(type == "Participation (N)"), 
         aes(x=x, y=predicted, color = party, fill = party)) +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), 
              alpha = .08, linetype=0, size = .08) +  geom_line() +
  facet_grid(~party) +
  coord_cartesian(ylim = c(20, 140)) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  labs(x = "FFI campaign contributions (in $100K)", y = "Participation (N)") +
  scale_color_manual(values=c("#FF0000", "#0015BC")) +
  scale_fill_manual(values=c("#FF0000", "#0015BC")) +
  theme_bw() +
  theme(legend.position="none"); plot_participation

plot_claim_making <- 
  ggplot(effect_data %>% filter(type == "Claim-making (N)"), 
         aes(x=x, y=predicted, color = party, fill = party)) +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), 
              alpha = .08, linetype=0, size = .08) +  geom_line() +
  facet_grid(~party) +
  scale_y_continuous(breaks = scales::pretty_breaks(7)) +
  labs(x = "FFI campaign contributions (in $100K)", y = "Claim-making (N)") +
  scale_color_manual(values=c("#FF0000", "#0015BC")) +
  scale_fill_manual(values=c("#FF0000", "#0015BC")) +
  theme_bw() +
  theme(legend.position="none"); plot_claim_making


# Effect plots for variables of interest

plot_participation_emp <- 
  rbind(
    predict_response(M_PA$pa_R_nb,
                     terms = "emp_fossil_per [all]",
                     vcov = M_PA$pa_R_vcov) %>% 
      as.data.frame() %>% 
      mutate(party = "Republicans", type = "Participation (N)"),
    predict_response(M_PA$pa_D_nb,
                     terms = "emp_fossil_per [all]",
                     vcov = M_PA$pa_D_vcov) %>% 
      as.data.frame() %>% 
      mutate(party = "Democrats", type = "Participation (N)"))  %>% 
  mutate(party = factor(party, levels = c("Republicans", "Democrats"))) %>% 
  ggplot(aes(x=x, y=predicted, color = party, fill = party)) +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), 
              alpha = .08, linetype=0, size = .08) +  geom_line() +
  facet_grid(~party) +
  scale_x_continuous(breaks = scales::pretty_breaks(7)) +
  scale_y_continuous(breaks = scales::pretty_breaks(7)) +
  labs(x = "FFI employment (in %)", y = "Participation (N)") +
  scale_color_manual(values=c("#FF0000", "#0015BC")) +
  scale_fill_manual(values=c("#FF0000", "#0015BC")) +
  theme_bw() +
  theme(legend.position="none"); plot_participation_emp

plot_claim_making_lobbying <- 
  rbind(
    predict_response(M_CL$cl_R_nb_corr,
                     terms = "lobbying_fossilfuel [all]",
                     vcov = M_CL$cl_R_corr_vcov) %>% 
      as.data.frame() %>% 
      mutate(party = "Republicans", type = "Claim-making (N)"),
    predict_response(M_CL$cl_D_nb_corr,
                     terms = "lobbying_fossilfuel [all]",
                     vcov = M_CL$cl_D_corr_vcov) %>% 
      as.data.frame() %>% 
      mutate(party = "Democrats", type = "Claim-making (N)"))  %>% 
  mutate(party = factor(party, levels = c("Republicans", "Democrats"))) %>%
  ggplot(aes(x=x, y=predicted, color = party, fill = party)) +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), 
              alpha = .08, linetype=0, size = .08) +  geom_line() +
  facet_grid(~party) +
  scale_x_continuous(breaks = scales::pretty_breaks(7)) +
  scale_y_continuous(breaks = scales::pretty_breaks(7)) +
  labs(x = "FFI lobbying (in $1M)", y = "Claim-making (N)") +
  scale_color_manual(values=c("#FF0000", "#0015BC")) +
  scale_fill_manual(values=c("#FF0000", "#0015BC")) +
  theme_bw() +
  theme(legend.position="none"); plot_claim_making_lobbying


# Save plots
ggsave(filename = file.path("../plots","plot_EMM_attendance.png"),
       plot_attendance,
       width = 1200, height = 700, unit = "px", dpi = 300)
ggsave(filename = file.path("../plots","plot_EMM_participation.png"),
       plot_participation,
       width = 1200, height = 700, unit = "px", dpi = 300)
ggsave(filename = file.path("../plots","plot_EMM_claim_making.png"),
       plot_claim_making,
       width = 1200, height = 700, unit = "px", dpi = 300)
ggsave(filename = file.path("../plots","plot_EMM_participation_emp.png"),
       plot_participation_emp,
       width = 1200, height = 700, unit = "px", dpi = 300)
ggsave(filename = file.path("../plots","plot_EMM_claim_making_lobbying.png"),
       plot_claim_making_lobbying,
       width = 1200, height = 700, unit = "px", dpi = 300)


# plot_results <- ggarrange(
#   ggplot(effect_data %>% filter(type == "Attendance (%)"),
#          aes(x=x, y=predicted, color = party, fill = party)) +
#     geom_ribbon(aes(ymin=conf.low, ymax=conf.high),
#                 alpha = .08, linetype=0, size = .08) +
#     geom_line() +
#     facet_grid(type~party) +
#     scale_y_continuous(labels = scales::percent,
#                        breaks = scales::pretty_breaks(),
#                        limits = c(0,1)) +
#     labs(x = "FFI campaign contributions (in $100K)", y = "Attendance (%)") +
#     scale_color_manual(values=c("#FF0000", "#0015BC")) +
#     scale_fill_manual(values=c("#FF0000", "#0015BC")) +
#     theme_bw() +
#     theme(legend.position="none",
#           axis.title = element_blank(),
#           axis.text.x = element_blank()),
#   ggplot(effect_data %>% filter(type == "Participation (N)"),
#          aes(x=x, y=predicted, color = party, fill = party)) +
#     geom_ribbon(aes(ymin=conf.low, ymax=conf.high),
#                 alpha = .08, linetype=0, size = .08) +  geom_line() +
#     facet_grid(type~party) +
#     coord_cartesian(ylim = c(20, 140)) +
#     scale_y_continuous(breaks = scales::pretty_breaks()) +
#     labs(x = "FFI campaign contributions (in $100K)", y = "Participation (N)") +
#     scale_color_manual(values=c("#FF0000", "#0015BC")) +
#     scale_fill_manual(values=c("#FF0000", "#0015BC")) +
#     theme_bw() +
#     theme(legend.position="none",
#           axis.title = element_blank(),
#           strip.background.x = element_blank(),
#           strip.text.x = element_blank(),
#           axis.text.x = element_blank()),
#   ggplot(effect_data %>% filter(type == "Claim-making (N)"),
#          aes(x=x, y=predicted, color = party, fill = party)) +
#     geom_ribbon(aes(ymin=conf.low, ymax=conf.high),
#                 alpha = .08, linetype=0, size = .08) +  geom_line() +
#     facet_grid(type~party) +
#     scale_y_continuous(breaks = scales::pretty_breaks(7)) +
#     labs(x = "FFI campaign contributions (in $100K)", y = "Claim-making (N)") +
#     scale_color_manual(values=c("#FF0000", "#0015BC")) +
#     scale_fill_manual(values=c("#FF0000", "#0015BC")) +
#     theme_bw() +
#     theme(legend.position="none",
#           axis.title.y = element_blank(),
#           strip.background.x = element_blank(),
#           strip.text.x = element_blank()),
#   nrow = 3, align = "v", heights = c(1.1,1,1.125)); plot_results
# 
# 
# ggsave(filename = file.path("../plots","plot_EMM_IV.png"),
#        plot_results,
#        width = 1200, height = 2300, unit = "px", dpi = 300)

# 6. Results export ----

covariate_labels <- c("FFI campaign contributions (in \\$100K)",
                      "FFI employment (in \\%)",
                      "Age",
                      "Gender: Male",
                      "Seniority: Chairperson",
                      "Seniority: Ranking minority member",
                      "Chamber: Senate",
                      "Majority: Democratic",
                      "FFI lobbying (in \\$1M)")
## In text ----

### Attendance ----
stargazer(M_AT$at_R_log, M_AT$at_D_log, 
          type = "latex",
          title = "Attendance: Logistic regression model of the hearing attendance 
          of Republican and Democratic committee members, respectively. 
          Significance levels are based on cluster-robust standard errors 
          clustered within the committee and the year. 
          Both the raw and the cluster-robust standard errors can be found in
          Tab. XXX.",
          model.names = FALSE,
          report = "vcs*", 
          dep.var.labels = "Attendance",
          star.cutoffs = c(.1, .05, .01, .001),
          star.char = c(".", "*", "**", "***"),
          column.labels = c("Republicans","Democrats"),
          se = list(M_AT$at_R_robustSE$`Std. Error`,
                    M_AT$at_D_robustSE$`Std. Error`),
          p = list(M_AT$at_R_robustSE$`Pr(>|z|)`,
                   M_AT$at_D_robustSE$`Pr(>|z|)`),
          covariate.labels = covariate_labels,
          notes.append = FALSE,
          notes = "$.p<0.01, ^{*}p<0.05; ^{*}p<0.01; ^{***}p<0.001$",
          align = TRUE,
          no.space = TRUE, 
          single.row = TRUE)

### Participation ----
stargazer(M_PA$pa_R_nb, M_PA$pa_D_nb, 
          type = "text",
          title = "Participation: Negative binomial regression model of the 
          count of paragraphs per committee member and hearing for the 
          Republican and the Democratic party, respectively. Significance levels 
          are based on cluster-robust standard errors clustered within the 
          committee and the year. Both the raw and the 
          cluster-robust standard errors can be found in Tab. XXX.",
          model.names = FALSE,
          report = "vcs*", 
          dep.var.labels = "Participation",
          star.cutoffs = c(.075, .05, .01, .001),
          star.char = c(".", "*", "**", "***"),
          column.labels = c("Republicans","Democrats"),
          se = list(M_PA$pa_R_robustSE$`Std. Error`,
                    M_PA$pa_D_robustSE$`Std. Error`),
          p = list(M_PA$pa_R_robustSE$`Pr(>|z|)`,
                   M_PA$pa_D_robustSE$`Pr(>|z|)`),
          covariate.labels = covariate_labels,
          notes.append = FALSE,
          notes = "$.p<0.01, ^{*}p<0.05; ^{*}p<0.01; ^{***}p<0.001$",
          align = TRUE,
          no.space = TRUE, 
          single.row = TRUE)

### Claim-making ----
stargazer(M_CL$cl_R_nb_corr, M_CL$cl_D_nb_corr,
          type = "latex",
          title = "Contrarian claim-making: Negative binomial regression model 
          of the bias-adjusted count of paragraphs predicted to contain a
          solutions contrarianism claim per committee member and hearing for 
          Republican and Democratic committee members, respectively. 
          Significance levels are based on cluster-robust standard errors
          clustered within the committee and the year. Both 
          the raw and the cluster-robust standard errors can be found in Tab.
          XXX.",
          model.names = FALSE,
          report = "vcs*", 
          dep.var.labels = "Claim paragraphs",
          star.cutoffs = c(.1, .05, .01, .001),
          star.char = c(".", "*", "**", "***"),
          column.labels = c("Republicans","Democrats"),
          se = list(M_CL$cl_R_corr_robustSE$`Std. Error`,
                    M_CL$cl_D_corr_robustSE$`Std. Error`),
          p = list(M_CL$cl_R_corr_robustSE$`Pr(>|z|)`,
                   M_CL$cl_D_corr_robustSE$`Pr(>|z|)`),
          covariate.labels = covariate_labels,
          notes.append = FALSE,
          notes = "$.p<0.01, ^{*}p<0.05; ^{*}p<0.01; ^{***}p<0.001$",
          align = TRUE,
          no.space = TRUE, 
          single.row = TRUE)


## Appendix ----

### Attendance ----
stargazer(M_AT$at_R_log, M_AT$at_R_log,
          M_AT$at_D_log, M_AT$at_D_log,
          type = "latex",
          title = "Attendance: Logistic regression model of the hearing attendance 
          of Republican and Democratic committee members, respectively, with 
          raw standard errors, as well as, cluster-robust standard errors 
          clustered within the committee and the year.",
          model.names = FALSE,
          report = "vcs*", 
          dep.var.labels = "Attendance",
          star.cutoffs = c(.1, .05, .01, .001),
          star.char = c(".", "*", "**", "***"),
          column.labels = c("Republicans", "Republicans",
                            "Democrats", "Democrats"),
          covariate.labels = covariate_labels,
          se = list(NULL, M_AT$at_R_robustSE$`Std. Error`,
                    NULL, M_AT$at_D_robustSE$`Std. Error`),
          p = list(NULL, M_AT$at_R_robustSE$`Pr(>|z|)`,
                   NULL, M_AT$at_D_robustSE$`Pr(>|z|)`),
          notes.append = FALSE,
          notes = "$.p<0.01, ^{*}p<0.05; ^{*}p<0.01; ^{***}p<0.001$",
          align = TRUE,
          no.space = TRUE,
          single.row = TRUE)

### Participation ----
stargazer(M_PA$pa_R_nb, M_PA$pa_R_nb,
          M_PA$pa_D_nb, M_PA$pa_D_nb,
          type = "latex",
          title = "Participation: Negative binomial regression model of the 
          count of paragraphs per committee member and hearing for the 
          Republican and the Democratic party, respectively, with raw standard 
          errors, as well as, cluster-robust standard errors clustered within 
          the committee and the year.",
          model.names = FALSE,
          report = "vcs*", 
          dep.var.labels = "Participation",
          star.cutoffs = c(.1, .05, .01, .001),
          star.char = c(".", "*", "**", "***"),
          column.labels = c("Republicans", "Republicans",
                            "Democrats", "Democrats"),
          covariate.labels = covariate_labels,
          se = list(NULL, M_PA$pa_R_robustSE$`Std. Error`,
                    NULL, M_PA$pa_D_robustSE$`Std. Error`),
          p = list(NULL, M_PA$pa_R_robustSE$`Pr(>|z|)`,
                   NULL, M_PA$pa_D_robustSE$`Pr(>|z|)`),
          notes.append = FALSE,
          notes = "$.p<0.01, ^{*}p<0.05; ^{*}p<0.01; ^{***}p<0.001$",
          align = TRUE,
          no.space = TRUE,
          single.row = TRUE)

### Claim-making ----

stargazer(M_CL$cl_R_nb, M_CL$cl_R_nb,
          M_CL$cl_D_nb, M_CL$cl_D_nb,
          type = "latex",
          title = "Contrarian claim-making: Negative binomial regression model 
          of the raw and the bias-adjusted, respectively, count of paragraphs 
          predicted to contain a solutions contrarianism claim per committee 
          member and hearing for Republican committee members with raw standard 
          errors, as well as, cluster-robust standard errors clustered within 
          the committee and the year.",
          model.names = FALSE,
          report = "vcs*", 
          dep.var.labels = "Claim-making (raw predictions)",
          star.cutoffs = c(.1, .05, .01, .001),
          star.char = c(".", "*", "**", "***"),
          column.labels = c("Republicans", "Republicans",
                            "Democrats", "Democrats"),
          covariate.labels = covariate_labels,
          se = list(NULL, M_CL$cl_R_robustSE$`Std. Error`,
                    NULL, M_CL$cl_D_robustSE$`Std. Error`),
          p = list(NULL, M_CL$cl_R_robustSE$`Pr(>|z|)`,
                   NULL, M_CL$cl_D_robustSE$`Pr(>|z|)`),
          notes.append = FALSE,
          notes = "$.p<0.01, ^{*}p<0.05; ^{*}p<0.01; ^{***}p<0.001$",
          align = TRUE,
          no.space = TRUE,
          single.row = TRUE)

stargazer(M_CL$cl_R_nb_corr, M_CL$cl_R_nb_corr,
          M_CL$cl_D_nb_corr, M_CL$cl_D_nb_corr,
          type = "latex",
          title = "Contrarian claim-making: Negative binomial regression model 
          of the raw and the bias-adjusted, respectively, count of paragraphs 
          predicted to contain a solutions contrarianism claim per committee 
          member and hearing for Republican committee members with raw standard 
          errors, as well as, cluster-robust standard errors clustered within 
          the committee and the year.",
          model.names = FALSE,
          report = "vcs*", 
          dep.var.labels = "Claim-making (corrected predictions)",
          star.cutoffs = c(.1, .05, .01, .001),
          star.char = c(".", "*", "**", "***"),
          column.labels = c("Republicans", "Republicans",
                            "Democrats", "Democrats"),
          covariate.labels = covariate_labels,
          se = list(NULL, M_CL$cl_R_corr_robustSE$`Std. Error`,
                    NULL, M_CL$cl_D_corr_robustSE$`Std. Error`),
          p = list(NULL, M_CL$cl_R_corr_robustSE$`Pr(>|z|)`,
                   NULL, M_CL$cl_D_corr_robustSE$`Pr(>|z|)`),
          notes.append = FALSE,
          notes = "$.p<0.01, ^{*}p<0.05; ^{*}p<0.01; ^{***}p<0.001$",
          align = TRUE,
          no.space = TRUE,
          single.row = TRUE)


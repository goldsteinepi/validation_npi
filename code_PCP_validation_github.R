#################
# NPI validation
# Requires: NPI monthly data file, Medicare claims data
#################

#######################CREATE A MERGED DATASET#######################
### FUNCTIONS ###
library(data.table) #read large CSVs
library(dplyr) #filtering

## STEP 1A ##
### READ DATA ###
#NPI data: https://www.cms.gov/medicare/regulations-guidance/administrative-simplification/data-dissemination
npi_data <- fread(file="npidata.csv", stringsAsFactors=F)
#2021 Medicare claims data: https://data.cms.gov/resources/medicare-physician-other-practitioners-by-provider-and-service-data-dictionary
claims_data <- read.csv(file="Medicare_Physician_Other_Practitioners_by_Provider_and_Service_2021.csv", stringsAsFactors=F)

## STEP 1B ##
#subset to physicians, NPs and PAs using primary taxonomy only
all_providers_of_interest<- c("207K00000X", "207KA0200X", "207KI0005X", "207L00000X", "207LA0401X", "207LC0200X", "207LH0002X", "207LP2900X", "208U00000X", "208C00000X", "207N00000X", "207NI0002X", "207ND0900X", "207ND0101X", "207NS0135X", "204R00000X", "207P00000X", "207PE0004X", "207PH0002X", "207PT0002X", "207PP0204X", "207PS0010X", "207PE0005X", "207Q00000X", "207QA0401X", "207QA0505X", "207QG0300X", "207QH0002X", "207QB0002X", "207QS1201X", "207QS0010X", "208D00000X", "208M00000X", "202C00000X", "202D00000X", "207R00000X", "207RA0401X", "207RA0002X", "207RA0001X", "207RA0201X", "207RC0000X", "207RI0001X", "207RC0001X", "207RC0200X", "207RE0101X", "207RG0100X", "207RG0300X", "207RH0000X", "207RH0003X", "207RI0008X", "207RH0002X", "207RH0005X", "207RI0200X", "207RI0011X", "207RM1200X", "207RX0202X", "207RN0300X", "207RB0002X", "207RP1001X", "207RR0500X", "207RS0012X", "207RS0010X", "207RT0003X", "209800000X", "207SG0202X", "207SC0300X", "207SG0201X", "207SG0203X", "207SG0207X", "207SM0001X", "207SG0205X", "207T00000X", "204D00000X", "204C00000X", "207U00000X", "207UN0903X", "207UN0901X", "207UN0902X", "207V00000X", "207VC0300X", "207VC0200X", "207VF0040X", "207VX0201X", "207VG0400X", "207VH0002X", "207VM0101X", "207VB0002X", "207VX0000X", "207VE0102X", "207W00000X", "207WX0120X", "207WX0009X", "207WX0109X", "207WX0200X", "207WX0107X", "207WX0108X", "204E00000X", "207X00000X", "207XS0114X", "207XX0004X", "207XS0106X", "207XS0117X", "207XX0801X", "207XX0005X", "207Y00000X", "207YS0123X", "207YX0602X", "207YX0905X", "207YX0901X", "207YX0007X", "207YS0012X", "208VP0014X", "208VP0000X", "207ZP0101X", "207ZP0102X", "207ZB0001X", "207ZP0104X", "207ZC0008X", "207ZC0006X", "207ZP0105X", "207ZC0500X", "207ZD0900X", "207ZF0201X", "207ZH0000X", "207ZI0100X", "207ZM0300X", "207ZP0007X", "207ZN0500X", "202K00000X", "208100000X", "2081P0301X", "2081H0002X", "2081N0008X", "2081P2900X", "2081P0004X", "2081S0010X", "208200000X", "2082S0099X", "2082S0105X", "2083A0300X", "2083A0100X", "2083C0008X", "2083T0002X", "2083B0002X", "2083X0100X", "2083P0500X", "2083P0901X", "2083S0010X", "2083P0011X", "2084A0401X", "2084P0802X", "2084B0040X", "2084P0301X", "2084N0600X", "2084D0003X", "2084E0001X", "2084F0202X", "2084P0805X", "2084H0002X", "2084A2900X", "2084P0005X", "2084N0400X", "2084N0402X", "2084N0008X", "2084B0002X", "2084P2900X", "2084P0800X", "2084P0015X", "2084S0012X", "2084S0010X", "2084V0102X", "2085B0100X", "2085D0003X", "2085R0202X", "2085U0001X", "2085H0002X", "2085N0700X", "2085N0904X", "2085R0001X", "2085R0205X", "2085R0203X", "2085R0204X", "208600000X", "2086H0002X", "2086S0122X", "2086S0105X", "2086S0102X", "2086X0206X", "2086S0127X", "2086S0129X", "208G00000X", "204F00000X", "208800000X", "2088F0040X","363A00000X", "363AM0700X","363AS0400X", "363L00000X", "363LA2100X", "363LA2200X", "363LC1500X", "363LC0200X", "363LF0000X", "363LG0600X", "363LX0001X", "363LX0106X", "363LP2300X", "363LP0808X", "363LW0102X")

npi_data = subset(npi_data, (npi_data$`Healthcare Provider Primary Taxonomy Switch_1`=="Y" & npi_data$`Healthcare Provider Taxonomy Code_1` %in% all_providers_of_interest) |
                    (npi_data$`Healthcare Provider Primary Taxonomy Switch_2`=="Y" & npi_data$`Healthcare Provider Taxonomy Code_2` %in% all_providers_of_interest) |
                    (npi_data$`Healthcare Provider Primary Taxonomy Switch_3`=="Y" & npi_data$`Healthcare Provider Taxonomy Code_3` %in% all_providers_of_interest) |
                    (npi_data$`Healthcare Provider Primary Taxonomy Switch_4`=="Y" & npi_data$`Healthcare Provider Taxonomy Code_4` %in% all_providers_of_interest) |
                    (npi_data$`Healthcare Provider Primary Taxonomy Switch_5`=="Y" & npi_data$`Healthcare Provider Taxonomy Code_5` %in% all_providers_of_interest) |
                    (npi_data$`Healthcare Provider Primary Taxonomy Switch_6`=="Y" & npi_data$`Healthcare Provider Taxonomy Code_6` %in% all_providers_of_interest) |
                    (npi_data$`Healthcare Provider Primary Taxonomy Switch_7`=="Y" & npi_data$`Healthcare Provider Taxonomy Code_7` %in% all_providers_of_interest) |
                    (npi_data$`Healthcare Provider Primary Taxonomy Switch_8`=="Y" & npi_data$`Healthcare Provider Taxonomy Code_8` %in% all_providers_of_interest) |
                    (npi_data$`Healthcare Provider Primary Taxonomy Switch_9`=="Y" & npi_data$`Healthcare Provider Taxonomy Code_9` %in% all_providers_of_interest) |
                    (npi_data$`Healthcare Provider Primary Taxonomy Switch_10`=="Y" & npi_data$`Healthcare Provider Taxonomy Code_10` %in% all_providers_of_interest) |
                    (npi_data$`Healthcare Provider Primary Taxonomy Switch_11`=="Y" & npi_data$`Healthcare Provider Taxonomy Code_11` %in% all_providers_of_interest) |
                    (npi_data$`Healthcare Provider Primary Taxonomy Switch_12`=="Y" & npi_data$`Healthcare Provider Taxonomy Code_12` %in% all_providers_of_interest) |
                    (npi_data$`Healthcare Provider Primary Taxonomy Switch_13`=="Y" & npi_data$`Healthcare Provider Taxonomy Code_13` %in% all_providers_of_interest) |
                    (npi_data$`Healthcare Provider Primary Taxonomy Switch_14`=="Y" & npi_data$`Healthcare Provider Taxonomy Code_14` %in% all_providers_of_interest) |
                    (npi_data$`Healthcare Provider Primary Taxonomy Switch_15`=="Y" & npi_data$`Healthcare Provider Taxonomy Code_15` %in% all_providers_of_interest))

rm(all_providers_of_interest)

#create role indicators
physician_taxonomies<-c("207K00000X", "207KA0200X", "207KI0005X", "207L00000X", "207LA0401X", "207LC0200X", "207LH0002X", "207LP2900X", "208U00000X", "208C00000X", "207N00000X", "207NI0002X", "207ND0900X", "207ND0101X", "207NS0135X", "204R00000X", "207P00000X", "207PE0004X", "207PH0002X", "207PT0002X", "207PP0204X", "207PS0010X", "207PE0005X", "207Q00000X", "207QA0401X", "207QA0505X", "207QG0300X", "207QH0002X", "207QB0002X", "207QS1201X", "207QS0010X", "208D00000X", "208M00000X", "202C00000X", "202D00000X", "207R00000X", "207RA0401X", "207RA0002X", "207RA0001X", "207RA0201X", "207RC0000X", "207RI0001X", "207RC0001X", "207RC0200X", "207RE0101X", "207RG0100X", "207RG0300X", "207RH0000X", "207RH0003X", "207RI0008X", "207RH0002X", "207RH0005X", "207RI0200X", "207RI0011X", "207RM1200X", "207RX0202X", "207RN0300X", "207RB0002X", "207RP1001X", "207RR0500X", "207RS0012X", "207RS0010X", "207RT0003X", "209800000X", "207SG0202X", "207SC0300X", "207SG0201X", "207SG0203X", "207SG0207X", "207SM0001X", "207SG0205X", "207T00000X", "204D00000X", "204C00000X", "207U00000X", "207UN0903X", "207UN0901X", "207UN0902X", "207V00000X", "207VC0300X", "207VC0200X", "207VF0040X", "207VX0201X", "207VG0400X", "207VH0002X", "207VM0101X", "207VB0002X", "207VX0000X", "207VE0102X", "207W00000X", "207WX0120X", "207WX0009X", "207WX0109X", "207WX0200X", "207WX0107X", "207WX0108X", "204E00000X", "207X00000X", "207XS0114X", "207XX0004X", "207XS0106X", "207XS0117X", "207XX0801X", "207XX0005X", "207Y00000X", "207YS0123X", "207YX0602X", "207YX0905X", "207YX0901X", "207YX0007X", "207YS0012X", "208VP0014X", "208VP0000X", "207ZP0101X", "207ZP0102X", "207ZB0001X", "207ZP0104X", "207ZC0008X", "207ZC0006X", "207ZP0105X", "207ZC0500X", "207ZD0900X", "207ZF0201X", "207ZH0000X", "207ZI0100X", "207ZM0300X", "207ZP0007X", "207ZN0500X", "202K00000X", "208100000X", "2081P0301X", "2081H0002X", "2081N0008X", "2081P2900X", "2081P0004X", "2081S0010X", "208200000X", "2082S0099X", "2082S0105X", "2083A0300X", "2083A0100X", "2083C0008X", "2083T0002X", "2083B0002X", "2083X0100X", "2083P0500X", "2083P0901X", "2083S0010X", "2083P0011X", "2084A0401X", "2084P0802X", "2084B0040X", "2084P0301X", "2084N0600X", "2084D0003X", "2084E0001X", "2084F0202X", "2084P0805X", "2084H0002X", "2084A2900X", "2084P0005X", "2084N0400X", "2084N0402X", "2084N0008X", "2084B0002X", "2084P2900X", "2084P0800X", "2084P0015X", "2084S0012X", "2084S0010X", "2084V0102X", "2085B0100X", "2085D0003X", "2085R0202X", "2085U0001X", "2085H0002X", "2085N0700X", "2085N0904X", "2085R0001X", "2085R0205X", "2085R0203X", "2085R0204X", "208600000X", "2086H0002X", "2086S0122X", "2086S0105X", "2086S0102X", "2086X0206X", "2086S0127X", "2086S0129X", "208G00000X", "204F00000X", "208800000X", "2088F0040X")
PA_taxonomies<-c("363A00000X", "363AM0700X","363AS0400X")
NP_taxonomies<-c("363L00000X", "363LA2100X", "363LA2200X", "363LC1500X", "363LC0200X", "363LF0000X", "363LG0600X", "363LX0001X", "363LX0106X", "363LP2300X", "363LP0808X", "363LW0102X")

npi_data$physician = ifelse((npi_data$`Healthcare Provider Primary Taxonomy Switch_1`=="Y" & npi_data$`Healthcare Provider Taxonomy Code_1` %in% physician_taxonomies) |
                              (npi_data$`Healthcare Provider Primary Taxonomy Switch_2`=="Y" & npi_data$`Healthcare Provider Taxonomy Code_2` %in% physician_taxonomies) |
                              (npi_data$`Healthcare Provider Primary Taxonomy Switch_3`=="Y" & npi_data$`Healthcare Provider Taxonomy Code_3` %in% physician_taxonomies) |
                              (npi_data$`Healthcare Provider Primary Taxonomy Switch_4`=="Y" & npi_data$`Healthcare Provider Taxonomy Code_4` %in% physician_taxonomies) |
                              (npi_data$`Healthcare Provider Primary Taxonomy Switch_5`=="Y" & npi_data$`Healthcare Provider Taxonomy Code_5` %in% physician_taxonomies) |
                              (npi_data$`Healthcare Provider Primary Taxonomy Switch_6`=="Y" & npi_data$`Healthcare Provider Taxonomy Code_6` %in% physician_taxonomies) |
                              (npi_data$`Healthcare Provider Primary Taxonomy Switch_7`=="Y" & npi_data$`Healthcare Provider Taxonomy Code_7` %in% physician_taxonomies) |
                              (npi_data$`Healthcare Provider Primary Taxonomy Switch_8`=="Y" & npi_data$`Healthcare Provider Taxonomy Code_8` %in% physician_taxonomies) |
                              (npi_data$`Healthcare Provider Primary Taxonomy Switch_9`=="Y" & npi_data$`Healthcare Provider Taxonomy Code_9` %in% physician_taxonomies) |
                              (npi_data$`Healthcare Provider Primary Taxonomy Switch_10`=="Y" & npi_data$`Healthcare Provider Taxonomy Code_10` %in% physician_taxonomies) |
                              (npi_data$`Healthcare Provider Primary Taxonomy Switch_11`=="Y" & npi_data$`Healthcare Provider Taxonomy Code_11` %in% physician_taxonomies) |
                              (npi_data$`Healthcare Provider Primary Taxonomy Switch_12`=="Y" & npi_data$`Healthcare Provider Taxonomy Code_12` %in% physician_taxonomies) |
                              (npi_data$`Healthcare Provider Primary Taxonomy Switch_13`=="Y" & npi_data$`Healthcare Provider Taxonomy Code_13` %in% physician_taxonomies) |
                              (npi_data$`Healthcare Provider Primary Taxonomy Switch_14`=="Y" & npi_data$`Healthcare Provider Taxonomy Code_14` %in% physician_taxonomies) |
                              (npi_data$`Healthcare Provider Primary Taxonomy Switch_15`=="Y" & npi_data$`Healthcare Provider Taxonomy Code_15` %in% physician_taxonomies), 1, 0)

rm(physician_taxonomies)

npi_data$pa = ifelse((npi_data$`Healthcare Provider Primary Taxonomy Switch_1`=="Y" & npi_data$`Healthcare Provider Taxonomy Code_1` %in% PA_taxonomies) |
                       (npi_data$`Healthcare Provider Primary Taxonomy Switch_2`=="Y" & npi_data$`Healthcare Provider Taxonomy Code_2` %in% PA_taxonomies) |
                       (npi_data$`Healthcare Provider Primary Taxonomy Switch_3`=="Y" & npi_data$`Healthcare Provider Taxonomy Code_3` %in% PA_taxonomies) |
                       (npi_data$`Healthcare Provider Primary Taxonomy Switch_4`=="Y" & npi_data$`Healthcare Provider Taxonomy Code_4` %in% PA_taxonomies) |
                       (npi_data$`Healthcare Provider Primary Taxonomy Switch_5`=="Y" & npi_data$`Healthcare Provider Taxonomy Code_5` %in% PA_taxonomies) |
                       (npi_data$`Healthcare Provider Primary Taxonomy Switch_6`=="Y" & npi_data$`Healthcare Provider Taxonomy Code_6` %in% PA_taxonomies) |
                       (npi_data$`Healthcare Provider Primary Taxonomy Switch_7`=="Y" & npi_data$`Healthcare Provider Taxonomy Code_7` %in% PA_taxonomies) |
                       (npi_data$`Healthcare Provider Primary Taxonomy Switch_8`=="Y" & npi_data$`Healthcare Provider Taxonomy Code_8` %in% PA_taxonomies) |
                       (npi_data$`Healthcare Provider Primary Taxonomy Switch_9`=="Y" & npi_data$`Healthcare Provider Taxonomy Code_9` %in% PA_taxonomies) |
                       (npi_data$`Healthcare Provider Primary Taxonomy Switch_10`=="Y" & npi_data$`Healthcare Provider Taxonomy Code_10` %in% PA_taxonomies) |
                       (npi_data$`Healthcare Provider Primary Taxonomy Switch_11`=="Y" & npi_data$`Healthcare Provider Taxonomy Code_11` %in% PA_taxonomies) |
                       (npi_data$`Healthcare Provider Primary Taxonomy Switch_12`=="Y" & npi_data$`Healthcare Provider Taxonomy Code_12` %in% PA_taxonomies) |
                       (npi_data$`Healthcare Provider Primary Taxonomy Switch_13`=="Y" & npi_data$`Healthcare Provider Taxonomy Code_13` %in% PA_taxonomies) |
                       (npi_data$`Healthcare Provider Primary Taxonomy Switch_14`=="Y" & npi_data$`Healthcare Provider Taxonomy Code_14` %in% PA_taxonomies) |
                       (npi_data$`Healthcare Provider Primary Taxonomy Switch_15`=="Y" & npi_data$`Healthcare Provider Taxonomy Code_15` %in% PA_taxonomies), 1, 0)

rm(PA_taxonomies)

npi_data$np = ifelse((npi_data$`Healthcare Provider Primary Taxonomy Switch_1`=="Y" & npi_data$`Healthcare Provider Taxonomy Code_1` %in% NP_taxonomies) |
                       (npi_data$`Healthcare Provider Primary Taxonomy Switch_2`=="Y" & npi_data$`Healthcare Provider Taxonomy Code_2` %in% NP_taxonomies) |
                       (npi_data$`Healthcare Provider Primary Taxonomy Switch_3`=="Y" & npi_data$`Healthcare Provider Taxonomy Code_3` %in% NP_taxonomies) |
                       (npi_data$`Healthcare Provider Primary Taxonomy Switch_4`=="Y" & npi_data$`Healthcare Provider Taxonomy Code_4` %in% NP_taxonomies) |
                       (npi_data$`Healthcare Provider Primary Taxonomy Switch_5`=="Y" & npi_data$`Healthcare Provider Taxonomy Code_5` %in% NP_taxonomies) |
                       (npi_data$`Healthcare Provider Primary Taxonomy Switch_6`=="Y" & npi_data$`Healthcare Provider Taxonomy Code_6` %in% NP_taxonomies) |
                       (npi_data$`Healthcare Provider Primary Taxonomy Switch_7`=="Y" & npi_data$`Healthcare Provider Taxonomy Code_7` %in% NP_taxonomies) |
                       (npi_data$`Healthcare Provider Primary Taxonomy Switch_8`=="Y" & npi_data$`Healthcare Provider Taxonomy Code_8` %in% NP_taxonomies) |
                       (npi_data$`Healthcare Provider Primary Taxonomy Switch_9`=="Y" & npi_data$`Healthcare Provider Taxonomy Code_9` %in% NP_taxonomies) |
                       (npi_data$`Healthcare Provider Primary Taxonomy Switch_10`=="Y" & npi_data$`Healthcare Provider Taxonomy Code_10` %in% NP_taxonomies) |
                       (npi_data$`Healthcare Provider Primary Taxonomy Switch_11`=="Y" & npi_data$`Healthcare Provider Taxonomy Code_11` %in% NP_taxonomies) |
                       (npi_data$`Healthcare Provider Primary Taxonomy Switch_12`=="Y" & npi_data$`Healthcare Provider Taxonomy Code_12` %in% NP_taxonomies) |
                       (npi_data$`Healthcare Provider Primary Taxonomy Switch_13`=="Y" & npi_data$`Healthcare Provider Taxonomy Code_13` %in% NP_taxonomies) |
                       (npi_data$`Healthcare Provider Primary Taxonomy Switch_14`=="Y" & npi_data$`Healthcare Provider Taxonomy Code_14` %in% NP_taxonomies) |
                       (npi_data$`Healthcare Provider Primary Taxonomy Switch_15`=="Y" & npi_data$`Healthcare Provider Taxonomy Code_15` %in% NP_taxonomies), 1, 0)

rm(NP_taxonomies)

## STEP 1C ##

#filtering to only include providers that signed up in 2021 or earlier
#dates are in this format: (MM/DD/YYYY) for "Provider Enumeration Date"; "NPI Deactivation Date"; "NPI Reactivation Date"
npi_data$`Provider Enumeration Date` <- as.Date(npi_data$`Provider Enumeration Date`, format = "%m/%d/%Y")
npi_data <- npi_data[npi_data$`Provider Enumeration Date` <= as.Date("2021-12-31"), ] 

#filtering to only include providers that are active in 2021 (`NPI Deactivation Date` is NA OR `NPI Deactivation Date` is after 1/1/2021 OR before 1/1/2021 but 'NPI Reactivation Date' is between 1/1/2021 and 12/31/2021 )
npi_data$`NPI Deactivation Date` <- as.Date(npi_data$`NPI Deactivation Date`, format = "%m/%d/%Y")
npi_data$`NPI Reactivation Date` <- as.Date(npi_data$`NPI Reactivation Date`, format = "%m/%d/%Y")

npi_data <- npi_data %>%
  filter(
    is.na(`NPI Deactivation Date`) |
      `NPI Deactivation Date` > as.Date("2021-01-01") |
      (`NPI Deactivation Date` <= as.Date("2021-01-01") & 
         `NPI Reactivation Date` >= as.Date("2021-01-01") & 
         `NPI Reactivation Date` <= as.Date("2021-12-31"))
  )

## STEP 1D ##

#filter to include individual NPI accounts only
npi_data <- subset(npi_data, `Entity Type Code` == 1)

## STEP 1E ##

#merging the 2021 active NPIs with the 2021 claims to create a new dataset
claims_npi_data <- inner_join(claims_data, npi_data, by = c("Rndrng_NPI" = "NPI"))

### CREATE STRATIFIED DATASETS ###

physicians = subset(claims_npi_data, claims_npi_data$physician==1)
nps = subset(claims_npi_data, claims_npi_data$np==1)
pas = subset(claims_npi_data, claims_npi_data$pa==1)

#cleanup and save
rm(claims_data,claims_npi_data,npi_data)
save.image("merged_npi_claims_data.RData")

#####################TAKE A RANDOM SAMPLE OF 785 FOR EACH ROLE#######################

#PHYSICIANS
#create list of unique physicians
unique_NPI<-unique(physicians$Rndrng_NPI)
set.seed(123)
random_NPI_785<-sample(unique_NPI, size=785)

#create a df of random sample of 785 physicians and claims
random_physicians_785<-physicians[physicians$Rndrng_NPI %in% random_NPI_785, ]

#exporting
write.csv(random_physicians_785, file = "random_physicians_785.csv", row.names = FALSE,na="")

#NURSE PRACTITIONERS
#create list of unique NPs
unique_NPI_nps<-unique(nps$Rndrng_NPI)

#take a random sample of 785 providers
set.seed(123)
random_NPI_nps_785<-sample(unique_NPI_nps, size=785)

#create a df of random sample of 785 physicians and claims
random_nps_785<-nps[nps$Rndrng_NPI %in% random_NPI_nps_785, ]

#exporting
write.csv(random_nps_785, file = "random_nps_785.csv", row.names = FALSE,na="")

#PHYSICIAN ASSISTANTS
#create list of unique PAs
unique_NPI_pas<-unique(pas$Rndrng_NPI)

#random sample of 785 providers
set.seed(123)
random_NPI_pas_785<-sample(unique_NPI_pas, size=785)

#create a df of random sample of 785 PAs and claims
random_pas_785<-pas[pas$Rndrng_NPI %in% random_NPI_pas_785, ]

#exporting
write.csv(random_pas_785, file = "random_pas_785.csv", row.names = FALSE,na="")

#######################VALIDATION SECTION#######################
#installing required packages
library(readr)
library(irr)
library(boot)
library(gridExtra)
library (epiR)
library(caret)
library(dplyr)

###PHYSICIANS####
## CREATING INDICATORS ##
#specify the PCP primary taxonomy codes of interest
physician_pcp_codes<-c("207Q00000X","207QA0505X","207QG0300X","208D00000X","207R00000X","207RG0300X","207V00000X","207VG0400X")
#create primary taxonomy pcp indicator: 1=yes, 0=no
random_physicians_785$primary_pcp = ifelse(
  (random_physicians_785$`Healthcare Provider Primary Taxonomy Switch_1` == "Y" & random_physicians_785$`Healthcare Provider Taxonomy Code_1` %in% physician_pcp_codes) |
    (random_physicians_785$`Healthcare Provider Primary Taxonomy Switch_2` == "Y" & random_physicians_785$`Healthcare Provider Taxonomy Code_2` %in% physician_pcp_codes) |
    (random_physicians_785$`Healthcare Provider Primary Taxonomy Switch_3` == "Y" & random_physicians_785$`Healthcare Provider Taxonomy Code_3` %in% physician_pcp_codes) |
    (random_physicians_785$`Healthcare Provider Primary Taxonomy Switch_4` == "Y" & random_physicians_785$`Healthcare Provider Taxonomy Code_4` %in% physician_pcp_codes) |
    (random_physicians_785$`Healthcare Provider Primary Taxonomy Switch_5` == "Y" & random_physicians_785$`Healthcare Provider Taxonomy Code_5` %in% physician_pcp_codes) |
    (random_physicians_785$`Healthcare Provider Primary Taxonomy Switch_6` == "Y" & random_physicians_785$`Healthcare Provider Taxonomy Code_6` %in% physician_pcp_codes) |
    (random_physicians_785$`Healthcare Provider Primary Taxonomy Switch_7` == "Y" & random_physicians_785$`Healthcare Provider Taxonomy Code_7` %in% physician_pcp_codes) |
    (random_physicians_785$`Healthcare Provider Primary Taxonomy Switch_8` == "Y" & random_physicians_785$`Healthcare Provider Taxonomy Code_8` %in% physician_pcp_codes) |
    (random_physicians_785$`Healthcare Provider Primary Taxonomy Switch_9` == "Y" & random_physicians_785$`Healthcare Provider Taxonomy Code_9` %in% physician_pcp_codes) |
    (random_physicians_785$`Healthcare Provider Primary Taxonomy Switch_10` == "Y" & random_physicians_785$`Healthcare Provider Taxonomy Code_10` %in% physician_pcp_codes) |
    (random_physicians_785$`Healthcare Provider Primary Taxonomy Switch_11` == "Y" & random_physicians_785$`Healthcare Provider Taxonomy Code_11` %in% physician_pcp_codes) |
    (random_physicians_785$`Healthcare Provider Primary Taxonomy Switch_12` == "Y" & random_physicians_785$`Healthcare Provider Taxonomy Code_12` %in% physician_pcp_codes) |
    (random_physicians_785$`Healthcare Provider Primary Taxonomy Switch_13` == "Y" & random_physicians_785$`Healthcare Provider Taxonomy Code_13` %in% physician_pcp_codes) |
    (random_physicians_785$`Healthcare Provider Primary Taxonomy Switch_14` == "Y" & random_physicians_785$`Healthcare Provider Taxonomy Code_14` %in% physician_pcp_codes) |
    (random_physicians_785$`Healthcare Provider Primary Taxonomy Switch_15` == "Y" & random_physicians_785$`Healthcare Provider Taxonomy Code_15` %in% physician_pcp_codes),
  1, 0
)

#create any taxonomy pcp indicator: 1=yes, 0=no
random_physicians_785$any_pcp = ifelse(
  (random_physicians_785$`Healthcare Provider Taxonomy Code_1` %in% physician_pcp_codes) |
    (random_physicians_785$`Healthcare Provider Taxonomy Code_2` %in% physician_pcp_codes) |
    (random_physicians_785$`Healthcare Provider Taxonomy Code_3` %in% physician_pcp_codes) |
    (random_physicians_785$`Healthcare Provider Taxonomy Code_4` %in% physician_pcp_codes) |
    (random_physicians_785$`Healthcare Provider Taxonomy Code_5` %in% physician_pcp_codes) |
    (random_physicians_785$`Healthcare Provider Taxonomy Code_6` %in% physician_pcp_codes) |
    (random_physicians_785$`Healthcare Provider Taxonomy Code_7` %in% physician_pcp_codes) |
    (random_physicians_785$`Healthcare Provider Taxonomy Code_8` %in% physician_pcp_codes) |
    (random_physicians_785$`Healthcare Provider Taxonomy Code_9` %in% physician_pcp_codes) |
    (random_physicians_785$`Healthcare Provider Taxonomy Code_10` %in% physician_pcp_codes) |
    (random_physicians_785$`Healthcare Provider Taxonomy Code_11` %in% physician_pcp_codes) |
    (random_physicians_785$`Healthcare Provider Taxonomy Code_12` %in% physician_pcp_codes) |
    (random_physicians_785$`Healthcare Provider Taxonomy Code_13` %in% physician_pcp_codes) |
    (random_physicians_785$`Healthcare Provider Taxonomy Code_14` %in% physician_pcp_codes) |
    (random_physicians_785$`Healthcare Provider Taxonomy Code_15` %in% physician_pcp_codes),
  1, 0
)

#specify the medicare HCPCS codes of interest
hcpcs_codes <- c("G0439", "G0438", "G0468", "S5190", "G0402")

#create medicare indicator for PCP: 1=yes, 0=no
random_physicians_785 <- random_physicians_785 %>%
  group_by(Rndrng_NPI) %>%
  mutate(claim_pcp = as.numeric(any(hcpcs_codes %in% HCPCS_Cd))) %>%
  ungroup()

#create a data frame with unique providers
physicians <- random_physicians_785 %>%
  distinct(Rndrng_NPI, .keep_all = TRUE)

table(physicians$claim_pcp)
#  0   1 
#687  98

table(physicians$any_pcp)
#0   1 
#477  308 

table(physicians$primary_pcp)
#  0   1 
#527  258

#calculate confusion matrix for primary taxonomy pcp
#convert to factor, req of confusionMatrix function
physicians$claim_pcp <- factor(physicians$claim_pcp, levels = c(0, 1))
physicians$primary_pcp <- factor(physicians$primary_pcp, levels = c(0, 1))
physicians_confusion<-confusionMatrix(data = physicians$primary_pcp, reference = physicians$claim_pcp,positive="1")
#calculating 95% CI
epi.tests(physicians_confusion$table, conf.level=0.95)
#specificity: 0.7598 (0.73, 0.79), sensitivity: 0.9490 (0.88, 0.98)

#calculate confusion matrix for any taxonomy pcp
#convert to factor, req of confusionMatrix function
physicians$any_pcp <- factor(physicians$any_pcp, levels = c(0, 1))
any_physicians_confusion<-confusionMatrix(data = physicians$any_pcp, reference = physicians$claim_pcp,positive="1")
#calculating 95% CI
epi.tests(any_physicians_confusion$table, conf.level=0.95)
#specificity: 0.6885 (0.65, 0.72), sensitivity 0.9592 (0.90, 0.99)

write.csv(physicians, file = "classified_physicians.csv", row.names = FALSE,na="")

###PHYSICIAN ASSISTANTS###
#specify the PCP primary taxonomy codes of interest
pa_pcp_codes<-c("363A00000X","363AM0700X")

#create primary taxonomy pcp indicator: 1=yes, 0=no
random_pas_785$primary_pcp = ifelse((random_pas_785$`Healthcare Provider Primary Taxonomy Switch_1`=="Y" & random_pas_785$`Healthcare Provider Taxonomy Code_1` %in% pa_pcp_codes) | 
                                      (random_pas_785$`Healthcare Provider Primary Taxonomy Switch_2`=="Y" & random_pas_785$`Healthcare Provider Taxonomy Code_2` %in% pa_pcp_codes) | 
                                      (random_pas_785$`Healthcare Provider Primary Taxonomy Switch_3`=="Y" & random_pas_785$`Healthcare Provider Taxonomy Code_3` %in% pa_pcp_codes) | 
                                      (random_pas_785$`Healthcare Provider Primary Taxonomy Switch_4`=="Y" & random_pas_785$`Healthcare Provider Taxonomy Code_4` %in% pa_pcp_codes) | 
                                      (random_pas_785$`Healthcare Provider Primary Taxonomy Switch_5`=="Y" & random_pas_785$`Healthcare Provider Taxonomy Code_5` %in% pa_pcp_codes) | 
                                      (random_pas_785$`Healthcare Provider Primary Taxonomy Switch_6`=="Y" & random_pas_785$`Healthcare Provider Taxonomy Code_6` %in% pa_pcp_codes) | 
                                      (random_pas_785$`Healthcare Provider Primary Taxonomy Switch_7`=="Y" & random_pas_785$`Healthcare Provider Taxonomy Code_7` %in% pa_pcp_codes) | 
                                      (random_pas_785$`Healthcare Provider Primary Taxonomy Switch_8`=="Y" & random_pas_785$`Healthcare Provider Taxonomy Code_8` %in% pa_pcp_codes) | 
                                      (random_pas_785$`Healthcare Provider Primary Taxonomy Switch_9`=="Y" & random_pas_785$`Healthcare Provider Taxonomy Code_9` %in% pa_pcp_codes) | 
                                      (random_pas_785$`Healthcare Provider Primary Taxonomy Switch_10`=="Y" & random_pas_785$`Healthcare Provider Taxonomy Code_10` %in% pa_pcp_codes) | 
                                      (random_pas_785$`Healthcare Provider Primary Taxonomy Switch_11`=="Y" & random_pas_785$`Healthcare Provider Taxonomy Code_11` %in% pa_pcp_codes) | 
                                      (random_pas_785$`Healthcare Provider Primary Taxonomy Switch_12`=="Y" & random_pas_785$`Healthcare Provider Taxonomy Code_12` %in% pa_pcp_codes) | 
                                      (random_pas_785$`Healthcare Provider Primary Taxonomy Switch_13`=="Y" & random_pas_785$`Healthcare Provider Taxonomy Code_13` %in% pa_pcp_codes) | 
                                      (random_pas_785$`Healthcare Provider Primary Taxonomy Switch_14`=="Y" & random_pas_785$`Healthcare Provider Taxonomy Code_14` %in% pa_pcp_codes) | 
                                      (random_pas_785$`Healthcare Provider Primary Taxonomy Switch_15`=="Y" & random_pas_785$`Healthcare Provider Taxonomy Code_15` %in% pa_pcp_codes), 1, 0)


random_pas_785$any_pcp = ifelse((random_pas_785$`Healthcare Provider Taxonomy Code_1` %in% pa_pcp_codes) | 
                                  (random_pas_785$`Healthcare Provider Taxonomy Code_2` %in% pa_pcp_codes) | 
                                  (random_pas_785$`Healthcare Provider Taxonomy Code_3` %in% pa_pcp_codes) | 
                                  (random_pas_785$`Healthcare Provider Taxonomy Code_4` %in% pa_pcp_codes) | 
                                  (random_pas_785$`Healthcare Provider Taxonomy Code_5` %in% pa_pcp_codes) | 
                                  (random_pas_785$`Healthcare Provider Taxonomy Code_6` %in% pa_pcp_codes) | 
                                  (random_pas_785$`Healthcare Provider Taxonomy Code_7` %in% pa_pcp_codes) | 
                                  (random_pas_785$`Healthcare Provider Taxonomy Code_8` %in% pa_pcp_codes) | 
                                  (random_pas_785$`Healthcare Provider Taxonomy Code_9` %in% pa_pcp_codes) | 
                                  (random_pas_785$`Healthcare Provider Taxonomy Code_10` %in% pa_pcp_codes) | 
                                  (random_pas_785$`Healthcare Provider Taxonomy Code_11` %in% pa_pcp_codes) | 
                                  (random_pas_785$`Healthcare Provider Taxonomy Code_12` %in% pa_pcp_codes) | 
                                  (random_pas_785$`Healthcare Provider Taxonomy Code_13` %in% pa_pcp_codes) | 
                                  (random_pas_785$`Healthcare Provider Taxonomy Code_14` %in% pa_pcp_codes) | 
                                  (random_pas_785$`Healthcare Provider Taxonomy Code_15` %in% pa_pcp_codes), 1, 0)

#specify the medicare HCPCS codes of interest
hcpcs_codes <- c("G0439", "G0438", "G0468", "S5190", "G0402")

#create medicare indicator for PCP: 1=yes, 0=no
random_pas_785 <- random_pas_785 %>%
  group_by(Rndrng_NPI) %>%
  mutate(claim_pcp = as.numeric(any(hcpcs_codes %in% HCPCS_Cd))) %>%
  ungroup()

#create a data frame with unique providers
pas <- random_pas_785 %>%
  distinct(Rndrng_NPI, .keep_all = TRUE)

table(pas$claim_pcp)
#  0   1 
#719  66

table(pas$any_pcp)
#0   1 
#44  741 

table(pas$primary_pcp)
#  0   1 
#55  730

table(pas$primary_pcp, pas$claim_pcp)
table(pas$any_pcp, pas$claim_pcp)

##CALCULATING KAPPA
#defining our "rater" columns
rater_cols <- c("primary_pcp", "claim_pcp")

#function to compute kappa
compute_kappa <- function(data, indices) {
  kappa_result <- kappa2(data[indices, rater_cols])
  return(kappa_result$value)
}
#[1] 0.01373699

#bootstrap resampling
boot_kappas <- boot(data = pas, statistic = compute_kappa, R = 1000)

#mean kappa
boot_kappas$t0
#[1] 0.01373699

#storing the 1000 kappa values
t<-boot_kappas$t

#95% CI
quantile(t, c(0.025, 0.975))
#     2.5%     97.5% 
#0.009197366 0.019548618 

##simulation-based method adapted from Burstyn et al.: https://pubmed.ncbi.nlm.nih.gov/24302507/
##DEFINING VALUES
k<-100000 #size of simulation
KP.LO<-runif(k, 0.008, 0.01) #UNIFORM DISTN of lower bound on kappa
KP.HI<-runif(k, 0.01, 0.03) # UNIFORM DISTN of high bound of kappa
#PCP prev: 34.26%
PREV.CLBRT<-rbeta(k, 10.594, 19.41) #BETA distribution of exposure prevalence: : https://shiny.vet.unimelb.edu.au/epi/beta.buster/

##CALCULATIONS
#lower bound on SN and SP
SN.LO<-KP.LO/((1-PREV.CLBRT) + KP.LO*PREV.CLBRT)
SP.LO<-KP.LO/(PREV.CLBRT + KP.LO*(1-PREV.CLBRT))

#unconstrained priors on SN and SP
SN<-runif(k, SN.LO,1)
SP<-runif(k, SP.LO,1)

#apply constraints
p<-PREV.CLBRT
kappa.naive<-(p*(SP-1+SN)^2)*(p-1)/((p*SN-SP-p+p*SP)*(p*SN+1-SP-p+p*SP))
lo<-rep(0, k)
hi<-rep(0, k)
for (i in 1:k) {if(kappa.naive[i] < KP.LO[i] ) lo[i] <- 1}
sum(lo)
for (i in 1:k) {if(kappa.naive[i] > KP.HI[i] ) hi[i] <- 1}
sum(hi)
random<-rep(0, k)
add<-SN+SP
for (i in 1:k) {if(add[i]<1) random[i] <- 1}
sum(random)

#prior after constraints
pq1<-cbind(SN, SP, lo, hi, random)
pq2<-data.frame(pq1)
prior_ <- subset(pq2, lo == 0 & hi == 0 & random==0)
#8.1667% met the criteria

##plotting results
primary.pa.plot<- ggplot(data = prior_, aes(x = SN, y = SP)) +
  geom_point() +
  geom_vline(xintercept = median(prior_$SN), linetype = "dashed") +
  geom_hline(yintercept = median(prior_$SP), linetype = "dashed") +
  labs(x = "Sensitivity (SN)", y = "Specificity (SP)") +
  xlim(0.3, 1) +
  ylim(0.3, 1)

primary.pa.plot

median(prior_$SN)
#[1] 0.5722692
quantile(prior_$SN, c(0.025, 0.975))
#2.5%     97.5% 
#0.1098669 0.9690370 

median(prior_$SP)
#[1] 0.5613397
quantile(prior_$SP, c(0.025, 0.975))
#     2.5%     97.5% 
#0.1022304 0.9552774 

###PHYSICIAN ASSISTANTS, ANY TAXONOMY###
##calculating kappa using our data
#defining our "rater" columns
rater_cols <- c("any_pcp", "claim_pcp")

#function to compute kappa
compute_kappa <- function(data, indices) {
  kappa_result <- kappa2(data[indices, rater_cols])
  return(kappa_result$value)
}

#bootstrap resampling
boot_kappas <- boot(data = pas, statistic = compute_kappa, R = 1000)

#mean kappa
boot_kappas$t0
#[1] 0.01084223

#storing the 1000 kappa values
t<-boot_kappas$t

#95% CI
quantile(boot_kappas$t, c(0.025, 0.975))
#     2.5%     97.5% 
#0.007107062 0.014996744

###simulation-based method adapted from Burstyn et al.: https://pubmed.ncbi.nlm.nih.gov/24302507/
##DEFINING VALUES
k<-100000 #size of simulation
KP.LO<-runif(k, 0.006, 0.008) #UNIFORM DISTN of lower bound on kappa
KP.HI<-runif(k, 0.005, 0.025) # UNIFORM DISTN of high bound of kappa
#PCP prev: 34.26%
PREV.CLBRT<-rbeta(k, 10.594, 19.41) #BETA distribution of exposure prevalence: https://shiny.vet.unimelb.edu.au/epi/beta.buster/

##CALCULATIONS
#lower bound on SN and SP
SN.LO<-KP.LO/((1-PREV.CLBRT) + KP.LO*PREV.CLBRT)
SP.LO<-KP.LO/(PREV.CLBRT + KP.LO*(1-PREV.CLBRT))

#unconstrained priors on SN and SP
SN<-runif(k, SN.LO,1)
SP<-runif(k, SP.LO,1)

#apply constraints
p<-PREV.CLBRT
kappa.naive<-(p*(SP-1+SN)^2)*(p-1)/((p*SN-SP-p+p*SP)*(p*SN+1-SP-p+p*SP))
lo<-rep(0, k)
hi<-rep(0, k)
for (i in 1:k) {if(kappa.naive[i] < KP.LO[i] ) lo[i] <- 1}
sum(lo)
for (i in 1:k) {if(kappa.naive[i] > KP.HI[i] ) hi[i] <- 1}
sum(hi)
random<-rep(0, k)
add<-SN+SP
for (i in 1:k) {if(add[i]<1) random[i] <- 1}
sum(random)

#prior after constraints
pq1<-cbind(SN, SP, lo, hi, random)
pq2<-data.frame(pq1)
prior_ <- subset(pq2, lo == 0 & hi == 0 & random==0)

##plotting results
any.pa.plot<- ggplot(data = prior_, aes(x = SN, y = SP)) +
  geom_point() +
  geom_vline(xintercept = median(prior_$SN), linetype = "dashed") +
  geom_hline(yintercept = median(prior_$SP), linetype = "dashed") +
  labs(x = "Sensitivity (SN)", y = "Specificity (SP)") +
  xlim(0.3, 1) +
  ylim(0.3, 1)

any.pa.plot

median(prior_$SN)
#[1] 0.5614611
quantile(prior_$SN, c(0.025, 0.975))
#     2.5%     97.5% 
#0.09513245 0.96821061

median(prior_$SP)
#[1] 0.5554938
quantile(prior_$SP, c(0.025, 0.975))
#     2.5%     97.5% 
#0.09530181 0.96107476 

#arrange the plots side by side
grid.arrange(primary.pa.plot, any.pa.plot, ncol = 1)

write.csv(pas, file = "classified_pas.csv", row.names = FALSE,na="")
rm(list = ls())

###NURSE PRACTITIONERS, PRIMARY###
#specify the PCP primary taxonomy codes of interest
np_pcp_codes<-c("363L00000X","363LA2200X", "363LC1500X", "363LF0000X", "363LG0600X", "363LX0001X", "363LX0106X","363LP2300X","363LW0102X")

#create primary taxonomy pcp indicator: 1=yes, 0=no
random_nps_785$primary_pcp = ifelse((random_nps_785$`Healthcare Provider Primary Taxonomy Switch_1`=="Y" & random_nps_785$`Healthcare Provider Taxonomy Code_1` %in% np_pcp_codes) | 
                                      (random_nps_785$`Healthcare Provider Primary Taxonomy Switch_2`=="Y" & random_nps_785$`Healthcare Provider Taxonomy Code_2` %in% np_pcp_codes) | 
                                      (random_nps_785$`Healthcare Provider Primary Taxonomy Switch_3`=="Y" & random_nps_785$`Healthcare Provider Taxonomy Code_3` %in% np_pcp_codes) | 
                                      (random_nps_785$`Healthcare Provider Primary Taxonomy Switch_4`=="Y" & random_nps_785$`Healthcare Provider Taxonomy Code_4` %in% np_pcp_codes) | 
                                      (random_nps_785$`Healthcare Provider Primary Taxonomy Switch_5`=="Y" & random_nps_785$`Healthcare Provider Taxonomy Code_5` %in% np_pcp_codes) | 
                                      (random_nps_785$`Healthcare Provider Primary Taxonomy Switch_6`=="Y" & random_nps_785$`Healthcare Provider Taxonomy Code_6` %in% np_pcp_codes) | 
                                      (random_nps_785$`Healthcare Provider Primary Taxonomy Switch_7`=="Y" & random_nps_785$`Healthcare Provider Taxonomy Code_7` %in% np_pcp_codes) | 
                                      (random_nps_785$`Healthcare Provider Primary Taxonomy Switch_8`=="Y" & random_nps_785$`Healthcare Provider Taxonomy Code_8` %in% np_pcp_codes) | 
                                      (random_nps_785$`Healthcare Provider Primary Taxonomy Switch_9`=="Y" & random_nps_785$`Healthcare Provider Taxonomy Code_9` %in% np_pcp_codes) | 
                                      (random_nps_785$`Healthcare Provider Primary Taxonomy Switch_10`=="Y" & random_nps_785$`Healthcare Provider Taxonomy Code_10` %in% np_pcp_codes) | 
                                      (random_nps_785$`Healthcare Provider Primary Taxonomy Switch_11`=="Y" & random_nps_785$`Healthcare Provider Taxonomy Code_11` %in% np_pcp_codes) | 
                                      (random_nps_785$`Healthcare Provider Primary Taxonomy Switch_12`=="Y" & random_nps_785$`Healthcare Provider Taxonomy Code_12` %in% np_pcp_codes) | 
                                      (random_nps_785$`Healthcare Provider Primary Taxonomy Switch_13`=="Y" & random_nps_785$`Healthcare Provider Taxonomy Code_13` %in% np_pcp_codes) | 
                                      (random_nps_785$`Healthcare Provider Primary Taxonomy Switch_14`=="Y" & random_nps_785$`Healthcare Provider Taxonomy Code_14` %in% np_pcp_codes) | 
                                      (random_nps_785$`Healthcare Provider Primary Taxonomy Switch_15`=="Y" & random_nps_785$`Healthcare Provider Taxonomy Code_15` %in% np_pcp_codes), 1, 0)

#create any taxonomy pcp indicator: 1=yes, 0=no
random_nps_785$any_pcp = ifelse((random_nps_785$`Healthcare Provider Taxonomy Code_1` %in% np_pcp_codes) | 
                                  (random_nps_785$`Healthcare Provider Taxonomy Code_2` %in% np_pcp_codes) | 
                                  (random_nps_785$`Healthcare Provider Taxonomy Code_3` %in% np_pcp_codes) | 
                                  (random_nps_785$`Healthcare Provider Taxonomy Code_4` %in% np_pcp_codes) | 
                                  (random_nps_785$`Healthcare Provider Taxonomy Code_5` %in% np_pcp_codes) | 
                                  (random_nps_785$`Healthcare Provider Taxonomy Code_6` %in% np_pcp_codes) | 
                                  (random_nps_785$`Healthcare Provider Taxonomy Code_7` %in% np_pcp_codes) | 
                                  (random_nps_785$`Healthcare Provider Taxonomy Code_8` %in% np_pcp_codes) | 
                                  (random_nps_785$`Healthcare Provider Taxonomy Code_9` %in% np_pcp_codes) | 
                                  (random_nps_785$`Healthcare Provider Taxonomy Code_10` %in% np_pcp_codes) | 
                                  (random_nps_785$`Healthcare Provider Taxonomy Code_11` %in% np_pcp_codes) | 
                                  (random_nps_785$`Healthcare Provider Taxonomy Code_12` %in% np_pcp_codes) | 
                                  (random_nps_785$`Healthcare Provider Taxonomy Code_13` %in% np_pcp_codes) | 
                                  (random_nps_785$`Healthcare Provider Taxonomy Code_14` %in% np_pcp_codes) | 
                                  (random_nps_785$`Healthcare Provider Taxonomy Code_15` %in% np_pcp_codes), 1, 0)

#specify the medicare HCPCS codes of interest
hcpcs_codes <- c("G0439", "G0438", "G0468", "S5190", "G0402")

#create medicare indicator for PCP: 1=yes, 0=no
library(dplyr)
random_nps_785 <- random_nps_785 %>%
  group_by(Rndrng_NPI) %>%
  mutate(claim_pcp = as.numeric(any(hcpcs_codes %in% HCPCS_Cd))) %>%
  ungroup()

#create a data frame with unique providers
nps <- random_nps_785 %>%
  distinct(Rndrng_NPI, .keep_all = TRUE)

table(nps$claim_pcp)
#  0   1 
#703  82 

table(nps$any_pcp)
#  0   1 
# 83 702

table(nps$primary_pcp)
#  0   1 
#108 677

table(nps$primary_pcp, nps$claim_pcp)
table(nps$any_pcp, nps$claim_pcp)

##CALCULATING KAPPA
#defining our "rater" columns
rater_cols <- c("primary_pcp", "claim_pcp")

#function to compute kappa
compute_kappa <- function(data, indices) {
  kappa_result <- kappa2(data[indices, rater_cols])
  return(kappa_result$value)
}

#bootstrap resampling
boot_kappas <- boot(data = nps, statistic = compute_kappa, R = 1000)

#mean kappa
boot_kappas$t0
#0.0332971

#storing the 1000 kappa values
t<-boot_kappas$t

#95% CI
quantile(boot_kappas$t, c(0.025, 0.975))
#     2.5%     97.5% 
#0.02268182 0.04508933 

##simulation-based method adapted from Burstyn et al.: https://pubmed.ncbi.nlm.nih.gov/24302507/
##DEFINING VALUES
k<-100000 #size of simulation
KP.LO<-runif(k, 0.013, 0.033) #UNIFORM DISTN of lower bound on kappa
KP.HI<-runif(k, 0.035, 0.055) # UNIFORM DISTN of high bound of kappa
#PCP prev: 55.03%
PREV.CLBRT<-rbeta(k, 7.743, 6.51) #BETA distribution of exposure prevalence: https://shiny.vet.unimelb.edu.au/epi/beta.buster/

##CALCULATIONS
#lower bound on SN and SP
SN.LO<-KP.LO/((1-PREV.CLBRT) + KP.LO*PREV.CLBRT)
SP.LO<-KP.LO/(PREV.CLBRT + KP.LO*(1-PREV.CLBRT))

#unconstrained priors on SN and SP
SN<-runif(k, SN.LO,1)
SP<-runif(k, SP.LO,1)

#apply constraints
p<-PREV.CLBRT
kappa.naive<-(p*(SP-1+SN)^2)*(p-1)/((p*SN-SP-p+p*SP)*(p*SN+1-SP-p+p*SP))
lo<-rep(0, k)
hi<-rep(0, k)
for (i in 1:k) {if(kappa.naive[i] < KP.LO[i] ) lo[i] <- 1}
sum(lo)
for (i in 1:k) {if(kappa.naive[i] > KP.HI[i] ) hi[i] <- 1}
sum(hi)
random<-rep(0, k)
add<-SN+SP
for (i in 1:k) {if(add[i]<1) random[i] <- 1}
sum(random)

#prior after constraints
pq1<-cbind(SN, SP, lo, hi, random)
pq2<-data.frame(pq1)
prior_ <- subset(pq2, lo == 0 & hi == 0 & random==0)
#11.77% retained

#plotting results
primary.np.plot<- ggplot(data = prior_, aes(x = SN, y = SP)) +
  geom_point() +
  geom_vline(xintercept = median(prior_$SN), linetype = "dashed") +
  geom_hline(yintercept = median(prior_$SP), linetype = "dashed") +
  labs(x = "Sensitivity (SN)", y = "Specificity (SP)") +
  xlim(0.3, 1) +
  ylim(0.3, 1)

primary.np.plot

median(prior_$SN)
#[1] 0.582719
quantile(prior_$SN, c(0.025, 0.975))
#     2.5%     97.5% 
#0.1324323 0.9692334 

median(prior_$SP)
#[1] 0.6053765
quantile(prior_$SP, c(0.025, 0.975))
#     2.5%     97.5% 
#0.1353706 0.9747765 

###NURSE PRACTITIONERS, ANY###
##CALCULATING KAPPA
#defining our "rater" columns
rater_cols <- c("any_pcp", "claim_pcp")

#function to compute kappa
compute_kappa <- function(data, indices) {
  kappa_result <- kappa2(data[indices, rater_cols])
  return(kappa_result$value)
}

#bootstrap resampling
boot_kappas <- boot(data = nps, statistic = compute_kappa, R = 1000)

#mean kappa
boot_kappas$t0
#[1] 0.02406898

#storing the 1000 kappa values
t<-boot_kappas$t

#95% CI
quantile(boot_kappas$t, c(0.025, 0.975))
#     2.5%     97.5% 
#0.01493253 0.03414926

###simulation-based method adapted from Burstyn et al.: https://pubmed.ncbi.nlm.nih.gov/24302507/
##DEFINING VALUES
k<-100000 #size of simulation
KP.LO<-runif(k, 0.005, 0.025) #UNIFORM DISTN of lower bound on kappa
KP.HI<-runif(k, 0.024, 0.044) # UNIFORM DISTN of high bound of kappa
#PCP prev: 55.03%
PREV.CLBRT<-rbeta(k, 7.743, 6.51) #BETA distribution of exposure prevalence:https://shiny.vet.unimelb.edu.au/epi/beta.buster/

##CALCULATIONS
#lower bound on SN and SP
SN.LO<-KP.LO/((1-PREV.CLBRT) + KP.LO*PREV.CLBRT)
SP.LO<-KP.LO/(PREV.CLBRT + KP.LO*(1-PREV.CLBRT))

#unconstrained priors on SN and SP
SN<-runif(k, SN.LO,1)
SP<-runif(k, SP.LO,1)

#apply constraints
p<-PREV.CLBRT
kappa.naive<-(p*(SP-1+SN)^2)*(p-1)/((p*SN-SP-p+p*SP)*(p*SN+1-SP-p+p*SP))
lo<-rep(0, k)
hi<-rep(0, k)
for (i in 1:k) {if(kappa.naive[i] < KP.LO[i] ) lo[i] <- 1}
sum(lo)
for (i in 1:k) {if(kappa.naive[i] > KP.HI[i] ) hi[i] <- 1}
sum(hi)
random<-rep(0, k)
add<-SN+SP
for (i in 1:k) {if(add[i]<1) random[i] <- 1}
sum(random)

#prior after constraints
pq1<-cbind(SN, SP, lo, hi, random)
pq2<-data.frame(pq1)
prior_ <- subset(pq2, lo == 0 & hi == 0 & random==0)
#11.96%

#plotting results
any.np.plot<- ggplot(data = prior_, aes(x = SN, y = SP)) +
  geom_point() +
  geom_vline(xintercept = median(prior_$SN), linetype = "dashed") +
  geom_hline(yintercept = median(prior_$SP), linetype = "dashed") +
  labs(x = "Sensitivity (SN)", y = "Specificity (SP)") +
  xlim(0.3, 1) +
  ylim(0.3, 1)

any.np.plot

median(prior_$SN)
#[1] 0.5739604
quantile(prior_$SN, c(0.025, 0.975))
#     2.5%     97.5% 
#0.1109591 0.9655901

median(prior_$SP)
#[1] 0.582381
quantile(prior_$SP, c(0.025, 0.975))
#     2.5%     97.5% 
#0.1113706 0.9725808  

#arrange the plots side by side
grid.arrange(primary.np.plot, any.np.plot, ncol = 1)

write.csv(nps, file = "classified_nps.csv", row.names = FALSE,na="")

#####################USE CASE#######################

#usecase_sn = 0.95
#usecase_sp = 0.76
usecase_sn = 0.88
usecase_sp = 0.73
usecase_rstar = 0.42

(usecase_rstar-(1-usecase_sp))/(usecase_sn+usecase_sp-1)

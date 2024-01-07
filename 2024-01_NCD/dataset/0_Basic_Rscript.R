# Preamble ----------------------------------------------------------------

## Author: Mohhd Azmi Bin Suliman
## Date: 07 January 2024
## Title: Simple R Script


# R Basic Example ---------------------------------------------------------

1
1 + 2
one <- 1
list_num <- c(1,2,3)
my_cars <- c("merc", "bimmer", "supra")


# Wrangling Example -------------------------------------------------------

## Data Import (SPSS) ----

library(tidyverse)
library(haven)

asthmads_savh <- haven::read_sav("../dataset/asthma_ds.sav") # also achieve same
asthmads_savh <- haven::as_factor(asthmads_savh)
asthmads_savh

## Data Import (CSV) ----

asthmads_csv <- read.csv("../dataset/asthma_ds.csv")
asthmads_csv

## Select Variable/Column ----

select(asthmads_savh, idR:Weight_Post)

## Sort by Variable/Column ----

arrange(asthmads_savh, Height)
arrange(asthmads_savh, desc(Height))


## Filter/Subset ----

filter(asthmads_savr, Age >= 15 & Age <= 64)

## Recode ----

asthma_ds <- read.csv("../dataset/asthma_ds_nolab.csv")
asthma_ds$Gender <- as.character(asthma_ds$Gender)
asthma_ds$Gender <- fct_recode(asthma_ds$Gender, "Male" = "1", "Female" = "2")

asthma_ds$Gender


## Basic Numerical Transformation ----

asthma_ds$Ht_m <- asthma_ds$Height/100
select(asthma_ds, Height, Ht_m)

asthma_ds$BMI_Pre <- asthma_ds$Weight_Pre/(asthma_ds$Ht_m^2)
select(asthma_ds, Weight_Pre, Ht_m, BMI_Pre)


## Binning: Categorizing Numerical Variable ----

asthma_ds$BMI_PreCat <- cut(asthma_ds$BMI_Pre,
                            breaks = c(0, 18.49, 22.99, 24.99, 100),
                            labels = c("Underweight", "Normal", "Overweight", "Obese"))

select(asthma_ds, BMI_Pre, BMI_PreCat)

## Reshape Dataset ----

sthma_wide <- read_sav("../dataset/asthma24_wide.sav")
asthma_wide <- as_factor(asthma_wide)
asthma_wide <- select(asthma_wide, idR:Weight, PEFR_Pre, PEFR_Post)
asthma_wide
asthma_long <- pivot_longer(data = asthma_wide,
                            cols = c(PEFR_Pre, PEFR_Post),
                            names_to = "Time",
                            values_to = "PEFR")
asthma_long

asthma_wide2 <- pivot_wider(data = asthma_long,
                           names_from = Time,
                           values_from = PEFR)
asthma_wide2


## Join Dataset ----

asthma_pre <- read_sav("../dataset/asthma_pre.sav")
asthma_pre <- as_factor(asthma_pre)
asthma_pre
asthma_post <- read_sav("../dataset/asthma_post.sav")
asthma_post <- as_factor(asthma_post)
asthma_post

left_join(asthma_pre, asthma_post)


## Data Wrangling Full ----

asthma_ds0 <- read.csv("../dataset/asthma_ds_nolab.csv")
asthmads_clean <- asthma_ds0 %>%
  mutate(Gender = as.character(Gender),
         Gender = fct_recode(Gender, "Male" = "1", "Female" = "2"),
         Gender = fct_relevel(Gender, "Female", "Male"),
         Tx2 = fct_recode(as.character(Tx2),
                          "Placebo" = "1", "Drug A" = "2", "Drug B" = "3"),
         Tx1 = fct_collapse(Tx2,
                            "Intervention" = c("Drug A", "Drug B"),
                            "Control" = "Placebo"),
         Ht_m = Height/100,
         BMI_Pre = round(Weight_Pre/(Ht_m^2), digits = 2),
         BMI_PreCat = cut(BMI_Pre,
                          breaks = c(0, 18.49, 22.99, 24.99, 100),
                          labels = c("Underweight", "Normal", "Overweight", "Obese")),
         BMI_Post = round(Weight_Post/(Ht_m^2), digits = 2),
         BMI_PostCat = cut(BMI_Post,
                           breaks = c(0, 18.49, 22.99, 24.99, 100),
                           labels = c("Underweight", "Normal", "Overweight", "Obese")),
         across(.cols = starts_with("SxWheeze"),
                .fns = ~ as.character(.)),
         across(.cols = starts_with("SxWheeze"),
                .fns = ~ fct_recode(., "No" = "1", "Yes" = "2"))) %>%
  relocate(Tx1, .before = Tx2) %>%
  relocate(Ht_m, .after = Height) %>%
  relocate(BMI_Pre, BMI_PreCat, .after = Weight_Pre) %>%
  relocate(BMI_Post, BMI_PostCat, .after = Weight_Post)


# Exploration (Descriptive Analysis) --------------------------------------

summary(asthmads_savh)
summary(asthmads_savh$Gender)
summary(asthmads_savh$Age)


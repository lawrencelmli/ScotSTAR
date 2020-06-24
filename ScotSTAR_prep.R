setwd("D:/Lawrence/Documents/R_Projects/ScotSTAR/")

library(tidyverse)
library(readxl)

intubation <- read_excel("drug_table.xlsx", sheet = 1,
                         range = "A3:T7", col_names = FALSE)

intubation <- intubation %>%
  select(-...2, -...4, -...6, -...8, -...9, -...10, -...11, -...12, -...17, -...19)

drug.col <- c("drug",
              "formulation",
              "rec_dose",
              "dilution",
              "volume",
              "vol_unit",
              "dose",
              "dose_unit",
              "prescription",
              "check")

colnames(intubation) <- drug.col


saveRDS(intubation, file = "intubation.rda")


emergency <- read_excel("drug_table.xlsx", sheet = 1,
                         range = "A9:T12", col_names = FALSE)

emergency <- emergency %>%
  select(-...2, -...4, -...6, -...8, -...9, -...10, -...11, -...12, -...17, -...19)

colnames(emergency) <- drug.col

saveRDS(emergency, "emergency.rda")

infusion <- read_excel("drug_table.xlsx", sheet = 2,
                       range = "A2:T9", col_names = F)

infusion <- infusion %>%
  select(-...2, -...4, -...6, -...9, -...10, -...11, -...12, -...17, -...19)


infuse.name <- c("drug",
                 "formulation",
                 "rec_dose",
                 "dilution",
                 "dilutant",
                 "to_syringe",
                 "dose_unit",
                 "rate",
                 "dose_equiv",
                 "prescription",
                 "check")

colnames(infusion) <- infuse.name

saveRDS(infusion, "infusion.rda")

broncho <- read_excel("drug_table.xlsx", sheet = 1,
                       range = "A23:T25", col_names = F)

broncho <- broncho %>% 
  filter(!is.na(...1)) %>% 
  select(-...2, -...5, -...8, -...12, -...14, -...17, -...19)

broncho.names <- c("drug",
                   "formulation",
                   "st_dilution",
                   "dose_load",
                   "dose_infusion",
                   "LD_dose",
                   "LD_unit",
                   "LD_vol",
                   "LD_rate",
                   "duration",
                   "maintenance_rate",
                   "prescription", 
                   "check")
colnames(broncho) <- broncho.names

saveRDS(broncho, "broncho.rda")


####### move plot treatments to data_processed folder

treatments <- read.csv(here("data", "data_raw", "treatments.csv"))

write.csv(treatments, file = "data/data_processed/treatments.csv", row.names = FALSE)

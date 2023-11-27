library(dplyr)

meteorites <- read.csv("Meteorite_Landings.csv")

comets <- read.csv("Near-Earth_Comets_-_Orbital_Elements.csv")

comets$TP..TDB. <- as.numeric(substr(as.Date((comets$TP..TDB.) - 2400000.5, origin = as.Date("1858-11-17")), 1, 4))

colnames(comets)[1] <- "name"

colnames(comets)[3] <- "year"

meteorites <- meteorites[-sample(1:nrow(meteorites), 20876), ]

unified_dataset <- bind_rows(meteorites, comets)

write.csv(unified_dataset, "unified_dataset.csv", row.names = FALSE)

unified_dataset <- read.csv("unified_dataset.csv")

unified_dataset <- mutate(unified_dataset, crush = case_when(MOID..AU. <= 0.05 ~ "Danger", MOID..AU. > 0.05 ~ "Safe"))

unified_dataset <- mutate(unified_dataset, avg_speed = 2 * 3.14159265358974 * sqrt(((unified_dataset$q..AU.)^2 + (unified_dataset$Q..AU.)^2) / 2) / unified_dataset$P..yr.)

summary <- summarise(unified_dataset, mean_year = mean(year, na.rm = TRUE), mean_mass = mean(mass..g., na.rm = TRUE), mean_speed = mean(avg_speed, na.rm = TRUE))

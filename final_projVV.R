install.packages(c("readxl", "tidyr"))

library(readxl)
library(tidyr)

#loading excel
file <- "Vaccine coverage at subnational level (2).xlsx"
raw  <- read_excel(file, sheet = "Data")

#keep only 2018 selected columns
cols_keep <- c("iso", "CountryName", "WHORegion", "Year",
               "Admin1", "Admin2", "Vaccine", "Coverage")

data2018 <- raw[raw$Year == 2018, cols_keep]

#keep vaccines i care about
vaccines_keep <- c("MCV1", "Pol3", "HepB1", "HepB2", "HepB3", "IPV1", "Pol2")

data_sub <- data2018[data2018$Vaccine %in% vaccines_keep, ]

data_sub_unique <- aggregate(
  Coverage ~ iso + CountryName + WHORegion + Year + Admin1 + Admin2 + Vaccine,
  data = data_sub,
  FUN  = mean
)

#pivot
wide2018 <- pivot_wider(
  data_sub_unique,
  id_cols     = c("iso", "CountryName", "WHORegion", "Year", "Admin1", "Admin2"),
  names_from  = "Vaccine",
  values_from = "Coverage"
)

#drop rows with missing values in the main variables
needed <- c("MCV1", "Pol3", "HepB1", "HepB2", "HepB3", "IPV1", "Pol2")
complete_rows <- complete.cases(wide2018[, needed])

#keep only 0–100% for all vaccines
valid_range <- wide2018$MCV1 >= 0 & wide2018$MCV1 <= 100 &
  wide2018$Pol3 >= 0 & wide2018$Pol3 <= 100 &
  wide2018$HepB1 >= 0 & wide2018$HepB1 <= 100 &
  wide2018$HepB2 >= 0 & wide2018$HepB2 <= 100 &
  wide2018$HepB3 >= 0 & wide2018$HepB3 <= 100 &
  wide2018$IPV1  >= 0 & wide2018$IPV1  <= 100 &
  wide2018$Pol2  >= 0 & wide2018$Pol2  <= 100

analysis <- wide2018[complete_rows & valid_range, ]

cat("Number of rows used in analysis:", nrow(analysis), "\n")


analysis <- wide2018[complete_rows, ]

cat("Number of rows used in analysis:", nrow(analysis), "\n")

#regression model
analysis$WHORegion <- factor(analysis$WHORegion)

model <- lm(
  MCV1 ~ Pol3 + HepB1 + HepB2 + HepB3 + IPV1 + Pol2 + WHORegion,
  data = analysis
)

summary(model)
library(ggplot2)

#histogram of MCV1 (zoom to 0–110) bc i have some cray cray data points
ggplot(analysis, aes(x = MCV1)) +
  geom_histogram(binwidth = 5) +
  coord_cartesian(xlim = c(0, 110)) +
  labs(
    title = "Distribution of MCV1 coverage (2018, subnational)",
    x = "MCV1 coverage (%)",
    y = "Number of districts"
  )
# histogram of Pol3
ggplot(analysis, aes(x = Pol3)) +
  geom_histogram(binwidth = 5) +
  coord_cartesian(xlim = c(0, 110)) +
  labs(
    title = "Distribution of Pol3 coverage (2018, subnational)",
    x = "Pol3 coverage (%)",
    y = "Number of districts"
  )
#scatterplot MCV1 vs Pol3 (zoom both axes to 0–110) bc of cray cray data points
ggplot(analysis, aes(x = Pol3, y = MCV1)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  coord_cartesian(xlim = c(0, 110), ylim = c(0, 110)) +
  labs(
    title = "MCV1 vs Pol3 coverage",
    x = "Pol3 coverage (%)",
    y = "MCV1 coverage (%)"
  )
    par(mfrow = c(2, 2))
    plot(model)
  )

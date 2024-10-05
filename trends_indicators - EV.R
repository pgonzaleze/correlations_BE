
##### ================================================ #####
#####                                                  #####
#####       Compute trends of each indicator           #####
#####                                                  #####
##### ================================================ #####

# Simon Fraser University
# Resource & Environmental Management
# Author: Pedro G. Gonzalez-Espinosa
# Created: 13/ SEP /2023
# Last update: 28/ SEP /2024

library(readxl)
library(tidyr)
library(dplyr)
library(broom)
library(ggplot2)
library(countrycode)
library(gridExtra)

# Read files
# adjust the name if necessary
#Arrivals= read_xlsx("Tourism_arrivals.xlsx", sheet = "Arrivals_timeseries")
PortInf= read_xlsx("Port infrastructure.xlsx", sheet = "QualPortInf_timeseries")
HRRL = read_xlsx("Human rights and rule of law.xlsx", sheet = "HRRL_timeseries")
GII = read_xlsx("Gender Inequality Index.xlsx", sheet = 'GII_timeseries')
GrpGriev = read_xlsx("Group Grievance.xlsx", sheet = "GrpGriev_timeseries")
FacElit = read_xlsx("Factionalized Elites.xlsx", sheet = "FactElit_timeseries")
EconIneq = read_xlsx('Economic Inequality.xlsx', sheet = "EconIneq_timeseries")
HFBD = read_xlsx('Human Flight and Brain Drain.xlsx', sheet = 'HFBD_timeseries')
ContCorr = read_xlsx("Control_of_corruption.xlsx", sheet = "ContCorr_timeseries")
StatLegit = read_xlsx("State Legitimacy.xlsx", sheet = "StatLegit_timeseries")
GovEff = read_xlsx("Government Effectiveness.xlsx", sheet = "GovEff_timeseries")
StatEcon = read_xlsx("State of the Economy.xlsx", sheet = "StatEcon_timeseries")
InvPI = read_xlsx("Investor_protection_index.xlsx", sheet = "InvPI_timeseries")
RegQual = read_xlsx("Regulatory quality.xlsx", sheet = "RegQual_timeseries")
BEnv = read_xlsx('Business_environment.xlsx', sheet = "BEnv_timeseries") 
PubServ = read_xlsx("Public services.xlsx", sheet = "PubServ_timeseries")
ShipConn = read_xlsx("Shipping connectivity.xlsx", sheet = "ShipConn_timeseries")
AirTransport = read_xlsx("Air transport.xlsx", sheet = "AirTransport_timeseries")
SecIntServ = read_xlsx("Secure Internet servers.xlsx", sheet = "SecIntServ_timeseries")
BioHabitat = read_xlsx("Biodiversity_Habitat.xlsx", sheet = "BioHabitat_timeseries")
BioSpp = read_xlsx("Biodiversity_Species.xlsx", sheet = "BioSpp_timeseries")
CleanWaters = read_xlsx("clean_waters.xlsx", sheet ="CleanWaters_timeseries")

# Verify data is numeric
#str(Arrivals)
str(EconIneq)
str(ContCorr)
str(FacElit)
str(GrpGriev)
str(HRRL)
str(HFBD)
str(GII) # ranges from 0 to 10
str(StatLegit)
str(GovEff)
str(StatEcon)
str(InvPI) # ranges from 0 (little to no investor protection) to 10 (greater investor protection). 
str(RegQual)
str(BEnv) # ranges from 1 (easiest) to 183 (most difficult)
str(PubServ)
str(ShipConn) # ranges from 0-174 (100 = highest connectivity) But, in this case China has 174
str(AirTransport) # Number of registered takeoffs
str(SecIntServ) # Internet servers per million people
str(BioHabitat) # range 0-100  
str(BioSpp) # range 0-100 
str(CleanWaters) # range 0-100 

# Covert to numeric if necessary
FacElit$FactElit = as.numeric(FacElit$FactElit)
PortInf$PortInf = as.numeric((PortInf$PortInf))

##### ================================================== #####
#####            interpolation of data                   #####
##### ================================================== #####

AirTransport <- AirTransport %>%
  group_by(Territory) %>%
  fill(AirTransport, .direction = "down") %>%
  fill(AirTransport, .direction = "up")

ContCorr <- ContCorr %>%
  group_by(Territory) %>%
  fill(ContCorr, .direction = "down") %>%
  fill(ContCorr, .direction = "up")

EconIneq <- EconIneq %>%
  group_by(Territory) %>%
  fill(EconIneq, .direction = "down") %>%
  fill(EconIneq, .direction = "up")

FacElit <- FacElit %>%
  group_by(Territory) %>%
  fill(FactElit, .direction = "down") %>%
  fill(FactElit, .direction = "up")

GII <- GII %>%
  group_by(Territory) %>%
  fill(GII, .direction = "down") %>%
  fill(GII, .direction = "up")
GII <- GII %>%
  mutate(GII = if_else(Territory == "Somalia", replace_na(GII, 0), GII))

GrpGriev <- GrpGriev %>%
  group_by(Territory) %>%
  fill(GrpGriev, .direction = "down") %>%
  fill(GrpGriev, .direction = "up")

GovEff <- GovEff %>%
  group_by(Territory) %>%
  fill(GovEff, .direction = "down") %>%
  fill(GovEff, .direction = "up")

InvPI <- InvPI %>% 
  group_by(Territory) %>% 
  fill(InvPI, .direction = "down") %>%
  fill(InvPI, .direction = "up")

BEnv <- BEnv %>% 
  group_by(Territory) %>% 
  fill(BEnv, .direction = "down") %>%
  fill(BEnv, .direction = "up")

HFBD <- HFBD %>%
  group_by(Territory) %>%
  fill(HFBD, .direction = "down") %>%
  fill(HFBD, .direction = "up")

HRRL <- HRRL %>%
  group_by(Territory) %>%
  fill(HRRL, .direction = "down") %>%
  fill(HRRL, .direction = "up")

PortInf <- PortInf %>%
  group_by(Territory) %>%
  fill(PortInf, .direction = "down") %>%
  fill(PortInf, .direction = "up")

PubServ <- PubServ %>%
  group_by(Territory) %>%
  fill(PubServ, .direction = "down") %>%
  fill(PubServ, .direction = "up")

RegQual <- RegQual %>%
  group_by(Territory) %>%
  fill(RegQual, .direction = "down") %>%
  fill(RegQual, .direction = "up")

SecIntServ <- SecIntServ %>%
  group_by(Territory) %>%
  fill(SecIntServ, .direction = "down") %>%
  fill(SecIntServ, .direction = "up")

ShipConn <- ShipConn %>%
  group_by(Territory) %>%
  fill(ShipConn, .direction = "down") %>%
  fill(ShipConn, .direction = "up")

StatEcon <- StatEcon %>%
  group_by(Territory) %>%
  fill(StatEcon, .direction = "down") %>%
  fill(StatEcon, .direction = "up")

StatLegit <- StatLegit %>%
  group_by(Territory) %>%
  fill(StatLegit, .direction = "down") %>%
  fill(StatLegit, .direction = "up")



################################################################################
' Normalize data to avoid negative and positive values. E.g., because corruption
  runs from -2-5 (weak) to 2.5 (strong), or BEnv which runs from 1-183
  it will be normalized to run from 1 to 10'


# ##### Arrivals #####
# # Calculate original data range (each one at a time, it cannot be at the same moment)
# original_range <- max(Arrivals$Arrivals, na.rm = T) - min(Arrivals$Arrivals, na.rm = T)
# # Target data range 
# target_min <- 0
# target_max <- 1
# # Calculate scaling factor
# scaling_factor <- (target_max - target_min) / original_range
# # Normalize the data to the range 1 to 10
# Arrivals$Arrivals = (Arrivals$Arrivals - min(Arrivals$Arrivals, na.rm = T)) * scaling_factor + target_min


##### ContCorr #####
original_range <- max(ContCorr$ContCorr, na.rm = T) - min(ContCorr$ContCorr, na.rm = T)
# Target data range 
target_min <- 1
target_max <- 10
# Calculate scaling factor
scaling_factor <- (target_max - target_min) / original_range
# Normalize the data to the range 1 to 10
ContCorr$ContCorr = (ContCorr$ContCorr - min(ContCorr$ContCorr, na.rm = T)) * scaling_factor + target_min


##### GovEff #####
original_range <- max(GovEff$GovEff, na.rm = T) - min(GovEff$GovEff, na.rm = T)
# Target data range 
target_min <- 1
target_max <- 10
# Calculate scaling factor
scaling_factor <- (target_max - target_min) / original_range
# Normalize the data to the range 1 to 10
GovEff$GovEff = (GovEff$GovEff - min(GovEff$GovEff, na.rm = T)) * scaling_factor + target_min


##### RegQual #####
original_range <- max(RegQual$RegQual, na.rm = T) - min(RegQual$RegQual, na.rm = T)
# Target data range 
target_min <- 1
target_max <- 10
# Calculate scaling factor
scaling_factor <- (target_max - target_min) / original_range
# Normalize the data to the range 1 to 10
RegQual$RegQual = (RegQual$RegQual - min(RegQual$RegQual, na.rm = T)) * scaling_factor + target_min


##### BEnv #####
original_range <- max(BEnv$BEnv, na.rm = T) - min(BEnv$BEnv, na.rm = T) # this should be reversed as well
# Target data range 
target_min <- 1
target_max <- 10
# Calculate scaling factor
scaling_factor <- (target_max - target_min) / original_range
# Normalize the data to the range 1 to 10
BEnv$BEnv = (BEnv$BEnv - min(BEnv$BEnv, na.rm = T)) * scaling_factor + target_min


##### ShipConn #####
original_range <- max(ShipConn$ShipConn, na.rm = T) - min(ShipConn$ShipConn, na.rm = T)
# Target data range 
target_min <- 1
target_max <- 10
# Calculate scaling factor
scaling_factor <- (target_max - target_min) / original_range
# Normalize the data to the range 1 to 10
ShipConn$ShipConn = (ShipConn$ShipConn - min(ShipConn$ShipConn, na.rm = T)) * scaling_factor + target_min


##### AirTransport #####
original_range <- max(AirTransport$AirTransport, na.rm = T) - min(AirTransport$AirTransport, na.rm = T)
# Target data range 
target_min <- 1
target_max <- 10
# Calculate scaling factor
scaling_factor <- (target_max - target_min) / original_range
# Normalize the data to the range 1 to 10
AirTransport$AirTransport = (AirTransport$AirTransport - min(AirTransport$AirTransport, na.rm = T)) *scaling_factor + target_min

##### PortInf #####
original_range <- max(PortInf$PortInf, na.rm = T) - min(PortInf$PortInf, na.rm = T)
# Target data range 
target_min <- 1
target_max <- 10
# Calculate scaling factor
scaling_factor <- (target_max - target_min) / original_range
# Normalize the data to the range 1 to 10
PortInf$PortInf = (PortInf$PortInf - min(PortInf$PortInf, na.rm = T)) * scaling_factor + target_min

##### SecIntServ #####
original_range <- max(SecIntServ$SecIntServ, na.rm = T) - min(SecIntServ$SecIntServ, na.rm = T)
# Target data range 
target_min <- 1
target_max <- 10
# Calculate scaling factor
scaling_factor <- (target_max - target_min) / original_range
# Normalize the data to the range 1 to 10
SecIntServ$SecIntServ = (SecIntServ$SecIntServ - min(SecIntServ$SecIntServ, na.rm = T)) * scaling_factor + target_min


##### BioHabitat #####
original_range <- max(BioHabitat$BioHabitat, na.rm = T) - min(BioHabitat$BioHabitat, na.rm = T)
# Target data range 
target_min <- 1
target_max <- 10
# Calculate scaling factor
scaling_factor <- (target_max - target_min) / original_range
# Normalize the data to the range 1 to 10
BioHabitat$BioHabitat = (BioHabitat$BioHabitat - min(BioHabitat$BioHabitat, na.rm = T)) * scaling_factor + target_min


##### BioSpp #####
original_range <- max(BioSpp$BioSpp, na.rm = T) - min(BioSpp$BioSpp, na.rm = T)
# Target data range 
target_min <- 1
target_max <- 10
# Calculate scaling factor
scaling_factor <- (target_max - target_min) / original_range
# Normalize the data to the range 1 to 10
BioSpp$BioSpp = (BioSpp$BioSpp - min(BioSpp$BioSpp, na.rm = T)) * scaling_factor + target_min


##### CleanWaters #####
original_range <- max(CleanWaters$CleanWaters, na.rm = T) - min(CleanWaters$CleanWaters, na.rm = T)
# Target data range 
target_min <- 1
target_max <- 10
# Calculate scaling factor
scaling_factor <- (target_max - target_min) / original_range
# Normalize the data to the range 1 to 10
CleanWaters$CleanWaters = (CleanWaters$CleanWaters - min(CleanWaters$CleanWaters, na.rm = T)) * scaling_factor + target_min


################################################################################
   'Reverse values of the indicator if necessary, original data runs 1-10
   As an example, for economic inequality values, the lower the value the 
   lower the inequality, this should be reversed where the higher the value 
   lower inequality.' 

##### Economic Inequalities #####
EconIneq$EconIneq <- (1+max(EconIneq$EconIneq, na.rm = T)) - EconIneq$EconIneq

##### Factionalized Elites #####
FacElit$FactElit <- (1+max(FacElit$FactElit, na.rm = T)) - FacElit$FactElit

##### Group Grievance #####
GrpGriev$GrpGriev <- (1+max(GrpGriev$GrpGriev, na.rm = T)) - GrpGriev$GrpGriev

##### Human rights and rule of law #####
HRRL$HRRL <- (1+max(HRRL$HRRL, na.rm = T)) - HRRL$HRRL

##### Human flight and Brain drain #####
HFBD$HFBD <- (1+max(HFBD$HFBD, na.rm = T)) - HFBD$HFBD

##### Gender Inequality Index ####  0 to 1
GII$GII <- (0+max(GII$GII, na.rm = T)) - GII$GII

##### State of the Economy #####
StatEcon$StatEcon  <- (1+max(StatEcon$StatEcon, na.rm = T)) - StatEcon$StatEcon

#### State legitimacy #####
StatLegit$StatLegit <- (1+max(StatLegit$StatLegit, na.rm = T)) - StatLegit$StatLegit

##### Business Environment #####
BEnv$BEnv <- (1+max(BEnv$BEnv, na.rm = T)) - BEnv$BEnv

##### Public services #####
PubServ$PubServ <- (1+max(PubServ$PubServ, na.rm = T)) - PubServ$PubServ


##### ========================================== #####
#####      Calculate slope and intercept         #####
#####           for each country                 #####
##### ========================================== #####

#omit NAs
AirTransport <- na.omit(AirTransport)
#Arrivals <- na.omit(Arrivals)
BEnv <- na.omit(BEnv)
BioHabitat <- na.omit(BioHabitat)
BioSpp <- na.omit(BioSpp)
CleanWaters <- na.omit(CleanWaters)
ContCorr <- na.omit(ContCorr)
EconIneq <- na.omit(EconIneq)
FacElit <- na.omit(FacElit)
GII <- na.omit(GII)
GovEff <- na.omit(GovEff)
GrpGriev <- na.omit(GrpGriev)
HFBD <- na.omit(HFBD)
HRRL <- na.omit(HRRL)
InvPI <- na.omit(InvPI)
PortInf <- na.omit(PortInf)
PubServ <- na.omit(PubServ)
RegQual <- na.omit(RegQual)
SecIntServ <- na.omit(SecIntServ)
ShipConn <- na.omit(ShipConn)
StatEcon <- na.omit(StatEcon)
StatLegit <- na.omit(StatLegit)

# List of data frames
dataframes_list <- list(AirTransport, BEnv, GovEff, InvPI, PortInf,
                        PubServ, RegQual, SecIntServ, ShipConn,
                        StatEcon, StatLegit)

# Initialize an empty list to store results
slope_intercept_list <- list()

# Specify the path to your new folder
output_folder <- "C:/Users/PedroG_SFU/OneDrive - Simon Fraser University (1sfu)/Documents/SFU/BE_indicator_interactions/timeseries_dataset/trends_df"  # Specify the path to your new folder
dir.create(output_folder, showWarnings = FALSE)  # Create a new folder

# Loop over the list of data frames
for (df in dataframes_list) {
    result <- df %>%
    group_by(Territory) %>%
    summarise(
      mean_value = mean(.data[[colnames(df)[3]]]),
      sd_value = sd(.data[[colnames(df)[3]]]),
      mean_year = mean(Year),
      sd_year = sd(Year),
      correlation = ifelse(sd(Year) != 0, cor(Year, .data[[colnames(df)[3]]]), NA),
      slope = ifelse(sd(Year) != 0, correlation * (sd(.data[[colnames(df)[3]]]) / sd(Year)), NA),
      intercept = ifelse(sd(Year) != 0, mean_value - slope * mean_year, NA),
      indicator = colnames(df)[3]
    )
  
  # Print the result
  cat("Slope and Intercept for", colnames(df)[3], ":\n")
  print(result)
  cat("\n")
  
  # Store the result in the list
  slope_intercept_list[[paste0("slope_intercept_", colnames(df)[3])]] <- result
  
  # Save the result as a CSV
  #write.csv(result, file = paste0(output_folder, "/", colnames(df)[3], "_SlopeIntercept.csv"), col.names = TRUE, row.names = FALSE)
}




# Plot the combined dataframe
P1 <- ggplot(ContCorr, aes(x = Year, y = ContCorr, colour = Territory)) +
  geom_line(aes(group = Territory)) +
  theme_minimal() +
  theme(legend.position = "none") +  # Remove the legend box
  ggtitle("Combined Timeseries Plot")

print(P1)




# Merge all dataframes in the list into a single dataframe
merged_df <- bind_rows(slope_intercept_list, .id = "indicator")

# Remove "slope_intercept_" prefix from the indicator column
merged_df$indicator <- gsub("^slope_intercept_", "", merged_df$indicator)

# Drop NA values
merged_df <- na.omit(merged_df)

# View the merged dataframe
print(merged_df)

# Replace the name in summary_df
merged_df$Territory <- gsub("United States Virgin Islands", "Virgin Islands (U.S.)", merged_df$Territory)
merged_df$Territory <- gsub("CuraÃ§ao", "Curacao", merged_df$Territory)
merged_df$Territory <- gsub("Curacao", "Curaçao", merged_df$Territory)
merged_df$Territory <- gsub("Turkiye", "Turkey", merged_df$Territory)
merged_df$Territory <- gsub("Congo Democratic Republic", "Democratic Republic of the Congo", merged_df$Territory)
merged_df$Territory <- gsub("Congo Republic", "Republic of the Congo", merged_df$Territory)
merged_df$Territory <- gsub("Macedonia, FYR", "North Macedonia", merged_df$Territory)
merged_df$Territory <- gsub("Moldova, Republic of", "Moldova", merged_df$Territory)
merged_df$Territory <- gsub("Timor-Leste", "East Timor", merged_df$Territory)
merged_df$Territory <- gsub("Syrian Arab Republic", "Syria", merged_df$Territory)
merged_df$Territory <- gsub("Cabo Verde", "Cape Verde", merged_df$Territory)
merged_df$Territory <- gsub("Republic of Mauritius", "Mauritius", merged_df$Territory)

# Filter countries with access to the sea
# Manually create a list of landlocked countries
remove_country_names <- c("Afghanistan", "Andorra", "Armenia", "Austria", "Azerbaijan", "Belarus", "Bhutan", "Bolivia", 
                              "Botswana", "Burkina Faso", "Burundi", "Central African Republic", "Chad", "Czechia", 
                              "Czech Republic", "Eswatini", "Ethiopia", "Hungary", "Kazakhstan", "Kosovo", "Kyrgyzstan", 
                              "Laos", "Lesotho", "Liechtenstein", "Luxembourg", "Macedonia", "Malawi", "Mali", "Moldova", 
                              "Monaco", "Mongolia", "Nepal", "Niger", "Paraguay", "Rwanda", "San Marino", "Serbia", 
                              "Slovakia", "South Sudan", "Switzerland", "Tajikistan", "Turkmenistan", "Uganda", "Uzbekistan",
                              "Vatican City", "Zambia", "Zimbabwe", "Ile Europa", "Ile Tromelin","Norfolk Island",  "Palestine",                                   
                              "Pitcairn", "Sint Eustatius", "Amsterdam Island and Saint Paul Island", "Andaman and Nicobar", 
                              "Azores", "Bassas da India","Bouvet Island", "British Indian Ocean Territory", "Israel and West Bank",
                              "Wallis and Futuna Islands", "Sint Maarten (Dutch part)", "Falkland Islands (Malvinas)" ,"Guadeloupe",
                              "Isle of Man", "West Bank and Gaza", "Tokelau", "Wake Island", "Wallis and Futuna", "Gibraltar",
                              "Saba" ,"Martinique", "Tristan da Cunha", "Reunion", "Northern Mariana Islands and Guam", 
                              "Northern Saint-Martin", "Oecussi Ambeno", "Palmyra Atoll", "Prince Edward Islands", "Montserrat",
                              "Heard and McDonald Islands", "Howland Island and Baker Island", "Jan Mayen", "Jarvis Island",
                              "Jersey", "Johnston Atoll", "Juan de Nova Island", "Kerguelen Islands", "Macquarie Island", "Anguilla",
                              "Madeira","Guadeloupe and Martinique", "Glorioso Islands", "South Georgia and the South Sandwich Islands",
                              "Puerto Rico and Virgin Islands of the United States", "Northern Mariana Islands", "Taiwan",
                              "Saint-Pierre and Miquelon", "Amsterdam and Saint Paul Island", "Clipperton",  "China, Hong Kong SAR", 
                              "China, Taiwan Province of", "Clipperton Island", "Crozet Islands", "Netherlands Antilles (former)",
                              "Ascension","Sint Maarten", "Saint Helena", "Greenland", "Cocos Islands", "Christmas Island",
                              "Canary Islands", "Sudan (...2011)", "Faeroe Islands", "Faroe Islands", "Falkland Islands", 
                              "Hong Kong SAR, China", "Hong Kong SAR, China", "Netherlands Antilles", "North America",
                              "Macao SAR, China", "Lao PDR", "Guernsey", "North Macedonia", "Slovak Republic", "Mayotte",
                              "Puerto Rico", "British Virgin Islands", "Virgin Islands (U.S.)", "Western Sahara"
                          )

# Filter out landlocked countries from merged_df
merged_df_filtered <- merged_df[!merged_df$Territory %in% remove_country_names, ]

# add a column with the Continent 
merged_df_filtered$continent <- countrycode(merged_df_filtered$Territory, origin = 'country.name', destination = 'continent')

# add a column with the Continent 
merged_df_filtered$ISO3 <- countrycode(merged_df_filtered$Territory, origin = 'country.name', destination = 'iso3c')

# Extract unique values from the "Territory" column in merged_df_filtered
territories <- unique(merged_df_filtered$Territory)

# Create heatmap
heatmap <- ggplot(merged_df_filtered, aes(x = ISO3, y = indicator, fill = slope)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", mid = "gray95", high = "blue", 
                       midpoint = 0, limits = c(-1, 1), name = "Trend")  +  # RdBu color scale
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +  # Rotate x-axis labels vertically
  labs(x = "", y = "", title = "Heatmap of trends by Territory")

# Print heatmap
print(heatmap)


# Create a list to store the plots
plots_list <- list()

# Split the data by continent
data_by_continent <- split(merged_df_filtered, merged_df_filtered$continent)

# Loop through each continent and create a plot
for (continent in names(data_by_continent)) {
  continent_data <- data_by_continent[[continent]]
  
  p <- ggplot(continent_data, aes(x = ISO3, y = indicator, fill = slope)) +
    geom_tile() +
    scale_fill_gradient2(low = "red", mid = "gray95", high = "blue", 
                         midpoint = 0, limits = c(-1, 1), name = "Trend")  +  # RdBu color scale
    theme_classic() +
    theme(legend.position = "none") +  # Remove the legend box
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +  # Rotate x-axis labels vertically
    labs(x = "", y = "", title = paste("Trends -", continent))
  
  # Add the plot to the list
  plots_list[[continent]] <- p
}

# # Print each plot separately
# for (continent in names(plots_list)) {
#   print(plots_list[[continent]])
# }

# Arrange heat maps in a 2x3 grid
grid.arrange(grobs = plots_list, nrow = 3, ncol = 2)

###### ========================================================== #######
######       Most negative and positive country by trend          #######
###### ========================================================== #######

# Group by Territory (country) and calculate total trend score
country_trends <- merged_df_filtered %>%
  group_by(Territory, continent) %>%
  summarise(Total_Trend = sum(slope))

# Find country with most negative trend
most_negative_country <- country_trends$Territory[which.min(country_trends$Total_Trend)]

# Find country with most positive trend
most_positive_country <- country_trends$Territory[which.max(country_trends$Total_Trend)]

# Print results
cat("Country with the most negative trends:", most_negative_country, "\n")
cat("Country with the most positive trends:", most_positive_country, "\n")


###### ============================================================= #####
#####       count positive and negative trends by country            #####
###### ============================================================= #####

### Recode Slope values to classify as positive, negative, or neutral ###
merged_df_filtered <- merged_df_filtered %>%
  mutate(Trend_Type = case_when(
    slope > 0 ~ "Positive",
    slope < 0 ~ "Negative",
    TRUE ~ "Neutral"  # Assuming Slope == 0 is considered neutral
  ))

# Count number of positive and negative trends for each country
trend_counts <- merged_df_filtered %>%
  group_by(Territory, continent) %>%
  summarise(
    Positive_Count = sum(Trend_Type == "Positive"),
    Negative_Count = sum(Trend_Type == "Negative")
  )

# Print results
print(trend_counts)

#Find country with the most positive trends 
most_positive <- trend_counts[which.max(trend_counts$Positive_Count), ]

# Find country with the most negative trends
most_negative <- trend_counts[which.max(trend_counts$Negative_Count), ]

# Print results
cat("Country with the most positive trends:", most_positive$Territory, "with", most_positive$Positive_Count, "positive trends\n")
cat("Country with the most negative trends:", most_negative$Territory, "with", most_negative$Negative_Count, "negative trends\n")

# Find top 3 countries with the most positive trends
top_positive <- trend_counts %>% 
  group_by(continent) %>% 
  arrange(continent, desc(Positive_Count)) %>%  # Sort by Positive_Count in descending order
  slice_head(n = 2)  # Select the top 3 rows for each continent
top_positive

# Find top 3 countries with the most negative trends
top_negative <- trend_counts %>% 
  group_by(continent) %>% 
  arrange(continent, desc(Negative_Count)) %>%  # Sort by Negative_Count in descending order
  slice_head(n = 2)  # Select the top 3 rows for each continent
top_negative


###### ============================================================= #####
#####       count positive and negative trends by indicator          #####
###### ============================================================= #####

### Recode Slope values to classify as positive, negative, or neutral ###
merged_df_filtered <- merged_df_filtered %>%
  mutate(Trend_Type = case_when(
    slope > 0 ~ "Positive",
    slope < 0 ~ "Negative",
    TRUE ~ "Neutral"  # Assuming Slope == 0 is considered neutral
  ))

# Count number of positive and negative trends for each indicator
indicator_trend_counts <- merged_df_filtered %>%
  group_by(indicator) %>%
  summarise(
    Positive_Count = sum(Trend_Type == "Positive"),
    Negative_Count = sum(Trend_Type == "Negative")
  )

# Print results
print(indicator_trend_counts)
#write.csv(indicator_trend_counts, "indicator_trend_counts.csv")

#Find country with the most positive trends 
most_positive <- indicator_trend_counts[which.max(indicator_trend_counts$Positive_Count), ]

# Find country with the most negative trends
most_negative <- indicator_trend_counts[which.max(indicator_trend_counts$Negative_Count), ]

# Print results
cat("Indicator with the most positive trends:", most_positive$indicator, "with", most_positive$Positive_Count, "positive trends\n")
cat("Indicator with the most negative trends:", most_negative$indicator, "with", most_negative$Negative_Count, "negative trends\n")

# Find top 3 indicators with the most positive trends
top_positive_inicators <- indicator_trend_counts %>% 
    arrange(desc(Positive_Count)) %>%  # Sort by Positive_Count in descending order
  slice_head(n = 5)  # Select the top 3 rows for each continent
top_positive_inicators

# Find top 5 indicators with the most negative trends
top_negative_indicators <- indicator_trend_counts %>% 
  arrange(desc(Negative_Count)) %>%  # Sort by Negative_Count in descending order
  slice_head(n = 5)  # Select the top 3 rows for each continent
top_negative_indicators

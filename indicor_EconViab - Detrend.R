
##### ================================================== #####
#####             Classify correlations;                 #####
#####            a longitudinal analysis                 #####
##### ================================================== #####

# Simon Fraser University
# Resource & Environmental Management
# Author: Pedro C. Gonzalez-Espinosa
# Created: 13/ SEP /2023
# Last update: 28/ SEP /2024

library(readxl)
library(tidyr)
library(dplyr)
library(svglite) # save plots as SVG
library(broom)

# Read files
# adjust the name if necessary
#Arrivals= read_xlsx("Tourism_arrivals.xlsx", sheet = "Arrivals_timeseries")
PortInf= read_xlsx("Port infrastructure.xlsx", sheet = "QualPortInf_timeseries")
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

# Verify data is numeric
str(StatLegit)
str(PortInf)
str(GovEff)
str(StatEcon)
str(InvPI) # ranges from 0 (little to no investor protection) to 10 (greater investor protection). 
str(RegQual)
str(BEnv) # ranges from 1 (easiest) to 183 (most difficult)
str(PubServ)
str(ShipConn) # ranges from 0-174 (100 = highest connectivity) But, in this case China has 174
str(AirTransport) # Number of registered takeoffs
str(SecIntServ) # Internet servers per million people

# Covert to numeric if necessary
PortInf$PortInf = as.numeric((PortInf$PortInf))

# Function to detrend data with dynamic column naming
detrend <- function(df) {
  # Extract the name of the third column
  column_name <- colnames(df)[3]
  # Fit the linear model
  model <- lm(df[[3]] ~ df[[2]], data = df)
  # Create the dynamic name for the detrended values column
  detrended_column_name <- paste0(column_name, "_detrended_value")
  # Add the detrended values as a new column with the dynamic name
  df[[detrended_column_name]] <- as.numeric(resid(model))
  return(df)
}

# Function to apply the detrend and then merge the dataframes
detrend_value <- function(df) {
  df_detrended <- df %>%
    group_by(Territory) %>%
    # Drop NA values before applying the detrend function
    na.omit() %>%
    do(detrend(.)) %>%
   # select(Territory, Year)  # Only keep necessary columns
  #df <- inner_join(df, df_detrended, by = c('Territory', 'Year'))
  return(df)
}

BEnv <- detrend_value(BEnv)
StatEcon <- detrend_value(StatEcon)
SecIntServ <- detrend_value(SecIntServ)
GovEff <- detrend_value(GovEff)
InvPI <- detrend_value(InvPI)
PortInf <- detrend_value(PortInf)
RegQual <- detrend_value(RegQual)
ShipConn <- detrend_value(ShipConn)
StatLegit <- detrend_value(StatLegit)
AirTransport <- detrend_value(AirTransport)
PubServ <- detrend_value(PubServ)


################################################################################
'Reverse values of the indicator if necessary, original data runs 1-10
   As an example, for economic inequality values, the lower the value the 
   lower the inequality, this should be reversed where the higher the value 
   lower inequality.' 

BEnv$BEnv_detrended_value <- (1+max(BEnv$BEnv_detrended_value, na.rm = T)) - BEnv$BEnv_detrended_value
StatEcon$StatEcon_detrended_value  <- (1+max(StatEcon$StatEcon_detrended_value, na.rm = T)) - StatEcon$StatEcon_detrended_value
StatLegit$StatLegit_detrended_value <- (1+max(StatLegit$StatLegit_detrended_value, na.rm = T)) - StatLegit$StatLegit_detrended_value
PubServ$detrended_value <- (1+max(PubServ$PubServ_detrended_value, na.rm = T)) - PubServ$PubServ_detrended_value

##### ========================================== #####
#####       Create a list of data frames         #####
##### ========================================== #####

list_of_dataframes <- list(StatLegit, StatEcon, ShipConn, SecIntServ, RegQual,
                           PubServ, PortInf, InvPI, GovEff, BEnv, AirTransport)

##### ========================================== #####
##### Merge data frames by = c(X1 = X2, Y1 = Y2) #####
##### ========================================== #####

# Define a function to inner join two data frames
join_two_dataframes <- function(df1, df2) {
  result <- inner_join(df1, df2, by = c('Territory'='Territory',
                                        'Year'='Year')) # Replace "common column" with the actual column name you want to join on
  return(result)
}

# Initialize an empty list to store the result data frames
result_list <- list()

# Loop through the list of data frames and perform correlation tests on each pair
for (i in 1:(length(list_of_dataframes) - 1)) {
  for (j in (i + 1):length(list_of_dataframes)) {
    # join two df by applying the function created above
    result_df <- join_two_dataframes(list_of_dataframes[[i]], list_of_dataframes[[j]])
    result_list[[paste(colnames(result_df[,4]), " and ", colnames(result_df[,6]),
                       sep = "")]] <- result_df
  }
}

# Initialize an empty list to store the result data frames
filtered_list <- list()

for (i in seq_along(result_list)) {
  # Ungroup if necessary
  df_filtered <- result_list[[i]] %>%
    ungroup() %>% # Ensure no grouping issues
    filter(!is.na(.[[4]]) & !is.na(.[[6]])) 
  # Add the filtered data frame to the filtered_list
  filtered_list[[i]] <- df_filtered
}

# Check for NA values in each data frame using lapply
na_check_list <- lapply(filtered_list, function(df) {
  any_na <- any(is.na(df))
  return(any_na)
})

# Print the results
print(na_check_list)

##### ========================================= ######
#####     Perform the correlation analyses      ######
#####   and classification of the interaction   ######
##### ========================================= ######

# Initialize an empty list to store the correlation test results
correlation_test_results <- list()

for (i in seq_along(filtered_list)){
  # Get the column names from filtered_list
  column_names <- colnames(filtered_list[[i]])
  col1 <- column_names[4]  # Adjust the index as needed
  col2 <- column_names[6]  # Adjust the index as needed
  # Drop rows with any NA values in col1 or col2
  filtered_list[[i]] <- filtered_list[[i]] %>%
    drop_na(.data[[col1]], .data[[col2]])
  # Compute the correlation for each country using only common years
    corr_df <- filtered_list[[i]] %>%
      group_by(Territory) %>%
      summarize(!!paste("Correlation between", col1, "and", col2) := cor(scale(.data[[col1]]), 
                                                                         scale(.data[[col2]]), 
                                                                         method = "spearman"),
                !!paste("p-value between", col1, "and", col2) := {
                  # Check number of non-NA observations
                  n <- sum(!is.na(.data[[col1]]) & !is.na(.data[[col2]]))
                  if (n < 3) {
                    NA  # If fewer than 3 observations, return NA for p-value
                  } else {
                    cor.test(.data[[col1]], .data[[col2]], method = "spearman",
                             exact = FALSE)$p.value
                  }
                })

    # Classification of synergies and trade-offs (Pradhan et al. 2017)
    # Create a new column to store the classification 
    corr_df$classification <- ifelse(corr_df[,2] > 0.6 & corr_df[,3] < 0.05, "Synergy", 
                                     ifelse(corr_df[,2] < -0.6 & corr_df[,3] < 0.05, "Trade-off", "Neutral"))
    # Add the filtered data frame to the filtered_list
    correlation_test_results[[paste(col1, col2)]] <- corr_df
}


###### ============================================ ########
######     Quantify interactions per data frame     ########
###### ============================================ ########

# Initialize an empty list to store the summary of classifications
interaction_list <- list()

# Count the occurrences of each interaction type
for (i in seq_along(correlation_test_results)){
  interaction_counts <- table(correlation_test_results[[i]][4])
  # Calculate the percentages
  interaction_percentages <- prop.table(interaction_counts) * 100
  # Add the data to the interaction_list
  interaction_list[[paste(colnames(correlation_test_results[[i]][2]))]] <- interaction_percentages
}

#THE SAME BUT STORE THE RESULTS AS DATA FRAME
interaction_listT <- list()
for (i in seq_along(correlation_test_results)){
  interaction_counts <- table(correlation_test_results[[i]][4])
  # Calculate the percentages
  interaction_percentages <- prop.table(interaction_counts) * 100
  # Convert counts and percentages to a data frame
  interac_as_df <- data.frame(
    InteractionType = names(interaction_counts),
    Count = as.vector(interaction_counts),
    Percentage = as.vector(interaction_percentages)
  )
  # Add the data to the interaction_list
  interaction_listT[[paste(colnames(correlation_test_results[[i]][2]))]] <- interac_as_df

}


############################################################
###### ============================================ ########
######              Plot interactions               ########
###### ============================================ ########
############################################################

library(ggplot2)
library(gridExtra)
library(patchwork)

# Create a function to generate simplified stacked bar plots
create_simplified_stacked_bar_plot <- function(df, plot_title) {
  ggplot(df, aes(x = 1, y = Percentage, fill = InteractionType)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    theme_void() +
    guides(fill = "none") +
    scale_fill_manual(values = c("#CCCC00", "#006699",  "#990000" )) + # neutral, synergy, trade-off
    ggtitle(plot_title) +
    theme(plot.title = element_text(size = 4, hjust = 0.25))  # Adjust the size as needed
}

# Create a list to store all the ggplot objects
plots <- list()

# Create simplified stacked bar plots for each data frame in the list
for (i in seq_along(interaction_listT)) {
  original_title <- names(interaction_listT)[i]
  plot_title <- gsub("^Correlation between ", "", original_title)  # Remove the common prefix
  #plot_name <- paste("Simplified_Stacked_Plot_", i, ".png", sep = "")
  g <- create_simplified_stacked_bar_plot(interaction_listT[[i]], plot_title)
  plots[[i]] <- g
  #ggsave(filename = plot_name, plot = g)
}

# Create blank plots for alignment
blank <- grid::nullGrob()

# Arrange the plots in the desired layout
grid.arrange(
  arrangeGrob(plots[[55]], blank, blank, blank, blank, blank, blank, blank, blank, blank, ncol = 10),
  arrangeGrob(plots[[54]], plots[[53]], blank, blank, blank, blank, blank, blank, blank, blank, ncol = 10),
  arrangeGrob(plots[[52]], plots[[51]], plots[[50]], blank, blank, blank, blank, blank, blank, blank, ncol = 10),
  arrangeGrob(plots[[49]], plots[[48]], plots[[47]], plots[[46]], blank, blank, blank, blank, blank, blank, ncol = 10),
  arrangeGrob(plots[[45]], plots[[44]], plots[[43]], plots[[42]], plots[[41]], blank, blank, blank, blank, blank, ncol = 10),
  arrangeGrob(plots[[40]], plots[[39]], plots[[38]], plots[[37]], plots[[36]], plots[[35]], blank, blank, blank, blank, ncol = 10),
  arrangeGrob(plots[[34]], plots[[33]], plots[[32]], plots[[31]], plots[[30]], plots[[29]], plots[[28]], blank, blank, blank, ncol = 10),
  arrangeGrob(plots[[27]], plots[[26]], plots[[25]], plots[[24]], plots[[23]], plots[[22]], plots[[21]], plots[[20]], blank, blank, ncol = 10),
  arrangeGrob(plots[[19]], plots[[18]], plots[[17]], plots[[16]], plots[[15]], plots[[14]], plots[[13]], plots[[12]], plots[[11]], blank, ncol = 10),
  arrangeGrob(plots[[10]], plots[[9]], plots[[8]], plots[[7]], plots[[6]], plots[[5]], plots[[4]], plots[[3]], plots[[2]], plots[[1]], ncol = 10),
  nrow = 10
)

# Save the final grid arrangement as an image
#ggsave("Correlation_Matrix_Plots.svg", plot = grid_arrange, width = 15, height = 18)


##########       pie charts       #####################
library(ggplot2)
library(gridExtra)

# Create a function to generate simplified pie charts without legend and with titles
create_simplified_pie_chart_with_title <- function(df, plot_title) {
  ggplot(df, aes(x = "", y = Percentage, fill = InteractionType)) +
    geom_bar(stat = "identity", width = 0.75) +
    coord_polar(theta = "y", start = 0) +
    theme_void() +
    guides(fill = "none") +
    scale_fill_manual(values = c("#CCCC00", "#006699",  "#990000" )) + # neutral, synergy, trade-off
    ggtitle(plot_title) +
    theme(plot.title = element_text(size = 4, hjust = 0.25))  # Adjust the size as needed
}

# Create a list to store all the ggplot objects
plots <- list()

# Create simplified pie charts for each data frame in the list
for (i in seq_along(interaction_listT)) {
  original_title <- names(interaction_listT)[i]
  plot_title <- gsub("^Correlation between ", "", original_title)  # Remove the common prefix
  g <- create_simplified_pie_chart_with_title(interaction_listT[[i]], plot_title)
  plots[[i]] <- g
}

# Create blank plots for alignment
blank <- grid::nullGrob()

# Arrange the plots in the desired layout
grid.arrange(
  arrangeGrob(plots[[55]], blank, blank, blank, blank, blank, blank, blank, blank, blank, ncol = 10),
  arrangeGrob(plots[[54]], plots[[53]], blank, blank, blank, blank, blank, blank, blank, blank, ncol = 10),
  arrangeGrob(plots[[52]], plots[[51]], plots[[50]], blank, blank, blank, blank, blank, blank, blank, ncol = 10),
  arrangeGrob(plots[[49]], plots[[48]], plots[[47]], plots[[46]], blank, blank, blank, blank, blank, blank, ncol = 10),
  arrangeGrob(plots[[45]], plots[[44]], plots[[43]], plots[[42]], plots[[41]], blank, blank, blank, blank, blank, ncol = 10),
  arrangeGrob(plots[[40]], plots[[39]], plots[[38]], plots[[37]], plots[[36]], plots[[35]], blank, blank, blank, blank, ncol = 10),
  arrangeGrob(plots[[34]], plots[[33]], plots[[32]], plots[[31]], plots[[30]], plots[[29]], plots[[28]], blank, blank, blank, ncol = 10),
  arrangeGrob(plots[[27]], plots[[26]], plots[[25]], plots[[24]], plots[[23]], plots[[22]], plots[[21]], plots[[20]], blank, blank, ncol = 10),
  arrangeGrob(plots[[19]], plots[[18]], plots[[17]], plots[[16]], plots[[15]], plots[[14]], plots[[13]], plots[[12]], plots[[11]], blank, ncol = 10),
  arrangeGrob(plots[[10]], plots[[9]], plots[[8]], plots[[7]], plots[[6]], plots[[5]], plots[[4]], plots[[3]], plots[[2]], plots[[1]], ncol = 10),
  nrow = 10
)

# Save the final grid arrangement as an image
#ggsave("Correlation_Matrix_Pie_Charts.svg", plot = grid_arrange, width = 20, height = 18)


###################################################
##### proportion of synergies and trade-offs ######
###################################################

# Step 1: Combine data frames into one with consistent column names
combined_df <- do.call(rbind, lapply(correlation_test_results, function(df) {
  df <- df[, c("Territory", "classification")]
  colnames(df) <- c("Territory", "classification")
  return(df)
}))

# Convert Territory and classification to factors
combined_df$Territory <- as.factor(combined_df$Territory)
combined_df$classification <- as.factor(combined_df$classification)

# Step 2: Create a summary data frame
summary_df <- table(combined_df$Territory, combined_df$classification)

# Convert the table to a data frame
summary_df <- as.data.frame.matrix(summary_df)

# Step 3: Calculate proportion of synergies and trade-offs
summary_df$Proportion_Neutral <- summary_df$Neutral / rowSums(summary_df[,c("Neutral","Synergy","Trade-off")])
summary_df$Proportion_Neutral <- summary_df$Proportion_Neutral * 100
summary_df$Proportion_Synergy <- summary_df$Synergy / rowSums(summary_df[,c("Neutral","Synergy","Trade-off")])
summary_df$Proportion_Synergy <- summary_df$Proportion_Synergy * 100
summary_df$Proportion_Tradeoff <- summary_df$`Trade-off` / rowSums(summary_df[,c("Neutral","Synergy","Trade-off")])
summary_df$Proportion_Tradeoff <- summary_df$Proportion_Tradeoff *100

# Print the summary data frame
print(summary_df)

#######################################################
############    Rank interactions    ##################
#######################################################

####===================================================######
## Calculate the rank based on the percentage of synergies ##
####===================================================######
ranked_df_list <- lapply(interaction_listT, function(df) {
  synergy_percentage <- df$Percentage[df$InteractionType == "Synergy"]
  rank <- ifelse(length(synergy_percentage) > 0, synergy_percentage, 0)
  return(rank)
})

# Sort the data frames based on the rank
sorted_df_list <- interaction_listT[order(unlist(ranked_df_list), decreasing = TRUE)]

# Display the 10 first ranked data frames
S10 <- sorted_df_list[1:10]
S10_Synergy <- bind_rows(S10, .id = "DataFrameName")

# Summarize the combined data frame
summary_S10_Synergy <- S10_Synergy %>%
  group_by(DataFrameName, InteractionType) %>%
  summarize(
    TotalCount = sum(Count),
    AveragePercentage = mean(Percentage)
  )

# Get the names of the sorted data frames
sorted_names <- names(sorted_df_list)

# Display the sorted names
synergy_top10 <- sorted_names[1:10]

# subset to synergies
df_S10S <- S10_Synergy[S10_Synergy$InteractionType == "Synergy", ]

# Order the data frame by Percentage in descending order
df_S10S <- df_S10S[order(-df_S10S$Percentage), ]

#### =================================================== ######
##  Calculate the rank based on the percentage of trade-offs ##
#### =================================================== ######
ranked_df_list <- lapply(interaction_listT, function(df) {
  trade_off_percentage <- df$Percentage[df$InteractionType == "Trade-off"]
  rank <- ifelse(length(trade_off_percentage) > 0, trade_off_percentage, 0)
  return(rank)
})

# Sort the data frames based on the rank
sorted_df_list <- interaction_listT[order(unlist(ranked_df_list), decreasing = TRUE)]

# Display the 10 first ranked data frames
sorted_df_list[1:10]

# Display the 10 first ranked data frames
S10 <- sorted_df_list[1:10]
S10_Trade_Off <- bind_rows(S10, .id = "DataFrameName")

# Summarize the combined data frame
summary_S10_Trade_Off <- S10_Trade_Off %>%
  group_by(DataFrameName, InteractionType) %>%
  summarize(
    TotalCount = sum(Count),
    AveragePercentage = mean(Percentage)
  )


# Get the names of the sorted data frames
sorted_names <- names(sorted_df_list)

# Display the sorted names
tradeoff_top10 <- sorted_names[1:10]

# subset to trade-offs
df_S10T <- S10_Trade_Off[S10_Trade_Off$InteractionType == "Trade-off", ]

# Order the data frame by Percentage in descending order
df_S10T <- df_S10T[order(-df_S10T$Percentage), ]

####### ============================= ######
####### interactions overall counts   ######
####### ============================= ######

interaction_counts <- bind_rows(interaction_listT, .id = "DataFrameName")
neutral_counts <- interaction_counts[interaction_counts$InteractionType == "Neutral", ]
synergy_counts <- interaction_counts[interaction_counts$InteractionType == "Synergy", ]
trade_offs_counts <- interaction_counts[interaction_counts$InteractionType == "Trade-off", ]

neutral_total_counts <- sum(neutral_counts$Count)
synergy_total_counts <- sum(synergy_counts$Count)
trade_offs_total_counts <- sum(trade_offs_counts$Count)

cat("Overall sum of 'neutral':", neutral_total_counts, "\n")
cat("Overall sum of 'synergy':", synergy_total_counts, "\n")
cat("Overall sum of 'trade-off':", trade_offs_total_counts, "\n")

neutral_pct <- mean(neutral_counts$Percentage)
synergy_pct <- mean(synergy_counts$Percentage)
trade_offs_pct <- mean(trade_offs_counts$Percentage)

cat("Overall sum of 'neutral':", neutral_pct, "\n")
cat("Overall sum of 'synergy':", synergy_pct, "\n")
cat("Overall sum of 'trade-off':", trade_offs_pct, "\n")


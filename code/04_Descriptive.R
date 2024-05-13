calculate_total_trade <- function(data) {
  # Group the data by year, filter rows where exporter_iso3 is not equal to importer_iso3, and then calculate the sum of trade for each year
  result <- data %>%
    filter(exporter_iso3 != importer_iso3) %>%
    group_by(year) %>%
    summarise(total_intl_trade = sum(trade, na.rm = TRUE))
  
  # Merge the result back to the original dataframe
  data1 <- merge(data, result, by = "year", all.x = TRUE)
  
  # Group the data by year and calculate the sum of trade for each year (including intranational trade)
  result2 <- data %>%
    group_by(year) %>%
    summarise(total_trade = sum(trade, na.rm = TRUE))
  
  # Merge the result back to the original dataframe
  data1 <- merge(data1, result2, by = "year", all.x = TRUE)
  
  return(data1)
}

x1<- calculate_total_trade(x)

## Plotting Macedonia and Croatia's individual trade over time
generate_and_plot_data <- function(x1, country_code, country) {
  data_country <- subset(x1, exporter_iso3 == country_code)
  data_country <- data_country %>%
    rename(world_intl_trade = total_intl_trade,
           world_total_trade = total_trade)
  data_country <- calculate_total_trade(data_country)
  data_country$trade_share_intl <- data_country$total_intl_trade / data_country$world_intl_trade * 100
  data_country$trade_share <- data_country$total_trade / data_country$world_total_trade * 100
  data_country <- unique(data_country[, c("year", "trade_share", "trade_share_intl")])
  
  colors <- c("Inc Intranational" = "blue", "International" = "red")
  names(colors) <- c("Inc Intranational", "International")
  
  ggplot(data_country, aes(x = year)) +
    geom_line(aes(y = trade_share, color = "Inc Intranational")) +
    geom_line(aes(y = trade_share_intl, color = "International")) +
    labs(x = "Year", y = "Trade Share", color = "Legend", 
         title = paste("Percentage Trade Share ", country)) +
    scale_color_manual(values = colors) +
    guides(color = guide_legend(override.aes = list(size = 1))) +
    theme(legend.position = "bottom")+
    geom_vline(xintercept = c(1997, 2002, 2003, 2004, 2005, 2007, 2013),
               linetype = "dashed", color = "red")
}

graph_MKD <- generate_and_plot_data(x1, "MKD", "Macedonia")
graph_HRV <- generate_and_plot_data(x1, "HRV", "Croatia")

graph_trade_share <- grid.arrange(graph_MKD, graph_HRV, ncol = 2)

# Save the plots
ggsave("output/figures/graph_trade_share.png", plot = graph_trade_share, width = 8, height = 4, dpi = 300)

## analyzing trade creation and trade diversion by doing the same exercise for filtered data

filtered_data <- subset(x1, exporter_iso3 == "MKD" & importer_iso3 == "HRV")
filtered_data2 <- subset(x1, exporter_iso3 == "HRV" & importer_iso3 == "MKD")


graph_MKD <- generate_and_plot_data(filtered_data, "MKD", "Macedonia")
graph_HRV <- generate_and_plot_data(filtered_data2, "HRV", "Croatia")

graph_tc_td <- grid.arrange(graph_MKD, graph_HRV, ncol = 2)

# Save the plots
ggsave("output/figures/graph_tc_td.png", plot = graph_tc_td, width = 8, height = 4, dpi = 300)

##

## plotting macedonia and croatia's trade share over time in a single plot along with percentage change 

# Sum the trade per year
summarized_data <- filtered_data %>%
  group_by(year) %>%
  summarize(total_trade = sum(trade))
names(summarized_data)[names(summarized_data) == "total_trade"] <- "total_bilateral_trade"
data <- merge(filtered_data, summarized_data, by = "year", all.x = TRUE)
# Calculate trade share
data$trade_share <- ifelse(data$total_bilateral_trade > 0, 
                           data$total_bilateral_trade / data$total_intl_trade * 100, 
                           NA)

data <- unique(data[, c("year", "trade_share")])

# Calculate percentage change
data <- data %>%
  mutate(pct_change = (trade_share - lag(trade_share, default = first(trade_share))) / lag(trade_share, default = first(trade_share)) * 100)  # Calculate percentage change

# Create plots
p1 <- ggplot(data = data, aes(x = year, y = trade_share)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = round(trade_share, 4)), vjust = -0.5, color = "black", size = 3) +  # Add data labels rounded to 0 decimal points
  geom_vline(xintercept = c(1997, 2002, 2003, 2004, 2005, 2007, 2013), linetype = "dashed", color = "red") +  # Add vertical lines
  labs(y = "Total Trade", x = NULL) +
  ggtitle("Macedonia (Exporter) and Croatia (Importer) Bilateral / Overall World Trade Trade Share (Percantage)") +  # Add title
  theme_minimal()

p2 <- ggplot(data = data, aes(x = year, y = pct_change)) +
  geom_bar(stat = "identity", fill = "orange") +
  geom_text(aes(label = paste0(round(pct_change, 0), "%")), vjust = -0.5, color = "black", size = 3) +  # Add data labels with formatting and rounded to 0 decimal points
  geom_vline(xintercept = c(1997, 2002, 2003, 2004, 2005, 2007, 2013), linetype = "dashed", color = "red") +  # Add vertical lines
  labs(y = "Percentage Change", x = "Year") +
  theme_minimal()

# Combine plots
combined_plot <- grid.arrange(p1, p2, ncol = 1)

# Export plot
ggsave("output/figures/trade_analysis.png", combined_plot, width = 10, height = 8)

## Calculate percentage bilateral trade share over indivodual countries total trade share
trade_analysis_plot <- function(country1, country2, x1) {
  data_exporter <- filter(x1, exporter_iso3 == country1)
  data_exporter <- data_exporter %>%
    group_by(year) %>%
    summarize(annual_trade = sum(trade))
  
  filtered_data <- filter(x1, exporter_iso3 == country1 & importer_iso3 == country2)
  summarized_data <- filtered_data %>%
    group_by(year) %>%
    summarize(total_trade = sum(trade))
  names(summarized_data)[names(summarized_data) == "total_trade"] <- "total_bilateral_trade"
  data <- merge(summarized_data, data_exporter, by = "year", all.x = TRUE)
  data$trade_share <- ifelse(data$total_bilateral_trade != 0, 
                             data$total_bilateral_trade / data$annual_trade * 100, 
                             NA)
  
  data <- data %>%
    mutate(pct_change = (trade_share - lag(trade_share, default = first(trade_share))) / lag(trade_share, default = first(trade_share)) * 100)  # Calculate percentage change
  
  
  # Create plots
  p1 <- ggplot(data = data, aes(x = year, y = trade_share)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    geom_text(aes(label = round(trade_share, 4)), vjust = -0.5, color = "black", size = 3) +  # Add data labels rounded to 0 decimal points
    geom_vline(xintercept = c(1997, 2002, 2003, 2004, 2005, 2007, 2013), linetype = "dashed", color = "red") +  # Add vertical lines
    labs(y = "Total Trade", x = NULL) +
    ggtitle(paste(country1, "(Exporter) and", country2, "(Importer) Bilateral / " ,country1, "Total Trade Share (Percantage)")) +  # Add title
    theme_minimal()
  
  p2 <- ggplot(data = data, aes(x = year, y = pct_change)) +
    geom_bar(stat = "identity", fill = "orange") +
    geom_text(aes(label = paste0(round(pct_change, 0), "%")), vjust = -0.5, color = "black", size = 3) +  # Add data labels with formatting and rounded to 0 decimal points
    geom_vline(xintercept = c(1997, 2002, 2003, 2004, 2005, 2007, 2013), linetype = "dashed", color = "red") +  # Add vertical lines
    labs(y = "Percentage Change", x = "Year") +
    theme_minimal()
  
  # Combine plots
  combined_plot <- grid.arrange(p1, p2, ncol = 1)
  
  return(combined_plot)
}

combined_plot_MKD <- trade_analysis_plot("MKD", "HRV", x1)

# Export plot
ggsave("output/figures/trade_share_analysis_MKD.png", combined_plot_MKD , width = 10, height = 8)

combined_plot_HRV <- trade_analysis_plot("HRV", "MKD", x1)

# Export plot
ggsave("output/figures/trade_share_analysis_HRV.png", combined_plot_HRV , width = 10, height = 8)


#Top Trading Partner of Croatia (%Share)

# Step 2: Filter the data for Croatia
croatia_data <- subset(x, exporter_iso3 == "HRV" | importer_iso3 == "HRV")

# Step 3: Group data by year and calculate total trade value with each partner and the whole world
trade_summary <- croatia_data %>%
  group_by(year, partner = ifelse(exporter_iso3 == "HRV", importer_iso3, exporter_iso3)) %>%
  summarise(TotalTrade = sum(trade)) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(TotalTradeWorld = sum(TotalTrade))

# Step 4: Calculate the trade share of Croatia with each partner country relative to the whole world
croatia_trade_share <- trade_summary %>%
  mutate(TradeShare = TotalTrade / TotalTradeWorld * 100)

# Step 5: Identify the top 5 trading partners in each year
top5_partners_each_year <- croatia_trade_share %>%
  group_by(year) %>%
  top_n(5, wt = TotalTrade) %>%
  ungroup()

# Step 6: Generate the bar chart with a continuous color scale
ggplot(top5_partners_each_year, aes(x = year, y = TradeShare, fill = partner)) +
  geom_bar(stat = "identity") +
  labs(title = "Croatia's Top 5 Trading Partners",
       x = "Year", y = "Trade Share (%)",
       fill = "Trading Partner") +
  scale_fill_manual(values = rainbow(length(unique(top5_partners_each_year$partner)))) +  # Manual color scale
  theme_minimal()

# Export plot
ggsave("output/figures/HRV_tp.png")


# Top Trading Partner of North Macedonia (%Share)

# Step 2: Filter the data for North Macedonia
macedonia_data <- subset(x, exporter_iso3 == "MKD" | importer_iso3 == "MKD")

# Step 3: Group data by year and calculate total trade value with each partner and the whole world
trade_summary <- macedonia_data %>%
  group_by(year, partner = ifelse(exporter_iso3 == "MKD", importer_iso3, exporter_iso3)) %>%
  summarise(TotalTrade = sum(trade)) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(TotalTradeWorld = sum(TotalTrade))

# Step 4: Calculate the trade share of North Macedonia with each partner country relative to the whole world
macedonia_trade_share <- trade_summary %>%
  mutate(TradeShare = TotalTrade / TotalTradeWorld * 100)

# Step 5: Identify the top 5 trading partners in each year
top5_partners_each_year <- macedonia_trade_share %>%
  group_by(year) %>%
  top_n(5, wt = TotalTrade) %>%
  ungroup()

# Step 6: Generate the bar chart with a continuous color scale
ggplot(top5_partners_each_year, aes(x = year, y = TradeShare, fill = partner)) +
  geom_bar(stat = "identity") +
  labs(title = "Macedonia's Top 5 Trading Partners",
       x = "Year", y = "Trade Share (%)",
       fill = "Trading Partner") +
  scale_fill_manual(values = rainbow(length(unique(top5_partners_each_year$partner)))) +  # Manual color scale
  theme_minimal()

# Export plot
ggsave("output/figures/MKD_tp.png")


#Broad Sectors-Croatia to Macedonia# 
# Filter the data for trade from Croatia to North Macedonia
cro_to_mk_trade <- x %>%
  filter(exporter_iso3 == "HRV" & importer_iso3 == "MKD")

# Group data by broad sector classifications and sum trade values for each year
sector_wise_trade_cro_to_mk <- cro_to_mk_trade %>%
  group_by(broad_sector, year) %>%
  summarise(total_trade = sum(trade)) %>%
  ungroup()

# Calculate the total trade for each year
yearly_trade_cro_to_mk <- sector_wise_trade_cro_to_mk %>%
  group_by(year) %>%
  summarise(total_trade_year = sum(total_trade)) %>%
  ungroup()

# Merge the total trade for each year with the sector-wise trade data
sector_wise_trade_cro_to_mk <- left_join(sector_wise_trade_cro_to_mk, yearly_trade_cro_to_mk, by = "year")

# Calculate the percentage share of each sector with respect to overall broad sector trade for each year
sector_wise_trade_cro_to_mk <- sector_wise_trade_cro_to_mk %>%
  mutate(percentage_share = (total_trade / total_trade_year) * 100)

# Plot sector-wise trade percentage share over the years with year interval of 3
ggplot(sector_wise_trade_cro_to_mk, aes(x = year, y = percentage_share, color = broad_sector)) +
  geom_line() +
  labs(title = "From Croatia to North Macedonia",
       x = "Year",
       y = "Percentage Share",
       color = "Broad Sector") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(sector_wise_trade_cro_to_mk$year), max(sector_wise_trade_cro_to_mk$year), by = 3)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Export plot
ggsave("output/figures/HRV-MKD_bs.png")

## Broad Sectors- North Macedonia to Croatia##

# Filter the data for trade from North Macedonia to Croatia
mk_to_cro_trade <- x %>%
  filter(exporter_iso3 == "MKD" & importer_iso3 == "HRV")

# Group data by broad sector classifications and sum trade values for each year
sector_wise_trade_mk_to_cro <- mk_to_cro_trade %>%
  group_by(broad_sector, year) %>%
  summarise(total_trade = sum(trade)) %>%
  ungroup()

# Calculate the total trade for each year
yearly_trade_mk_to_cro <- sector_wise_trade_mk_to_cro %>%
  group_by(year) %>%
  summarise(total_trade_year = sum(total_trade)) %>%
  ungroup()

# Merge the total trade for each year with the sector-wise trade data
sector_wise_trade_mk_to_cro <- left_join(sector_wise_trade_mk_to_cro, yearly_trade_mk_to_cro, by = "year")

# Calculate the percentage share of each sector with respect to overall broad sector trade for each year
sector_wise_trade_mk_to_cro <- sector_wise_trade_mk_to_cro %>%
  mutate(percentage_share = (total_trade / total_trade_year) * 100)

# Plot sector-wise trade percentage share over the years with year interval of 3
ggplot(sector_wise_trade_mk_to_cro, aes(x = year, y = percentage_share, color = broad_sector)) +
  geom_line() +
  labs(title = "North Macedonia to Croatia",
       x = "Year",
       y = "Percentage Share",
       color = "Broad Sector") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(sector_wise_trade_mk_to_cro$year), max(sector_wise_trade_mk_to_cro$year), by = 3)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("output/figures/MKD-HRV_bs.png")


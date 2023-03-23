# Sukesh
# Horticulture data

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(cowplot)

crop_data = read_csv("table_07_02_01_satyam.csv")

min_production <- crop_data %>%
  group_by(type) %>%
  summarise(min_production = min(production),
            state_or_ut = states_or_ut[which.min(production)])

ggplot(min_production, aes(x = type, y = min_production)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  xlab("Type") +
  ylab("Minimum Production") +
  ggtitle("Highest Producer of each Horticulture crop: 2017-18") +
  # Add state/UT names to the top of the bars
  geom_text(aes(label = state_or_ut), vjust = -0.5, angle = 90, hjust = 0.3) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

satyam_table = read_csv("ProjectTables_Satyam_7.2.1.csv", )

satyam_table$production <- as.numeric(satyam_table$production)

production_analysis <- aggregate(cbind(area, production) ~ type, data = satyam_table, FUN = function(x) mean(x, na.rm = TRUE))

p <- ggplot(production_analysis, aes(x = type))

p <- p + geom_point(aes(y = area, color = "area"))

p <- p + geom_point(aes(y = production, color = "production"))

p <- p + labs(x = "type", y = "Production", title = "Crop Type Vs Area") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

p <- p + scale_color_manual(name = "Area", values = c("area" = "green", "production" = "darkblue"))

print(p)

production_by_region <- aggregate(production ~ states_or_ut, data = satyam_table, FUN = mean, na.rm = TRUE)

g2 <- ggplot(production_by_region, aes(x = states_or_ut, y = production)) +labs(x = "Region", y = "Production")

g2 + geom_point(color="red") + coord_flip()

satyam_table_2 = read_csv("ProjectTables_Satyam_7.2.2.csv")

satyam_table_2$area <- as.numeric(satyam_table_2$area)

fruits_crop_means <- satyam_table_2 %>%
  filter(type == "Fruits Crops") %>% # select only rows where type is "Fruits Crops"
  group_by(year) %>% # group the data by year
  summarize(mean_area = mean(area, na.rm = TRUE), 
            mean_production = mean(production, na.rm = TRUE))

p <- ggplot(fruits_crop_means, aes(x = year))

p <- p + geom_point(aes(y = mean_area, color = "Area"))

p <- p + geom_point(aes(y = mean_production, color = "Production"))

p <- p + labs(x = "Year", y = "Production", title = "Year Vs Area and production of Fruits Crops") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

p <- p + scale_color_manual(name = "Area", values = c("Area" = "green", "Production" = "darkblue"))

print(p)

production_by_region <- aggregate(production ~ states_or_ut, data = satyam_table_2, FUN = mean, na.rm = TRUE)

g2 <- ggplot(production_by_region, aes(x = states_or_ut, y = production)) +labs(x = "Region", y = "Production") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

g2 + geom_point(color="blue")

satyam_table_3 = read_csv("ProjectTables_Satyam_7.2.3.csv")

satyam_table_3$production <- as.numeric(satyam_table_3$production)

satyam_table_3$area <- as.numeric(satyam_table_3$area)

vegetable_crop_means <- satyam_table_3 %>%
  filter(type == "Vegetable Crops") %>% # select only rows where type is "Vegetable Crops"
  group_by(year) %>% # group the data by year
  summarize(mean_area = mean(area, na.rm = TRUE), 
            mean_production = mean(production, na.rm = TRUE))

p <- ggplot(vegetable_crop_means, aes(x = year))

p <- p + geom_point(aes(y = mean_area, color = "Area"))

p <- p + geom_point(aes(y = mean_production, color = "Production"))

p <- p + labs(x = "Year", y = "Production", title = "Year Vs Area and production of Vegetable Crops") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

p <- p + scale_color_manual(name = "Area", values = c("Area" = "red", "Production" = "green"))

print(p)

production_by_region <- aggregate(production ~ states_or_ut, data = satyam_table_3, FUN = mean, na.rm = TRUE)

g2 <- ggplot(production_by_region, aes(x = states_or_ut, y = production)) +labs(x = "Region", y = "Production") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

g2 + geom_point(color="blue")

crop_data = read_csv("tables_7.2.2_to_7.2.8_satyam_shyam.csv")

top_producer_states <- crop_data %>%
  group_by(year, type) %>%
  filter(area == max(area)) %>%
  select(year, type, states_or_ut, production)


ggplot(top_producer_states, aes(x = year, y = type, color = states_or_ut)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  labs(x = "Year", y = "Type", color = "State/UT") +
  ggtitle("Biggest Area consumer State for each Horticulture Crop: 2014-18")

crop_data = read_csv("tables_7.2.2_to_7.2.8_satyam_shyam.csv")

top_producer_states <- crop_data %>%
  group_by(year, type) %>%
  filter(production == max(production)) %>%
  select(year, type, states_or_ut, production)


ggplot(top_producer_states, aes(x = year, y = type, color = states_or_ut)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  labs(x = "Year", y = "Type", color = "State/UT") +
  ggtitle("highest Producer State for each Horticulture Crop: 2014-18")

data_subset <- crop_data %>%
  select(states_or_ut, year, type, production)

required_states <- c("ANDHRA PRADESH", "GUJRAT", "KERALA", "MADHYA PRADESH", "MAHARASHTRA", "PUNJAB", "RAJASTHAN", "TAMILNADU", "UTTAR PRADESH", "WEST BENGAL")
data_subset_filtered <- data_subset %>%
  filter(states_or_ut %in% required_states)

data_grouped <- data_subset_filtered %>%
  group_by(type, year) %>%
  summarise(total_production = sum(production))

ggplot(data_grouped, aes(x = year, y = total_production)) +
  geom_line() +
  facet_wrap(~type, ncol = 2, scales = "free_y") +
  labs(title = "Production data for required states and union territories",
       x = "Year",
       y = "Production") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

crop_data = read_csv("tables_7.2.2_to_7.2.8_satyam_shyam.csv")

production_by_region <- aggregate(production ~ states_or_ut, data = crop_data, FUN = mean, na.rm = TRUE)

g2 <- ggplot(production_by_region, aes(x = states_or_ut, y = production)) +labs(x = "Region", y = "Production")

g2 + geom_point(color="blue") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

crop_means <- crop_data %>%
  filter(type == "Fruits Crops") %>% # select only rows where type is "Fruits Crops"
  group_by(year) %>% # group the data by year
  summarize(area_of_fruits = mean(area, na.rm = TRUE))

veggies <- subset(crop_data, type == "Vegetable Crops")

means <- aggregate(area ~ year, data = veggies, FUN = mean)

crop_means <- merge(crop_means, means, by = "year", all.x = TRUE)

colnames(crop_means)[colnames(crop_means) == "area"] <- "area_of_vegetable"

plantation <- subset(crop_data, type == "Plantation Crop")

means <- aggregate(area ~ year, data = plantation, FUN = mean)

crop_means <- merge(crop_means, means, by = "year", all.x = TRUE)

colnames(crop_means)[colnames(crop_means) == "area"] <- "area_of_plantation"

p <- ggplot(crop_means, aes(x = year))

p <- p + geom_point(aes(y = area_of_fruits, color = "area_of_fruits"))

p <- p + geom_point(aes(y = area_of_vegetable, color = "area_of_vegetable"))

p <- p + geom_point(aes(y = area_of_plantation, color = "area_of_plantation"))

p <- p + labs(x = "Year", y = "Area", title = "Year Vs Area of Crops") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

p <- p + scale_color_manual(name = "Area", values = c("area_of_fruits" = "green", "area_of_vegetable" = "darkblue", "area_of_plantation" = "red"))

print(p)

crop_means <- crop_data %>%
  filter(type == "Spice Crops") %>% # select only rows where type is "Fruits Crops"
  group_by(year) %>% # group the data by year
  summarize(area_of_spice = mean(area, na.rm = TRUE))

flowers <- subset(crop_data, type == "flowers")

means <- aggregate(area ~ year, data = flowers, FUN = mean)

crop_means <- merge(crop_means, means, by = "year", all.x = TRUE)

colnames(crop_means)[colnames(crop_means) == "area"] <- "area_of_flowers"

medicinal <- subset(crop_data, type == "Aromatics & Medicinal Plants")

means <- aggregate(area ~ year, data = medicinal, FUN = mean)

crop_means <- merge(crop_means, means, by = "year", all.x = TRUE)

colnames(crop_means)[colnames(crop_means) == "area"] <- "area_of_medicinal"

p <- ggplot(crop_means, aes(x = year))

p <- p + geom_point(aes(y = area_of_spice, color = "area_of_spice"))

p <- p + geom_point(aes(y = area_of_flowers, color = "area_of_flowers"))

p <- p + geom_point(aes(y = area_of_medicinal, color = "area_of_medicinal"))

p <- p + labs(x = "Year", y = "Area", title = "Year Vs Area of Crops") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

p <- p + scale_color_manual(name = "Area", values = c("area_of_spice" = "blue", "area_of_flowers" = "purple", "area_of_medicinal" = "brown"))

print(p)

area_by_region <- aggregate(area ~ states_or_ut, data = crop_data, FUN = mean, na.rm = TRUE)

g2 <- ggplot(area_by_region, aes(x = states_or_ut, y = area)) +labs(x = "Region", y = "Area")

g2 + geom_point(color="purple") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

crop_means <- crop_data %>%
  filter(type == "Fruits Crops") %>% # select only rows where type is "Fruits Crops"
  group_by(year) %>% # group the data by year
  summarize(production_of_fruits = mean(production, na.rm = TRUE))

veggies <- subset(crop_data, type == "Vegetable Crops")

means <- aggregate(production ~ year, data = veggies, FUN = mean)

crop_means <- merge(crop_means, means, by = "year", all.x = TRUE)

colnames(crop_means)[colnames(crop_means) == "production"] <- "production_of_vegetable"

plantation <- subset(crop_data, type == "Plantation Crop")

means <- aggregate(production ~ year, data = plantation, FUN = mean)

crop_means <- merge(crop_means, means, by = "year", all.x = TRUE)

colnames(crop_means)[colnames(crop_means) == "production"] <- "production_of_plantation"

p <- ggplot(crop_means, aes(x = year))

p <- p + geom_point(aes(y = production_of_fruits, color = "production_of_fruits"))

p <- p + geom_point(aes(y = production_of_vegetable, color = "production_of_vegetable"))

p <- p + geom_point(aes(y = production_of_plantation, color = "production_of_plantation"))

p <- p + labs(x = "Year", y = "Area", title = "Year Vs Area of Crops") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

p <- p + scale_color_manual(name = "Area", values = c("production_of_fruits" = "green", "production_of_vegetable" = "darkblue", "production_of_plantation" = "red"))

print(p)

crop_means <- crop_data %>%
  filter(type == "Spice Crops") %>% # select only rows where type is "Fruits Crops"
  group_by(year) %>% # group the data by year
  summarize(production_of_spice = mean(production, na.rm = TRUE))

flowers <- subset(crop_data, type == "flowers")

means <- aggregate(production ~ year, data = flowers, FUN = mean)

crop_means <- merge(crop_means, means, by = "year", all.x = TRUE)

colnames(crop_means)[colnames(crop_means) == "production"] <- "production_of_flowers"

medicinal <- subset(crop_data, type == "Aromatics & Medicinal Plants")

means <- aggregate(production ~ year, data = medicinal, FUN = mean)

crop_means <- merge(crop_means, means, by = "year", all.x = TRUE)

colnames(crop_means)[colnames(crop_means) == "production"] <- "production_of_medicinal"

p <- ggplot(crop_means, aes(x = year))

p <- p + geom_point(aes(y = production_of_spice, color = "production_of_spice"))

p <- p + geom_point(aes(y = production_of_flowers, color = "production_of_flowers"))

p <- p + geom_point(aes(y = production_of_medicinal, color = "production_of_medicinal"))

p <- p + labs(x = "Year", y = "Area", title = "Year Vs Area of Crops") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

p <- p + scale_color_manual(name = "Area", values = c("production_of_spice" = "blue", "production_of_flowers" = "purple", "production_of_medicinal" = "brown"))

print(p)

crop_data = read_csv("tables_7.2.8_to_7.2.16_Sukesh.csv")

max_share <- crop_data %>%
  group_by(crop) %>%
  summarise(max_share = max(share),
            state_or_ut = states_or_ut[which.max(share)])

ggplot(max_share, aes(x = crop, y = max_share)) +
  geom_bar(stat = "identity", fill = "green") +
  xlab("Type") +
  ylab("Maximum Share") +
  ggtitle("Highest Share of each Horticulture crop 2018") +
  # Add state/UT names to the top of the bars
  geom_text(aes(label = state_or_ut), vjust = -0.5) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

sukesh_data = read_csv("tables_7.2.8_to_7.2.16_Sukesh.csv", )

production_analysis <- aggregate(production ~ states_or_ut, data = sukesh_data, FUN = function(x) mean(x, na.rm = TRUE))

g2 <- ggplot(production_analysis, aes(x = states_or_ut, y = production)) +labs(x = "Region", y = "Production")

g2 + geom_point(color="blue") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

production_analysis <- aggregate(production ~ crop, data = sukesh_data, FUN = function(x) mean(x, na.rm = TRUE))

g2 <- ggplot(production_analysis, aes(x = crop, y = production)) +labs(x = "Crop Type", y = "Production")

g2 + geom_point(color="purple") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

production_analysis <- aggregate(share ~ crop, data = sukesh_data, FUN = function(x) mean(x, na.rm = TRUE))

g2 <- ggplot(production_analysis, aes(x = crop, y = share)) +labs(x = "Crop Type", y = "share in %")

g2 + geom_point(color="purple") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

production_analysis <- aggregate(share ~ states_or_ut, data = sukesh_data, FUN = function(x) mean(x, na.rm = TRUE))

g2 <- ggplot(production_analysis, aes(x = states_or_ut, y = share)) +labs(x = "Region", y = "Share in %")

g2 + geom_point(color="blue") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

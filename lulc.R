lulc_data<-read.csv(file.choose())
print(lulc_data)

lulc_data$Total <- rowSums(lulc_data[, c("Vegetation", "Water", "Urban", "Barrenland")])
lulc_data$Vegetation_Percent <- (lulc_data$Vegetation / lulc_data$Total) * 100
lulc_data$Water_Percent <- (lulc_data$Water / lulc_data$Total) * 100
lulc_data$Urban_Percent <- (lulc_data$Urban / lulc_data$Total) * 100
lulc_data$Barrenland_Percent <- (lulc_data$Barrenland / lulc_data$Total) * 100


print(lulc_data)


library(ggplot2)
library(reshape2)


#land cover change from 2002 to 2022

lulc_2002_2022 <- subset(lulc_data, Year >= 2002 & Year <= 2022)
lulc_long <- melt(lulc_2002_2022, id.vars = "Year", 
                  measure.vars = c("Vegetation", "Water", "Urban", "Barrenland"),
                  variable.name = "Land_Cover_Type", value.name = "Value")

ggplot(lulc_long, aes(x = Year, y = Value, color = Land_Cover_Type)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Vegetation" = "green", "Water" = "blue", "Urban" = "red", "Barrenland" = "yellow"),
                     labels = c("Vegetation", "Water", "Urban", "Bareland")) +
  labs(title = "Land Cover Change Trend (2002-2022)",
       x = "Year", 
       y = "Area (Sq.Km)",
       color = "Land Cover Change") +
  theme_minimal(base_family = "Times New Roman") +
  theme(text = element_text(family = "Times New Roman"))

#Compare Historical and Predicted Data

lulc_full <- subset(lulc_data, Year >= 2002 & Year <= 2040)

lulc_full_long <- melt(lulc_full, id.vars = "Year", 
                       measure.vars = c("Vegetation", "Water", "Urban", "Barrenland"),
                       variable.name = "Land_Cover_Type", value.name = "Value")

ggplot(lulc_full_long, aes(x = Year, y = Value, color = Land_Cover_Type)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Vegetation" = "green", "Water" = "blue", "Urban" = "red", "Barrenland" = "yellow"),
                     labels = c("Vegetation", "Water", "Urban", "Bareland")) +
  labs(title = "Land Cover Trend Comparison (2002-2040)",
       x = "Year", 
       y = "Area (Sq.Km)",
       color = "Land Cover Change") +
  theme_minimal(base_family = "Times New Roman") +
  theme(text = element_text(family = "Times New Roman"))

#percentage change
lulc_percent_long <- melt(lulc_full, id.vars = "Year", 
                          measure.vars = c("Vegetation_Percent", "Water_Percent", "Urban_Percent", "Barrenland_Percent"),
                          variable.name = "Land_Cover_Type", value.name = "Percentage")


ggplot(lulc_percent_long, aes(x = Year, y = Percentage, color = Land_Cover_Type)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Vegetation_Percent" = "green", "Water_Percent" = "blue", "Urban_Percent" = "red", "Barrenland_Percent" = "yellow"),
                     labels = c("Vegetation", "Water", "Urban", "Bareland")) +
  labs(title = "Percentage Land Cover Change (2002-2040)",
       x = "Year", 
       y = "Percentage (%)",
       color = "Land Cover Change") +
  theme_minimal(base_family = "Times New Roman") +
  theme(text = element_text(family = "Times New Roman"))


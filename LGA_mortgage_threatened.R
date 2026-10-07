data <- read.csv("LGA_mortgagedata.csv", header = TRUE,  fileEncoding = "Windows-1252")
data

data$Count_T_and_NT_spp <- gsub("Ê", "", data$Count_T_and_NT_spp)
data$Count_T_and_NT_spp <- as.numeric(trimws(data$Count_T_and_NT_spp))
str(data$Count_T_and_NT_spp)
summary(data$Count_T_and_NT_spp)

quartiles <- quantile(
  data$Count_T_and_NT_spp,
  probs = c(0.25, 0.5, 0.75),
  na.rm = TRUE
)

unique(data$Count_T_and_NT_spp)

threshold <- 31

top_quartile_LGAs <- subset(data, Count_T_and_NT_spp >= threshold)

num_top_quartile <- nrow(top_quartile_LGAs)

cat("Number of LGAs in the top quartile (≥", threshold,
    "species):", num_top_quartile, "\n")

print(top_quartile_LGAs)
#NOTE: THIS WAS JUST TO GET THE TOP QUARTILES

#TOP QUARTILE NO. MORTGAGES:
# Total mortgages in the top-quartile LGAs
topq_mortgages <- sum(top_quartile_LGAs$Owned_with_mortgage, na.rm = TRUE)

# Total mortgages in all Australian LGAs
total_mortgages <- sum(data$Owned_with_mortgage, na.rm = TRUE)

# Percentage
percent_of_total <- (topq_mortgages / total_mortgages) * 100
percent_of_total


#Average percentage of dwellings that are mortgaged within the selected LGAs
mean_mortgage_percent <- mean(top_quartile_LGAs$Owned_with_mortgage_., na.rm = TRUE)
top_quartile_LGAs$`Owned_with_mortgage_%` <- as.numeric(
  gsub("[^0-9.]", "", top_quartile_LGAs$`Owned_with_mortgage_.`)
)

mean_mortgage_percent
cat("Number of top-quartile LGAs:", nrow(top_quartile_LGAs), "\n")
cat("Total mortgaged dwellings in top-quartile LGAs:", topq_mortgages, "\n")
cat("Total mortgaged dwellings in Australia:", total_mortgages, "\n")
cat("Percentage of Australia's mortgaged dwellings in the top-quartile LGAs:",
    round(percent_of_total, 2), "%\n")
cat("Average percentage of mortgaged dwellings in the selected LGAs:",
    round(mean_mortgage_percent, 2), "%\n")

total_LGAs <- nrow(data)
cat("Total number of LGAs:", total_LGAs, "\n")

library(ggplot2)

mortgage_summary <- data.frame(
  Group = c("Top-quartile LGAs", "All Australian LGAs"),
  Mortgaged_Dwellings = c(topq_mortgages, total_mortgages)
)

ggplot(mortgage_summary, aes(x = Group, y = Mortgaged_Dwellings, fill = Group)) +
  geom_col() +
  labs(
    title = "Mortgaged dwellings in top-quartile LGAs compared with Australia",
    x = "",
    y = "Number of mortgaged dwellings"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

install.packages("ggplot2")
library(ggplot2)
install.packages("scales")

#SCATTERPLOT
plot_data <- data[!is.na(data$Count_T_and_NT_spp) &
                    !is.na(data$Owned_with_mortgage_.), ]

plot_data$Conservation_group <- ifelse(
  plot_data$Count_T_and_NT_spp >= threshold,
  "High conservation",
  "Other LGAs"
)

ggplot(plot_data, aes(
  x = Count_T_and_NT_spp,
  y = Owned_with_mortgage_.,
  colour = Conservation_group
)) +
  geom_point(size = 1, alpha = 0.8) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    colour = "black",
    linewidth = 1.2
  ) +
  scale_colour_manual(
    values = c(
      "High conservation" = "red",
      "Other LGAs" = "grey50"
    )
  ) +
  labs(
    title = "Relationship between species richness and mortgaged",
    x = "Species Richness",
    y = "Mortgaged dwellings (%)",
    colour = ""
  ) +
  theme_minimal()

#
mortgage_percentage <- data.frame(
  Group = c("Top-quartile LGAs", "Other LGAs"),
  Percentage = c(
    percent_of_total,
    100 - percent_of_total
  )
)

ggplot(mortgage_percentage, aes(x = "", y = Percentage, fill = Group)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  labs(
    title = "Share of Australian mortgaged dwellings",
    fill = ""
  ) +
  theme_void()

#
ggplot(data, aes(x = Count_T_and_NT_spp, y = Owned_with_mortgage_.)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, colour = "blue") +
  labs(
    title = "Relationship between species richness and mortgage ownership",
    x = "Number of threatened and non-threatened species",
    y = "Mortgaged dwellings (%)"
  ) +
  theme_minimal()

#national median of mortgages
median_mortgages <- median(
  data$Owned_with_mortgage,
  na.rm = TRUE
)

median_mortgages

#high conservation LGA w above median mortgages
high_conservation_high_mortgage <- subset(
  top_quartile_LGAs,
  Owned_with_mortgage > median_mortgages
)

proportion_high_mortgage <- nrow(high_conservation_high_mortgage) /
  nrow(top_quartile_LGAs)

percentage_high_mortgage <- proportion_high_mortgage * 100

cat(
  "Proportion of high-conservation LGAs with above-median mortgage counts:",
  round(percentage_high_mortgage, 2),
  "%\n"
)

#ranked by mortgage count
names(data)

ranked_by_mortgages <- high_conservation_high_mortgage[
  order(high_conservation_high_mortgage$Owned_with_mortgage,
        decreasing = TRUE),
]

ranked_by_mortgages

#ranked by mortgage percentage
ranked_by_percentage <- high_conservation_high_mortgage[
  order(high_conservation_high_mortgage$Owned_with_mortgage_.,
        decreasing = TRUE),
]

ranked_by_percentage

#CLEANED
names(high_conservation_high_mortgage)
ranked_by_mortgages <- high_conservation_high_mortgage[
  order(high_conservation_high_mortgage$Owned_with_mortgage,
        decreasing = TRUE),
]

library(dplyr)

ranked_by_mortgages <- high_conservation_high_mortgage %>%
  arrange(desc(Owned_with_mortgage))

top10_mortgage_LGAs <- ranked_by_mortgages %>%
  select(
    LGA_2021_Name,
    Count_T_and_NT_spp,
    Owned_with_mortgage,
    Owned_with_mortgage_.
  ) %>%
  head(10)

top10_mortgage_LGAs

#TABLE
install.packages("gt")
library(gt)

top10_mortgage_LGAs %>%
  gt() %>%
  tab_header(
    title = "Top 10 High-Conservation LGAs by Mortgage Count"
  ) %>%
  cols_label(
    LGA_2021_Name = "LGA",
    Count_T_and_NT_spp = "Species count",
    Owned_with_mortgage = "Mortgaged dwellings",
    Owned_with_mortgage_. = "Mortgage (%)"
  ) %>%
  fmt_number(
    columns = c(
      Owned_with_mortgage,
      Owned_with_mortgage_.
    ),
    decimals = 1
  )

#BY AREA
str(data$Size)
data$Species_per_km2 <- data$Count_T_and_NT_spp / data$Size
summary(data$Species_per_km2)
density_threshold <- quantile(
  data$Species_per_km2,
  probs = 0.75,
  na.rm = TRUE
)

high_conservation_density <- subset(
  data,
  Species_per_km2 >= density_threshold
)

nrow(high_conservation_density)

median_mortgages <- median(
  data$Owned_with_mortgage,
  na.rm = TRUE
)

high_density_high_mortgage <- subset(
  high_conservation_density,
  Owned_with_mortgage > median_mortgages
)

high_density_high_mortgage <- subset(
  high_conservation_density,
  Owned_with_mortgage > median_mortgages
)

proportion_density_high_mortgage <- 
  nrow(high_density_high_mortgage) /
  nrow(high_conservation_density)

percentage_density_high_mortgage <- 
  proportion_density_high_mortgage * 100

cat(
  "Percentage of high-species-density LGAs with above-median mortgages:",
  round(percentage_density_high_mortgage, 2),
  "%\n"
)

library(dplyr)

ranked_density_mortgages <- high_density_high_mortgage %>%
  arrange(desc(Owned_with_mortgage)) %>%
  select(
    LGA_2021_Name,
    Species_per_km2,
    Count_T_and_NT_spp,
    Owned_with_mortgage,
    Owned_with_mortgage_.
  )

head(ranked_density_mortgages, 10)

ranked_density_percentage <- high_density_high_mortgage %>%
  arrange(desc(Owned_with_mortgage_.)) %>%
  select(
    LGA_2021_Name,
    Species_per_km2,
    Count_T_and_NT_spp,
    Owned_with_mortgage,
    Owned_with_mortgage_.
  )

head(ranked_density_percentage, 10)

#checkign for significance
overlap <- intersect(
  top_quartile_LGAs$LGA_2021_Code,
  high_conservation_density$LGA_2021_Code
)

length(overlap)

overlap_percent <- length(overlap) / nrow(top_quartile_LGAs) * 100

cat(
  "Percentage overlap between the two high-conservation definitions:",
  round(overlap_percent, 2),
  "%\n"
)

#mortgage characteristics differences?
t.test(
  top_quartile_LGAs$Owned_with_mortgage,
  high_conservation_density$Owned_with_mortgage
)

t.test(
  top_quartile_LGAs$Owned_with_mortgage_.,
  high_conservation_density$Owned_with_mortgage_.
)

#correlation species count and LGA size
cor.test(
  data$Size,
  data$Count_T_and_NT_spp,
  method = "pearson"
)

#regression model
model <- lm(
  Count_T_and_NT_spp ~ Owned_with_mortgage_. + Size,
  data = data
)

summary(model)

model_density <- lm(
  Species_per_km2 ~ Owned_with_mortgage_. + Size,
  data = data
)

summary(model_density)

t.test(
  top_quartile_LGAs$Owned_with_mortgage_.,
  high_conservation_density$Owned_with_mortgage_.
)

#AREA EFFECT
cor.test(
  data$Size,
  data$Count_T_and_NT_spp,
  method = "pearson"
)

length(overlap) / nrow(top_quartile_LGAs)

t.test(top_quartile_LGAs$Owned_with_mortgage_.,
       high_conservation_density$Owned_with_mortgage_.)

#
ggplot(data, aes(
  x = Owned_with_mortgage_.,
  y = Count_T_and_NT_spp
)) +
  geom_point(
    colour = "darkgreen",
    size = 1,
    alpha = 0.6
  ) +
  geom_smooth(
    method = "lm",
    colour = "red",
    linewidth = 1.2
  ) +
  labs(
    title = "Relationship between mortgage ownership and species richness",
    x = "Dwellings owned with a mortgage (%)",
    y = "Species Richness"
  ) +
  theme_minimal()

#species richness vs density
comparison <- data.frame(
  Definition = c(
    "High species count",
    "High species density"
  ),
  Mean_mortgage = c(
    mean(top_quartile_LGAs$Owned_with_mortgage_.,
         na.rm = TRUE),
    mean(high_conservation_density$Owned_with_mortgage_.,
         na.rm = TRUE)
  )
)


ggplot(comparison, aes(
  x = Definition,
  y = Mean_mortgage,
  fill = Definition
)) +
  geom_col(width = 0.6) +
  labs(
    title = "Mortgage ownership across biodiversity categories",
    x = "Biodiversity definition",
    y = "Mean mortgage ownership (%)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#table
library(knitr)

library(dplyr)

mortgage_rank_table <- high_conservation_high_mortgage %>%
  arrange(desc(Owned_with_mortgage)) %>%
  select(
    LGA_2021_Name,
    Count_T_and_NT_spp,
    Species_per_km2,
    Owned_with_mortgage,
    Owned_with_mortgage_.
  )

head(mortgage_rank_table, 10)
kable(
  mortgage_rank_table,
  col.names = c(
    "LGA",
    "Species count",
    "Species per km²",
    "Mortgaged dwellings",
    "Mortgage (%)"
  ),
  digits = 2,
  caption = "High-conservation LGAs ranked by mortgage count"
)

data$Species_per_km2 <- data$Count_T_and_NT_spp / data$Size
high_conservation_high_mortgage <- subset(
  data,
  Count_T_and_NT_spp >= threshold &
    Owned_with_mortgage > median_mortgages
)
names(high_conservation_high_mortgage)

kable(
  head(mortgage_rank_table, 20),
  col.names = c(
    "LGA",
    "Species count",
    "Species per km²",
    "Mortgaged dwellings",
    "Mortgage (%)"
  ),
  digits = 2,
  caption = "Top 20 high-conservation LGAs ranked by mortgage count"
)

# Regression: Does species richness predict mortgage ownership?
model_richness <- lm(
  Owned_with_mortgage_. ~ Count_T_and_NT_spp + Size,
  data = data
)

summary(model_richness)

data$Species_per_km2 <- data$Count_T_and_NT_spp / data$Size

# Regression: Does species density predict mortgage ownership?
model_density_reverse <- lm(
  Owned_with_mortgage_. ~ Species_per_km2 + Size,
  data = data
)

summary(model_density_reverse)

ggplot(data, aes(
  x = Count_T_and_NT_spp,
  y = Owned_with_mortgage_.
)) +
  geom_point(
    colour = "darkgreen",
    size = 1,
    alpha = 0.6
  ) +
  geom_smooth(
    method = "lm",
    colour = "red",
    linewidth = 1.2
  ) +
  labs(
    title = "Does species richness predict mortgage ownership?",
    x = "Number of threatened and non-threatened species",
    y = "Dwellings owned with a mortgage (%)"
  ) +
  theme_minimal()

ggplot(data, aes(
  x = Species_per_km2,
  y = Owned_with_mortgage_.
)) +
  geom_point(
    colour = "darkblue",
    size = 1,
    alpha = 0.6
  ) +
  geom_smooth(
    method = "lm",
    colour = "red",
    linewidth = 1.2
  ) +
  labs(
    title = "Does species density predict mortgage ownership?",
    x = "Species richness per km²",
    y = "Dwellings owned with a mortgage (%)"
  ) +
  theme_minimal()

#mapaus
library(sf)

LGA_map <- st_read("Non_ABS_Structures_2021.gpkg",
                   layer = "LGA_2021_AUST_GDA2020")

names(LGA_map)

data$LGA_2021_Code <- as.character(data$LGA_2021_Code)
LGA_map$LGA_CODE_2021 <- as.character(LGA_map$LGA_CODE_2021)

library(dplyr)

map_data <- LGA_map %>%
  left_join(
    data,
    by = c("LGA_CODE_2021" = "LGA_2021_Code")
  )

summary(map_data$Count_T_and_NT_spp)

sum(is.na(map_data$Count_T_and_NT_spp))

library(ggplot2)
library(viridis)

ggplot(map_data) +
  geom_sf(aes(fill = Count_T_and_NT_spp),
          colour = NA) +
  scale_fill_viridis_c(
    option = "magma",
    na.value = "grey90",
    name = "Species richness"
  ) +
  labs(
    title = "Species richness across Australian LGAs"
  ) +
  theme_void()

ggplot(map_data) +
  geom_sf(aes(fill = Owned_with_mortgage_.),
          colour = NA) +
  scale_fill_viridis_c(
    option = "plasma",
    na.value = "grey90",
    name = "Mortgage (%)"
  ) +
  labs(
    title = "Mortgage ownership across Australian LGAs"
  ) +
  theme_void()

sum(is.na(map_data$Count_T_and_NT_spp))
nrow(map_data)

#MAP WITH OVERLAP
species_threshold <- quantile(
  map_data$Count_T_and_NT_spp,
  0.75,
  na.rm = TRUE
)

mortgage_threshold <- median(
  map_data$Owned_with_mortgage_.,
  na.rm = TRUE
)

map_data$Category <- with(
  map_data,
  ifelse(
    Count_T_and_NT_spp >= species_threshold &
      Owned_with_mortgage_. >= mortgage_threshold,
    "High biodiversity\nHigh mortgage",
    ifelse(
      Count_T_and_NT_spp >= species_threshold &
        Owned_with_mortgage_. < mortgage_threshold,
      "High biodiversity\nLow mortgage",
      ifelse(
        Count_T_and_NT_spp < species_threshold &
          Owned_with_mortgage_. >= mortgage_threshold,
        "Low biodiversity\nHigh mortgage",
        "Low biodiversity\nLow mortgage"
      )
    )
  )
)

ggplot(map_data) +
  geom_sf(aes(fill = Category),
          colour = "white",
          linewidth = 0.05) +
  scale_fill_manual(values = c(
    "High biodiversity\nHigh mortgage" = "#542788",
    "High biodiversity\nLow mortgage" = "#1B7837",
    "Low biodiversity\nHigh mortgage" = "#D95F02",
    "Low biodiversity\nLow mortgage" = "grey90"
  )) +
  labs(
    title = "Australian LGAs by biodiversity value and mortgage ownership",
    fill = ""
  ) +
  theme_void()

#bivariate
install.packages(c("biscale", "cowplot"))
library(sf)
library(ggplot2)
library(dplyr)
library(biscale)
library(cowplot)

map_bi <- bi_class(
  map_data,
  x = Count_T_and_NT_spp,
  y = Owned_with_mortgage_.,
  style = "quantile",
  dim = 3
)

bi_map <- ggplot() +
  geom_sf(
    data = map_bi,
    aes(fill = bi_class),
    colour = NA
  ) +
  bi_scale_fill(
    pal = "DkBlue",
    dim = 3,
    na.value = "grey90"
  ) +
  theme_void() +
  labs(
    title = "Species richness and mortgage ownership across Australian LGAs",
    subtitle = "Bivariate choropleth map"
  )

legend <- bi_legend(
  pal = "DkBlue",
  dim = 3,
  xlab = "Higher species richness",
  ylab = "Higher mortgage ownership",
  size = 8
)

final_map <- ggdraw() +
  draw_plot(bi_map, 0, 0, 1, 1) +
  draw_plot(
    legend,
    0.70, 0.08,
    0.25, 0.25
  )

final_map

#bystate
unique(LGA_map$STATE_NAME_2021)
table(LGA_map$STATE_NAME_2021)

map_data <- LGA_map %>%
  left_join(
    data,
    by = c("LGA_CODE_2021" = "LGA_2021_Code")
  )

top_quartile_LGAs <- top_quartile_LGAs %>%
  left_join(
    LGA_map %>%
      st_drop_geometry() %>%
      select(
        LGA_CODE_2021,
        STATE_NAME_2021
      ),
    by = c("LGA_2021_Code" = "LGA_CODE_2021")
  )

table(top_quartile_LGAs$STATE_NAME_2021)

top_quartile_by_state <- top_quartile_LGAs %>%
  group_by(STATE_NAME_2021) %>%
  arrange(desc(Owned_with_mortgage), .by_group = TRUE)

top_quartile_by_state

#attempt2
library(sf)
library(dplyr)

data$LGA_2021_Code <- as.character(data$LGA_2021_Code)
LGA_map$LGA_CODE_2021 <- as.character(LGA_map$LGA_CODE_2021)

data <- data %>%
  left_join(
    LGA_map %>%
      st_drop_geometry() %>%
      select(
        LGA_CODE_2021,
        STATE_NAME_2021
      ),
    by = c("LGA_2021_Code" = "LGA_CODE_2021")
  )

table(data$STATE_NAME_2021, useNA = "ifany")

state_relationship <- data %>%
  filter(
    !is.na(STATE_NAME_2021),
    !is.na(Count_T_and_NT_spp),
    !is.na(Owned_with_mortgage_.)
  ) %>%
  group_by(STATE_NAME_2021) %>%
  summarise(
    n_LGAs = n(),
    
    correlation = cor(
      Count_T_and_NT_spp,
      Owned_with_mortgage_.,
      method = "pearson"
    ),
    
    .groups = "drop"
  ) %>%
  arrange(desc(abs(correlation)))

state_relationship

library(ggplot2)

ggplot(
  state_relationship,
  aes(
    x = reorder(STATE_NAME_2021, correlation),
    y = correlation
  )
) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  geom_hline(
    yintercept = 0,
    colour = "black"
  ) +
  labs(
    title = "Species richness–mortgage relationship by state",
    x = "State",
    y = "Pearson correlation"
  ) +
  theme_minimal()

#national
national_correlation <- cor(
  data$Count_T_and_NT_spp,
  data$Owned_with_mortgage_.,
  use = "complete.obs",
  method = "pearson"
)

national_correlation

national_model <- lm(
  Owned_with_mortgage_. ~ Count_T_and_NT_spp,
  data = data
)

summary(national_model)

#state summary
library(dplyr)

state_summary <- data %>%
  filter(
    !is.na(STATE_NAME_2021),
    !is.na(Count_T_and_NT_spp),
    !is.na(Owned_with_mortgage_.)
  ) %>%
  group_by(STATE_NAME_2021) %>%
  summarise(
    n_LGAs = n(),
    
    mean_species = mean(
      Count_T_and_NT_spp,
      na.rm = TRUE
    ),
    
    mean_mortgage = mean(
      Owned_with_mortgage_.,
      na.rm = TRUE
    ),
    
    correlation = cor(
      Count_T_and_NT_spp,
      Owned_with_mortgage_.,
      method = "pearson"
    ),
    
    .groups = "drop"
  ) %>%
  arrange(desc(n_LGAs))

state_summary

states <- unique(
  na.omit(data$STATE_NAME_2021)
)

leave_one_state_out <- lapply(states, function(s) {
  
  test_data <- data %>%
    filter(
      STATE_NAME_2021 != s,
      !is.na(Count_T_and_NT_spp),
      !is.na(Owned_with_mortgage_.)
    )
  
  model <- lm(
    Owned_with_mortgage_. ~ Count_T_and_NT_spp,
    data = test_data
  )
  
  data.frame(
    State_removed = s,
    LGAs_removed = sum(
      data$STATE_NAME_2021 == s &
        !is.na(data$Count_T_and_NT_spp) &
        !is.na(data$Owned_with_mortgage_.)
    ),
    Remaining_LGAs = nrow(test_data),
    Correlation_without_state = cor(
      test_data$Count_T_and_NT_spp,
      test_data$Owned_with_mortgage_.,
      method = "pearson"
    ),
    R_squared_without_state = summary(model)$r.squared,
    Slope_without_state = coef(model)[2]
  )
})

leave_one_state_out <- bind_rows(leave_one_state_out)

leave_one_state_out

#compare with national
leave_one_state_out <- leave_one_state_out %>%
  mutate(
    Change_in_correlation =
      Correlation_without_state - national_correlation,
    
    Absolute_change =
      abs(Change_in_correlation)
  ) %>%
  arrange(desc(Absolute_change))

leave_one_state_out

#graph
ggplot(
  leave_one_state_out,
  aes(
    x = reorder(State_removed, Correlation_without_state),
    y = Correlation_without_state
  )
) +
  geom_col(fill = "steelblue") +
  geom_hline(
    yintercept = national_correlation,
    colour = "red",
    linewidth = 1.2,
    linetype = "dashed"
  ) +
  coord_flip() +
  labs(
    title = "Influence of individual states on the national relationship",
    subtitle = "Dashed red line = national correlation using all LGAs",
    x = "State removed",
    y = "Species richness–mortgage correlation"
  ) +
  theme_minimal()

#Does species richness still predict mortgage percentage after accounting for systematic differences between states?
model_state <- lm(
  Owned_with_mortgage_. ~ Count_T_and_NT_spp + Size + STATE_NAME_2021,
  data = data
)

summary(model_state)

model_interaction <- lm(
  Owned_with_mortgage_. ~
    Count_T_and_NT_spp * STATE_NAME_2021 +
    Size,
  data = data
)

summary(model_interaction)

summary(model_state)$coefficients

anova(
  model_state,
  model_interaction
)

library(dplyr)

state_slopes <- data %>%
  filter(
    !is.na(STATE_NAME_2021),
    !is.na(Count_T_and_NT_spp),
    !is.na(Owned_with_mortgage_.),
    !is.na(Size)
  ) %>%
  group_by(STATE_NAME_2021) %>%
  group_modify(~ {
    
    # Number of usable LGAs
    n <- nrow(.x)
    
    # Don't fit a model if there aren't enough LGAs
    if (n < 5) {
      return(data.frame(
        n_LGAs = n,
        slope = NA_real_,
        p_value = NA_real_,
        r_squared = NA_real_
      ))
    }
    
    model <- lm(
      Owned_with_mortgage_. ~ Count_T_and_NT_spp + Size,
      data = .x
    )
    
    data.frame(
      n_LGAs = n,
      slope = coef(model)["Count_T_and_NT_spp"],
      p_value = summary(model)$coefficients[
        "Count_T_and_NT_spp", "Pr(>|t|)"
      ],
      r_squared = summary(model)$r.squared
    )
  }) %>%
  ungroup() %>%
  arrange(desc(slope))

state_slopes

ggplot(
  state_slopes %>%
    filter(!is.na(slope)),
  aes(
    x = reorder(STATE_NAME_2021, slope),
    y = slope
  )
) +
  geom_col(fill = "steelblue") +
  geom_hline(
    yintercept = 0,
    colour = "black",
    linewidth = 0.8
  ) +
  coord_flip() +
  labs(
    title = "Species richness–mortgage relationship by state",
    subtitle = "Slopes adjusted for LGA size",
    x = "State",
    y = "Species richness coefficient"
  ) +
  theme_minimal()

library(ggplot2)

ggplot(
  data %>%
    filter(
      STATE_NAME_2021 %in% c(
        "Western Australia",
        "Queensland",
        "Tasmania",
        "South Australia",
        "New South Wales",
        "Victoria",
        "Northern Territory"
      ),
      !is.na(Count_T_and_NT_spp),
      !is.na(Owned_with_mortgage_.),
      !is.na(Size)
    ),
  aes(
    x = Count_T_and_NT_spp,
    y = Owned_with_mortgage_.
  )
) +
  geom_point(
    alpha = 0.5,
    colour = "grey30"
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    colour = "darkgreen"
  ) +
  facet_wrap(~ STATE_NAME_2021, scales = "free") +
  labs(
    title = "Species richness and mortgage ownership by state",
    subtitle = "State-specific relationships with fitted regression lines",
    x = "Species richness",
    y = "mortgaged dwellings (%)"
  ) +
  theme_minimal()

#thresholds for figure:
mean_species_richness <- mean(
  data$Count_T_and_NT_spp,
  na.rm = TRUE
)

cat("Mean species richness:", 
    round(mean_species_richness, 2), 
    "species\n")

# Highest species richness
max_species <- max(data$Count_T_and_NT_spp, na.rm = TRUE)
# Lowest species richness
min_species <- min(data$Count_T_and_NT_spp, na.rm = TRUE)

cat("Highest species richness:", max_species, "species\n")
cat("Lowest species richness:", min_species, "species\n")

data$Mortgage_Percent <- as.numeric(
  gsub("[^0-9.]", "", data$Owned_with_mortgage_.)
)
max_mortgage <- max(data$Mortgage_Percent, na.rm = TRUE)
min_mortgage <- min(data$Mortgage_Percent, na.rm = TRUE)
cat("Highest mortgage percentage:", max_mortgage, "%\n")
cat("Lowest mortgage percentage:", min_mortgage, "%\n")

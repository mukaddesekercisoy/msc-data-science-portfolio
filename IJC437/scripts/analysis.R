# =========================================================
# 0) Setup / Reproducibility
# =========================================================
library(tidyverse)
library(lubridate)
library(zoo)
library(ggcorrplot)
library(openair)

library(caret)
library(randomForest)

set.seed(123)

# setwd("C:/Users/Mukaddes/Documents/raw_data_sets")  # optional
data_dir <- "./data_folder"

# =========================================================
# 1) DOWNLOAD + DATA INGESTION
# =========================================================

# 1.1 Weather read + combine (skip=3)
weather_ldn_2022 <- read.csv(file.path(data_dir, "weather_2022.csv"), skip = 3, header = TRUE)
weather_ldn_2023 <- read.csv(file.path(data_dir, "weather_2023.csv"), skip = 3, header = TRUE)
weather_ldn_2024 <- read.csv(file.path(data_dir, "weather_2024.csv"), skip = 3, header = TRUE)

weather_all <- rbind(weather_ldn_2022, weather_ldn_2023, weather_ldn_2024)
View(weather_all)

# Parse weather time (original format)
weather_all$time <- as.POSIXct(
  weather_all$time,
  format = "%Y-%m-%dT%H:%M",
  tz = "UTC"
)

weather_all <- weather_all %>% arrange(time)

# Weather time integrity check (optional)
expected_time <- seq(from = min(weather_all$time), to = max(weather_all$time), by = "hour")
missing_time <- setdiff(expected_time, weather_all$time)
length(missing_time)

# 1.2 Pollution read (skip=10)
london_nken_pm25  <- read.csv(file.path(data_dir, "LondonNKensington_PM25.csv"), skip = 10, header = TRUE)
london_nken_no2   <- read.csv(file.path(data_dir, "LondonNKensington_NO2.csv"),  skip = 10, header = TRUE)

london_marly_pm25 <- read.csv(file.path(data_dir, "LondonMaryleboneRoad_PM25.csv"), skip = 10, header = TRUE)
london_marly_no2  <- read.csv(file.path(data_dir, "LondonMaryleboneRoad_NO2.csv"),  skip = 10, header = TRUE)

# 1.3 Parse Date+Time -> time and clean columns

# Marylebone NO2
london_marly_no2$time <- as.POSIXct(
  paste(london_marly_no2$Date, london_marly_no2$Time),
  format = "%Y-%m-%d %H:%M:%S",
  tz = "UTC"
)
london_marly_no2_clean <- london_marly_no2 %>%
  select(-Date, -Time, -Status) %>%
  rename(NO2_marly = Nitrogen.dioxide)

# Marylebone PM2.5
london_marly_pm25$time <- as.POSIXct(
  paste(london_marly_pm25$Date, london_marly_pm25$Time),
  format = "%Y-%m-%d %H:%M:%S",
  tz = "UTC"
)
london_marly_pm25_clean <- london_marly_pm25 %>%
  select(-Date, -Time, -Status) %>%
  rename(pm25_marly = PM2.5.particulate.matter..Hourly.measured.)

# NK NO2
london_nken_no2$time <- as.POSIXct(
  paste(london_nken_no2$Date, london_nken_no2$Time),
  format = "%Y-%m-%d %H:%M:%S",
  tz = "UTC"
)
london_nken_no2_clean <- london_nken_no2 %>%
  select(-Date, -Time, -Status) %>%
  rename(NO2_nken = Nitrogen.dioxide)

# NK PM2.5
london_nken_pm25$time <- as.POSIXct(
  paste(london_nken_pm25$Date, london_nken_pm25$Time),
  format = "%Y-%m-%d %H:%M:%S",
  tz = "UTC"
)
london_nken_pm25_clean <- london_nken_pm25 %>%
  select(-Date, -Time, -Status) %>%
  rename(pm25_nken = PM2.5.particulate.matter..Hourly.measured.)

# 1.4 Merge pollution streams
pollution_all <- london_marly_no2_clean %>%
  full_join(london_marly_pm25_clean, by = "time") %>%
  full_join(london_nken_no2_clean,  by = "time") %>%
  full_join(london_nken_pm25_clean, by = "time") %>%
  relocate(time) %>%
  mutate(
    NO2_marly  = as.numeric(NO2_marly),
    pm25_marly = as.numeric(pm25_marly),
    NO2_nken   = as.numeric(NO2_nken),
    pm25_nken  = as.numeric(pm25_nken)
  )
summary(pollution_all)

# Duplicate time check (optional)
pollution_all %>% count(time) %>% filter(n > 1)


# =========================================================
# 2) PRE-PROCESS / STRUCTURE
# =========================================================

# 2.2 Weather as reference: left join
weather_pollution <- left_join(weather_all, pollution_all, by = "time")

# 2.3 Rename weather variables
weather_pollution <- weather_pollution %>%
  rename(
    temp_c         = temperature_2m...C.,
    relt_hum       = relative_humidity_2m....,
    precipitation  = precipitation..mm.,
    wind_speed     = wind_speed_10m..km.h.,
    wind_direction = wind_direction_10m....
  )

weather_pollution_clean <- weather_pollution

# 2.4 Interpolation (maxgap=2)
weather_pollution_clean <- weather_pollution %>%
  arrange(time) %>%
  mutate(
    NO2_marly  = na.approx(NO2_marly,  maxgap = 2, na.rm = FALSE),
    pm25_marly = na.approx(pm25_marly, maxgap = 2, na.rm = FALSE),
    NO2_nken   = na.approx(NO2_nken,   maxgap = 2, na.rm = FALSE),
    pm25_nken  = na.approx(pm25_nken,  maxgap = 2, na.rm = FALSE)
  )

# 2.5 Time features (EDA + RQ1 + RQ3)
weather_pollution_clean <- weather_pollution_clean %>%
  rename(date = time) %>%                    # time -> date
  mutate(date = as.POSIXct(date, tz = "Europe/London"))

weather_pollution_clean$hour    <- hour(weather_pollution_clean$date)
weather_pollution_clean$weekday <- wday(weather_pollution_clean$date, week_start = 1)
weather_pollution_clean$month   <- month(weather_pollution_clean$date)

# Season label
weather_pollution_clean$season <- factor(
  ifelse(weather_pollution_clean$month %in% c(12,1,2), "Winter",
         ifelse(weather_pollution_clean$month %in% c(3,4,5), "Spring",
                ifelse(weather_pollution_clean$month %in% c(6,7,8), "Summer", "Autumn"))),
  levels = c("Winter","Spring","Summer","Autumn")
)
head(weather_pollution_clean)


# =========================================================
# 3) EXPORT (optional)
# =========================================================
write.csv(
  head(weather_pollution_clean, 10),
  file.path(data_dir, "sample_head10.csv"),
  row.names = FALSE
)


# =========================================================
# 4) EDA (Exploratory Data Analysis) ✅
# =========================================================

# EDA for weather_pollution_clean

# Libraries
library(tidyverse)
library(lubridate)
library(ggcorrplot)

# Quick checks
glimpse(weather_pollution_clean)
summary(weather_pollution_clean)

nrow(weather_pollution_clean)
ncol(weather_pollution_clean)
names(weather_pollution_clean)

range(weather_pollution_clean$date, na.rm = TRUE)

# Missing values
na_before <- tibble(
  variable = names(weather_pollution),
  na_n_before = colSums(is.na(weather_pollution)),
  na_pct_before = round(100 * colSums(is.na(weather_pollution)) / nrow(weather_pollution), 2)
)

na_after <- tibble(
  variable = names(weather_pollution_clean),
  na_n_after = colSums(is.na(weather_pollution_clean)),
  na_pct_after = round(100 * colSums(is.na(weather_pollution_clean)) / nrow(weather_pollution_clean), 2)
)

na_compare <- na_before %>%
  left_join(na_after, by = "variable") %>%
  arrange(desc(na_n_before))

na_compare


# Summary stats (numeric)
num_vars <- weather_pollution_clean %>%
  select(where(is.numeric))

num_summary <- num_vars %>%
  summarise(across(
    everything(),
    list(
      mean = ~mean(.x, na.rm = TRUE),
      sd   = ~sd(.x, na.rm = TRUE),
      min  = ~min(.x, na.rm = TRUE),
      q1   = ~quantile(.x, 0.25, na.rm = TRUE),
      med  = ~median(.x, na.rm = TRUE),
      q3   = ~quantile(.x, 0.75, na.rm = TRUE),
      max  = ~max(.x, na.rm = TRUE)
    ),
    .names = "{.col}_{.fn}"
  ))

num_summary


# Distributions (histograms)
library(ggplot2)

# NO2 - Marley
ggplot(weather_pollution_clean, aes(x = NO2_marly)) +
  geom_histogram(bins = 40) +
  labs(
    title = "NO2 (Marley)",
    x = "NO2 concentration (µg/m³)",
    y = "Count"
  ) +
  theme_minimal()

# NO2 - Nken
ggplot(weather_pollution_clean, aes(x = NO2_nken)) +
  geom_histogram(bins = 40) +
  labs(
    title = "NO2 (Nken)",
    x = "NO2 concentration (µg/m³)",
    y = "Count"
  ) +
  theme_minimal()

# PM2.5 - Marley
ggplot(weather_pollution_clean, aes(x = pm25_marly)) +
  geom_histogram(bins = 40) +
  labs(
    title = "PM2.5 (Marley)",
    x = "PM2.5 concentration (µg/m³)",
    y = "Count"
  ) +
  theme_minimal()

# PM2.5 - Nken
ggplot(weather_pollution_clean, aes(x = pm25_nken)) +
  geom_histogram(bins = 40) +
  labs(
    title = "PM2.5 (Nken)",
    x = "PM2.5 concentration (µg/m³)",
    y = "Count"
  ) +
  theme_minimal()


# Outliers (boxplots)
ggplot(num_long, aes(x = var, y = value)) +
  geom_boxplot(outlier.alpha = 0.4) +
  coord_flip() +
  labs(title = "EDA: Boxplots (Outlier Scan)",
       x = NULL, y = NULL) +
  theme_minimal()


# Daily mean (quick look)
daily_mean <- weather_pollution_clean %>%
  mutate(date_day = as.Date(date)) %>%
  group_by(date_day) %>%
  summarise(
    NO2_marly  = mean(NO2_marly, na.rm = TRUE),
    NO2_nken   = mean(NO2_nken,  na.rm = TRUE),
    pm25_marly = mean(pm25_marly, na.rm = TRUE),
    pm25_nken  = mean(pm25_nken,  na.rm = TRUE),
    temp_c     = mean(temp_c, na.rm = TRUE),
    wind_speed = mean(wind_speed, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(daily_mean, aes(x = date_day)) +
  geom_line(aes(y = NO2_marly, color = "NO2 Marylebone")) +
  geom_line(aes(y = NO2_nken,  color = "NO2 North Kensington")) +
  labs(title = "EDA: Daily Mean NO2 Over Time",
       x = "Date", y = "NO2 (µg/m³)", color = NULL) +
  theme_minimal()

ggplot(daily_mean, aes(x = date_day)) +
  geom_line(aes(y = pm25_marly, color = "PM2.5 Marylebone")) +
  geom_line(aes(y = pm25_nken,  color = "PM2.5 North Kensington")) +
  labs(title = "EDA: Daily Mean PM2.5 Over Time",
       x = "Date", y = "PM2.5 (µg/m³)", color = NULL) +
  theme_minimal()


# Diurnal patterns (hourly mean)
hourly_mean <- weather_pollution_clean %>%
  group_by(hour) %>%
  summarise(
    NO2_marly  = mean(NO2_marly, na.rm = TRUE),
    NO2_nken   = mean(NO2_nken,  na.rm = TRUE),
    pm25_marly = mean(pm25_marly, na.rm = TRUE),
    pm25_nken  = mean(pm25_nken,  na.rm = TRUE),
    .groups = "drop"
  )

ggplot(hourly_mean, aes(x = hour)) +
  geom_line(aes(y = NO2_marly, color = "Marylebone"), linewidth = 1.1) +
  geom_line(aes(y = NO2_nken,  color = "North Kensington"), linewidth = 1.1) +
  scale_x_continuous(breaks = 0:23) +
  scale_color_manual(values = c("Marylebone" = "red", "North Kensington" = "navy")) +
  labs(title = "EDA: Diurnal Pattern (NO2)",
       x = "Hour (0–23)", y = "Mean NO2 (µg/m³)", color = "Station") +
  theme_minimal()

ggplot(hourly_mean, aes(x = hour)) +
  geom_line(aes(y = pm25_marly, color = "Marylebone"), linewidth = 1.1) +
  geom_line(aes(y = pm25_nken,  color = "North Kensington"), linewidth = 1.1) +
  scale_x_continuous(breaks = 0:23) +
  scale_color_manual(values = c("Marylebone" = "red", "North Kensington" = "navy")) +
  labs(title = "EDA: Diurnal Pattern (PM2.5)",
       x = "Hour (0–23)", y = "Mean PM2.5 (µg/m³)", color = "Station") +
  theme_minimal()


# Seasonal variation (boxplots)
season_long <- weather_pollution_clean %>%
  select(season, NO2_marly, NO2_nken, pm25_marly, pm25_nken) %>%
  pivot_longer(-season, names_to = "pollutant", values_to = "value")

ggplot(season_long, aes(x = season, y = value)) +
  geom_boxplot(outlier.alpha = 0.3) +
  facet_wrap(~pollutant, scales = "free_y") +
  labs(title = "EDA: Seasonal Variation (Boxplots)",
       x = "Season", y = NULL) +
  theme_minimal()


# Weekday effect (simple)
weekday_mean <- weather_pollution_clean %>%
  group_by(weekday) %>%
  summarise(
    NO2_marly  = mean(NO2_marly, na.rm = TRUE),
    NO2_nken   = mean(NO2_nken,  na.rm = TRUE),
    pm25_marly = mean(pm25_marly, na.rm = TRUE),
    pm25_nken  = mean(pm25_nken,  na.rm = TRUE),
    .groups = "drop"
  )

ggplot(weekday_mean, aes(x = factor(weekday))) +
  geom_col(aes(y = NO2_marly), width = 0.6) +
  labs(title = "EDA: Mean NO2 by Weekday (Marylebone)",
       x = "Weekday (0=Sun ... 6=Sat?)", y = "Mean NO2 (µg/m³)") +
  theme_minimal()


# Correlation matrix (weather + pollution)
corr_df <- weather_pollution_clean %>%
  select(temp_c, relt_hum, precipitation, wind_speed, wind_direction,
         NO2_marly, NO2_nken, pm25_marly, pm25_nken)

corr_mat <- cor(corr_df, use = "complete.obs")

ggcorrplot(
  corr_mat,
  hc.order = TRUE,
  type = "lower",
  lab = TRUE,
  lab_size = 3,
  title = "EDA: Correlation Matrix (Weather & Pollution)"
)


# Rain vs No Rain (PM2.5)
rain_impact <- weather_pollution_clean %>%
  mutate(rain_status = ifelse(precipitation > 0, "Rain", "No Rain"),
         rain_status = factor(rain_status, levels = c("No Rain","Rain"))) %>%
  summarise(
    maryl_mean_pm25 = mean(pm25_marly, na.rm = TRUE),
    nken_mean_pm25  = mean(pm25_nken,  na.rm = TRUE)
  )

rain_impact

# Rain vs PM2.5 by season (optional)
rain_season <- weather_pollution_clean %>%
  mutate(rain_status = ifelse(precipitation > 0, "Rain", "No Rain"),
         rain_status = factor(rain_status, levels = c("No Rain","Rain"))) %>%
  group_by(season, rain_status) %>%
  summarise(
    mean_pm25_nken = mean(pm25_nken, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(rain_season, aes(x = season, y = mean_pm25_nken, fill = rain_status)) +
  geom_col(position = "dodge") +
  labs(title = "EDA: Rain vs PM2.5 by Season (North Kensington)",
       x = "Season", y = "Mean PM2.5 (µg/m³)", fill = "Condition") +
  theme_minimal()


# =========================================================
# RQ1) Diurnal differences (Traffic vs Background) ✅
# =========================================================

# RQ1.A Diurnal NO2
no2_hourly <- weather_pollution_clean %>%
  group_by(hour) %>%
  summarise(
    mean_no2_marly = mean(NO2_marly, na.rm = TRUE),
    mean_no2_nken  = mean(NO2_nken,  na.rm = TRUE),
    .groups = "drop"
  )

p_rq1_no2 <- ggplot(no2_hourly, aes(x = hour)) +
  geom_line(aes(y = mean_no2_marly)) +
  geom_line(aes(y = mean_no2_nken)) +
  labs(
    title = "RQ1: Diurnal NO2 (Marylebone vs North Kensington)",
    x = "Hour",
    y = "Mean NO2"
  ) +
  theme_minimal()


# Peak traffic hours (same as old code)
peak_hours <- c(7, 9, 16, 19)

p_rq1_no2 <- ggplot(no2_hourly, aes(x = hour)) +
  # Lines
  geom_line(
    aes(y = mean_no2_marly, color = "Marylebone"),
    linewidth = 1.2
  ) +
  geom_line(
    aes(y = mean_no2_nken, color = "North Kensington"),
    linewidth = 1.2
  ) +
  # Points
  geom_point(
    aes(y = mean_no2_marly, color = "Marylebone")
  ) +
  geom_point(
    aes(y = mean_no2_nken, color = "North Kensington")
  ) +
  # Peak hour reference lines
  geom_vline(
    xintercept = peak_hours,
    linetype = "dashed",
    color = "grey40",
    linewidth = 0.8
  ) +
  # Labels
  labs(
    title = expression("Hourly Average NO"[2]*" Concentration: Traffic vs. Background Stations in London"),
    x = "Hour of the Day (0–23)",
    y = expression("Average NO"[2]*" ("*mu*"g/m"^3*")"),
    color = "Station"
  ) +
  # Axis formatting
  scale_x_continuous(breaks = 0:23) +
  # Manual colors
  scale_color_manual(
    values = c(
      "Marylebone" = "red",
      "North Kensington" = "navy"
    )
  ) +
  theme_minimal()

p_rq1_no2


# RQ1.B Diurnal PM2.5
pm25_hourly <- weather_pollution_clean %>%
  group_by(hour) %>%
  summarise(
    mean_pm25_marly = mean(pm25_marly, na.rm = TRUE),
    mean_pm25_nken  = mean(pm25_nken,  na.rm = TRUE),
    .groups = "drop"
  )

p_rq1_pm25 <- ggplot(pm25_hourly, aes(x = hour)) +
  geom_line(aes(y = mean_pm25_marly)) +
  geom_line(aes(y = mean_pm25_nken)) +
  labs(
    title = "RQ1: Diurnal PM2.5 (Marylebone vs North Kensington)",
    x = "Hour",
    y = "Mean PM2.5"
  ) +
  theme_minimal()

p_rq1_pm25 <- ggplot(pm25_hourly, aes(x = hour)) +
  # Lines
  geom_line(
    aes(y = mean_pm25_marly, color = "Marylebone"),
    linewidth = 1.2
  ) +
  geom_line(
    aes(y = mean_pm25_nken, color = "North Kensington"),
    linewidth = 1.2
  ) +
  # Labels
  labs(
    title = "Hourly Average PM2.5 Concentration (Marylebone vs North Kensington)",
    x = "Hour of the Day (0–23)",
    y = "Mean PM2.5 (µg/m³)",
    color = "Station"
  ) +
  # Axis formatting
  scale_x_continuous(breaks = 0:23) +
  # Manual colors
  scale_color_manual(
    values = c(
      "Marylebone" = "red",
      "North Kensington" = "navy"
    )
  ) +
  theme_minimal()

p_rq1_pm25


# =========================================================
# RQ2) Wind dispersion differs by site ✅
# =========================================================
library(openair)

polarPlot(
  weather_pollution_clean,
  pollutant = "NO2_marly",
  x  = "wind_speed",
  wd = "wind_direction",
  main = "Marylebone Road (Traffic) — NO2",
  breaks = seq(0, 50, by = 10),
  wdlabels = 45,
  limits = c(0, 80),
  key = list(
    header = "Mean NO2 (µg/m³)",
    footer = "",
    space = "right"
  ),
  cols = "default",
  linewidth = 1
)

polarPlot(
  weather_pollution_clean,
  pollutant = "NO2_nken",
  x  = "wind_speed",
  wd = "wind_direction",
  main = "North Kensington (Background) — NO2",
  breaks = seq(0, 50, by = 10),
  wdlabels = 45,
  limits = c(0, 80),
  key = list(
    header = "Mean NO2 (µg/m³)",
    footer = "",
    space = "right"
  ),
  cols = "default",
  linewidth = 1
)


# =========================================================
# RQ3) Predict risk events using meteo + time features ✅
# =========================================================
df <- weather_pollution_clean %>%
  filter(!is.na(NO2_marly), !is.na(NO2_nken))

df$NO2_increment <- df$NO2_marly - df$NO2_nken

thr_y <- quantile(df$NO2_increment, 0.85, na.rm = TRUE)
df$risk_grubu <- factor(
  ifelse(df$NO2_increment >= thr_y, "High", "Normal"),
  levels = c("Normal","High")
)

# Cyclic encodings
df$wind_sin  <- sin(df$wind_direction * pi / 180)
df$wind_cos  <- cos(df$wind_direction * pi / 180)
df$hour_sin  <- sin(2 * pi * df$hour / 24)
df$hour_cos  <- cos(2 * pi * df$hour / 24)
df$month_sin <- sin(2 * pi * df$month / 12)
df$month_cos <- cos(2 * pi * df$month / 12)

df$is_weekend <- factor(ifelse(df$weekday %in% c(6,7), "Weekend", "Weekday"))

# Ensure season factor levels
df$season <- factor(df$season, levels = c("Winter","Spring","Summer","Autumn"))

# Model data (no leakage: NO2/PM25 not used as predictors)
model_verisi <- df[, c(
  "temp_c",
  "relt_hum",
  "precipitation",
  "wind_speed",
  "wind_sin",
  "wind_cos",
  "hour_sin",
  "hour_cos",
  "month_sin",
  "month_cos",
  "is_weekend",
  "season",
  "risk_grubu"
)]

# Train/test split
train_index <- createDataPartition(model_verisi$risk_grubu, p = 0.8, list = FALSE)
train_set <- model_verisi[train_index, ]
test_set  <- model_verisi[-train_index, ]

# Logistic regression
ctrl_glm <- trainControl(
  method = "cv", number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)

glm_model <- train(
  risk_grubu ~ .,
  data = train_set,
  method = "glm",
  family = binomial,
  trControl = ctrl_glm,
  metric = "ROC"
)

pred_prob_glm <- predict(glm_model, newdata = test_set, type = "prob")[, "High"]
threshold <- 0.30

pred_class_glm <- factor(ifelse(pred_prob_glm >= threshold, "High", "Normal"),
                         levels = c("Normal","High"))

cm_glm <- confusionMatrix(pred_class_glm, test_set$risk_grubu, positive = "High")
print(cm_glm)

# Random Forest
ctrl_rf <- trainControl(
  method = "cv", number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)

grid_rf <- expand.grid(mtry = c(2,4,6))

rf_model <- train(
  risk_grubu ~ .,
  data = train_set,
  method = "rf",
  trControl = ctrl_rf,
  tuneGrid = grid_rf,
  metric = "ROC",
  ntree = 200
)

pred_prob_rf <- predict(rf_model, newdata = test_set, type = "prob")[, "High"]

pred_class_rf <- factor(ifelse(pred_prob_rf >= threshold, "High", "Normal"),
                        levels = c("Normal","High"))

cm_rf <- confusionMatrix(pred_class_rf, test_set$risk_grubu, positive = "High")
print(cm_rf)

# Feature importance (RF)
rf_imp <- varImp(rf_model, scale = TRUE)
print(rf_imp)
plot(rf_imp, top = 10)


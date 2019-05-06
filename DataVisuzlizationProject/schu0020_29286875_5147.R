# install.packages("")
library(tidyverse) # general
library(ggalt) # dumbbell plots
library(countrycode) # continent
library(rworldmap) # quick country-level heat maps
library(gridExtra) # plots
library(broom) # significant trends within countries

theme_set(theme_light())

##################################################################
## 1 Introduction 

# 1) IMPORT & DATA CLEANING
data <- read_csv("/Users/eileen/Desktop/u/5147DataVisualization/Assignments/Final Project/master.csv")

# 在清理資料的過程中給我對照用的
original_data <- read_csv("/Users/eileen/Desktop/u/5147DataVisualization/Assignments/Final Project/master.csv")


# glimpse(data) # will tidy up these variable names

# sum(is.na(data$`HDI for year`)) # remove, > 2/3 missing, not useable

# table(data$age, data$generation) # don't like this variable

data <- data %>% 
  select(-c('HDI for year', 'suicides/100k pop')) %>%
  rename(gdp_for_year = 'gdp_for_year ($)', 
         gdp_per_capita = 'gdp_per_capita ($)', 
         country_year = 'country-year') %>%
  as.data.frame()


# 2) OTHER ISSUES

# a) this SHOULD give 12 rows for every county-year combination (6 age bands * 2 genders):

data %>% 
group_by(country_year) %>%
count() %>%
filter(n != 12)  # 在2016年中，應該要有12 rows (6種不同年齡層＊2種性別)
# note: there appears to be an issue with 2016 data
# not only are there few countries with data, but those that do have data are incomplete

data <- data %>%
  filter(year != 2016) %>% # I therefore exclude 2016 data
  select(-country_year)


# b) excluding countries with <= 3 years of data:

minimum_years <- data %>%
  group_by(country) %>%
  summarize(rows = n(), 
            years = rows / 12) %>%
  arrange(years)

data <- data %>%
  filter(!(country %in% head(minimum_years$country, 7)))


# no other major data issues found yet


# 3) TIDYING DATAFRAME
data$age <- gsub(pattern = " years", replacement = "", data$age) # 把原本年齡後面的“空格years“移除
data$sex <- ifelse(data$sex == "male", "Male", "Female") # 把小寫都換成大寫


# getting continent data:
data$continent <- countrycode(sourcevar = data[, "country"],
                              origin = "country.name",
                              destination = "continent")


# Nominal factors
data_nominal <- c('country', 'sex', 'continent')
data[data_nominal] <- lapply(data[data_nominal], function(x){factor(x)})


# Making age ordinal
data$age <- factor(data$age, 
                   ordered = T, 
                   levels = c("5-14",
                              "15-24", 
                              "25-34", 
                              "35-54", 
                              "55-74", 
                              "75+"))


# Making generation ordinal
data$generation <- factor(data$generation, 
                          ordered = T, 
                          levels = c("G.I. Generation", 
                                     "Silent",
                                     "Boomers", 
                                     "Generation X", 
                                     "Millenials", 
                                     "Generation Z"))

# as_tibble is a new S3 generic with more efficient methods for matrices and data frames
data <- as_tibble(data)


# the global rate over the time period will be useful:

global_average <- (sum(as.numeric(data$suicides_no)) / sum(as.numeric(data$population))) * 100000

# view the finalized data
glimpse(data)





##################################################################
## 2 Global Analysis

###############
###############
## 2.1 Global Trend ##

data %>%
  group_by(year) %>%
  summarize(population = sum(population), 
            suicides = sum(suicides_no), 
            suicides_per_100k = (suicides / population) * 100000) %>%
  ggplot(aes(x = year, y = suicides_per_100k)) + 
  geom_line(col = "deepskyblue3", size = 1) + 
  geom_point(col = "deepskyblue3", size = 2) + 
  geom_hline(yintercept = global_average, linetype = 2, color = "grey35", size = 1) +
  labs(title = "Global Suicides (per 100k)",
       subtitle = "Trend over time, 1985 - 2015.",
       x = "Year", 
       y = "Suicides per 100k") + 
  scale_x_continuous(breaks = seq(1985, 2015, 2)) + 
  scale_y_continuous(breaks = seq(10, 20))


###############
###############
## 2.2 By Continent ##
continent <- data %>%
  group_by(continent) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000) %>%
  arrange(suicide_per_100k)

continent$continent <- factor(continent$continent, ordered = T, levels = continent$continent)

continent_plot <- ggplot(continent, aes(x = continent, y = suicide_per_100k, fill = continent)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Global Suicides (per 100k), by Continent",
       x = "Continent", 
       y = "Suicides per 100k", 
       fill = "Continent") +
  theme(legend.position = "none", title = element_text(size = 10)) + 
  scale_y_continuous(breaks = seq(0, 20, 1), minor_breaks = F)

continent_time <- data %>%
  group_by(year, continent) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000)

continent_time$continent <- factor(continent_time$continent, ordered = T, levels = continent$continent)

continent_time_plot <- ggplot(continent_time, aes(x = year, y = suicide_per_100k, col = factor(continent))) + 
  facet_grid(continent ~ ., scales = "free_y") + 
  geom_line() + 
  geom_point() + 
  labs(title = "Trends Over Time, by Continent", 
       x = "Year", 
       y = "Suicides per 100k", 
       color = "Continent") + 
  theme(legend.position = "none", title = element_text(size = 10)) + 
  scale_x_continuous(breaks = seq(1985, 2015, 5), minor_breaks = F)

grid.arrange(continent_plot, continent_time_plot, ncol = 2)


###############
###############
## 2.3 By Sex ##
sex_plot <- data %>%
  group_by(sex) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000) %>%
  ggplot(aes(x = sex, y = suicide_per_100k, fill = sex)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Global suicides (per 100k), by Sex",
       x = "Sex", 
       y = "Suicides per 100k") +
  theme(legend.position = "none") + 
  scale_y_continuous(breaks = seq(0, 25), minor_breaks = F)

### with time
sex_time_plot <- data %>%
  group_by(year, sex) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000) %>%
  ggplot(aes(x = year, y = suicide_per_100k, col = factor(sex))) + 
  facet_grid(sex ~ ., scales = "free_y") + 
  geom_line() + 
  geom_point() + 
  labs(title = "Trends Over Time, by Sex", 
       x = "Year", 
       y = "Suicides per 100k", 
       color = "Sex") + 
  theme(legend.position = "none") + 
  scale_x_continuous(breaks = seq(1985, 2015, 5), minor_breaks = F)

grid.arrange(sex_plot, sex_time_plot, ncol = 2)


###############
###############
## 2.4 By Age ##
age_plot <- data %>%
  group_by(age) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000) %>%
  ggplot(aes(x = age, y = suicide_per_100k, fill = age)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Global suicides per 100k, by Age",
       x = "Age", 
       y = "Suicides per 100k") +
  theme(legend.position = "none") + 
  scale_y_continuous(breaks = seq(0, 30, 1), minor_breaks = F)

### with time
age_time_plot <- data %>%
  group_by(year, age) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000) %>%
  ggplot(aes(x = year, y = suicide_per_100k, col = age)) + 
  facet_grid(age ~ ., scales = "free_y") + 
  geom_line() + 
  geom_point() + 
  labs(title = "Trends Over Time, by Age", 
       x = "Year", 
       y = "Suicides per 100k", 
       color = "Age") + 
  theme(legend.position = "none") + 
  scale_x_continuous(breaks = seq(1985, 2015, 5), minor_breaks = F)


grid.arrange(age_plot, age_time_plot, ncol = 2)


###############
###############
## 2.5 By Country ##
# 2.5.1 Overall
country <- data %>%
  group_by(country, continent) %>%
  summarize(n = n(), 
            suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000) %>%
  arrange(desc(suicide_per_100k))

country$country <- factor(country$country, 
                          ordered = T, 
                          levels = rev(country$country))

ggplot(country, aes(x = country, y = suicide_per_100k, fill = continent)) + 
  geom_bar(stat = "identity") + 
  geom_hline(yintercept = global_average, linetype = 2, color = "grey35", size = 1) +
  labs(title = "Global suicides per 100k, by Country",
       x = "Country", 
       y = "Suicides per 100k", 
       fill = "Continent") +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 45, 2)) + 
  theme(legend.position = "bottom")

# Below is a geographical heat map of the suicide rates between the timeframe of this analysis
country <- data %>%
  group_by(country) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000)

countrydata <- joinCountryData2Map(country, joinCode = "NAME", nameJoinColumn = "country")

par(mar=c(0, 0, 0, 0)) # margins

mapCountryData(countrydata, 
               nameColumnToPlot="suicide_per_100k", 
               mapTitle="", 
               colourPalette = "heat", 
               oceanCol="lightblue", 
               missingCountryCol="grey65", 
               catMethod = "pretty")

# Europe + Asia
mapCountryData(countrydata, 
               nameColumnToPlot="suicide_per_100k", 
               mapTitle="", 
               mapRegion = "eurasia", 
               colourPalette = "heat", 
               oceanCol="lightblue", 
               missingCountryCol="grey65", 
               addLegend = FALSE, 
               catMethod = "pretty")


# 2.5.2 Linear Trends
country_year <- data %>%
  group_by(country, year) %>%
  summarize(suicides = sum(suicides_no), 
            population = sum(population), 
            suicide_per_100k = (suicides / population) * 100000, 
            gdp_per_capita = mean(gdp_per_capita))


country_year_trends <- country_year %>%
  ungroup() %>%
  nest(-country) %>% # format: country, rest of data (in list column)
  mutate(model = map(data, ~ lm(suicide_per_100k ~ year, data = .)), # for each item in 'data', fit a linear model
         tidied = map(model, tidy)) %>% # tidy each of these into dataframe format - call this list 'tidied'
  unnest(tidied)

country_year_sig_trends <- country_year_trends %>%
  filter(term == "year") %>%
  mutate(p.adjusted = p.adjust(p.value, method = "holm")) %>%
  filter(p.adjusted < .05) %>%
  arrange(estimate)

country_year_sig_trends$country <- factor(country_year_sig_trends$country, 
                                          ordered = T, 
                                          levels = country_year_sig_trends$country)

# plot 1
ggplot(country_year_sig_trends, aes(x=country, y=estimate, col = estimate)) + 
  geom_point(stat='identity', size = 4) +
  geom_hline(yintercept = 0, col = "grey", size = 1) +
  scale_color_gradient(low = "green", high = "red") +
  geom_segment(aes(y = 0, 
                   x = country, 
                   yend = estimate, 
                   xend = country), size = 1) +
  labs(title="Change per year (Suicides per 100k)", 
       subtitle="Of countries with significant trends (p < 0.05)", 
       x = "Country", y = "Change Per Year (Suicides per 100k)") +
  scale_y_continuous(breaks = seq(-2, 2, 0.2), limits = c(-1.5, 1.5)) +
  theme(legend.position = "none") +
  coord_flip()


### Lets look at those countries with the steepest increasing trends

top12_increasing <- tail(country_year_sig_trends$country, 12)

country_year %>%
  filter(country %in% top12_increasing) %>%
  ggplot(aes(x = year, y = suicide_per_100k, col = country)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  facet_wrap(~ country) + 
  theme(legend.position = "none") + 
  labs(title="12 Steepest Increasing Trends", 
       subtitle="Of countries with significant trends (p < 0.05)", 
       x = "Year", 
       y = "Suicides per 100k")


### Now those with the steepest decreasing trend

top12_decreasing <- head(country_year_sig_trends$country, 12)

country_year %>%
  filter(country %in% top12_decreasing) %>%
  ggplot(aes(x = year, y = suicide_per_100k, col = country)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  facet_wrap(~ country) + 
  theme(legend.position = "none") + 
  labs(title="12 Steepest Decreasing Trends", 
       subtitle="Of countries with significant trends (p < 0.05)", 
       x = "Year", 
       y = "Suicides per 100k")


###############
###############
## 2.6 Gender differences, by Continent ##
data %>%
  group_by(continent, sex) %>%
  summarize(n = n(), 
            suicides = sum(as.numeric(suicides_no)), 
            population = sum(as.numeric(population)), 
            suicide_per_100k = (suicides / population) * 100000) %>%
  ggplot(aes(x = continent, y = suicide_per_100k, fill = sex)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_hline(yintercept = global_average, linetype = 2, color = "grey35", size = 1) +
  labs(title = "Gender Disparity, by Continent",
       x = "Continent", 
       y = "Suicides per 100k", 
       fill = "Sex") +
  coord_flip()


###############
###############
## 2.7 Gender differences, by Country ##
country_long <- data %>%
  group_by(country, continent) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000) %>%
  mutate(sex = "OVERALL")

### by country, continent, sex

sex_country_long <- data %>%
  group_by(country, continent, sex) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000)


sex_country_wide <- sex_country_long %>%
  spread(sex, suicide_per_100k) %>%
  arrange(Male - Female)


sex_country_wide$country <- factor(sex_country_wide$country, 
                                   ordered = T, 
                                   levels = sex_country_wide$country)

sex_country_long$country <- factor(sex_country_long$country, 
                                   ordered = T, 
                                   levels = sex_country_wide$country) # using the same order

### this graph shows us how the disparity between deaths varies across gender for every country
# it also has the overall blended death rate - generally countries with a higher death rate have a higher disparity
# this is because, if suicide is more likely in a country, the disparity between men and women is amplified

ggplot(sex_country_wide, aes(y = country, color = sex)) + 
  geom_dumbbell(aes(x=Female, xend=Male), color = "grey", size = 1) + 
  geom_point(data = sex_country_long, aes(x = suicide_per_100k), size = 3) +
  geom_point(data = country_long, aes(x = suicide_per_100k)) + 
  geom_vline(xintercept = global_average, linetype = 2, color = "grey35", size = 1) +
  theme(axis.text.y = element_text(size = 8), 
        legend.position = c(0.85, 0.2)) + 
  scale_x_continuous(breaks = seq(0, 80, 10)) +
  labs(title = "Gender Disparity, by Continent & Country", 
       subtitle = "Ordered by difference in deaths per 100k.", 
       x = "Suicides per 100k", 
       y = "Country", 
       color = "Sex")

### Proportions of suicides that are Male & Female, by Country

country_gender_prop <- sex_country_wide %>%
  mutate(Male_Proportion = Male / (Female + Male)) %>%
  arrange(Male_Proportion)

sex_country_long$country <- factor(sex_country_long$country, 
                                   ordered = T,
                                   levels = country_gender_prop$country)

ggplot(sex_country_long, aes(y = suicide_per_100k, x = country, fill = sex)) + 
  geom_bar(position = "fill", stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Proportions of suicides that are Male & Female, by Country", 
       x = "Country", 
       y = "Suicides per 100k",
       fill = "Sex") + 
  coord_flip()


###############
###############
## 2.8 Age differences, by Continent ##
data %>%
  group_by(continent, age) %>%
  summarize(n = n(), 
            suicides = sum(as.numeric(suicides_no)), 
            population = sum(as.numeric(population)), 
            suicide_per_100k = (suicides / population) * 100000) %>%
  ggplot(aes(x = continent, y = suicide_per_100k, fill = age)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_hline(yintercept = global_average, linetype = 2, color = "grey35", size = 1) +
  labs(title = "Age Disparity, by Continent",
       x = "Continent", 
       y = "Suicides per 100k", 
       fill = "Age")


###############
###############
## 2.9 As a country gets richer, does it’s suicide rate decrease? ##
country_year_gdp <- data %>%
  group_by(country, year) %>%
  summarize(gdp_per_capita = mean(gdp_per_capita))

country_year_gdp_corr <- country_year_gdp %>%
  ungroup() %>%
  group_by(country) %>%
  summarize(year_gdp_correlation = cor(year, gdp_per_capita))

# This was answered earlier in (2.5.2) - it depends on the country! Some countries are increasing with time, most are decreasing.
# Instead, I ask a slightly different question below.


###############
###############
## 2.10 Do richer countries have a higher rate of suicide? ##
country_mean_gdp <- data %>%
  group_by(country, continent) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000, 
            gdp_per_capita = mean(gdp_per_capita))

ggplot(country_mean_gdp, aes(x = gdp_per_capita, y = suicide_per_100k, col = continent)) + 
  geom_point() + 
  scale_x_continuous(labels=scales::dollar_format(prefix="$"), breaks = seq(0, 70000, 10000)) + 
  labs(title = "Correlation between GDP (per capita) and Suicides per 100k", 
       subtitle = "Plot containing every country",
       x = "GDP (per capita)", 
       y = "Suicides per 100k", 
       col = "Continent")

# I assess the statistics of this model (with outliers removed) below.
model1 <- lm(suicide_per_100k ~ gdp_per_capita, data = country_mean_gdp)

gdp_suicide_no_outliers <- model1 %>%
  augment() %>%
  arrange(desc(.cooksd)) %>%
  filter(.cooksd < 4/nrow(.)) %>% # removes 5/93 countries
  inner_join(country_mean_gdp, by = c("suicide_per_100k", "gdp_per_capita")) %>%
  select(country, continent, gdp_per_capita, suicide_per_100k)

model2 <- lm(suicide_per_100k ~ gdp_per_capita, data = gdp_suicide_no_outliers)

summary(model2)

### What does all this mean?
# There is a weak but significant positive linear relationship - 
# richer countries are associated with higher rates of suicide, 
# but this is a weak relationship which can be seen from the graph below.

ggplot(gdp_suicide_no_outliers, aes(x = gdp_per_capita, y = suicide_per_100k, col = continent)) + 
  geom_point() + 
  geom_smooth(method = "lm", aes(group = 1)) + 
  scale_x_continuous(labels=scales::dollar_format(prefix="$"), breaks = seq(0, 70000, 10000)) + 
  labs(title = "Correlation between GDP (per capita) and Suicides per 100k", 
       subtitle = "Plot with high CooksD countries removed (5/93 total)",
       x = "GDP (per capita)", 
       y = "Suicides per 100k", 
       col = "Continent") + 
  theme(legend.position = "none")


###############
###############
## 2.11 The reason I haven’t used the generation variable ##
data %>%
  group_by(generation, age, year) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000) %>%
  ggplot(aes(x = year, y = suicide_per_100k, col = factor(generation, ordered = F))) + 
  geom_point() + 
  geom_line() + 
  facet_grid(age ~ ., scales = "free_y") + 
  scale_x_continuous(breaks = seq(1985, 2015, 5), minor_breaks = NULL) +
  labs(title = "Relationship between Generation, Age & Year", 
       x = "Year", 
       y = "Suicides per 100k", 
       col = "Generation") + 
  theme(legend.position = "bottom")


###  
generation_rate <- data %>%
  group_by(generation, year) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000) %>%
  ggplot(aes(x = year, y = suicide_per_100k, col = factor(generation, ordered = F))) + 
  geom_point() + 
  geom_line() + 
  facet_grid(generation ~ ., scales = "free_y") + 
  scale_x_continuous(breaks = seq(1985, 2015, 5), minor_breaks = NULL) +
  labs(title = "Suicides per 100k, by Generation", 
       x = "Year", 
       y = "Suicides per 100k") + 
  theme(legend.position = "none")

generation_population <- data %>%
  group_by(generation, year) %>%
  summarize(population = sum(as.numeric(population))) %>%
  ggplot(aes(x = year, y = population / 1000000, col = factor(generation, ordered = F))) + 
  geom_point() + 
  geom_line() + 
  facet_grid(generation ~ ., scales = "free_y") + 
  scale_x_continuous(breaks = seq(1985, 2015, 5), minor_breaks = NULL) +
  labs(title = "Population, by Generation", 
       x = "Year", 
       y = "Population (Millions)", 
       col = "Generation") + 
  theme(legend.position = "none")

grid.arrange(generation_rate, generation_population, ncol = 2)


###############
###############
## 2.12 The 5% highest risk instances in history ##
demographic_most <- data %>%
  mutate(suicides_per_100k = suicides_no * 100000 / population) %>%
  arrange(desc(suicides_per_100k)) %>% 
  filter(year != 1985) %>%
  head(n = round(nrow(.) * 5 / 100))

demographic_most$time <- ifelse(demographic_most$year <= 1995, "1986 - 1995", 
                                ifelse(demographic_most$year <= 2005, "1996 - 2005", 
                                       "2006 - 2015"))

# bar
ggplot(demographic_most, aes(x = age, fill = sex)) + 
  geom_bar() + 
  labs(title = "5% Most At-Risk Instances in History", 
       subtitle = "Volumes by Decade, Age & Sex",
       x = "Age", 
       y = "Number of Demographics", 
       fill = "Sex") + 
  facet_wrap(~ time) + 
  scale_y_continuous(breaks = seq(0, 300, 20))

# scatter
set.seed(1)

ggplot(demographic_most, aes(x = age, y = suicides_per_100k, col = sex)) + 
  geom_jitter(alpha = 0.5) + 
  labs(title = "5% Most At-Risk Instances in History", 
       subtitle = "Instances by Decade, Age, & Sex",
       x = "Age", 
       y = "Suicides per 100k", 
       col = "Sex") + 
  facet_wrap(~ time) + 
  scale_y_continuous(breaks = seq(50, 300, 10))

# Korea & Hungary
data %>%
  filter(country %in% c('Republic of Korea', 'Hungary'), sex == "Male") %>%
  group_by(country, age, year) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000) %>%
  ggplot(aes(x = year, y = suicide_per_100k, col = country)) + 
  geom_line() + 
  geom_point() + 
  facet_wrap(~ age) + 
  geom_hline(yintercept = min(demographic_most$suicides_per_100k)) + 
  theme(legend.position = "bottom") + 
  scale_y_continuous(breaks = seq(0, 220, 40)) +
  labs(title = "Male Age-Group Trends in Hungary & South Korea", 
       subtitle = "Black reference line indicates where the demographic enters the 'top 5% in history'",
       x = "Year", 
       y = "Suicides per 100k",
       col = "Country")





##################################################################
## 3 Comparing the UK, Ireland, America, France & Denmark


###############
###############



###############
###############



###############
###############



###############
###############



###############
###############



###############
###############



###############
###############




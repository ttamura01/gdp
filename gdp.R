library(tidyverse)
library(ggtext)
library(patchwork)
library(scales)
library(lubridate)
library(glue)

## download data from csv file

gdp <- read_csv("https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1318&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=GDP&scale=left&cosd=1947-01-01&coed=2024-04-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Quarterly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2024-07-25&revision_date=2024-07-25&nd=1947-01-01") %>% 
  rename_all(tolower) %>% 
  drop_na() 
tail(gdp)

## familiarize the dataset
str(gdp)

summary(gdp)

gdp %>% 
  ggplot(aes(x = gdp)) +
  geom_histogram()

gdp %>% 
  ggplot(aes(x = date, y = gdp)) +
  geom_line()


## extract the initial and latest data
# initial data
initial_data <- gdp %>% 
  slice_min(date)

initial_date <- initial_data$date
initial_gdp <- round(initial_data$gdp)

initial_year <- year(initial_date)
initial_month <- month(initial_date)
initial_day <- day(initial_date)

# latest data
latest_data <- gdp %>% 
  slice_max(date)

latest_date <- latest_data$date
latest_gdp <- latest_data$gdp
gdp_label <- comma(round(latest_gdp))

# find gdp growth over the interval 
multiple <- latest_gdp/initial_gdp
gdp_multiple <- round(multiple)

# find interval years 
years <- interval(initial_date, latest_date)/years(1)

# annualized growth rate
# (gdp_multiple)^(1/years) = (x + 1)
annual_gdp_growth_rate <-  round(((gdp_multiple)^(1/years) - 1)*100, 2)


## line plot of quaterly data/gdp
gdp %>% 
  ggplot(aes(x = date, y = gdp)) +
  geom_line() +
  geom_text(data=latest_data, aes(x = date, y = gdp, label = gdp_label, vjust = -0.3), color = "blue") +
  scale_y_continuous(
    limits = c(0, 30000),
    breaks = seq(0, 30000, 5000),
    labels = label_comma(accuracy = 0.1)) +
  labs(title = glue("US GDP grew {gdp_multiple}x to ${gdp_label} billion past for {years} years from ${initial_gdp} billion in {initial_year} (= annual growth rate at {annual_gdp_growth_rate}%)"),
       x = NULL,
       y = "GDP (in $ billion)") +
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(),
    text = element_text(face = "bold"), 
    legend.position = "none"
  )
  
## transform quaterly gdp to annualized gdp
annual_gdp <- gdp %>% 
  drop_na() %>% 
  separate(date, sep = "-", into = c("year","month","day")) %>% 
  group_by(year) %>% 
  mutate("annualized_gdp" = sum(gdp)/4,
         ) %>% 
  mutate("gdp" = round(annualized_gdp)) %>% 
  slice_tail() %>% 
  filter(year != "2024") %>% 
  select(year, gdp) %>% 
  ungroup()
# 
# sapply(annual_gdp, class)
# 
# annual_gdp$year <- as.Date(annual_gdp$year, format = "%Y")

## extract the initial and latest data
# initial data

initial_annual_data <- annual_gdp %>% 
  filter(year == min(year))

initial_annual_data <- annual_gdp %>% 
  slice_min(year)

initial_year <- initial_annual_data$year
initial_annual_gdp <- initial_annual_data$gdp

# initial_date <- initial_data$date
# initial_gdp <- round(initial_data$gdp)

# latest data

latest_annual_data <- annual_gdp %>% 
  slice_max(year)
latest_annual_data$year <- as.Date(latest_annual_data$year, "%Y")

latest_year <- latest_annual_data$year
latest_annual_gdp <- latest_annual_data$gdp
latest_gdp_label <-  comma(round(latest_annual_gdp))

# find gdp growth over the interval 
multiple <- latest_annual_gdp/initial_annual_gdp
annual_gdp_multiple <- round(multiple)

# find interval years 
# years <- as.numeric(latest_year) - as.numeric(initial_year)
years_annual <- interval(as.Date(initial_year, "%Y"), as.Date(latest_year, "%Y")) / years(1)

years <- interval(as.Date(initial_year, "%Y"), as.Date(latest_year, "%Y"))/years(1)

# annualized growth rate
# (gdp_multiple)^(1/years) = (x + 1)
annual_gdp_growth_rate_annual <-  round(((gdp_multiple)^(1/years) - 1)*100, 2)

# transform year from character to Date
annual_gdp$year <- as.Date(annual_gdp$year, format = "%Y")

annual_gdp %>% 
  ggplot(aes(x = year, y = gdp)) +
  geom_line() +
  geom_text(data=latest_annual_data, aes(x = year, y = gdp, label = latest_gdp_label, vjust = -0.5), color = "blue") +
  scale_y_continuous(
    limits = c(0, 30000),
    breaks = seq(0, 30000, 5000),
    labels = label_comma(accuracy = 0.1)) +
  labs(title = glue("US GDP grew {annual_gdp_multiple}x to ${latest_gdp_label} billion past for {years} years from ${initial_annual_gdp} billion in {initial_year} (= annual growth rate at {annual_gdp_growth_rate_annual}%)"),
       x = NULL,
       y = "GDP (in $ billion)",
       caption = "source: FRED (Federal Reserve Economic Data") +
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(),
    text = element_text(face = "bold"),
    legend.position = "none"
  )

ggsave("/Users/takayukitamura/Documents/R_Computing/us_pop_gdp/figures/US_GDP_grwoth.png", width = 6, height = 4)
getwd()


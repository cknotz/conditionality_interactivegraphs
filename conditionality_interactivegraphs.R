
### Benefit conditions/sanctions vs. unemployment graphs for dataset website
############################################################################

library(ggplot2)
library(tidyverse)
library(ggiraph)
library(OECD)
library(countrycode)

# Downloading data
##################

# Reading in benefit conditionality data
data <-
    readRDS(url(
        "https://www.dropbox.com/s/nrs8icuylmowr4t/shinydata.rda?dl=1"
    ))

Hmisc::label(data[["conditionality"]]) <-
    "Overall benefit conditionality"
Hmisc::label(data[["conditions"]]) <-
    "Job-search and avail. requirements"
Hmisc::label(data[["sanctions"]]) <- "Sanction rules"
Hmisc::label(data[["occup"]]) <- "Occupational protection"
Hmisc::label(data[["wage"]]) <- "Wage protection"
Hmisc::label(data[["oth"]]) <- "Other valid reasons"
Hmisc::label(data[["jsr"]]) <- "Job-search requirements"
Hmisc::label(data[["vol"]]) <- "Sanctions for voluntary unemp."
Hmisc::label(data[["ref"]]) <- "Sanctions for first refusal"
Hmisc::label(data[["rep"]]) <- "Sanctions for repeated ref."
Hmisc::label(data[["fail"]]) <-"Sanctions for failure to report"

data$country <- as.character(data$country)

# Getting unemployment data from the OECD
unem <- get_dataset("ALFS_SUMTAB",
                    "AUS+AUT+BEL+CAN+DNK+FIN+FRA+DEU+GRC+IRL+ITA+JPN+KOR+NLD+NZL+NOR+PRT+ESP+SWE+CHE+GBR.YGTT06PC_ST.A")

# Data mangling
###############
unem <- select(unem,LOCATION, obsTime, obsValue)

unem$year <- as.numeric(unem$obsTime)
    unem$obsTime <- NULL

unem %>% filter(year >= 1980 & year<=2012)

unem$country <- countrycode(unem$LOCATION,
                            origin = "iso3c",
                            destination = "country.name")
unem$country[unem$country=="South Korea"] <- "Korea"

data <- merge(data,unem, by=c("country","year"))
    unem <- NULL
    data <-  rename(data,unem = obsValue)


data <- select(data,country,year,unem,conditions,sanctions)


# Aggregating data for conditions graph
cond_data <- data %>%
    select(country,year,unem,conditions) %>%
    na.omit()

cond_data <- cond_data %>%
    group_by(country) %>%
    summarise(unem=mean(unem),conditions=mean(conditions))

cond_data$ccode <- countrycode(cond_data$country,
                              origin = "country.name",
                              destination = "iso2c")

# Aggregating data for sanctions graph
san_data <- data %>%
    select(country,year,unem,sanctions) %>%
    na.omit()

san_data <- san_data %>%
    group_by(country) %>%
    summarise(unem=mean(unem),sanctions=mean(sanctions))

san_data$ccode <- countrycode(san_data$country,
                              origin = "country.name",
                              destination = "iso2c")
# Graphs
########
tooltip_css <- "background-color:gray;color:white;padding:10px;border-radius:5px;font-family: Lora, sans-serif;font-weight:lighter;font-size:12px;"

# Conditions
cond_data$ttip <- c(paste0(cond_data$country,":\n",
                           "Avg. strictness score: ",round(cond_data$conditions,digits = 2),"\n",
                           "Avg. unemployment: ",round(cond_data$unem,digits=2),"%"))

p <- ggplot(cond_data,aes(x=conditions,y=unem,data_id=country,tooltip=ttip)) +
    geom_point_interactive(shape=16, size=5, color="orangered4", alpha=.4) +
    geom_smooth_interactive(method =lm, se=F,
                colour="orangered4",
                alpha=.4,
                size=.5,
                linetype="dashed",
                aes(tooltip="Fitted values", data_id="smooth")) +
    theme_bw() +
        theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
        theme(panel.grid.minor.x = element_blank()) +
        theme(panel.grid.minor.y = element_blank()) +
    ylab("Average unemployment rate (%, 1980-2012)") +
    xlab("Average strictness of job-search & availability requirements (1980-2012)") +
    scale_x_continuous(limits = c(0.2,0.9))

    con <- girafe(ggobj = p,
                  options = list(
                      opts_hover_inv(css = "opacity:0.1;"),
                      opts_hover(css = "fill:#A93226;stroke:black;"),
                      opts_tooltip(offx = 20, offy = 20,css = tooltip_css)))
    con

    htmlwidgets::saveWidget(con, file = "congraph.html")

# Sanctions
san_data$ttip <- c(paste0(san_data$country,":\n",
                           "Avg. strictness score: ",round(san_data$sanctions,digits = 2),"\n",
                           "Avg. unemployment: ",round(san_data$unem,digits=2),"%"))

p <-  ggplot(san_data,aes(x=sanctions,y=unem,data_id=country,tooltip=ttip)) +
    geom_point_interactive(shape=16, size=5, color="orangered4", alpha=.4) +
    geom_smooth_interactive(method=lm, se=F,
                colour="orangered4",
                alpha=.4,
                size=.5,
                linetype="dashed",
                aes(tooltip="Fitted values", data_id="smooth")) +
    theme_bw(base_family = "sans") +
        theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
        theme(panel.grid.minor.x = element_blank()) +
        theme(panel.grid.minor.y = element_blank()) +
    ylab("Average unemployment rate (%, 1980-2012)") +
    xlab("Average strictness of sanction rules (1980-2012)") +
    scale_x_continuous(limits = c(0.2,0.9))

    san <- girafe(ggobj = p,
                  options = list(
                      opts_hover_inv(css = "opacity:0.1;"),
                      opts_hover(css = "fill: #A93226;stroke:black;"),
                      opts_tooltip(offx = 20, offy = 20,css = tooltip_css)))

    san

    htmlwidgets::saveWidget(san, file = "sangraph.html")

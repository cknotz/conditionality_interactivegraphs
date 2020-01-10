
# Interactive plot for conditionality-dataset website
#####################################################

# Author: Carlo Knotz

library("readstata13")
library(plotly)

# download data
url <- "https://www.dropbox.com/s/xw2nr8gzdf8v940/conditionality.dta?dl=1"

data <- read.dta13(url)
    rm(url)

# subsetting
data <- data[c("year","country","conditionality","conditions","sanctions")]

# generating & appending overall averages
mean <- aggregate(data[,3:5],by=list(data$year),FUN=mean,na.rm=T)
    mean$country <-vector(mode="character",length=33)
    mean$country <-c("Average")
    names(mean)[names(mean) == "Group.1"] <- "year"
    
data <- rbind(data,mean)
    rm(mean)
    
# Rounding
data$conditionality <- round(data$conditionality,digits=2)
    data$conditions <- round(data$conditions,digits=2)
    data$sanctions <- round(data$sanctions,digits=2)

##############################
# Line graphs with highcharter
##############################

# Credit is due to Rob Kabacoff (https://rkabacoff.github.io/datavis/index.html)    
    
#detach("package:plotly", unload=TRUE)
library("highcharter")
library("tidyr")
library("htmlwidgets")

# Benefit conditions 
####################

# convert from long to wide
dat <- data[c("country", "year", "conditions")]
plotdata <- spread(dat, country,conditions)

conditions <- highchart() %>%
    hc_title(text="Job-search & work-availability requirements") %>%
    hc_xAxis(categories=plotdata$year) %>%
    hc_add_series(name="Australia",
                  data=plotdata$Australia, type="line") %>%
    hc_add_series(name="Austria",
                  data=plotdata$Austria, type="line",visible=F) %>%
    hc_add_series(name="Belgium",
                  data=plotdata$Belgium, type="line",visible=F) %>%
    hc_add_series(name="Canada",
                  data=plotdata$Canada, type="line",visible=F) %>%
    hc_add_series(name="Denmark",
                  data=plotdata$Denmark, type="line") %>%
    hc_add_series(name="Finland",
                  data=plotdata$Finland, type="line",visible=F) %>%
    hc_add_series(name="France",
                  data=plotdata$France, type="line",visible=F) %>%
    hc_add_series(name="Germany",
                  data=plotdata$Germany, type="line",visible=F) %>%
    hc_add_series(name="Greece",
                  data=plotdata$Greece, type="line",visible=F) %>%
    hc_add_series(name="Ireland",
                  data=plotdata$Ireland, type="line",visible=F) %>%
    hc_add_series(name="Italy",
                  data=plotdata$Italy, type="line",visible=F) %>%
    hc_add_series(name="Japan",
                  data=plotdata$Japan, type="line",visible=F) %>%
    hc_add_series(name="Korea",
                  data=plotdata$Korea, type="line",visible=F) %>%
    hc_add_series(name="Netherlands",
                  data=plotdata$Netherlands, type="line",visible=F) %>%
    hc_add_series(name="New Zealand",
                  data=plotdata$`New Zealand`, type="line",visible=F) %>%
    hc_add_series(name="Norway", 
                  data=plotdata$Norway, type="line",visible=F) %>%
    hc_add_series(name="Portugal",
                  data=plotdata$Portugal, type="line",visible=F) %>%
    hc_add_series(name="Spain",
                  data=plotdata$Spain, type="line") %>%
    hc_add_series(name="Sweden",
                  data=plotdata$Sweden, type="line",visible=F) %>%
    hc_add_series(name="Switzerland",
                  data=plotdata$Switzerland, type="line",visible=F) %>%
    hc_add_series(name="United Kingdom",
                  data=plotdata$ `United Kingdom`, type="line",visible=F) %>%
    hc_add_series(name="Average",
                  data=plotdata$Average, type="line",visible=T) %>%
    hc_yAxis(title = list(text = "Strictness score"),max=1,min=0) %>%
    hc_add_theme(hc_theme_tufte()) %>%
    hc_exporting(enabled = TRUE,
                 filename = "conditions")
conditions
saveWidget(conditions,file="conditions.html", selfcontained=T)


# Benefit sanctions 
####################

# convert from long to wide
dat <- data[c("country", "year", "sanctions")]
plotdata <- spread(dat, country,sanctions)

sanctions <- highchart() %>%
    hc_title(text="Sanction rules") %>%
    hc_xAxis(categories=plotdata$year) %>%
    hc_add_series(name="Australia",
                  data=plotdata$Australia, type="line") %>%
    hc_add_series(name="Austria",
                  data=plotdata$Austria, type="line",visible=F) %>%
    hc_add_series(name="Belgium",
                  data=plotdata$Belgium, type="line",visible=F) %>%
    hc_add_series(name="Canada",
                  data=plotdata$Canada, type="line",visible=F) %>%
    hc_add_series(name="Denmark",
                  data=plotdata$Denmark, type="line") %>%
    hc_add_series(name="Finland",
                  data=plotdata$Finland, type="line",visible=F) %>%
    hc_add_series(name="France",
                  data=plotdata$France, type="line",visible=F) %>%
    hc_add_series(name="Germany",
                  data=plotdata$Germany, type="line",visible=F) %>%
    hc_add_series(name="Greece",
                  data=plotdata$Greece, type="line",visible=F) %>%
    hc_add_series(name="Ireland",
                  data=plotdata$Ireland, type="line",visible=F) %>%
    hc_add_series(name="Italy",
                  data=plotdata$Italy, type="line",visible=F) %>%
    hc_add_series(name="Japan",
                  data=plotdata$Japan, type="line",visible=F) %>%
    hc_add_series(name="Korea",
                  data=plotdata$Korea, type="line",visible=F) %>%
    hc_add_series(name="Netherlands",
                  data=plotdata$Netherlands, type="line",visible=F) %>%
    hc_add_series(name="New Zealand",
                  data=plotdata$`New Zealand`, type="line",visible=F) %>%
    hc_add_series(name="Norway", 
                  data=plotdata$Norway, type="line",visible=F) %>%
    hc_add_series(name="Portugal",
                  data=plotdata$Portugal, type="line",visible=F) %>%
    hc_add_series(name="Spain",
                  data=plotdata$Spain, type="line") %>%
    hc_add_series(name="Sweden",
                  data=plotdata$Sweden, type="line",visible=F) %>%
    hc_add_series(name="Switzerland",
                  data=plotdata$Switzerland, type="line",visible=F) %>%
    hc_add_series(name="United Kingdom",
                  data=plotdata$ `United Kingdom`, type="line",visible=F) %>%
    hc_add_series(name="Average",
                  data=plotdata$Average, type="line",visible=T) %>%
    hc_yAxis(title = list(text = "Strictness score"),max=1,min=0) %>%
    hc_add_theme(hc_theme_tufte()) %>%
    hc_exporting(enabled = TRUE,
                 filename = "sanctions")
sanctions
saveWidget(sanctions,file="sanctions.html", selfcontained = T)

# Benefit conditionality 
########################

# convert from long to wide
dat <- data[c("country", "year", "conditionality")]
plotdata <- spread(dat, country, conditionality)

conditionality <- highchart() %>%
    hc_title(text="Overall benefit conditionality") %>%
    hc_xAxis(categories=plotdata$year) %>%
    hc_add_series(name="Australia",
                  data=plotdata$Australia, type="line") %>%
    hc_add_series(name="Austria",
                  data=plotdata$Austria, type="line",visible=F) %>%
    hc_add_series(name="Belgium",
                  data=plotdata$Belgium, type="line",visible=F) %>%
    hc_add_series(name="Canada",
                  data=plotdata$Canada, type="line",visible=F) %>%
    hc_add_series(name="Denmark",
                  data=plotdata$Denmark, type="line") %>%
    hc_add_series(name="Finland",
                  data=plotdata$Finland, type="line",visible=F) %>%
    hc_add_series(name="France",
                  data=plotdata$France, type="line",visible=F) %>%
    hc_add_series(name="Germany",
                  data=plotdata$Germany, type="line",visible=F) %>%
    hc_add_series(name="Greece",
                  data=plotdata$Greece, type="line",visible=F) %>%
    hc_add_series(name="Ireland",
                  data=plotdata$Ireland, type="line",visible=F) %>%
    hc_add_series(name="Italy",
                  data=plotdata$Italy, type="line",visible=F) %>%
    hc_add_series(name="Japan",
                  data=plotdata$Japan, type="line",visible=F) %>%
    hc_add_series(name="Korea",
                  data=plotdata$Korea, type="line",visible=F) %>%
    hc_add_series(name="Netherlands",
                  data=plotdata$Netherlands, type="line",visible=F) %>%
    hc_add_series(name="New Zealand",
                  data=plotdata$`New Zealand`, type="line",visible=F) %>%
    hc_add_series(name="Norway", 
                  data=plotdata$Norway, type="line",visible=F) %>%
    hc_add_series(name="Portugal",
                  data=plotdata$Portugal, type="line",visible=F) %>%
    hc_add_series(name="Spain",
                  data=plotdata$Spain, type="line") %>%
    hc_add_series(name="Sweden",
                  data=plotdata$Sweden, type="line",visible=F) %>%
    hc_add_series(name="Switzerland",
                  data=plotdata$Switzerland, type="line",visible=F) %>%
    hc_add_series(name="United Kingdom",
                  data=plotdata$ `United Kingdom`, type="line",visible=F) %>%
    hc_add_series(name="Average",
                  data=plotdata$Average, type="line",visible=T) %>%
    hc_yAxis(title = list(text = "Strictness score"),max=1,min=0) %>%
    hc_add_theme(hc_theme_tufte()) %>%
    hc_exporting(enabled = TRUE,
                 filename = "conditionality")
conditionality
saveWidget(conditionality,file="conditionality.html", selfcontained = T)



# # Bar graphs with plotly
# ########################
# plot_ov <- plot_ly(data,x=~country,y=~conditionality,
#                    frame=~year,
#                    type="bar")
# plot_ov
# 
# plot_drop <- plot_ly(data, x=~country) %>%
#     add_bars(y=~conditionality, name="Conditionality", visible=T,frame=~year) %>%
#     add_bars(y=~conditions, name="Conditions", visible=F,frame=~year) %>%
#     add_bars(y=~sanctions, name="Sanctions", visible=F,frame=~year) %>%
#     layout(
#         yaxis = list(title = "Strictness score"),
#         xaxis = list(title = " "),
#         updatemenus=list(
#             list(
#                 y=8,
#                 buttons = list(
#                     list(method = "restyle",
#                          args = list("visible", list(TRUE,FALSE,FALSE)),
#                          label="Conditionality"),
#                     list(method = "restyle",
#                          args = list("visible", list(FALSE,TRUE,FALSE)),
#                          label="Conditions"),
#                     list(method = "restyle",
#                          args = list("visible", list(FALSE,FALSE,TRUE)),
#                          label="Sanctions")))))
# plot_drop
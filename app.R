#########################################################
# Interactive shiny-App for conditionality data website #
#########################################################

# Carlo Knotz

library(ggplot2)
library(plotly)
library(shiny)
library(dplyr)
library(shinyWidgets)
library(shinythemes)
library(rsconnect)


# To do:
    # export

data <- readRDS(url("https://www.dropbox.com/s/nrs8icuylmowr4t/shinydata.rda?dl=1"))
# List of countries
    clist <- as.character(unique(data$country))

# Bar graph
ui <-navbarPage(" ", #theme = shinytheme("sandstone"),
    tabPanel("Bar",
            sidebarLayout(
                sidebarPanel(
                    pickerInput("var_bar","Indicator", 
                                choices = list(
                                    "Aggregate indicators"=c("Overall conditionality"="conditionality",
                                                      "Job-search & avail. requirements"="conditions",
                                                      "Sanction rules"="sanctions"),
                                    "Sub-indicators"=c("Occupational protection" = "occup",
                                                       "Wage protection" = "wage",
                                                       "Other valid reasons" = "oth",
                                                       "Job-search requirements" = "jsr",
                                                       "Sanctions for voluntary unemp." = "vol",
                                                       "Sanctions for first refusal" = "ref",
                                                       "Sanctions for rep. refusal" = "rep",
                                                       "Sanctions for failure to report" = "fail"))),
                    #pickerInput("year_bar","Year",choices = data$year,label = "Year"),
                    sliderInput("year_bar","Year", min=1980,max = 2012,value = 2005,step=1,sep=""),
                    pickerInput("country_bar","Countries",choices = clist, selected=clist,
                                multiple = T,options = pickerOptions(actionsBox = T))),
                mainPanel(
                    plotlyOutput("bar")
                ))),
    tabPanel("Line",
             sidebarLayout(
                 sidebarPanel(
                     pickerInput("var_line","Indicator", choices = list(
                         "Aggregate indicators"=c("Overall conditionality"="conditionality",
                                                  "Job-search & avail. requirements"="conditions",
                                                  "Sanction rules"="sanctions"),
                         "Sub-indicators"=c("Occupational protection" = "occup",
                                            "Wage protection" = "wage",
                                            "Other valid reasons" = "oth",
                                            "Job-search requirements" = "jsr",
                                            "Sanctions for voluntary unemp." = "vol",
                                            "Sanctions for first refusal" = "ref",
                                            "Sanctions for rep. refusal" = "rep",
                                            "Sanctions for failure to report" = "fail"))),
                     pickerInput("country_line","Countries",choices = clist ,multiple = T,selected = c("Sweden","United Kingdom","Germany"),
                                 options = list(pickerOptions(actionsBox = T),"max-options" = 6))),
                 mainPanel(
                     plotlyOutput("line")
                 ))),
    tabPanel("What do these numbers mean?",
             h2("Background info"),
             p("The data shown in the graphs here measure the strictness of job-search and work-availability requirements and 
        sanction rules for unemployment benefit claimants. Simply put, higher numbers mean that unemployment
        benefit claimants are under greater pressure to actively search for and accept employment, and that
        they face harsher sanctions if they fail to comply with these requirements."),
             br(),
             p("You can choose between a number of different indicators that all measure different aspects of these rules.
          These indicators are inspired by, and therefore resemble, related indicators developed by the",a("OECD",
            href="https://doi.org/10.1787/5jrxtk1zw8f2-en"),"or the",
          a("Danish Ministry of Finance", 
          href="https://uk.fm.dk/publications/working-papers/2005/working-paper-no,-d-,-12-2005"),
          ". All indicators range between 0 (very lenient rules) and 1 (very strict rules). Further information can also be found in
          Knotz (",a("2018, ",href="https://doi.org/10.1080/21699763.2018.1472136"),a("2019, ", href="https://doi.org/10.1017/S0047279418000740"),
            a("forthc.", href="https://doi.org/10.1093/esr/jcz041"), ")"),
             br(),
          p("The general logic behind and connection between the various indicators is illustrated in the figure just below:"),
          img(src='https://www.dropbox.com/s/dxwiqque0m2clw0/indicators.png?dl=1', align = "right",height=350,width=500),
          br(),
             p(strong("Aggregate indicators:")),
             p("The",em("Overall Conditionality"), "indicator provides, as the name suggests, a measurement for the overall 
          or aggregate strictness of all rules and requirements and is constructed from the aggregate indicators for
          the strictness of",em("Job-search & availability requirements"), "and for",em("Sanction rules."),"The former 
          captures only under how much pressure the unemployed are to seek and accept work, but not the strictness of
          sanction rules. These are measured by the latter."),
             br(),
             p(strong("Sub-indicators:")),
             p("The three aggreate indicators are themselves built from several sub-indicators that capture the strictness
          of more specific rules:"),
             p("Occupational protection: To what extent can unemployed workers reject work in other occupations than their own 
          without risking a sanction?"),
             p("Wage protection: To what extent can they reject work paying less than their previous or a usual salary?"),
             p("Other valid reasons: What is the extent of other valid reasons to reject work? Are for instance caring responsibilities
          considered in determining what a suitable job is for a particular unemployed worker?"),
             p("Job-search requirements: How frequently do unemployed workers have to report their job-search activities?"),
             p("Sanctions for voluntary unemployment: How severely can unemployed workers sanctioned if they became unemployed
          voluntarily?"),
             p("Sanctions for first refusal: How severe are sanctions for a first refusal of a suitable offer of work?"),
             p("Sanctions for repeated refusals: How severe are sanctions when multiple offers are refused?"),
             p("Sanctions for failure to report: How harshly can unemployed workers be sanctioned if they fail to
          report their job-search activities as required?"),
          br(),
          br(),
          p("If you have any questions or comments about the dataset or this application, please contact",a("Carlo Knotz",href="https://benefitconditionality.weebly.com/carlo-knotz.html"),"."))
)

server <-function(input,output,session){
    # Reactive to create plot data
    bardata <- reactive({
        filter(data,year==input$year_bar & country %in% input$country_bar)
    })
    linedata <- reactive({
        filter(data, country %in% input$country_line)
    })
    output$bar <- renderPlotly({
    p <- ggplot(bardata(), aes(
                x=reorder(!!sym("country"), -!!sym(input$var_bar)),
                y=!!sym(input$var_bar)))  + 
            geom_bar(stat = "identity") +
            guides(fill=FALSE) +
            scale_fill_grey(start = 0, end = .9) +
            theme_bw() +
            theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
            theme(panel.grid.major.x = element_blank()) +
            theme(axis.text.x = element_text(angle = 45,size=14,hjust=1)) +
            theme(axis.title.y = element_text(size=14)) +
            scale_y_continuous(limits = c(0,1)) +
            xlab(" ") +
            ylab("Strictness score") +
            annotate("text",x=-Inf,y=.935,hjust=-.99,colour="gray85",
                     label="The Comparative Unemployment Benefit Conditions & Sanctions Dataset")
    p <- ggplotly(p, tooltip = c("y"))
    })
    output$line <- renderPlotly({
        p <-ggplot(linedata(), aes(x=year,y=!!sym(input$var_line),group=!!sym("country"),color=!!sym("country"))) +
            geom_line(aes(linetype=!!sym("country")),color="black") + 
            theme_bw() +
            theme(legend.position = "bottom",
                  legend.title = element_blank(),
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank()) +
            xlab(" ") + ylab("Strictness score")
        
        p <- ggplotly(p,width=700,height=500,tooltip = c("x", "y")) %>% 
            layout(legend = list(orientation = "h",x=0.4,y=-0.2))
        p
    })
}

shinyApp(ui,server,options=list(width=800,height=500))


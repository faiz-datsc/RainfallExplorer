library(leaflet)

# Choices for drop-downs

stateDropdown <- c("Federal Territory of Kuala Lumpur", "Perlis", "Kedah", "Penang",  "Johor", "Kelantan", "Melaka", "Negeri Sembilan", "Pahang", "Perak", "Selangor", "Terengganu")
yearDropdown <- c("All Years", "2014", "2015", "2016", "2017", "2018", "2019", "2020")
monthDropdown <- as.list(setNames(c(1:12), month.name))
monthDropdown <- c("All Months", monthDropdown)

navbarPage("Rainfall Explorer Analytics", id="nav",
           
           tabPanel("Malaysia",
                    div(class="outer",
                        
                        tags$head(
                            # Include our custom CSS
                            includeCSS("styles.css"),
                            includeScript("gomap.js")
                        ),
                        
                        # If not using custom CSS, set height of leafletOutput to a number instead of percent
                        leafletOutput("map", width="100%", height="60%"),
                        
                        # Shiny versions prior to 0.11 should use class = "modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = FALSE, top = 60, left = 0, right = "auto", bottom = "auto",
                                      width = "20%", height = "45%",
                                      
                                      h4("State the time you wish to explore"),
                                      selectInput("Year", "Year", yearDropdown, width = "100%"),
                                      selectInput("Month", "Month", monthDropdown, width = "100%"),
                                      conditionalPanel(
                                          "input.Year != 'All Years' && input.Month != 'All Months'",
                                                       selectInput("Day", "Day", c("All Days", 1:31), width = "100%")
                                      ),
                                      tags$head(
                                          tags$style(HTML('#Today{background-color:darkgoldenrod; width:100%}'))
                                      ),
                                      actionButton("Today", label = "Jump to Today's Date"),
                                      tags$div(id="cite", "Data Source :", a("MAMPU - Data Terbuka Sektor Awam", href="http://www.data.gov.my/", target="_blank"))

                        ),
                        
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = FALSE, top = 60, right = 0, left = "auto", bottom = "auto",
                                      width = "30%", height = "35%",
                                      plotOutput("overallTrend", height = "100%")
                                      
                        ),
                            
                        fixedPanel(draggable = FALSE, top = "auto", width = "100%", height = "40%",
                                      
                                      column(4,
                                      selectInput("StateAnalyze", "", stateDropdown, width = "100%"),
                                      plotOutput("stateTrend", height = 200)
                                      ),
                                      
                                      column(4,
                                      selectInput("YearAnalyze", "", yearDropdown[-1], width = "100%"),
                                      plotOutput("stateYearlyTrend", height = 200)
                                      ),
                                      
                                      column(4,
                                      selectInput("MonthAnalyze", "", monthDropdown[-1], width = "100%"),
                                      plotOutput("stateMonthlyTrend", height = 200)
                                      )
                                      
                        )
                        
                    )
           ),
           
           conditionalPanel("false", icon("crosshair"))
           
)
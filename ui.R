# references
# invest allocation: http://www.wealthadvisors.com/assets/vanguard%20allocation%20updatetrf%20changes.pdf
# personal saving rate: http://research.stlouisfed.org/fred2/series/PSAVERT/
# compound interest: http://www.moneychimp.com/articles/finworks/fmbasinv.htm

library(ggplot2)
library(rCharts)

# clear environment
rm(list=ls())
# penalize/disable scientific notation
options(scipen=999)

shinyUI(
        fluidPage(
                # Application title
                titlePanel("Retirement Saving Calculator"),
                fluidRow(
                    p('Are you looking forward to retirement? Everybody is. But are you ready for it financially?
                      You can use this retirement saving calculator to assess through several tools.'),
                    tags$ol(
                        tags$li("Asset Allocation tab: calculate your time till retirement and get recommended investment allocation."), 
                        tags$li("Historical Saving Rate tab: check your current saving rate against historical personal saving rate data."), 
                        tags$li("Saving Calculator tab: calculate compounded growth of your saving and annual contributions.")
                    ),
                    em('Note: If you are already retired, the analyses here may not be applicable to you but please explore the tools. 
                       Furthermore, the data used here is for the U.S. market and thus may not be as applicable to non-U.S. users.'),
                    em('Some charts may take a few seconds to load. Please be patient.')
                    
                ),
                sidebarPanel(
                        conditionalPanel( # specific sidebar for each tab
                                "$('li.active a').first().html()==='Asset Allocation'", # make the condition statement a JQuery
                                sliderInput('age_slide', strong('Please use the slide to select your current age (left) and retiring age (right)'), 
                                            min=10, max = 100, 
                                            value = c(20, 65), step = 1),
                                hr(),
                                p('Now you can select to display recommended investment allocation either for 
                                   people who have the same number of years till retirement as you do or for your current age group.'),
                                em('Note: Charts for specific age groups assume retirement at age 65. 
                                   Stacked area charts show recommended allocation for the entire range of chart metrics.'),
                                selectInput('chart_metric', strong('chart metric'), c("years till retirement", "age")),
                                p('Please select the chart type.'),
                                selectInput('chart_type', strong('chart type'), c("stacked area chart", "pie chart")),
                                p('Press the Submit button after you input the information.'),
                                submitButton('Submit'),
                                p('Hover over the charts for more information. Displayed numbers are percentages. 
                                  Recommendations for some groups are similar so the pie charts may look the same.
                                  Try to select different ages or ranges to see various recommendations.')
                        ),
                        conditionalPanel(
                                "$('li.active a').first().html()==='Historical Saving Rate'",
                                numericInput('current_save_rate', 
                                             strong('Enter your current annual save rate 
                                                    (annual saving amount as a percentage of annual income)'), 0, 
                                             min=0, max = 100, step = 0.1),
                                hr(),
                                strong("Historical Annual Personal Saving Rate"),
                                p('To the right is a chart showing historical annual personal saving rate. 
                                  Data is available from 1959 to May 2014. Use the slide to change the time window for the chart.'),
                                sliderInput('year', strong('year range'), min=1959, max=2014, value=c(1959, 2014), 
                                            step=1, format = "###0"),
                                p('You can also select features of the chart below.'),
                                selectInput('linetype', strong('line type'), c("solid", "dashed", "dotted", "dotdash")),
                                selectInput('linecolor', strong('line color'), c("red", "dark blue", "dark green", "purple")),
                                p('Press the Submit button after you input the information.'),
                                submitButton('Submit')
                        ),
                        conditionalPanel(
                                "$('li.active a').first().html()==='Saving Calculator'",
                                p('What is the amount of your current saving?'),
                                numericInput('current_save_amount', strong('your current saving amount ($)'), 0, min=0),
                                p('What is the amount that you plan to save each year?'),
                                numericInput('contribution', strong('annual contribution ($)'), 0, min=0),
                                p('What is your expected rate of gain or interest?'),
                                numericInput('compound_rate', strong('projected gain/interest rate (%)'), 4, min=0, step=0.1),
                                p('How many times is your saving compounded each year? Monthly means 12 times, annually means 1 time.'),
                                selectInput('comp_cycle', strong('compound interest cycle'), c("monthly", "annually")),
                                strong('To change the time for growth, go back to the Asset Allocation tab, 
                                       change values using the slide, press Submit on that tab, then come back here to view the charts. 
                                       Otherwise press the Submit button here.'),
                                submitButton('Submit'),
                                em('Note: This calculation assumes that contributions are made at the start of cycles.
                                   Link to source of the formula is listed below the charts.')
                        )
                ),
                
                mainPanel(
                        tabsetPanel(
                                tabPanel("Asset Allocation", tableOutput("time"), verbatimTextOutput("joke"),
                                         showOutput("investChart", "nvd3"),
                                         helpText("Recommended allocation is based on ", 
                                                  a("glide path of Vanguard target funds",target="_blank",
                                                    href="http://www.wealthadvisors.com/assets/vanguard%20allocation%20updatetrf%20changes.pdf"),
                                                  "Data are in invest_allocation_2.csv and invest_allocation_year.csv on GitHub."
                                         )), 
                                tabPanel("Historical Saving Rate", 
                                         h4('your current annual saving rate'),
                                         verbatimTextOutput("inputValue1"),
                                         #verbatimTextOutput("testing"),
                                         plotOutput("rate_plot"),
                                         helpText("Historical data on annual personal saving rate is obtained from the 
                                                  website of the Federal Reserve Bank of St Louis and can be downloaded ", 
                                                  a("here.",
                                                    target="_blank",
                                                    href="http://research.stlouisfed.org/fred2/series/PSAVERT/f"))
                                ),
                                tabPanel("Saving Calculator", 
                                         tableOutput("calc_table"), 
                                         h4('your future amount'),
                                         verbatimTextOutput("calc_result"),
                                         h4('your projected saving growth'),
                                         showOutput("growChart1", "nvd3"),
                                         showOutput("growChart2", "nvd3"),
                                         helpText("Formula for this calculator is based on the ",a("formula of moneychimp.com.",
                                                    target="_blank",
                                                    href="http://www.moneychimp.com/articles/finworks/fmbasinv.htm"))
                                ) 
                                
                        )
                )
        )
)
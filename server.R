library(shiny)
library(ggplot2)
library(rCharts) # for interactive pie chart and stacked area chart
library(lubridate)
library(reshape2)

# clear environment
rm(list=ls())
# penalize/disable scientific notation
options(scipen=999)

#=================================
# Load data
#=================================
# personal annual saving rate data
save_rate <- read.table('fredgraph.txt', header = T, skip = 10, stringsAsFactors = F)
save_rate$observation_date <- as.Date(save_rate$observation_date)
save_rate$year <- year(save_rate$observation_date)

# investment allocation glide path by age
invest <- read.csv('invest_allocation_2.csv', header = T, stringsAsFactors = F)
# rename age to variable for later function
names(invest)[names(invest)=="age"] <- "variable"

# investment allocation glide path by years till retirement
invest2 <- read.csv('invest_allocation_year.csv', header = T, stringsAsFactors = F)
# rename age to variable for later function
names(invest2)[names(invest2)=="years_till_retirement"] <- "variable"


#=================================
# Functions
#=================================
# calculate time horizon
time <- function(x){
        time2 <- x[2] - x[1]
        return(time2)
}

n_chart <- function(input, invest_data, axis_name=NULL){
        if (input == "pie chart"){
            n <- nPlot(x = "type", y = "percent", 
                       data = invest_data, 
                       type = "pieChart")
            # n$addParams(dom = 'myChart')
            n$chart(margin = list(left = 50))
            n$params$width = 400
            n$params$height = 400
        }
        if (input == "stacked area chart"){
            n <- nPlot(percent ~ variable, group = "type", data = invest_data, 
                       type = "stackedAreaChart")
            # n$addParams(dom = 'myChart')
            n$chart(useInteractiveGuideline = TRUE)
            n$yAxis( axisLabel = "Percentage" )
            n$xAxis( axisLabel = axis_name)
            n$chart(margin = list(left = 80))
            n$params$width = 600
            n$params$height = 400
            
        }
        return(n)
}
# calculate compound interest with regular contribution
comp_int_calc <- function(amt, rate, cycle, time, contribute){
        # from moneychimp.com
        # Balance(Y)   =   P(1 + r)Y   +   c[ ((1 + r)Y + 1 - (1 + r)) / r ] # start of year contribution
        # Balance(Y)   =   P(1 + r)Y   +   c[ ((1 + r)Y - 1) / r ] # end of year contribution
        balance <- amt * (1 + rate/cycle)^(cycle * time) + 
                contribute * (((1 + rate/cycle)^(cycle * time + 1) - (1 + rate/cycle)) / (rate/cycle))
        return(balance)
}

p_grow_calc <- function(amt, rate, cycle, time){
        balance <- amt * (1 + rate/cycle)^(cycle * time) 
        return(balance)
}

c_grow_calc <- function(rate, cycle, time, contribute){
        balance <- contribute * (((1 + rate/cycle)^(cycle * time + 1) - (1 + rate/cycle)) / (rate/cycle))
        return(balance)
}

money_put_in <- function(amt, contribute, time){
        balance <- amt + contribute * time
        return(balance)
}

#=================================
# Server main function
#=================================
shinyServer(
        function(input, output) {

                #=================================
                # Tab "Time Horizon"
                #=================================
                sliderValues <- reactive({
                        # Compose data frame
                        data.frame(
                                'current age' = as.character(input$age_slide[1]),
                                'retiring age' = as.character(input$age_slide[2]),
                                period = paste(time(input$age_slide), 'years'))#,
                        #stringsAsFactors=FALSE)
                }) 
                
                # Show the values using an HTML table
                output$time <- renderTable({
                        sliderValues()
                })
                
                output$joke <- renderText({
                        if (input$age_slide[1] <20) j1 <- "You are one smart kid!"
                        else j1 <- NULL
                        if (input$age_slide[2] >70) j2 <- "Wow, you are really hardworking!"
                        else j2 <- NULL
                        if (time(input$age_slide) == 0) j3 <- "Lucky you! No more working!"
                        else j3 <- NULL 
                        return(c(j1,j2, j3))
                })
#                 
#                 # NVD3 charts from rCharts
#                 output$investChart <- renderChart2({ #renderChart does not display properly
#                         if (input$chart_type == "pie chart"){
#                                 n <- nPlot(x = "type", y = "percent", 
#                                            data = invest[invest$age==input$age_slide[1],], 
#                                            type = "pieChart")
#                                 # n$addParams(dom = 'myChart')
#                                 n$chart(margin = list(left = 50))
#                                 n$params$width = 400
#                                 n$params$height = 400
#                         }
#                         if (input$chart_type == "stacked area chart"){
#                                 n <- nPlot(percent ~ age, group = "type", data = invest, 
#                                            type = "stackedAreaChart")
#                                 # n$addParams(dom = 'myChart')
#                                 n$chart(useInteractiveGuideline = TRUE)
#                                 n$yAxis( axisLabel = "Percentage" )
#                                 n$xAxis( axisLabel = "Age")
#                                 n$chart(margin = list(left = 80))
#                                 n$params$width = 500
#                                 n$params$height = 400
#                                 
#                         }
#                         
#                         return(n)
#                 })
#                 
                # NVD3 charts from rCharts
                output$investChart <- renderChart2({ #renderChart does not display properly
                    if(input$chart_metric == "years till retirement" & input$chart_type == "pie chart"){
                        n <- n_chart(input = input$chart_type,
                                     invest_data = invest2[invest2$variable==time(input$age_slide),])
                    }
                    if(input$chart_metric == "years till retirement" & input$chart_type == "stacked area chart"){
                        n <- n_chart(input = input$chart_type,
                                     invest_data = invest2,
                                     axis_name = "Years Till Retirement")
                        
                        # reverse x axis, thanks to timelyportfolio
                        #could use R to calculate the domain for the x axis scale
                        n$chart( xDomain = sort(range(invest2$variable),decreasing=T) )
                    }
                    
                    if(input$chart_metric == "age" & input$chart_type == "pie chart"){
                        n <- n_chart(input = input$chart_type,
                                     invest_data = invest[invest$variable==input$age_slide[1],])
                    }
                    if(input$chart_metric == "age" & input$chart_type == "stacked area chart"){
                        n <- n_chart(input = input$chart_type,
                                     invest_data = invest,
                                     axis_name = "Age")
                    }
                    
                         return(n)
                })                
                #=================================
                # Tab "Historical Saving Rate"
                #=================================
                output$inputValue1 <- renderPrint({paste0(as.character(input$current_save_rate), '%')})
                
                e <- environment() # need to capture the environment for ggplot
            
                # plot historical data and add line for user's current saving rate
                output$rate_plot <- renderPlot({
                        p <- ggplot(data=save_rate[save_rate$year >= input$year[1] & save_rate$year <= input$year[2], ], 
                                    environment = e, aes(x=observation_date, y=PSAVERT)) + 
                                theme_minimal() + xlab("Obervation Date") + 
                                ylab("Annual Personal Saving Rate (%)") +
                                geom_line(col = input$linecolor, lty = input$linetype)
                        p <- p + geom_hline(aes(yintercept=input$current_save_rate))
                        return(p)
                })
                
                #=================================
                # Tab "Saving Calculator"
                #=================================
                calc_values <- reactive({
                        # Compose data frame
                        data.frame(principal = paste0('$',  input$current_save_amount),
                                   contribution = paste0('$', input$contribution),
                                   rate = paste0(input$compound_rate, '%'),
                                   cycle = input$comp_cycle,
                                   time = paste(time(input$age_slide), 'years'))#,
                        #stringsAsFactors=FALSE)
                }) 
                
                # Show the values using an HTML table
                output$calc_table <- renderTable({
                        calc_values()
                })        
                
                output$calc_result <- renderText({
                        if (input$comp_cycle == "monthly") {
                               b <- comp_int_calc(input$current_save_amount,
                                                  input$compound_rate/100, 
                                                  cycle = 12,
                                                  time = time(input$age_slide),
                                                  input$contribution/12)
                        } 
                        if (input$comp_cycle == "annually") {
                                b <- comp_int_calc(input$current_save_amount,
                                                   input$compound_rate/100, 
                                                   cycle = 1,
                                                   time = time(input$age_slide),
                                                   input$contribution)
                        } 
                        return(paste0('$',as.character(round(b,2))))
                })
                
                growValues1 <- reactive({ # these operations are not allowed unless in a reactive function
                        # Compose data frame
                        dt <- data.frame(Age = input$age_slide[1]:input$age_slide[2],
                                   current = input$age_slide[1] 
                                ) 
                        if (input$comp_cycle == "monthly") {
                                dt$Principal_Amount <- p_grow_calc(input$current_save_amount,
                                                           input$compound_rate/100, 
                                                           cycle = 12,
                                                           time = dt$Age - dt$current)
                                dt$Contribution_Amount <- c_grow_calc(input$compound_rate/100, 
                                                           cycle = 12,
                                                           time = dt$Age - dt$current,
                                                           input$contribution/12)
                                ## for line chart
                                #dt$Amount <- comp_int_calc(input$current_save_amount,
                                #                   input$compound_rate/100, 
                                #                   cycle = 12,
                                #                   time = dt$Age - dt$current,
                                #                   input$contribution/12)
                                
#                                 # for second chart
#                                 dt$money_put_in <- money_put_in(amt = input$current_save_amount,
#                                                                 contribute = input$contribution,
#                                                                 time = dt$Age - dt$current)
#                                 
#                                 dt$money_comp <- comp_int_calc(input$current_save_amount,
#                                                                input$compound_rate/100,
#                                                                cycle = 12,
#                                                                time = dt$Age - dt$current,
#                                                                input$contribution/12) - dt$money_put_in
                        } 
                        if (input$comp_cycle == "annually") {
                                dt$Principal_Amount <- p_grow_calc(input$current_save_amount,
                                                                   input$compound_rate/100, 
                                                                   cycle = 1,
                                                                   time = dt$Age - dt$current)
                                dt$Contribution_Amount <- c_grow_calc(input$compound_rate/100, 
                                                                        cycle = 1,
                                                                        time = dt$Age - dt$current,
                                                                        input$contribution)
                                
#                                 # for second chart
#                                 dt$money_put_in <- money_put_in(amt = input$current_save_amount,
#                                                                 contribute = input$contribution,
#                                                                 time = dt$Age - dt$current)
#                                 
#                                 dt$money_comp <- comp_int_calc(input$current_save_amount,
#                                                                input$compound_rate/100,
#                                                                cycle = 1,
#                                                                time = dt$Age - dt$current,
#                                                                input$contribution) - dt$money_put_in
#                                 
                        # for line chart instead of stacked area chart        
                                #dt$Amount <- comp_int_calc(input$current_save_amount,
                                #                   input$compound_rate/100, 
                                #                   cycle = 1,
                                #                   time = dt$Age - dt$current,
                                #                   input$contribution)
                        } 
                        #dt$amt_monthly <- comp_int_calc(input$current_save_amount,
                        #                                input$compound_rate/100, 
                        #                                cycle = 12,
                        #                                time = dt$age-dt$current,
                        #                                input$contribution/12)
                        # dt$amt_yearly <- comp_int_calc(input$current_save_amount,
                        #                                input$compound_rate/100, 
                        #                                cycle = 1,
                        #                                time = dt$age-dt$current,
                        #                                input$contribution)
                        #h <- hPlot(x = "Age", y = "Amount", data = dt, type = "line")
                        #h$chart(useInteractiveGuideline = TRUE)
                        #h$yAxis( axisLabel = "Project Amount ($)" )
                        #h$xAxis( axisLabel = "Age" )
                        #h$chart(margin = list(left = 80))
                        #h$params$width = 400
                        #h$params$height = 400
                        #return(h) 
                        
                        m <- melt(dt, id=c('Age', 'current')) # from reshape2 package
                        return(m)
#                         n2 <- nPlot(value ~ Age, group = "variable", data = m, 
#                                    type = "stackedAreaChart")
#                         n2$chart(useInteractiveGuideline = TRUE)
#                         n2$yAxis( axisLabel = "Projected Amount ($)" )
#                         n2$xAxis( axisLabel = "Age" )
#                         n2$chart(margin = list(left = 80))
#                         n2$params$width = 500
#                         n2$params$height = 400
#                         return(n2)
                })
                output$growChart1 <- renderChart2({ #renderChart does not display properly
                        plot <- growValues1()
                        
                        n1 <- nPlot(value ~ Age, group = "variable", data = plot,
                                    type = "stackedAreaChart")
                        n1$chart(useInteractiveGuideline = TRUE)
                        n1$yAxis( axisLabel = "Projected Amount ($)" )
                        n1$xAxis( axisLabel = "Age" )
                        n1$chart(margin = list(left = 80))
                        n1$params$width = 600
                        n1$params$height = 400
                        return(n1)
                })


                growValues2 <- reactive({ # for second chart
                    # Compose data frame
                    dt <- data.frame(Age = input$age_slide[1]:input$age_slide[2],
                                     current = input$age_slide[1] 
                    ) 
                    if (input$comp_cycle == "monthly") {
        
                        dt$Money_Put_In <- money_put_in(amt = input$current_save_amount,
                                                        contribute = input$contribution,
                                                        time = dt$Age - dt$current)
                        
                        dt$Money_From_Growth <- comp_int_calc(input$current_save_amount,
                                                       input$compound_rate/100,
                                                       cycle = 12,
                                                       time = dt$Age - dt$current,
                                                       input$contribution/12) - dt$Money_Put_In
                    } 
                    if (input$comp_cycle == "annually") {
                        
                        dt$Money_Put_In <- money_put_in(amt = input$current_save_amount,
                                                        contribute = input$contribution,
                                                        time = dt$Age - dt$current)
                        
                        dt$Money_From_Growth <- comp_int_calc(input$current_save_amount,
                                                       input$compound_rate/100,
                                                       cycle = 1,
                                                       time = dt$Age - dt$current,
                                                       input$contribution) - dt$Money_Put_In
        
                    }
                    m <- melt(dt, id=c('Age', 'current')) # from reshape2 package
                    return(m)
         
                }) 
                        
                output$growChart2 <- renderChart2({ #renderChart does not display properly
                        plot <- growValues2()
                        
                        n2 <- nPlot(value ~ Age, group = "variable", data = plot,
                                    type = "stackedAreaChart")
                        n2$chart(useInteractiveGuideline = TRUE)
                        n2$yAxis( axisLabel = "Projected Amount ($)") #, axisLabelDistance = 1 ) # tried to move axis label, didn't work
                        n2$xAxis( axisLabel = "Age")
                        n2$chart(margin = list(left = 80))
                        n2$params$width = 600
                        n2$params$height = 400
                        return(n2)
                })
 
        }
)
library(foreign)
library(survey)
library(tidyjson)
library(tidyr)
library(dplyr)
library(broom)
#library(devtools)
library(haven)
library(nopaco)
library(mitools)
library(UpSetR)
library(vcd)
library(readr)
#devtools::install_github("martinctc/surveytoolbox")
library(surveytoolbox)
library(sqldf)
library(ggplot2)
library(NLP)
library(stargazer)
library(jtools)
library(data.table)
library(shiny)
library(purrr)
library(stringr)
#library(RWeka)
#library(xlsx)
library(shiny)

# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

mydata <- read.csv("marketing_AB.csv")

colnames(mydata) <- c("index", 
                      "user_id",
                      "test_group",
                      "converted_boolean",
                      "total_ads",
                      "most_ads_day",
                      "most_ads_hour"
)

mydata$converted_binary <- c()
mydata$converted_binary[mydata$converted_boolean == "True"] <- 1
mydata$converted_binary[mydata$converted_boolean == "False"] <- 0

mydata$test_group_binary <- 0
mydata$test_group_binary[mydata$test_group == "ad"] <- 1

test_group_binom <- binom.test(nrow(mydata[mydata$test_group_binary == 1 & mydata$converted_binary == 1,]),nrow(mydata[mydata$test_group_binary == 1,]))

control_group_binom <- binom.test(nrow(mydata[mydata$test_group_binary == 0 & mydata$converted_binary == 1,]),nrow(mydata[mydata$test_group_binary == 0,]))


plot_data <- data.frame("prop" = c(round(test_group_binom$estimate[[1]]*100, digits = 2), 
                                   round(control_group_binom$estimate[[1]]*100, digits = 2)),
                        "group" = c("Ad", "PSA"),
                        "lower_ci" = c(test_group_binom$conf.int[[1]]*100,
                                       control_group_binom$conf.int[[1]]*100),
                        "upper_ci" = c(test_group_binom$conf.int[[2]]*100,
                                       control_group_binom$conf.int[[2]]*100))

# ab_table_chisquare <- table(mydata$test_group,
#                   mydata$converted_boolean)
# ab_table_chisquare
# 
# conversion_chisquare <- chisq.test(ab_table_chisquare)

ab_prop_success <- c(nrow(mydata[mydata$test_group_binary == 1 & 
                                   mydata$converted_binary == 1,]),
                     nrow(mydata[mydata$test_group_binary == 0 & 
                                   mydata$converted_binary == 1,]))

ab_prop_totals <- c(nrow(mydata[mydata$test_group_binary == 1,]),
                    nrow(mydata[mydata$test_group_binary == 0, ]))

conversion_chisquare <- prop.test(ab_prop_success, ab_prop_totals)


conversion_logit <- glm(converted_binary ~ test_group_binary,
                data = mydata,
                family = "binomial")

conversion_lpm <- glm(converted_binary ~ test_group_binary,
              data = mydata)
# 
# test <- ggplot(plot_data, 
#                aes(x= group,
#                    y = prop)) +
#   geom_bar(position = "dodge", stat = "identity", fill = c(cbPalette[1], cbPalette[2])) + 
#   geom_label(
#     aes(label = prop, y = upper_ci + .1),
#     position = position_dodge(0.9),
#     vjust = 0
#   ) +
#   geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), 
#                 width = .2, 
#                 linewidth = 1,
#                 position=position_dodge(.9))+
#   labs(x="", y ="")+
#   scale_fill_discrete(name = "Condition:")+ theme_classic()+ 
#   theme(legend.position="top") +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   ggtitle("Conversion Rate by Test Group") +
#   theme(axis.text.x = element_text(size=12, face="bold", color = "black"),
#         axis.text.y = element_text(size=12, face="bold", color = "black"))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("A/B Testing Dashboard"),

    fluidRow(
      column(6,
             plotOutput("ab_plot")),
      
      column(6,
             #verbatimTextOutput("ab_table")
             div(tableOutput("ab_table"), style = "font-size:160%")
             )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$ab_plot <- renderPlot({
    ab_plot()
  })
  
  ab_plot <- reactive({
    ggplot(plot_data, 
           aes(x= group,
               y = prop)) +
      ylim(0, max(plot_data$prop) + .5) +
      geom_bar(position = "dodge", stat = "identity", fill = c(cbPalette[1], cbPalette[2])) + 
      geom_text(
        aes(label = prop, y = upper_ci + .1),
        position = position_dodge(0.9),
        vjust = 0,
        size = 12
      ) +
      geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), 
                    width = .2, 
                    linewidth = 1,
                    position=position_dodge(.9))+
      labs(x="", y ="")+
      scale_fill_discrete(name = "Condition:")+ theme_classic()+ 
      theme(legend.position="top") +
      theme(plot.title = element_text(hjust = 0.5,
                                      size = 24)) +
      ggtitle("Conversion Rate by Test Group") +
      theme(axis.text.x = element_text(size=24, face="bold", color = "black"),
            axis.text.y = element_text(size=24, face="bold", color = "black"))
  })
  
  output$ab_table <- renderTable({
    ab_table()
  })
  
  ab_table <- reactive({
    ab_table <- data.frame("method" = c("Chi-Square", "Logit", "LPM"),
                           "conclusion" = c("test", "test", "test"))
    
    chi_square_conclusion <- function(conversion_chisquare_output){
      if(conversion_chisquare_output$p.value > .05){return("No difference")}else{
        if(conversion_chisquare_output$estimate[1] > conversion_chisquare_output$estimate[2])
        {return("Success")}else{return("Failure")}
      }
    }
    
    logit_conclusion <- function(conversion_logit_output){
      if(summary(conversion_logit_output)$coefficients[8] > .05){return("No difference")}else{
        if(summary(conversion_logit_output)$coefficients[2] > 0){return("Success")}else{return("Failure")}
      }
    }
    
    lpm_conclusion <- function(conversion_lpm_output){
      if(summary(conversion_lpm_output)$coefficients[8] > .05){return("No difference")}else{
        if(summary(conversion_lpm_output)$coefficients[2] > 0){return("Success")}else{return("Failure")}
      }
    }
    #ab_table$conclusion <- "test"
    ab_table$conclusion[ab_table$method == "Chi-Square"] <- chi_square_conclusion(conversion_chisquare)
    
    ab_table$conclusion[ab_table$method == "Logit"] <- logit_conclusion(conversion_logit)

    ab_table$conclusion[ab_table$method == "LPM"] <- logit_conclusion(conversion_lpm)
    
    colnames(ab_table) <- c("Method", "Conclusion")
    
    return(ab_table)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

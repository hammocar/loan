#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(hammond)
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Cabin Loan"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
         sidebarPanel(
            sliderInput("purchase_price",
                        "Purchase Price:",
                        min = 30000,
                        max = 82000,
                        value = 65000,
                        step = 1000),
            sliderInput("closing_costs",
                        "Closing Costs:",
                        min = 0,
                        max = 10000,
                        value = 4626.43,
                        step = 100),
            sliderInput("cash_to_close",
                        "Cash:",
                        min = 15000,
                        max = 35000,
                        value = 20000,
                        step = 500),
            sliderInput("property_taxes",
                        "Property Taxes:",
                        min = 0,
                        max = 500,
                        value = 138.42,
                        step = 50),
            sliderInput("home_insurance",
                        "Home Insurance:",
                        min = 0,
                        max = 500,
                        value = 66.67,
                        step = 50),
            sliderInput("samirya_monthly",
                        "Samirya $/month:",
                        min = 300,
                        max = 2000,
                        value = 500,
                        step = 10),
            sliderInput("carly_monthly",
                        "Carly $/month:",
                        min = 300,
                        max = 2000,
                        value = 500,
                        step = 10)
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("Plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$Plot <- renderPlot({
        purchase_price<- input$purchase_price
        closing_costs<- input$closing_costs
        cash_to_close<- input$cash_to_close
        loan_amount<-purchase_price+closing_costs-cash_to_close
        
        property_taxes<-input$property_taxes
        home_insurance<- input$home_insurance
        
        interest_rate<- 7.25
        c = (interest_rate/(100*12))
        n = 15*12
        min_payment<-(loan_amount*(((interest_rate/100)/12)*((1 + ((interest_rate/100)/12))^(15*12))))/(((1 + ((interest_rate/100)/12))^(15*12) )- 1)
        `min_payment_plus_tax&ins` <- min_payment + property_taxes + home_insurance
        
        new_payment <- (input$carly_monthly + input$samirya_monthly)- home_insurance - property_taxes
        # initialize output variables
        new_interest = new_principal = new_balance = interest = principal = balance = vector("numeric", n)
        
        # calc amortization schedule
        outstanding_principal = loan_amount
        new_outstanding_principal = loan_amount
        for (i in 1:n) {
            intr = outstanding_principal * c # interest to be paid
            prnp = min_payment - intr  # principal to be paid
            outstanding_principal = outstanding_principal - prnp  # principal left
            
            new_intr = new_outstanding_principal * c # interest to be paid
            new_prnp = new_payment - new_intr  # principal to be paid
            new_outstanding_principal = new_outstanding_principal - new_prnp
            
            
            interest[i]  = intr
            principal[i] = prnp
            balance[i] = outstanding_principal
            
            new_interest[i]  = new_intr
            new_principal[i] = new_prnp
            new_balance[i] = new_outstanding_principal
        }
        
        
        data<-tibble(n = 1:n, 
                     balance = balance,
                     new_balance = new_balance,
                     year = n/12,
                     interest = interest,
                     principal = principal,
                     new_interest = new_interest,
                     new_principal = new_principal)
        ggplot(data,aes(x = year))+
            geom_line(aes(y = balance))+
            geom_line(aes(y = new_balance),color = "green")+
            lims(y = c(0,loan_amount))+
            scale_x_continuous(breaks = seq(1,15,2), labels = seq(1,15,2))+
            ggtitle(paste("Ammortization Schedule for $", loan_amount, "loan @ ", interest_rate,"% interest for 15 years"),
                          subtitle = paste("Minimum Payment of $", round(`min_payment_plus_tax&ins`), "for loan + tax&ins"))+
            mytheme()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

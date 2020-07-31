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
library(DT)
library(hammond)
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Cabin Loan"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
         sidebarPanel(
            sliderInput("closing_costs",
                        "Closing Costs:",
                        min = 0,
                        max = 10000,
                        value = 1054.03,
                        step = 100),
            sliderInput("cash_to_close",
                        "Cash:",
                        min = 15000,
                        max = 35000,
                        value = 18500,
                        step = 500),
            sliderInput("property_taxes",
                        "Property Taxes:",
                        min = 0,
                        max = 500,
                        value = 143,
                        step = 5),
            sliderInput("home_insurance",
                        "Home Insurance:",
                        min = 0,
                        max = 100,
                        value = 57,
                        step = 5),
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
                        value = 1250,
                        step = 10)
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("Plot"),
           dataTableOutput("table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {


    output$Plot <- renderPlot({
        purchase_price<- 70000
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
            ggtitle(paste("Ammortization Schedule for $", loan_amount, "loan \n@ ", interest_rate,"% interest for 15 years"),
                          subtitle = paste("Minimum Payment of $", round(`min_payment_plus_tax&ins`), "for loan + tax&ins"))+
            mytheme()
    })

 output$table <- renderDataTable({
     purchase_price<- 70000
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
     data %>% 
         summarise(`Scheduled total interest` = paste("$",round(sum(interest),2), sep = ""),
                   `New total interest` = paste("$",round(sum(new_interest[which(new_interest > 0)]),2), sep = ""),
                   `Time to pay off` = as.character(paste(year[which(new_balance == max(new_balance[which(new_balance <= 0)]))] %/% 1,
                                "years",
                                round(((year[which(new_balance == max(new_balance[which(new_balance <= 0)]))] - (year[which(new_balance == max(new_balance[which(new_balance <= 0)]))] %/% 1))/(1/12))),
                                "months",
                                sep = " "))) 
                                     },options=list(iDisplayLength=5,                    # initial number of records
                                                     dom = "ft",
                                                     aLengthMenu=c(5,10),                  # records/page options
                                                     bLengthChange=0,                       # show/hide records per page dropdown
                                                     bFilter=0,                                    # global search box on/off
                                                     bInfo=0,                                      # information on/off (how many records filtered, etc)
                                                     bAutoWidth=4), rownames = FALSE)
}

#round((6.6 - (6.6 %/% 1))/ (1/12))

 # Run the application 
shinyApp(ui = ui, server = server)

library(ggplot2)
library(dplyr)
library(hammond)
purchase_price<- 65000
closing_costs<- 4626.43
cash_to_close<- 25000
loan_amount<-purchase_price+closing_costs-cash_to_close

property_taxes<-138.42
home_insurance<- 66.67

interest_rate<- 7.25
c = (interest_rate/(100*12))
n = 15*12
min_payment<-(loan_amount*(((interest_rate/100)/12)*((1 + ((interest_rate/100)/12))^(15*12))))/(((1 + ((interest_rate/100)/12))^(15*12) )- 1)
`min_payment_plus_tax&ins` <- min_payment + property_taxes + home_insurance

extra<-rep(c(500,600,700,800,900,1000), each = 180/6)
extra<-rep(1000,180)
# initialize output variables
interest = principal = balance = vector("numeric", n)

# calc amortization schedule
outstanding_principal = loan_amount
new_outstanding_principal = loan_amount
for (i in 1:n) {
  intr = outstanding_principal * c # interest to be paid
  prnp = min_payment - intr  # principal to be paid
  outstanding_principal = outstanding_principal - prnp  # principal left

  new_intr = new_outstanding_principal * c # interest to be paid
  new_prnp = min_payment - new_intr  # principal to be paid
  new_outstanding_principal = new_outstanding_principal - prnp - extra[i]
  
  
  interest[i]  = intr
  principal[i] = prnp
  balance[i] = outstanding_principal
  
  new_interest[i]  = intr
  new_principal[i] = prnp
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
  mytheme()



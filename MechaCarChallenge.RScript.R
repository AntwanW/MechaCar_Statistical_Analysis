#load packages
library(dplyr)

# Deliverable 1
# Technical Analysis ###

#Import and read the csv file
mechcar <- read.csv(file = 'MechaCar_mpg.csv')

#linear regression for all variables
lm(mpg ~ vehicle_weight + 
     spoiler_angle + 
     ground_clearance + 
     AWD + 
     vehicle_length,data=mechcar)

#p-value and r-squared  for all variables
summary(lm(mpg ~ vehicle_weight + 
             spoiler_angle + 
             ground_clearance + 
             AWD + 
             vehicle_length,data=mechcar))

# Deliverable 2

#Import and read the csv file
suspen_coil <- read.csv(file = 'Suspension_Coil.csv')

#Create total summary df, using summarize()
total_summary <-  suspen_coil %>% summarize(Mean=mean(PSI), 
                                            Median=median(PSI),
                                            Variance=var(PSI),SD=sd(PSI))


#Create lot summary df, using group_by() & summarize()
lot_summary <- suspen_coil %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI),
                                                                         Median=median(PSI),
                                                                         Variance=var(PSI),SD=sd(PSI))


#  Deliverable 3

#t-test across all manufacturing lots against the population mean = 1500 PSI
t.test(suspen_coil$PSI, mu=1500)


#t-test lot1 against population mean = 1500 PSI
lot1 <- suspen_coil %>% 
  subset(Manufacturing_Lot=="Lot1")
t.test(lot1$PSI, mu=1500)

#t-test lot2 against population mean = 1500 PSI
lot2 <- suspen_coil %>% 
  subset(Manufacturing_Lot=="Lot2")
t.test(lot2$PSI, mu=1500)

#t-test lot3 against population mean = 1500 PSI
lot3 <- suspen_coil %>% 
  subset(Manufacturing_Lot=="Lot3")
t.test(lot3$PSI, mu=1500)


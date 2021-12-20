#creating a visualizations to analyze the annual income of the customers. 

customer_data=read.csv("/Mall_Customers.csv")
summary(customer_data$Annual.Income..k..)

#ploting a histogram and then proceed to examine this data using a density 
hist(customer_data$Annual.Income..k..,
      col="#0867cc",
      main="Histogram for Annual Income",
      xlab="Annual Income Class",
      ylab="Frequency",
      labels=TRUE)


#density
plot(density(customer_data$Annual.Income..k..),
     col="yellow",
     main="Density Plot for Annual Income",
     xlab="Annual Income Class",
     ylab="Density")
polygon(density(customer_data$Annual.Income..k..),
        col="#257b8a")
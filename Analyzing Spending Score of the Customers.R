customer_data=read.csv("/Mall_Customers.csv")

summary(customer_data$Spending.Score..1.100.)
#boxplot
boxplot(customer_data$Spending.Score..1.100.,
        horizontal=TRUE,
        col="#7e42f5",
        main="BoxPlot for Descriptive Analysis of Spending Score")


#histogram
hist(customer_data$Spending.Score..1.100.,
     main="HistoGram for Spending Score",
     xlab="Spending Score Class",
     ylab="Frequency",
     col="#8103ab",
     labels=TRUE)
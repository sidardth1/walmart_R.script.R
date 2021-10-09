#Walmart_analysis


# Loading walmart_data and walmart_features files 

walmart_data = './rstudio project/walmart_data.csv'
walmart_features =  './rstudio project/walmart_features.csv'


walmart_data <- read.csv(file = walmart_data)
walmart_features <- read.csv(file = walmart_features)


# checking number of rows of walmart_data 

n_rows = nrow(walmart_data)

# first 5 rows of the dataframe walmart_data

head(walmart_data,n = 5)


# number of rows per store using the Table

rows_per_store =  table(walmart_data$Store)


# Convert rows_per_store to a data frame

rows_per_store = as.data.frame(rows_per_store)


# Medium Level Exercises

# store having the most rows

which.max(rows_per_store$Freq)

or

rows_per_store[ order(-rows_per_store$Freq) , ][1,]


# Sum the sales by store on walmart_data

sum_by_store = 
  aggregate(x = walmart_data$Weekly_Sales , by = list(walmart_data$Store),FUN= sum)


# renaming the columns of sum_by_store

colnames(sum_by_store) <- c('store_number',"total_sales")


# Ploting a bar plot using base r , 
#sorting total sales from sales with most sales to stores least sales 

plot <- table(sum_by_store)

barplot(height = plot)


#Mean of every column in walmart_features

sapply(X = walmart_features, FUN = mean)

 
# Calculating standardized_cpi: by subtracting the mean and dividing by the 
# standard deviation

walmart_features$standardized_cpi <-  
  walmart_features$CPI - mean(walmart_features$CPI, na.rm = TRUE)/ sd(walmart_features$CPI,na.rm = TRUE)


# Producing a line plot for sales of store number 1 for every department

agg <- walmart_data[walmart_data$Store==1,]


store_1_data <- aggregate( x = agg$Weekly_Sales , by = list(agg$Dept), FUN = sum )

store_1_plotting <- plot( x= store_1_data$Group.1 , y = store_1_data$x )

line(store_1_data$Group.1, store_1_data$x)


# Ploting the sales for the top 5 departments with more sales
# for store 2 with ggplot2 


store_2 <-  walmart_data[ walmart_data$Store ==2,]

store_2sale <-  aggregate(x = store_2$Weekly_Sales, 
                          by= list(store_2$Dept), FUN = sum)

top_5_dept =  store_2sale[order(-store_2sale$x) ,'Group.1' ][1:5]

top_5_Dept_sales = store_2[ top_5_dept %in% store_2$Dept, ]


ggplot(
  top_5_Dept_sales, 
  aes(x=Date, y=Weekly_Sales, group=Dept)
) + geom_line()

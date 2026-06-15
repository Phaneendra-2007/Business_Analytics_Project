library(ggplot2)
library(dplyr)

# Load Dataset
sales <- read.csv("sales_data.csv")

# Data Cleaning
sales <- na.omit(sales)

# Revenue Calculation
sales$Revenue <- sales$Quantity * sales$Price

# Save Cleaned Dataset
write.csv(sales,
          "cleaned_sales_data.csv",
          row.names = FALSE)

# Monthly Sales Trend
monthly_sales <- sales %>%
  group_by(Month) %>%
  summarise(TotalRevenue = sum(Revenue))

png("sales_trend.png")

ggplot(monthly_sales,
       aes(x = Month,
           y = TotalRevenue,
           group = 1)) +
  geom_line(color="blue") +
  geom_point(size=3) +
  ggtitle("Monthly Sales Trend")

dev.off()

# Top Products
product_sales <- sales %>%
  group_by(Product) %>%
  summarise(TotalRevenue = sum(Revenue))

png("top_products.png")

ggplot(product_sales,
       aes(x = reorder(Product,
                       TotalRevenue),
           y = TotalRevenue,
           fill = Product)) +
  geom_bar(stat="identity") +
  coord_flip() +
  ggtitle("Top Products")

dev.off()

# Revenue by Category
category_sales <- sales %>%
  group_by(Category) %>%
  summarise(TotalRevenue = sum(Revenue))

png("revenue_by_category.png")

ggplot(category_sales,
       aes(x = Category,
           y = TotalRevenue,
           fill = Category)) +
  geom_bar(stat="identity") +
  ggtitle("Revenue by Category")

dev.off()

# Customer Purchase Analysis
customer_purchase <- sales %>%
  group_by(CustomerID) %>%
  summarise(TotalSpent = sum(Revenue))

png("customer_segments.png")

ggplot(customer_purchase,
       aes(x = CustomerID,
           y = TotalSpent)) +
  geom_bar(stat="identity",
           fill="darkgreen") +
  ggtitle("Customer Spending Analysis")

dev.off()

summary(sales)

print("Business Analytics Completed")
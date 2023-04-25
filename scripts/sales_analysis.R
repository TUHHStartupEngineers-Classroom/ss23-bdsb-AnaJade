# Data Science at TUHH ------------------------------------------------------
# SALES ANALYSIS ----

# 1.0 Load libraries ----
library(tidyverse)
library(readxl)
library(lubridate)
library(writexl)

# 2.0 Importing Files ----
bikes_tbl      <- read_excel(path = "scripts/00_data/01_raw_data/bikes.xlsx")
orderlines_tbl <- read_excel(path = "scripts/00_data/01_raw_data/orderlines.xlsx")
bikeshops_tbl  <- read_excel(path = "scripts/00_data/01_raw_data/bikeshops.xlsx")

# 3.0 Examining Data ----
orderlines_tbl
glimpse(orderlines_tbl)

# 4.0 Joining Data ----
left_join(orderlines_tbl, bikes_tbl)
left_join(orderlines_tbl, bikes_tbl, by = c("product.id" = "bike.id"))
bike_orderlines_joined_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))
bike_orderlines_joined_tbl %>% glimpse()

# 5.0 Wrangling Data ----
# Print all entries that have Mountain as their main category
bike_orderlines_joined_tbl %>% 
  select(category) %>%
  filter(str_detect(category, "^Mountain")) %>% 
  unique()

# Split Category variable in 3 sub-variables
bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>%
  separate(col    = category,
           into   = c("category.1", "category.2", "category.3"),
           sep    = " - ") %>%
  # Add a total price column to the table
  mutate(total.price = price * quantity) %>%
  # Remove gender column
  select(-...1, -gender) %>%
  # Remove columns that end with .id
  select(-ends_with(".id")) %>%
  # Re-add the column order.id to the tibble
  bind_cols(bike_orderlines_joined_tbl %>% select(order.id)) %>% 
  # Reorder the data based on the defined column order
  select(order.id, contains("order"), contains("model"), contains("category"),
         price, quantity, total.price,
         everything()) %>%
  # Rename the columns to replace dots with underscores
  rename(bikeshop = name) %>%
  set_names(names(.) %>% str_replace_all("\\.", "_"))

# 6.0 Business Insights ----
# 6.1 Sales by Year ----

# Step 1 - Manipulate
sales_by_year_tbl <- bike_orderlines_wrangled_tbl %>%
  # Keep order_date and total_price columns
  select(order_date, total_price) %>%
  # Keep only the order year
  mutate(year = year(order_date)) %>%
  # Group by year (kinda like sort)
  group_by(year) %>% 
  # Sum sales per year
  summarize(sales = sum(total_price)) %>%
  # Convert the column into the currency format
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))
sales_by_year_tbl

# Step 2 - Visualize
sales_by_year_tbl %>%
  
  # Plot with sales(year)
  ggplot(aes(x = year, y = sales)) +
  
  # Geometries
  geom_col(fill = "#2DC6D6") + # Use geom_col for a bar plot
  geom_label(aes(label = sales_text)) + # Adding labels to the bars
  geom_smooth(method = "lm", se = FALSE) + # Adding a trendline
  
  # Formatting
  # scale_y_continuous(labels = scales::dollar) + # Change the y-axis. 
  # Again, we have to adjust it for euro values
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title    = "Revenue by year",
    subtitle = "Upward Trend",
    x = "", # Override defaults for x and y
    y = "Revenue"
  )

# 6.2 Sales by Year and Category 2 ----

# Step 1 - Manipulate
sales_by_year_cat_1_tbl <- bike_orderlines_wrangled_tbl %>%
  
  # Select columns and add a year
  select(order_date, total_price, category_1) %>%
  mutate(year = year(order_date)) %>%
  
  # Group by and summarize year and main catgegory
  group_by(year, category_1) %>%
  summarise(sales = sum(total_price)) %>%
  ungroup() %>%
  
  # Format $ Text
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

sales_by_year_cat_1_tbl  

# Step 2 - Visualize
sales_by_year_cat_1_tbl %>%
  
  # Set up x, y, fill
  ggplot(aes(x = year, y = sales, fill = category_1)) +
  
  # Geometries
  geom_col() + # Run up to here to get a stacked bar plot
  
  # Facet
  facet_wrap(~ category_1) +
  
  # Formatting
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title = "Revenue by year and main category",
    subtitle = "Each product category has an upward trend",
    fill = "Main category" # Changes the legend name
  )


# 7.0 Writing Files ----

# 7.1 Excel ----
bike_orderlines_wrangled_tbl %>%
  write_xlsx("scripts/00_data/01_bike_sales/02_wrangled_data/bike_orderlines.xlsx")

# 7.2 CSV ----
bike_orderlines_wrangled_tbl %>% 
  write_csv("scripts/00_data/01_bike_sales/02_wrangled_data/bike_orderlines.csv")

# 7.3 RDS ----
bike_orderlines_wrangled_tbl %>% 
  write_rds("scripts/00_data/01_bike_sales/02_wrangled_data/bike_orderlines.rds")

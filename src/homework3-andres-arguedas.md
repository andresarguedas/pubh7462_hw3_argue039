PubH7462 - Homework 3
================
Andrés Arguedas
17/2/2022

## Instacart

First, we will start by loading the `instacart.csv` file, located in the
`/data` folder into R as the `instacart` object:

``` r
# Load the data from `instacart.csv` into R as the `instacart` object, and
# change some variables for ease of use later
instacart <- read_csv("./data/instacart.csv") %>%
  # Change the `aisle`, `department`, and `product_name` to factors, for ease of
  # use, and also change the text in the `aisle` and `department` variables to
  # be more readable
  mutate(
    aisle = factor(aisle) %>% str_to_title(),
    department = factor(department) %>% str_to_title(),
    product_name = factor(product_name)
  )
```

Having loaded the data into R, we can proceed to create a first table to
summarize some of the variables of interest in the data set.
Specifically, we will look at the overall mean, median, and standard
deviation across all orders for customers regarding their days since
last order, as well as the number of items, aisles, departments, the
order number, and finally the percent of items which have been
reordered. These summaries are presented in the following table:

``` r
# Create a table with summary statistics for the variables of interest
instacart %>%
  # Group the data by user and order
  group_by(order_id, user_id) %>%
  # Create the necessary variables corresponding to every user and order
  summarise(
    # Since the days since last order is the same for each order, and stored in
    # the `days_since_prior_order` variable, then we can pick any of these
    # values and obtain the same result
    days_since_last_order = min(days_since_prior_order),
    # The number of items bought will just be equal to the number of
    # observations for each order
    number_of_items = n(),
    # To obtain the number of aisles, we just need to obtain the unique aisles
    # in the order and count them
    number_of_aisles = length(unique(aisle_id)),
    # Using the same reasoning as before, we can also obtain the number of
    # departments in which the person bought items
    number_of_departments = length(unique(department_id)),
    # As with the days since the last order, we just need to choose any one of
    # the values of `order_number` and the result will be the same
    order_number = min(order_number),
    # Since every item has an indicator for if the item had already been ordered
    # (`reordered`), then to obtain the percentage we just need to sum the
    # values of this variable, divide over the amount of items bought, and
    # multiply by 100 to obtain a percentage
    percent_reordered = sum(reordered) / n() * 100
  ) %>%
  # Now, we ungroup to be able to summarize over all customers' orders
  ungroup() %>%
  # Drop the order and user identification variables since we won't need them
  # anymore
  dplyr::select(-order_id, -user_id) %>%
  # Obtain the mean, median, and standard deviation of every variable across all
  # customers' orders
  summarise(across(.fns = list(mean = mean, median = median, sd = sd))) %>%
  # Since we have one column for every measure for every variable, then we need
  # to pivot the data into a longer form, where every row corresponds to a
  # single variable of interest. Note: the regex used in the `names_pattern`
  # creates two groups, separated by the last underscore in the string, since
  # after the last underscore we have the summary measure (mean, median, or sd),
  # and before it we have the name of the variable
  pivot_longer(everything(),
    names_pattern = c("(.*)_([^_]+$)"),
    names_to = c("variable", ".value")
  ) %>%
  # Now, we change the `variable` variable for better presentation in the table
  mutate(variable = str_replace_all(variable, "_", " ") %>%
    str_to_title()) %>%
  # Create the `gt()` table
  gt() %>%
  # Add a title to the table
  tab_header(
    title = "Overall mean, median, and standard deviation, for six variables of
    interest, for all customers' orders in the Instacart data"
  ) %>%
  # Change the labels of the columns
  cols_label(
    variable = md("**Variable**"), mean = md("**Mean**"),
    median = md("**Median**"), sd = md("**Std. Dev.**")
  ) %>%
  # Align the columns properly, so that the name of the variable is algined to
  # the left, and all the other summary measures are centered
  cols_align(align = "left", columns = variable) %>%
  cols_align(align = "center", columns = -variable)
```

![](../figures/summary-table.png)

Moving now into more information about specific orders, we are also
interested in the total number of orders for each aisle, which is
presented in the following figure:

``` r
# Create a plot for the number of orders for each aisle
instacart %>%
  # Group the data by aisle to be able to obtain the count for each aisle
  group_by(aisle) %>%
  # Since we're interested in the number of orders, and not the number of items
  # ordered, then we have to count the number of unique orders for each aisle,
  # since each order can potentially include more than one item
  summarise(n = length(unique(order_id))) %>%
  # Now, we reorder the levels of the `aisle` variable so that aisles are
  # ordered based on the number of orders in each one
  mutate(aisle = fct_reorder(aisle, n)) %>%
  # Create the plot, and color it according to the number of orders
  ggplot(aes(y = aisle, x = n, fill = n)) +
  # Specifically, we will create a gradient scale based on the 9-class OrRd
  # color palette from Color Brewer 2, so that it can take semi-continuous
  # values corresponding to the number of orders in each aisle
  scale_fill_gradientn(colors = c(
    "#FFF7EC", "#FEE8C8", "#FDD49E", "#FDBB84", "#FC8D59", "#EF6548", "#D7301F",
    "#B30000", "#7F0000"
  )) +
  # Add bars to the plot, and give them a black border for easier separation
  geom_col(col = "black") +
  # Add titles to the plot and axes
  labs(
    title = "Total number of orders per aisle", x = "Number of orders",
    y = "Aisle"
  ) +
  # Since we have a `fill` aesthetic, we eliminate the legend that appears,
  # since it just corresponds to the values in the x-axis
  theme(legend.position = "none")
```

![](homework3-andres-arguedas_files/figure-gfm/orders-per-aisle-1.png)<!-- -->

As we can see there are a large number of aisles which have a small
amount of orders, hence we can further reduce the information from the
above plot by focusing only on the top 6 aisles in the top 6
departments, according to the total number of items purchased, which is
presented below:

``` r
# First, we need to determine what the top 6 departments in terms of total
# number of items purchased are, to be able to only choose them in the plot
top_6_dep <- instacart %>%
  # First, group by `department` since we need to obtain the number of items per
  # department
  group_by(department) %>%
  # Obtain the number of items in each department by counting the number of rows
  # in each department
  summarise(n_items_dep = n()) %>%
  # Arrange the departments, in a descending order, according to the number of
  # items ordered in said department
  arrange(desc(n_items_dep)) %>%
  # Since we're only interested in the top 6 departments, then we can just pick
  # the first 6 rows from the ordered tibble
  slice_head(n = 6) %>%
  # Finally, since we only need the names of the departments, then we can use
  # `select()` and `pull()` to obtain the desired department names as a vector
  dplyr::select(department) %>%
  pull()

# Since we already know what the top 6 departments are by number of items
# purchased, then we can now determine what the top 6 aisles are in them, and
# how many items have been purchased in them, which we plot below
instacart %>%
  # Leave only the observations corresponding to items purchased in the top 6
  # departments obtained above
  filter(department %in% top_6_dep) %>%
  # Group by both department and aisle, since we're interested in the items
  # purchased for each aisle in each department
  group_by(department, aisle) %>%
  # Now, we can calculate the number of items purchased in each aisle as the
  # corresponding number of observations
  summarise(n_items_aisle = n()) %>%
  # We need to ungroup to be able to change the ordering of the department
  # variable
  ungroup() %>%
  # Order the `department` variable in a descending order according to the total
  # number of items purchased
  mutate(department = fct_reorder(department, n_items_aisle,
    .fun = sum,
    .desc = TRUE
  )) %>%
  # Then we group by department again, since we're now interested in the top 6
  # aisles in each department
  group_by(department) %>%
  # Order the `aisle` variable in a descending order according to the number of
  # items purchased in said aisle
  arrange(desc(n_items_aisle), by_group = TRUE) %>%
  # We take only the first six aisles, for every department, of the ordered data
  # set, which means the six aisles with the most items purchased in each of the
  # desired departments
  slice_head(n = 6) %>%
  # we ungroup again to be able to reorder the `aisle` variable
  ungroup() %>%
  # Reorder the levels in the `aisle` variable so that they are ordered
  # according to the number of items purchased
  mutate(aisle = fct_reorder(aisle, n_items_aisle, identity)) %>%
  # Create the desired figure, coloring the bars according to the corresponding
  # department, which are also ordered according to the total number of items
  # purchased in them
  ggplot(aes(x = n_items_aisle, y = aisle, fill = department)) +
  # Add a black border to the bars so that they can be easily differentiated
  geom_col(col = "black") +
  # Add titles to the plot, axes, and the legend
  labs(
    title = "Top 6 aisles in the top 6 departments with most items
    purchased, by number of items purchased",
    y = "Aisle", x = "Number of items purchased", fill = "Department"
  )
```

![](homework3-andres-arguedas_files/figure-gfm/top-6-dep-aisles-1.png)<!-- -->

Finally, we can also focus on the top 5 items bought in the top 5
aisles, once again by total number of products bought, which is
presented in the following table:

``` r
# As with the top 6 departments, we first need to determine which are the top 5
# aisles with most items purchased
top_5_aisle <- instacart %>%
  # Group by aisle since we're interested in the total number of items purchased
  # in each aisle
  group_by(aisle) %>%
  # Calculate the number of items purchased in each aisle as the number of
  # observations for each one
  summarise(n_items_purchased = n()) %>%
  # Add a new variable for compatibility with the data set used for the table
  mutate(product_name = "Total") %>%
  # Arrange the aisles, in a descending order, according to the total number of
  # items purchased in each one
  arrange(desc(n_items_purchased)) %>%
  # Leave only the top five aisles with most items purchased
  slice_head(n = 5)

# Create a table with the top 5 products purchased in the top 5 aisles
instacart %>%
  # Leave only the observations corresponding to the top 5 aisles
  filter(aisle %in% top_5_aisle$aisle) %>%
  # Group by aisle and product, since we want to know the number of purchases
  # for each item in each aisle
  group_by(aisle, product_name) %>%
  # Calculate the number of purchases for each item in each aisle as the
  # corresponding number of observations
  summarise(n_items_purchased = n()) %>%
  # Ungroup to be able to order the `aisle` variable
  ungroup() %>%
  # Add the total number of items purchased for each of the top 5 aisles
  bind_rows(top_5_aisle) %>%
  # Reorder the aisles so that they are ordered according to the total number of
  # items ordered in each one
  mutate(aisle = fct_reorder(aisle, n_items_purchased,
    .fun = sum,
    .desc = TRUE
  )) %>%
  # Now, we group again by aisle to be able to determine the top 5 items
  group_by(aisle) %>%
  # Arrange the products in a descending order, according to the number of items
  # purchased
  arrange(desc(n_items_purchased)) %>%
  # Although we're interested in the top 5 items, since we added the totals for
  # each aisle, then we need to take the top 6 observations for each aisle,
  # since the top one will always correspond to the total
  slice_head(n = 6) %>%
  # Create the `gt()` table
  gt() %>%
  # Add a title to the table
  tab_header(title = "Top 5 items purchased in the 5 aisles with the most total
             items purchased, by number of items purchased") %>%
  # Color the column with the number of items purchased, based on the 9-class
  # Purples palette from Color Brewer 2, taking values from 0 to the largest
  # number of items purchased in a single aisle
  data_color(
    columns = n_items_purchased,
    colors = scales::col_numeric(
      palette = c(
        "#F7FCF5", "#E5F5E0", "#C7E9C0", "#A1D99B", "#74C476", "#41AB5D",
        "#238B45", "#006D2C", "#00441B"
      ),
      domain = c(0, max(top_5_aisle$n_items_purchased))
    )
  ) %>%
  # Change the names of the columns in the table
  cols_label(
    product_name = md("**Product**"),
    n_items_purchased = md("**Number of items purchased**")
  ) %>%
  # Add a name for the grouping variable
  tab_stubhead(label = md("**Aisle**")) %>%
  # Change the alignment of the columns
  cols_align(align = "left", columns = product_name) %>%
  cols_align(align = "center", columns = n_items_purchased) %>%
  # Add the grouping variable to the left of the table. NOTE: Need the
  # development version of package `gt` to use this option!!! Run:
  # `devtools::install_github("rstudio/gt")`
  tab_options(row_group.as_column = TRUE)
```

![](../figures/top-5-table.png)

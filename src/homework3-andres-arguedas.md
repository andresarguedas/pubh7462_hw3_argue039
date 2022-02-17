PubH7462 - Homework 3
================
Andrés Arguedas
17/2/2022

-   [Instacart](#instacart)
    -   [3.1](#31)

## Instacart

### 3.1

``` r
instacart <- read_csv("data/instacart.csv") %>%
  mutate(
    aisle = factor(aisle) %>% str_to_title(),
    department = factor(department) %>% str_to_title(),
    product_name = factor(product_name)
  )
```

``` r
instacart %>%
  group_by(order_id, user_id) %>%
  summarise(
    days_since_last_order = min(days_since_prior_order),
    number_of_items = n(),
    number_of_aisles = length(unique(aisle_id)),
    number_of_departments = length(unique(department_id)),
    order_number = min(order_number),
    percent_reordered = sum(reordered) / n() * 100
  ) %>%
  ungroup() %>%
  dplyr::select(-order_id, -user_id) %>%
  summarise(across(.fns = list(mean = mean, median = median, sd = sd))) %>%
  pivot_longer(everything(),
    names_pattern = c("(.*)_([^_]+$)"),
    names_to = c("variable", ".value")
  ) %>%
  mutate(variable = str_replace_all(variable, "_", " ") %>%
    str_to_title()) %>%
  gt() %>%
  tab_header(
    title = "Overall mean, median, and standard deviation, for six variables of
    interest, for all customers' orders in the Instacart data"
  ) %>%
  cols_label(
    variable = md("**Variable**"), mean = md("**Mean**"),
    median = md("**Median**"), sd = md("**Std. Dev.**")
  ) %>%
  cols_align(align = "left", columns = variable) %>%
  cols_align(align = "center", columns = -variable)
```

![](../figures/summary-table.png)

``` r
instacart %>%
  group_by(aisle) %>%
  summarise(n = n(), n2 = length(unique(order_id))) %>%
  mutate(aisle = fct_reorder(aisle, n2)) %>%
  ggplot(aes(y = aisle, x = n2, fill = n2)) +
  # scale_fill_gradient(low = "#f7f4f9", high = "#67001f") +
  scale_fill_gradientn(colors = c(
    "#FFF7EC", "#FEE8C8", "#FDD49E", "#FDBB84",
    "#FC8D59", "#EF6548", "#D7301F", "#B30000",
    "#7F0000"
  )) +
  geom_col(col = "black") +
  labs(title = "", x = "Number of orders", y = "Aisle") +
  theme(legend.position = "none")
```

![](homework3-andres-arguedas_files/figure-gfm/orders-per-aisle-1.png)<!-- -->

``` r
top_6_dep <- instacart %>%
  group_by(department) %>%
  summarise(n_items_dep = n()) %>%
  arrange(desc(n_items_dep)) %>%
  slice_head(n = 6) %>%
  dplyr::select(department) %>%
  pull()

instacart %>%
  filter(department %in% top_6_dep) %>%
  group_by(department, aisle) %>%
  summarise(n_items_aisle = n()) %>%
  ungroup() %>%
  mutate(department = fct_reorder(department, n_items_aisle,
    .fun = sum,
    .desc = TRUE
  )) %>%
  group_by(department) %>%
  arrange(desc(n_items_aisle), by_group = TRUE) %>%
  slice_head(n = 6) %>%
  ungroup() %>%
  mutate(aisle = fct_reorder(aisle, n_items_aisle, identity)) %>%
  ggplot(aes(x = n_items_aisle, y = aisle, fill = department)) +
  geom_col(col = "black") +
  labs(
    title = "", y = "Aisle", x = "Number of items purchased",
    fill = "Department"
  )
```

![](homework3-andres-arguedas_files/figure-gfm/top-6-dep-aisles-1.png)<!-- -->

``` r
top_5_aisle <- instacart %>%
  group_by(aisle) %>%
  summarise(n_items_purchased = n()) %>%
  mutate(product_name = "Total") %>% 
  arrange(desc(n_items_purchased)) %>%
  slice_head(n = 5)

instacart %>% filter(aisle %in% top_5_aisle$aisle) %>% 
  group_by(aisle, product_name) %>% 
  summarise(n_items_purchased = n()) %>% 
  ungroup() %>%
  bind_rows(top_5_aisle) %>% 
  mutate(aisle = fct_reorder(aisle, n_items_purchased,
    .fun = sum,
    .desc = TRUE
  )) %>% 
  group_by(aisle) %>% 
  arrange(desc(n_items_purchased)) %>% 
  slice_head(n = 6) %>% 
  gt() %>% 
  tab_header(title = "") %>% 
  cols_label(product_name = md("**Product**"),
             n_items_purchased = md("**Number of items purchased**")) %>% 
  tab_stubhead(label = md("**Aisle**")) %>%
  cols_align(align = "left", columns = product_name) %>% 
  cols_align(align = "center", columns = n_items_purchased) %>%
  tab_options(row_group.as_column = TRUE)
```

    ## `summarise()` has grouped output by 'aisle'. You can override using the
    ## `.groups` argument.

<div id="mtfvqzkthh" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#mtfvqzkthh .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#mtfvqzkthh .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#mtfvqzkthh .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#mtfvqzkthh .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#mtfvqzkthh .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#mtfvqzkthh .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#mtfvqzkthh .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#mtfvqzkthh .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#mtfvqzkthh .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#mtfvqzkthh .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#mtfvqzkthh .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#mtfvqzkthh .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#mtfvqzkthh .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#mtfvqzkthh .gt_from_md > :first-child {
  margin-top: 0;
}

#mtfvqzkthh .gt_from_md > :last-child {
  margin-bottom: 0;
}

#mtfvqzkthh .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#mtfvqzkthh .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#mtfvqzkthh .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#mtfvqzkthh .gt_row_group_first td {
  border-top-width: 2px;
}

#mtfvqzkthh .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#mtfvqzkthh .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#mtfvqzkthh .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#mtfvqzkthh .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#mtfvqzkthh .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#mtfvqzkthh .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#mtfvqzkthh .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#mtfvqzkthh .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#mtfvqzkthh .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#mtfvqzkthh .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#mtfvqzkthh .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#mtfvqzkthh .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#mtfvqzkthh .gt_left {
  text-align: left;
}

#mtfvqzkthh .gt_center {
  text-align: center;
}

#mtfvqzkthh .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#mtfvqzkthh .gt_font_normal {
  font-weight: normal;
}

#mtfvqzkthh .gt_font_bold {
  font-weight: bold;
}

#mtfvqzkthh .gt_font_italic {
  font-style: italic;
}

#mtfvqzkthh .gt_super {
  font-size: 65%;
}

#mtfvqzkthh .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#mtfvqzkthh .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#mtfvqzkthh .gt_slash_mark {
  font-size: 0.7em;
  line-height: 0.7em;
  vertical-align: 0.15em;
}

#mtfvqzkthh .gt_fraction_numerator {
  font-size: 0.6em;
  line-height: 0.6em;
  vertical-align: 0.45em;
}

#mtfvqzkthh .gt_fraction_denominator {
  font-size: 0.6em;
  line-height: 0.6em;
  vertical-align: -0.05em;
}
</style>
<table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="3" class="gt_heading gt_title gt_font_normal gt_bottom_border" style></th>
    </tr>
    
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1"><strong>Aisle</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1"><strong>Product</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Number of items purchased</strong></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr class="gt_row_group_first"><td rowspan="6" class="gt_row gt_right gt_stub_row_group">Fresh Vegetables</td>
<td class="gt_row gt_left">Total</td>
<td class="gt_row gt_center">150609</td></tr>
    <tr><td class="gt_row gt_left">Organic Cucumber</td>
<td class="gt_row gt_center">4613</td></tr>
    <tr><td class="gt_row gt_left">Organic Zucchini</td>
<td class="gt_row gt_center">4589</td></tr>
    <tr><td class="gt_row gt_left">Organic Yellow Onion</td>
<td class="gt_row gt_center">4290</td></tr>
    <tr><td class="gt_row gt_left">Organic Garlic</td>
<td class="gt_row gt_center">4158</td></tr>
    <tr><td class="gt_row gt_left">Asparagus</td>
<td class="gt_row gt_center">3868</td></tr>
    <tr class="gt_row_group_first"><td rowspan="6" class="gt_row gt_right gt_stub_row_group">Fresh Fruits</td>
<td class="gt_row gt_left">Total</td>
<td class="gt_row gt_center">150473</td></tr>
    <tr><td class="gt_row gt_left">Banana</td>
<td class="gt_row gt_center">18726</td></tr>
    <tr><td class="gt_row gt_left">Bag of Organic Bananas</td>
<td class="gt_row gt_center">15480</td></tr>
    <tr><td class="gt_row gt_left">Organic Strawberries</td>
<td class="gt_row gt_center">10894</td></tr>
    <tr><td class="gt_row gt_left">Large Lemon</td>
<td class="gt_row gt_center">8135</td></tr>
    <tr><td class="gt_row gt_left">Organic Avocado</td>
<td class="gt_row gt_center">7409</td></tr>
    <tr class="gt_row_group_first"><td rowspan="6" class="gt_row gt_right gt_stub_row_group">Packaged Vegetables Fruits</td>
<td class="gt_row gt_left">Total</td>
<td class="gt_row gt_center">78493</td></tr>
    <tr><td class="gt_row gt_left">Organic Baby Spinach</td>
<td class="gt_row gt_center">9784</td></tr>
    <tr><td class="gt_row gt_left">Organic Raspberries</td>
<td class="gt_row gt_center">5546</td></tr>
    <tr><td class="gt_row gt_left">Organic Blueberries</td>
<td class="gt_row gt_center">4966</td></tr>
    <tr><td class="gt_row gt_left">Seedless Red Grapes</td>
<td class="gt_row gt_center">4059</td></tr>
    <tr><td class="gt_row gt_left">Organic Grape Tomatoes</td>
<td class="gt_row gt_center">3823</td></tr>
    <tr class="gt_row_group_first"><td rowspan="6" class="gt_row gt_right gt_stub_row_group">Yogurt</td>
<td class="gt_row gt_left">Total</td>
<td class="gt_row gt_center">55240</td></tr>
    <tr><td class="gt_row gt_left">Total 0% Greek Yogurt</td>
<td class="gt_row gt_center">1046</td></tr>
    <tr><td class="gt_row gt_left">Total 0% Nonfat Greek Yogurt</td>
<td class="gt_row gt_center">993</td></tr>
    <tr><td class="gt_row gt_left">Total 2% with Strawberry Lowfat Greek Strained Yogurt</td>
<td class="gt_row gt_center">973</td></tr>
    <tr><td class="gt_row gt_left">Total Greek Strained Yogurt</td>
<td class="gt_row gt_center">951</td></tr>
    <tr><td class="gt_row gt_left">Total 2% All Natural Greek Strained Yogurt with Honey</td>
<td class="gt_row gt_center">810</td></tr>
    <tr class="gt_row_group_first"><td rowspan="6" class="gt_row gt_right gt_stub_row_group">Packaged Cheese</td>
<td class="gt_row gt_left">Total</td>
<td class="gt_row gt_center">41699</td></tr>
    <tr><td class="gt_row gt_left">Organic Whole String Cheese</td>
<td class="gt_row gt_center">1993</td></tr>
    <tr><td class="gt_row gt_left">Grated Parmesan</td>
<td class="gt_row gt_center">1694</td></tr>
    <tr><td class="gt_row gt_left">Feta Cheese Crumbles</td>
<td class="gt_row gt_center">1449</td></tr>
    <tr><td class="gt_row gt_left">Organic Sticks Low Moisture Part Skim Mozzarella String Cheese</td>
<td class="gt_row gt_center">1342</td></tr>
    <tr><td class="gt_row gt_left">Shredded Parmesan</td>
<td class="gt_row gt_center">1324</td></tr>
  </tbody>
  
  
</table>
</div>

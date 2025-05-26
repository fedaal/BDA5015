library(ggplot2)
library(dplyr)

df <- read.csv("shopping_clean.csv") 

month_order <- c('January', 'February', 'March', 'April', 'May', 'June', 
                 'July', 'August', 'September', 'October', 'November', 'December')
df$Month <- factor(df$Month, levels = month_order, ordered = TRUE)

revenue_by_month <- df %>%
  group_by(Month) %>%
  summarise(Revenue = sum(Revenue)) %>%
  arrange(desc(Revenue)) %>%
  head(5)

set.seed(12)
revenue_masked_bar <- revenue_by_month %>%
  sample_frac(1)  
revenue_masked_bar$Month_Label <- paste("Option", seq_along(revenue_masked_bar$Revenue))

bar_actual_labels <- revenue_by_month$Month

revenue_masked_bar <- revenue_masked_bar %>%
  arrange(desc(Revenue))

bar_plot <- ggplot(revenue_masked_bar, aes(x = Month_Label, y = Revenue, fill = Month_Label)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c('#F67280', '#355C7D', '#99B898', '#FFB347', '#6C5B7B')) +
  labs(title = "Order the months from the highest total revenue to the lowest.") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    panel.background = element_rect(fill = "white", color = "white"),  
    plot.background = element_rect(fill = "white", color = "white")    
  )

print(bar_plot)

ggsave("bar_chart_months.png", plot = bar_plot, width = 6, height = 3, dpi = 300)

print("Bar Chart - Months Ordered by Revenue (Highest to Lowest):")
for (i in 1:length(revenue_masked_bar$Month_Label)) {
  print(paste(revenue_masked_bar$Month_Label[i], ":", revenue_masked_bar$Month[i]))
}

set.seed(17)
revenue_masked_pie <- revenue_by_month %>%
  sample_frac(1) 
revenue_masked_pie$Month_Label <- paste("Option", seq_along(revenue_masked_pie$Revenue))

pie_actual_labels <- revenue_by_month$Month

revenue_masked_pie <- revenue_masked_pie %>%
  arrange(desc(Revenue))

pie_plot <- ggplot(revenue_masked_pie, aes(x = "", y = Revenue, fill = Month_Label)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c('#F67280', '#355C7D', '#99B898', '#FFB347', '#6C5B7B')) +
  labs(title = "Order the months from the highest total revenue to the lowest.") +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = "white"),  
    plot.background = element_rect(fill = "white", color = "white")   
  )

print(pie_plot)

ggsave("pie_chart_months.png", plot = pie_plot, width = 6, height = 3, dpi = 300)

print("Pie Chart - Months Ordered by Revenue (Highest to Lowest):")
for (i in 1:length(revenue_masked_pie$Month_Label)) {
  print(paste(revenue_masked_pie$Month_Label[i], ":", revenue_masked_pie$Month[i]))
}

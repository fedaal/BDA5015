library(ggplot2)
library(dplyr)

df <- read.csv("shopping_clean.csv")  
revenue_by_group <- df %>%
  group_by(`Ad.Group`) %>%
  summarise(Revenue = sum(Revenue)) %>%
  arrange(desc(Revenue)) %>%
  head(5)


set.seed(14)
revenue_masked_bar <- revenue_by_group %>%
  mutate(Ad_Group_Label = paste("Option", seq_along(Revenue))) %>% 
  sample_frac(1) 
bar_actual_labels <- revenue_by_group$`Ad.Group`

bar_mapping <- setNames(bar_actual_labels, revenue_masked_bar$Ad_Group_Label)

revenue_masked_bar$Ad_Group_Label <- factor(revenue_masked_bar$Ad_Group_Label, 
                                            levels = revenue_masked_bar$Ad_Group_Label)

revenue_masked_bar <- revenue_masked_bar %>%
  arrange(desc(Revenue))

bar_plot <- ggplot(revenue_masked_bar, aes(x = Ad_Group_Label, y = Revenue, fill = Ad_Group_Label)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c('#F67280', '#355C7D', '#99B898', '#FFB347', '#6C5B7B')) +
  labs(title = "Order the Ad Groups from the highest revenue to the lowest revenue.") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    panel.background = element_rect(fill = "white", color = "white"),  
    plot.background = element_rect(fill = "white", color = "white")   
  )

print(bar_plot)

ggsave("bar_chart.png", plot = bar_plot, width = 6, height = 3, dpi = 300)

cat("\n📌 Bar Chart - Masked to Actual Ad Group Mapping:\n")
for (option in names(bar_mapping)) {
  cat(paste(option, ": ", bar_mapping[[option]], "\n"))
}

set.seed(7)
revenue_masked_pie <- revenue_by_group %>%
  mutate(Ad_Group_Label = paste("Option", seq_along(Revenue))) %>% 
  sample_frac(1)  
pie_actual_labels <- revenue_by_group$`Ad.Group`

pie_mapping <- setNames(pie_actual_labels, revenue_masked_pie$Ad_Group_Label)

revenue_masked_pie <- revenue_masked_pie %>%
  arrange(desc(Revenue))

pie_plot <- ggplot(revenue_masked_pie, aes(x = "", y = Revenue, fill = Ad_Group_Label)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c('#F67280', '#355C7D', '#99B898', '#FFB347', '#6C5B7B')) +
  labs(title = "Order the Ad Groups from the highest revenue to the lowest revenue.") +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = "white"),  
    plot.background = element_rect(fill = "white", color = "white")   
  )

print(pie_plot)

ggsave("pie_chart.png", plot = pie_plot, width = 6, height = 3, dpi = 300)

cat("\n📌 Pie Chart - Masked to Actual Ad Group Mapping:\n")
for (option in names(pie_mapping)) {
  cat(paste(option, ": ", pie_mapping[[option]], "\n"))
}


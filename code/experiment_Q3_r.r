library(ggplot2)
library(dplyr)

df <- read.csv("shopping_clean.csv")  
roi_df <- df %>% 
  group_by(`Ad.Group`) %>% 
  summarise(Revenue = sum(Revenue), Cost = sum(Cost)) %>% 
  filter(Cost > 0) 
roi_df$ROI <- roi_df$Revenue / roi_df$Cost

top_5_roi <- roi_df %>% 
  arrange(desc(ROI)) %>% 
  head(5)


set.seed(20)
bubble_masked_df <- top_5_roi[sample(nrow(top_5_roi)), ]
bubble_masked_df$Option <- paste("Option", seq_along(bubble_masked_df$ROI))

bubble_plot <- ggplot(bubble_masked_df, aes(x = Option, y = 1, size = ROI)) +
  geom_point(color = '#ff7f0e', alpha = 0.6) +  
  geom_text(aes(label = Option), vjust = 8, size = 3) +  
  labs(title = "Which has lowest ROI?", x = NULL, y = NULL) +  
  scale_size_continuous(range = c(10, 30)) + 
  theme_minimal() +
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank(),  
        axis.title = element_blank(), 
        plot.title = element_text(size = 10)) + 
  theme(axis.ticks = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  guides(size = "none")  

ggsave("bubble_plot_roi_smaller_with_labels_below_white_background.png", 
       plot = bubble_plot, 
       width = 4, 
       height = 2.5, 
       dpi = 300,
       bg = "white")  

cat("Order of points based on ROI:\n")
order_of_points <- order(bubble_masked_df$ROI, decreasing = TRUE)  
for (i in order_of_points) {
  cat(paste("Label:", bubble_masked_df$Option[i], "ROI:", bubble_masked_df$ROI[i], "\n"))
}

set.seed(10)
scatter_masked_df <- top_5_roi[sample(nrow(top_5_roi)), ]
scatter_actual_labels <- rownames(scatter_masked_df)
rownames(scatter_masked_df) <- paste("Option", seq_along(scatter_masked_df$ROI))

scatter_plot <- ggplot(scatter_masked_df, aes(x = rownames(scatter_masked_df), y = ROI)) +
  geom_point(color = '#1f77b4', size = 2) +
  labs(title = "Which has lowest ROI?", x = "Ad Group", y = "ROI") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.text = element_text(size = 8), 
        axis.title = element_text(size = 8), 
        plot.title = element_text(size = 10)) +
  theme(axis.ticks = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave("scatter_plot_roi.png", plot = scatter_plot, width = 7, height = 4, dpi = 300, bg = "white")

cat("\n📌 Scatter Chart - Masked to Actual Ad Group Mapping:\n")
scatter_mapping <- setNames(scatter_actual_labels, rownames(scatter_masked_df))
for (option in names(scatter_mapping)) {
  cat(paste(option, ": ", scatter_mapping[[option]], "\n"))
}

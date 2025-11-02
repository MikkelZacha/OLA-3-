######### Opg. 2.2 ################33

loadings_PC1 <- pca_mod$rotation[, 1]
sort(abs(loadings_PC1), decreasing = TRUE)[1:3]

library(ggplot2)
library(dplyr)

library(ggplot2)
library(dplyr)

spg_df <- data.frame(
  Spørgsmål = names(loadings_PC1),
  Loading = loadings_PC1
)

spg_df %>%
  mutate(Spørgsmål = reorder(Spørgsmål, Loading)) %>%
  ggplot(aes(x = Spørgsmål, y = Loading, fill = Loading > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_fill_manual(values = c("#E57373", "#64B5F6")) +
  labs(
    title = "Loadings på PC1 (forbrugerforventninger)",
    x = NULL,
    y = "Vægt (loading)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.y = element_text(size = 10)
  )


top3_df <- data.frame(
  Spørgsmål = names(sort(abs(loadings_PC1), decreasing = TRUE)[1:3]),
  Loading = loadings_PC1[names(sort(abs(loadings_PC1), decreasing = TRUE)[1:3])]
)

ggplot(top3_df, aes(x = reorder(Spørgsmål, abs(Loading)), y = abs(Loading))) +
  geom_col(fill = "#4C9F70", width = 0.6) +
  geom_text(aes(label = round(abs(Loading), 3)), 
            vjust = -0.5, size = 4.2, color = "#333333") +
  coord_flip() +
  labs(
    title = "Top 3 spørgsmål med højeste vægt på PC1",
    subtitle = "Spørgsmål, der mest påvirker den første hovedkomponent",
    x = NULL,
    y = "Absolut loading"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    axis.text.y = element_text(size = 10)
  )

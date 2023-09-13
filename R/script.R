# Carregar pacotes
library(tidyverse)
library(ggplot2)
library(plotly)
library(htmlwidgets)

# Importando data frame
imdb <- read.csv("Documentos/Alura/R/imdb_top_1000.csv") |> 
  mutate(Gross_final = as.numeric(str_replace_all(Gross, ",", ""))) |> 
  select(-IMDB_Rating2, -Gross, -Gross2)

# Análise exploratória dos dados
glimpse(imdb)
View(imdb)

# Categorizar e sumarizar o meta score
meta_score_category <- imdb |> 
  group_by(categoria = case_when(
    Meta_score <= 30 ~ "Até 30",
    Meta_score <= 60 ~ "Entre 30 e 60",
    TRUE ~ "Acima de 60")) |> 
  summarise(quantidade = n(),
    media = round(mean(Meta_score, na.rm = TRUE), 2))

# Criar gráfico
plot1 <- meta_score_category |> 
  ggplot(aes(x = categoria, y = quantidade, fill = categoria)) +
  geom_bar(stat = "identity", width = 0.5, alpha = 0.8) +
  scale_fill_manual(values = c("#FFBB5C", "#E86523", "#FF5026")) +
  labs(y = "Quantidade de filmes", x = "Meta Score") +
  theme_minimal() +
  ylim(0, 1000) +
  expand_limits(y = 0) +
  theme(
    legend.position="none",
    axis.text.x = element_text(size = 15, family = "Arial"),
    axis.text.y = element_text(size = 15, family = "Arial"),
    axis.title.y = element_text(size = 15, margin = margin(r = 20), 
                                family = "Arial"),
    axis.title.x = element_text(size = 15, margin = margin(t = 15), 
                                family = "Arial")) +
  geom_text(aes(label = paste("média:", media)), 
            vjust = -2, hjust = 0.5, size = 6, color = "#767676", family = "Arial")

# Transformar o gráfico em um gráfico interativo e salvar
meta_score_plot <- ggplotly(plot1, tooltip = c("quantidade"))

saveWidget(meta_score_plot, file = "meta_score_plot.html")


# Gêneros de acordo com as décadas
genre_years <- imdb |> 
  group_by(decadas = case_when(
    Released_Year < 1930 ~ "1920",
    Released_Year < 1940 ~ "1930",
    Released_Year < 1950 ~ "1940",
    Released_Year < 1960 ~ "1950",
    Released_Year < 1970 ~ "1960",
    Released_Year < 1980 ~ "1970",
    Released_Year < 1990 ~ "1980",
    Released_Year < 2000 ~ "1990",
    Released_Year < 2010 ~ "2000",
    Released_Year < 2020 ~ "2010",
    TRUE ~ "2020"), generos = Categories) |> 
  summarise(quantidade = n())

# Criar gráfico
plot2 <- genre_years |> 
  ggplot(aes(x = decadas, y = quantidade, fill = generos)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.9) +
  labs(y = "Quantidade de filmes", 
       x = "Décadas",
       fill = "Gêneros") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 15, family = "Arial"),
    axis.text.y = element_text(size = 15, family = "Arial"),
    axis.title.y = element_text(size = 15, margin = margin(r = 20), 
                                family = "Arial"),
    axis.title.x = element_text(size = 15, margin = margin(t = 15), 
                                family = "Arial"))

# Transformar o gráfico em um gráfico interativo e salvar
genre_years_plot <- ggplotly(plot2, tooltip = c("quantidade"))

saveWidget(genre_years_plot, file = "genre_years_plot.html")


# Gráfico da quantidade de votos de acordo com as avaliações dos usuários
plot3 <- imdb |> 
  ggplot(aes(x = IMDB_Rating, y = No_of_Votes, color = IMDB_Rating)) +
  geom_point(size = 4, alpha = 0.8) +
  scale_color_gradient(low = "#0099B3", high = "#FF7100") +
  theme_minimal() +
  labs(y = "Número de votos", 
       x = "Avaliações no IMDb") +
  ylim(0, 2500000) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 15, family = "Arial"),
    axis.text.y = element_text(size = 15, family = "Arial"),
    axis.title.y = element_text(size = 15, margin = margin(r = 20), 
                                family = "Arial"),
    axis.title.x = element_text(size = 15, margin = margin(t = 15), 
                                family = "Arial"))


# Transformar o gráfico em um gráfico interativo e salvar
votes_plot <- ggplotly(plot3)

saveWidget(votes_plot, file = "votes_plot.html")

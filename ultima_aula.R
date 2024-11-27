require(tidyverse)
require(magrittr)
require(data.table)

files = system("ls /home/est/rfl24/Documentos/ProjetoCE302_2024/CE302_2024_Aula/Data/filmes", intern = TRUE)

filmes = list()
for(i in 1:length(files)){
  nome = gsub(x = files[i], 
              pattern = ".csv", 
              replacement = "")
  
  filmes[[i]] <- fread(paste0("/home/est/rfl24/Documentos/ProjetoCE302_2024/CE302_2024_Aula/Data/filmes/",  files[i])) %>% 
    mutate(tipo = nome)
  
  filmes[[i]]$year %<>% as.integer()
  filmes[[i]]$`gross(in $)` %<>% as.numeric()
  
}

filmes %<>% bind_rows()

filmes %>% # Carregar o banco
  filter(tipo %in% "horror") %>% # seleciona um gênero de filme
  ggplot() + ## Chamar o ggplot
  aes( x = year, y = `gross(in $)`, ## aplicar a estética, isto é, quais são as variáveis e o que elas significam, x e y, neste caso
       color = tipo) + ## Adicionamos cor
  geom_point() +# Fazer o scatterplot
  #facet_wrap(vars(tipo)) + ## Fazemos o gráfico separado por tipo
  geom_smooth(method = "loess") + # O método aqui se refere ao método utilizado para estimação da curva
  scale_size_continuous(range=c(0.01, 2)) + # ajustar escala dos pontos
  labs(x = "Ano", 
       y = "Investimento ($)") + 
  theme_minimal() ## Incluímos tema

filmes %>% 
  group_by(year, tipo) %>% 
  summarise(Valor_Gasto_Medio = mean(`gross(in $)`, na.rm = TRUE)) %>%
  ggplot() +
  aes( x = year, 
       y = Valor_Gasto_Medio, 
       color = tipo) + 
  geom_line() + 
  facet_wrap(vars(tipo)) + 
  theme_minimal()

filmes %>% 
  group_by(year) %>% 
  summarise(Valor_Gasto_Medio = mean(`gross(in $)`, na.rm = TRUE), 
            n_filmes = n()) %>%
  ggplot() +
  aes( x = year, 
       y = Valor_Gasto_Medio) + 
  geom_line(alpha = 0.2) + 
  geom_point(aes(size = n_filmes)) + 
  labs(x = "Ano de Lançamento", 
       y = "Investimento ($)", 
       title = "Gráfico do investimento médio em filmes por ano") + 
  scale_y_continuous(labels = scales::label_dollar(),
                     trans = "log10") +
  scale_x_continuous(breaks = seq(from = 1900, to = 2020, by = 10)) + 
  scale_size_continuous(range = c(0.1, 10)) + 
  theme_minimal() +
  theme(text = element_text(size = 20, 
                            hjust = 0.5, 
                            face = "bold"),# tamanho da fonte
                            axis.text.x = element_text(angle = 45))

filmes %>% 
  group_by(tipo) %>% 
  summarise(Total_Investido = sum(`gross(in $)`, na.rm = TRUE)) %>%
  mutate(tipo = reorder(tipo, Total_Investido)) %>% 
  ggplot() +
  aes( x = tipo, 
       y = Total_Investido, 
       color = tipo, 
       fill = tipo) +
  geom_col(alpha = 0.2, 
           ltw = 2) + 
  scale_y_continuous(labels = scales::label_dollar()) +
  coord_flip() +
  labs(x = "Tipo de Filme", 
       y = "Total Investido ($)") + 
  theme_minimal() +
  theme(legend.position = "none")

filmes %>% 
  mutate(tipo = stringr::str_to_title(tipo)) %>% 
  mutate(ano_categorico = case_when(year < 1900 ~ "< 1900", 
                                    year < 1925 ~ "< 1925", 
                                    year < 1950 ~ "< 1950", 
                                    year < 1975 ~ "< 1975", 
                                    year < 2000 ~ "< 2000", 
                                    year < 2025 ~ "< 2025")) %>%
  group_by(tipo, ano_categorico) %>% 
  summarise(Total_Investido = sum(`gross(in $)`, na.rm = TRUE)) %>%
  ungroup() %>% 
  group_by(tipo) %>% 
  mutate(total = sum(Total_Investido, na.rm = TRUE)) %>%
  ggplot() +
  aes( x = reorder(tipo, total), 
       y = Total_Investido, 
       color = ano_categorico, 
       fill = ano_categorico) +
  geom_col() + 
  scale_y_continuous(labels = scales::label_dollar()) +
  scale_color_manual(values = colorRampPalette(c("Blue", "Pink"))(7)) + 
  scale_fill_manual(values = colorRampPalette(c("Blue", "Pink"))(7)) + 
  
  coord_flip() +
  labs(x = "Tipo de Filme", 
       y = "Total Investido ($)",
       color = "Ano", 
       fill = "Ano") + 
  theme_minimal() +
  theme(legend.position = "bottom", 
        text = element_text(size = 18))


filmes %>%
  filter(tipo %in% "animation") %>% 
  ggplot() + 
  aes(x = rating, y = year) + 
  geom_hex(bins = 20, 
           color = "white", 
           size = 2) +
  scale_fill_gradient(low = "#FED0BB",
                      high = "#461220", 
                      guide = FALSE) + 
  coord_flip() + 
  theme_minimal()

filmes %>%
  filter(tipo %in% "war") %>% 
  mutate(runtime_num = stringr::str_remove(runtime, "min") %>% 
           str_squish() %>% 
           as.numeric()) %>% 
  filter(!is.na(runtime_num)) %>% 
  filter(`gross(in $)` > 0 | !is.na(`gross(in $)`)) %>% 
  ggplot() + 
  aes(x = year, 
      y = runtime_num) + 
  geom_density_2d() +
  theme_minimal() + 
  theme(legend.position = "none")

require(patchwork)
require(ggrepel)

p1 = 
  filmes %>% 
  filter(tipo %in% c("animation")) %>% 
  mutate(nome = ifelse(`gross(in $)` > quantile(`gross(in $)`, 0.95, na.rm = T), movie_name, NA)) %>% 
  mutate(col = ifelse(!is.na(nome), "Importante", "Não Importante")) %>%
  ggplot() +
  aes(x = year, 
      y = rating, 
      label = nome, 
      color = col)+
  geom_point() +
  geom_text_repel(box.padding = 0.5, 
                  max.overlaps = 1000, 
                  nudge_x = .15,
                  nudge_y = 1,
                  segment.curvature = -0.1,
                  segment.ncp = 3,
                  segment.angle = 20
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

p2 = filmes %>% 
  filter(tipo %in% c("animation")) %>% 
  ggplot() +
  aes(group = year, 
      y = rating, 
  )+
  geom_boxplot() +
  theme_minimal() +
  theme(legend.position = "bottom")

pdf("/home/est/rfl24/Documentos/ProjetoCE302_2024/CE302_2024_Aula/grafico1.pdf", widht = 1200, height = 800) # ou png()
p3 = filmes %>% 
  filter(tipo %in% c("animation")) %>% 
  group_by(year) %>% 
  summarise(rate = mean(rating, na.rm = TRUE)) %>% 
  ggplot()+
  aes(x = year, 
      y = rate) + 
  geom_line() +
  geom_point() +
  theme_minimal()
dev.off()

p1 / p2

Cairo::CairoPDF("/home/est/rfl24/Documentos/ProjetoCE302_2024/CE302_2024_Aula/Ografico.pdf", width = 1920, height = 1080)
p1
dev.off()

require(plotly)

p3 %>%  ggplotly()

require(gganimate)

filmes %>% 
  group_by(tipo, year) %>% 
  summarise(n = log10(n())) %>%
  group_by(year) %>% 
  mutate(Rank = rank(n)) %>%
  ggplot() + 
  aes(x = Rank, 
      y = n, 
      color = tipo, 
      fill = tipo, 
      label = tipo) +
  geom_col(show.legend = F, alpha = 0.5) + 
  geom_text(show.legend = F, nudge_y = 1 , size = 10) + 
  coord_flip() + 
  scale_y_continuous(limits  = c(0,8)) + 
  # Agora, incluimos as partes do gganimate
  labs(#title = 'Ano: {frame_time}', 
    x = 'Tipo de Filme', 
    y = 'Número de Filmes') +
  #transition_time(year) +
  ggtitle('{closest_state}') +
  transition_states(year,
                    transition_length = 5,
                    state_length = 1) +
  ease_aes('linear') + 
  theme_minimal() +  
  theme(text = element_text(size = 20)) + 
  exit_fade() +   
  enter_fade() 

tmp = filmes[sample(1:nrow(filmes), 1000)]

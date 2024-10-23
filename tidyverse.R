library(tidyverse)
dados <- readr::read_csv("/home/est/rfl24/Documentos/ProjetoCE302_2024/CE302_2024_Aula/Data/MentalHealthDataset.csv")
# para achar o arquivo digitar 'dados <- readr::read_csv("")' e com o cursor entre as aspas digitar 'tab' no teclado.
# ou 'dados <- data.table::fread("~")' caso não tenha instalado os pacotes tidyverse
#####
class(dados)
head(dados)
dados
glimpse(dados)

#####
Poland <- subset(dados, Country =="Poland")
Poland
Coluna <- dados[, c("Country", "Gender")]

#####
# PIPES
x <- seq(1:10)
y <- sqrt(x)
z <- log(y)
z

tan(
  cos(
    log(
      sqrt(
        x
        )
      )
    )
  )

x %>% # |> pipe nativo do R
  sqrt() %>%
  log() %>%
  cos() %>% 
  tan() # atalho pipe: ctrl+shift+m

require(magrittr) # pacote que carrega o pipe, para operações matemáticas
set.seed(123)
rnorm(10)    %>%
  multiply_by(5) %>% # ou '*'
  add(5)            # ou '+'

#####
# Pipe de atribuição
require(dplyr)
## Atribuição explicita
meu_data_frame <- data.frame(
  nome = c("Alice", "Bob", "Carol", "Ana", "João", "Carlos", "Patrícia", "Leonardo"),
  idade = c(25, 30, 28, 20, 27, 50, 60, 45),
  salario = c(5000, 6000, 5500, 8000, 2000, 3500, 10000, 3800 ), 
  meio_de_transporte = c('onibus', 'bicicleta', 'onibus', 'carro', 'carro', 'onibus', 'onibus', 'bicicleta'))

meu_data_frame = meu_data_frame %>%
  mutate(idade_25 = idade > 25)

glimpse(meu_data_frame)

## Atribuição implicita
meu_data_frame %<>% 
  mutate(idade_50 = idade > 50) %>% 
  mutate(idade_30 = idade > 30)

glimpse(meu_data_frame)

#####
require(dplyr)
require(tidyr)
require(data.table)
car_crash = read_csv("Data/Brazil Total highway crashes 2010 - 2023.csv")
# Dados extraídos de https://www.kaggle.com/datasets/liamarguedas/brazil-total-highway-crashes-2010-2023

glimpse(car_crash)

car_crash[,c("onibus", "moto")]
car_crash %>% 
  select(onibus, moto)
car_crash %>% 
  select(c(1:4, 9))
car_crash %>% 
  select(-onibus)
car_crash %>% 
  select(-c(onibus, moto))

car_crash %>% 
  select(starts_with("tr"))
car_crash %>% 
  select(ends_with("as"))
car_crash %>% 
  select(ends_with("feridos"))
car_crash %>% 
  select(contains("_"))

car_crash %>% 
  select(where(is.numeric)) %>% 
  glimpse()
car_crash %>% 
  select(where(is.character)) %>% 
  glimpse()
car_crash %>% 
  select(where(is.logical)) %>% 
  glimpse()

car_crash %>% 
  mutate(tipo_log = tipo_de_ocorrencia == "sem vítima") %>% 
  select(where(is.logical))

vars_interesse = c("automovel",
                   "bicicleta",
                   "onibus")
car_crash %>% 
  select(automovel, bicicleta, onibus)
car_crash %>% 
  select(all_of(vars_interesse))

vars_interesse = c("automovel",
                   "bicicleta",
                   "onibus",
                   "canoa")
car_crash %>% 
  select(n_of(vars_interesse))

# função select() seleciona colunas
car_crash %>% 
  select(moto, starts_with("tr"),
         ends_with("feridos"))

## Vetor de moto
car_crash %>% 
  # pull() transforma em vetor
  pull(moto)
## Data frame de moto
car_crash %>% 
  select(moto)

car_crash %>% 
  select(moto, automovel, data) %>% 
  filter(moto > 2 & automovel == 2)
  
car_crash %>% 
  select(moto, automovel, data) %>% 
  filter(moto > 2 | automovel == 2)
  
car_crash %>% 
  group_by(tipo_de_ocorrencia) %>% 
  summarise(media = mean(automovel, na.rm = T),
            n = n()) %>%  View

car_crash %>% 
  filter(tipo_de_ocorrencia %in% c("sem vítima", "com vítima")) %>% 
  group_by(tipo_de_ocorrencia) %>% 
  summarise(media_carros = mean(automovel, na.rm = T),
            media_motos = mean(moto, na.rm = T),
            mediana_carros = median(automovel, na.rm = T),
            n = n(),
            quantil_25 = quantile(automovel, probs = 0.25, na.rm = T)) %>% 
  arrange(n) 

car_crash %>% 
  filter(tipo_de_ocorrencia %in% c("sem vítima", "com vítima")) %>% 
  group_by(tipo_de_ocorrencia) %>% 
  summarise(media_carros = mean(automovel, na.rm = T),
            media_motos = mean(moto, na.rm = T),
            mediana_carros = median(automovel, na.rm = T),
            n = n(),
            quantil_25 = quantile(automovel, probs = 0.25, na.rm = T)) %>% 
  arrange(desc(n)) 

#######
car_crash %>% 
  group_by(tipo_de_acidente) %>% 
  summarise(n = n()) %>%  View

car_crash %>% 
  filter(tipo_de_ocorrencia %in% c("sem vítima", "com vítima"),
         tipo_de_acidente %in% c("Colisão Traseira", "Saida de Pista")) %>% 
  group_by(tipo_de_ocorrencia, tipo_de_acidente) %>% 
  summarise(media_carros = mean(automovel, na.rm = T),
            media_motos = mean(moto, na.rm = T),
            mediana_carros = median(automovel, na.rm = T),
            n = n(),
            quantil_25 = quantile(automovel, probs = 0.25, na.rm = T)) %>% 
  arrange(desc(n)) %>% 
  View

# Exercícios 4.4
# 1.a
car_crash %>% 
  mutate(total = n()) %>% 
  select(data, tipo_de_ocorrencia, automovel, bicicleta, onibus, caminhao, moto,
         trator_maquinas, outros, total) 
# 1.b
car_crash %>% 
  select(contains("feridos"))
# 1.c
car_crash %>% 
  select(where(is.numeric))
# 1.d
car_crash %>% 
  select(where(is.logical))
# 1.e
car_crash %>% 
  select(ends_with("o"))
# 1.f
car_crash %>% 
  select(starts_with("t"))
# 1.g
car_crash %>% 
  filter(automovel <= 5 & moto == 3)
# 1.h
car_crash %>% 
  filter(automovel <= 5 | moto == 3)
# 1.i
car_crash %>% 
  filter()
# 1.j
car_crash %>% 
  filter()

##### StarWars
# 5
starwars %>% View
starwars %>% 
  select(name, birth_year, species) %>% 
  group_by(species) %>% 
  mutate(primeiro__da_sp = max(birth_year, na.rm = T)) %>% 
  filter(primeiro__da_sp == birth_year) %>% 
  View

## MANIPULAÇÃO DE DATA
car_crash$data
# String representando uma data
data_string <- "2024-10-23"
# Transformando a string em data
data <- as.Date(data_string)
# Exibindo a data
print(data)

data_string <- "23/10/2024"
data <- as.Date(data_string, 
               format = "%d/%m/%Y") # se o y for minúsculo ele identifica apenas 2 caracteres
print(data)

# Somando DIAS
data +7
data +31
data +365 # 1 ano

data1 <- as.Date("2023-08-21")
data2 <- as.Date("2023-08-15")
data1 > data2  # Verifica se data1 é posterior a data2
data > data1
(data + 365) < data

data <- as.Date("2023-08-21")
ano <- format(data, "%Y")
mes <- format(data, "%m")
dia <- format(data, "%d")

diferenca <- difftime(data1, data2, units = "days")  # Diferença em dias
diferenca

require(tidyverse)
require(lubridate)
data_ymd <- ymd("2023-08-21")
data_mdy <- mdy("08-21-2023")
data_dmy <- dmy("21-08-2023")

print(data_ymd)
print(data_mdy)
print(data_dmy)

data <- ymd("2024-10-23")
data_nova <- data + days(7)  # Adiciona 7 dias
data_nova
data_anterior <- data - months(2)  # Subtrai 2 meses
data_anterior
print(data_nova)

data <- ymd_hms("2024-10-23 18:45:45")
ano <- year(data)
mes <- month(data)
dia <- day(data)
hora <- hour(data)
minuto <- minute(data)
segundo <- second(data)

print(ano)
mes
hora
segundo

data1 <- ymd("2023-08-21")
data2 <- ymd("2023-08-15")
diferenca_em_dias <- as.numeric(data2 - data1)
diferenca_em_semanas <- as.numeric(weeks(data2 - data1))

print(diferenca_em_dias)
diferenca_em_semanas

# Data original no fuso horário de Nova Iorque
data_ny <- ymd_hms("2023-08-21 12:00:00", tz = "America/New_York")

# Converter para o fuso horário de Londres
data_london <- with_tz(data_ny, tz = "Europe/London")

print(data_ny)

# Duas datas em fusos horários diferentes
data_ny <- ymd_hms("2023-08-21 12:00:00", tz = "America/New_York")
data_london <- ymd_hms("2023-08-21 17:00:00", tz = "Europe/London")

# Calcular a diferença de tempo em horas
diferenca_horas <- as.numeric(data_london - data_ny)

print(diferenca_horas)

#### Exercícios com datas #4.7
# 1
car_crash  %>% 
  mutate(data <- as.Date(data, format = "%d/%m/%Y")) %>% 
  mutate(nova_hora = hms(horario)) %>% 
  mutate(mes = month(data,
         ano = year(data),
         hora = hour(nova_hora)))
# 2
car_crash %>% 
  group_by(mes) %>% 
  summarise(total_mes = n()) %>% 
  filter(total_mes == max(total_mes))

#### JUNÇÃO DE DADOS
require(nycflights13)
airlines

##
planes %>% 
  count(tailnum) %>%
  filter(n > 1) %>% # filtrar para ver se aparece mais de uma vez, caso se repita, não pode ser chave primária
  View

planes %>% 
  group_by(tailnum) %>% 
  summarise(n = n()) %>% 
  filter(n >1)

weather %>%
  count(time_hour, origin) %>%
  filter(n > 1)

## Junções Mutacionais
flights2 <- flights %>% 
  filter(distance > 2000) %>% 
  select(year, time_hour, origin, dest, tailnum, carrier)
flights2

# LeftJoin
flights2_airlines = 
  flights2  %>% 
  left_join(., airlines,
            by = "carrier")

flights2_airlines = 
  flights2  %>% 
  left_join(., airlines,
            by = c("carrier" = "carrier"))

# RightJoin
planes_flights = flights2 %>% 
  right_join(planes, by = "tailnum")


# InnerJoin
origin_flights = flights2 %>% 
  inner_join(airports, by = c("origin" = "faa"))

origin_flights = flights2 %>% 
  inner_join(airports, join_by(origin == faa))

# FullJoin
dest_flights = flights2 %>% 
  full_join(airports, by = c("dest"= "faa"))

dest_flights = flights2 %>% 
  full_join(airports, join_by(dest == faa))

flights2

## Junções de Filtragem
# SemiJoin
airports %>% 
  semi_join(flights2, join_by(faa == origin))
# AntiJoin
flights %>%
  anti_join(airports, join_by(dest == faa)) %>% 
  distinct(dest)


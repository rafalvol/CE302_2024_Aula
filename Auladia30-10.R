require(tidyverse)
require(magrittr)

table1

table1 %>% 
  select(country, year, cases) %>% 
  pivot_wider(names_from = year, values_from = cases, # o ano vira a coluna e os valores em outra coluna
              names_prefix = "ano_", # atribuir nome na coluna
              values_fill = 0, # preenche com zero quando NA
              values_fn = length) # ou max, ou min 

table1 %>% 
  select(country, year, cases) %>% 
  pivot_wider(names_from = year, values_from = c(cases, population), # o ano vira a coluna e os valores em outra coluna
              names_prefix = "ano_", # atribuir nome na coluna
              values_fill = 0, # preenche com zero quando NA
              values_fn = max)  # ou max, ou min 

table1 %>% 
  pivot_longer(cols = -c(country, year), 
               names_to = "variavel", 
               values_to = "tamanho")

table1 %>% 
  pivot_longer(cols = c(cases, population), 
               names_to = "variavel", 
               values_to = "tamanho")

table3

separated = table3 %>% 
  separate(rate, into = c("cases", "population"),
           sep = "/")
separated %>% 
  mutate(cases = as.numeric(cases),
         population = as.numeric(population)) %>% 
  mutate(perc = (cases/population)*100)

separated %>% 
  unite(rate, cases, population, sep = ":")

require(data.table)
TB <- fread("Data/TB.csv.gz")
            #sep = ";", sep2 = "&")

names(TB)

TB1 <- TB %>% 
  pivot_longer(
    cols = -c(1:4), 
    names_to = "chave", 
    values_to = "casos", 
    values_drop_na = TRUE
  )
TB1
TB1 %>% 
  count(chave)
TB1 %<>% filter(chave %like% "^new") # ^ significa que o new está no início da variável
TB1
TB2 <- TB1 %>% 
  mutate(chave = stringr::str_replace(chave, "newrel", "new_rel"))
TB2
TB3 <- TB2 %>% 
  separate(chave, c("new", "type", "sexage"), 
           sep = "_")
TB3
TB4 <- TB3 %>% 
  select(-new, -iso2, -iso3)
TB4
TB5 <- TB4 %>% 
  separate(sexage, c("sexo", "idade"), sep = 1) # primeira ocorrência; 1ª letra de um lado e o resto do outro
TB5

##############################################################################

require(tidyverse)
require(magrittr)

x <- "Seu nome aqui"
y <- 'Seu nome aqui'

x == y

var_com_aspas <- "Ela disse: 'Eu adoro lasanha.'"
var_com_aspas
str_view(var_com_aspas)
  
var_com_aspas2 <- 'Ela disse: "Eu adoro lasanha."'
var_com_aspas2
str_view(var_com_aspas2)

var_com_aspas3 <- "Ela disse: \"Eu adoro lasanha.\""
var_com_aspas3
str_view(var_com_aspas3)

texto <- "Isso é um exemplo de texto com espaços em branco."
str_view(texto)

texto_multilinhas <- "Primeira linha\nSegunda linha\nTerceira linha"
str_view(texto_multilinhas)

texto_tabulado <- "Primeira coluna\tSegunda coluna\tTerceira coluna" 
str_view(texto_tabulado)

texto_com_barras <- "Isso é uma única barra invertida: \\"
str_view(texto_com_barras)

texto_unicode_grau <- "A temperatura é de 25\u00B0C."
str_view(texto_unicode_grau)

texto_multilinhas_unicode <- "Primeira linha\u000ASegunda linha"
str_view(texto_multilinhas_unicode)

simbolo_somatorio <- "O símolo do somatório é: \u2211"
str_view(simbolo_somatorio)

emoji <- "OMG! Também posso usar emoji! \U1F631"
str_view(emoji)

df <- data.frame(nome = c("Ana", "Maria", "João", NA), 
                 sobrenome= c("Santos", "Silva", "Souza", NA))
df %>% 
  mutate(ola = str_c("Boa noite ", nome, " ", sobrenome, "!"))
df %>% 
  mutate(mensagem = str_glue("Boa noite {nome} {sobrenome}!"))
df$nome %>% 
  str_flatten(na.rm = TRUE, collapse = ", ", last = " e ")

df %<>% 
  mutate(Nome_Sobrenome = str_c(nome, sobrenome, sep = " "))
df$Nome_Sobrenome %>% 
  str_split(., " ", simplify = TRUE) # str_split() resultado é uma lista

df %>% 
  mutate(qtd_vogais = str_count(nome, "[aeiou]")) %>% # entre [] é uma RegEx (expressão regular)
  mutate(qtd_consoantes = str_count(nome, "[^aeiou]")) # o [^] significa que o que há no colchete deve ser ignorado

words

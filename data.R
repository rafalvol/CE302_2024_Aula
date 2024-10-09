#install.packages("data.table")
## DATAFRAMES
meu_df = data.frame(x=c(1:10),
                    y=LETTERS[1:10])
meu_df

# Exemplo de criação de Data Frame
meu_data_frame <- data.frame(
  nome = c("Alice", "Bob", "Carol", "Ana", "João", "Carlos", "Patrícia", "Leonardo"),
  idade = c(25, 30, 28, 20, 27, 50, 60, 45),
  salario = c(5000, 6000, 5500, 8000, 2000, 3500, 10000, 3800 ), 
  meio_de_transporte = c('onibus', 'bicicleta', 'onibus', 'carro', 'carro', 'onibus', 'onibus', 'bicicleta')
)
## Nomes de colunas do nosso data.frame não possuem espaço, podem ser separadas por "." ou "_".

meu_data_frame
head(meu_data_frame) # 6 primeiras linhas
tail(meu_data_frame)
meu_data_frame[1:2,3:4]
meu_data_frame$nome
class(meu_data_frame$nome)
class(meu_data_frame) # Qual a classe da tabela
str(meu_data_frame) # Estrutura da tabela

meu_data_frame[,"idade"]
meu_data_frame[,-2] # remove 2ª coluna

meu_data_frame$Gosta_de_Bolo = c(TRUE,
                                 FALSE,
                                 FALSE,
                                 TRUE,
                                 TRUE,
                                 FALSE,
                                 FALSE,
                                 TRUE)
str(meu_data_frame)
meu_data_frame$constante = "Sim"

subconjunto_df <- meu_data_frame[meu_data_frame$idade > 28, ]  # Seleciona pessoas com idade maior que 28
subconjunto_df

# Usando a função subset()
subconjunto_df <- subset(meu_data_frame, idade > 28)
subconjunto_df

# Combinação
subconjunto_combinado <- meu_data_frame[meu_data_frame$idade > 25 & meu_data_frame$idade < 30, ]
subconjunto_combinado
subconjunto2 = subset(meu_data_frame, idade > 28 & !Gosta_de_Bolo)
subconjunto2
subconjunto3 = subset(meu_data_frame, idade < 40 | Gosta_de_Bolo)
subconjunto3

dim(meu_data_frame) # Linha e Coluna
nrow(meu_data_frame) # Número de linhas
ncol(meu_data_frame) # Número de colunas

# Resumo estatístico das variáveis
summary(meu_data_frame)
mean(meu_data_frame$idade)
sd(meu_data_frame$idade)
median(meu_data_frame$idade)
quantile(meu_data_frame$idade)

# Exemplo de uso da função by()
resultado <- by(meu_data_frame$salario, 
                meu_data_frame$idade, 
                mean)
resultado
by(meu_data_frame$salario, 
   meu_data_frame$Gosta_de_Bolo, 
   mean)

# Ordenação
ordenado <- meu_data_frame[order(meu_data_frame$idade), ]
ordenado

## FATORES
# Exemplo de criação de fator
genero <- factor(c("Masculino", "Feminino", "Masculino", "Feminino"))

# Exenplo com diferentes níveis
estadiamento_doenca <- factor(c("Estágio I", "Estágio II", "Estágio I", "Estágio III", "Estágio IV"), 
                              levels = c("Estágio I", "Estágio II", "Estágio III", "Estágio IV"))

meu_data_frame$estad_doenca = c("I", "I", "II", 
                                "IV", "II", "III",
                                "I", "IV")
as.factor(meu_data_frame$estad_doenca)
meu_data_frame$estad_doenca = factor(meu_data_frame$estad_doenca,
       levels = c("IV", "III", "II", "I"))
meu_data_frame$estad_doenca
str(meu_data_frame)
summary(meu_data_frame)
levels(meu_data_frame$estad_doenca)
nlevels(meu_data_frame$estad_doenca)

##
table(meu_data_frame$estad_doenca)
tabela = table(meu_data_frame$estad_doenca,
      meu_data_frame$Gosta_de_Bolo)
prop.table(meu_data_frame$estad_doenca,
           meu_data_frame$Gosta_de_Bolo,
           margin = 2) # 1 = linha, 2 = coluna, ou c(1,2)
margin.table(tabela, 1) # Linha
margin.table(tabela, 2) # Coluna

chisq.test(tabela)
fisher.test(tabela, alternative = "greater")

## Dados externos
# Lendo os dados de queimadas
Q1 <- read.csv("~/Documentos/ProjetoCE302_2024/CE302_2024_Aula/Data/Dataset_FireWatch_Brazil_Q1_2024.csv")
head(Queimadas_Q1)
Q2 = read.csv("~/Documentos/ProjetoCE302_2024/CE302_2024_Aula/Data/Dataset_FireWatch_Brazil_Q2_2024.csv")
head(Queimadas_Q2)
Q3 = read.csv("~/Documentos/ProjetoCE302_2024/CE302_2024_Aula/Data/Dataset_FireWatch_Brazil_Q3_2024.csv")
head(Queimadas_Q3)

dim(Q1)
dim(Q2)
dim(Q3)
summary(Q1)
summary(Q2)
summary(Q3)

Queimadas = rbind(Q1,
                  Q2,
                  Q3)
dim(Queimadas)
nrow(Q1) + nrow(Q2) + nrow(Q3)

write.csv(Queimadas, "~/Documentos/ProjetoCE302_2024/CE302_2024_Aula/Data/Queimadas.csv")

## Exercício ##  
## Para os dados de Queimadas faça o que se pede:
# Imprima na tela as 9 primeiras observações.
head(Queimadas, 9)
# Imprima as últimas 3 observações.
tail(Queimadas, n=3)
# Quantas observações temos?
nrow(Queimadas)
# Quantas variáveis temos?
ncol(Queimadas)
# Apresente o sumário dos dados.
summary(Queimadas)
# Apresente a estrutura dos dados.
str(Queimadas)
# Quantos biomas estão sendo afetados?
unique(Queimadas$bioma)
lenght(unique(Queimadas$bioma))
Queimadas$bioma = factor((Queimadas$bioma))
nlevels(Queimadas$bioma)
levels(Queimadas$bioma)
# Qual a média de avg_numero_dias_sem_chuva para os estados da região sul e da região norte?
sul = subset(Queimadas, estado == "PARANÁ" | estado == "SANTA CATARINA" | estado == "RIO GRANDE DO SUL")
mean(sul$avg_numero_dias_sem_chuva)
summary(sul)
dim(sul)
r_sul = toupper(c("paraná", "Santa Catarina", "rio grande do sul"))
r_norte = toupper(c("amazonas", "amapá", "roraima", "acre", "rondônia"))
Queimadas_sul = subset(Queimadas, estado %in% r_sul)
mean(Queimadas_sul$avg_numero_dias_sem_chuva)

#### DATA.TABLE
require(data.table)
library(data.table)
# Criar um data.table
meu_data_table <- data.table(
  nome = c("Alice", "Bob", "Carol", "Ana", "João", "Carlos", "Patrícia", "Leonardo"),
  idade = c(25, 30, 28, 20, 27, 50, 60, 45),
  salario = c(5000, 6000, 5500, 8000, 2000, 3500, 10000, 3800 ), 
  meio_de_transporte = c('onibus', 'bicicleta', 'onibus', 'carro', 'carro', 'onibus', 'onibus', 'bicicleta'))
meu_data_table

class(meu_data_table)
# Queimadas = fread("~/Documentos/ProjetoCE302_2024/CE302_2024_Aula/Data/Queimadas.csv")
system.time(fread("~/Documentos/ProjetoCE302_2024/CE302_2024_Aula/Data/Queimadas.csv"))

# Selecionar colunas e filtrar linhas
resultado <- meu_data_table[idade > 25, .(nome, salario)]
resultado

# Agregar dados 
agregado <- meu_data_table[, .(media_salario = mean(salario)),]
agregado

# Agregar dados por meio de transporte
agregado_idade <- meu_data_table[, .(media_salario = mean(salario)), by = meio_de_transporte]
agregado_idade

#### TIBBLE
require(tibble)
require(magrittr)
require(dplyr)
meu_tibble <- tibble(
  nome = c("Alice", "Bob", "Carol", "Ana", "João", "Carlos", "Patrícia", "Leonardo"),
  idade_anos = c(25, 30, 28, 20, 27, 50, 60, 45),
  salario = c(5000, 6000, 5500, 8000, 2000, 3500, 10000, 3800 ), 
  `meio de transporte` = c('onibus', 'bicicleta', 'onibus', 'carro', 'carro', 'onibus', 'onibus', 'bicicleta'))
meu_tibble

str(meu_tibble)
glimpse(meu_tibble)

meu_tibble$nova_coluna <- c(1, 2, 3, 4, 5, 6, 7, 8)
meu_tibble

meu_tibble <- mutate(meu_tibble, `minha coluna` = 1:8)
meu_tibble <- rename(meu_tibble, idade = idade_anos)
meu_tibble

meu_tibble_sem_salario <- select(meu_tibble, -salario)
meu_tibble_sem_salario
meu_tibble_com_salario <- select(meu_tibble, salario)
meu_tibble_com_salario

salario = pull(meu_tibble, salario)
salario

# Filtrar e ordenar
resultado <- filter(meu_tibble, idade > 25) 
arrange(resultado, desc(salario)) # para ascendente só colocar salario

# Agregar por meio de transporte e calcular média de salários
agregado_por_transporte <-  group_by(meu_tibble, `meio de transporte`) 
agregado_por_transporte
summarize(agregado_por_transporte, media_salario = mean(salario))


#### LISTAS
## Agregado de qualquer tipo de dados de interesse. Não tem estrutura tabular
# Exemplo de criação de lista
minha_lista <- list(
  vetor = c(1, 2, 3, 4, 5),
  matriz = matrix(1:9, nrow = 3),
  data_frame = data.frame(
    nome = c("Alice", "Bob", "Carol"),
    idade = c(25, 30, 28)
  ),
  lista_aninhada = list(
    vetor_aninhado = c(10, 20, 30),
    matriz_aninhada = matrix(1:4, nrow = 2)
  )
)
minha_lista

minha_lista$data_frame
minha_lista$vetor
minha_lista$lista_aninhada
#minha_lista$matriz$data_frame

# Acessar elementos da lista

elemento1 <- minha_lista[[1]]  # Acessar o primeiro elemento

elemento2 <- minha_lista$data_frame  # Acessar o data frame

elemento3 <- minha_lista$lista_aninhada$vetor_aninhado  # Acessar o vetor aninhado

# Adicionar elementos a uma lista

minha_lista$nova_lista <- list(novo_vetor = c(1, 2, 3), nova_matriz = matrix(1:4, nrow = 2))

minha_lista

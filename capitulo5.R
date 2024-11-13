require(tidyverse)
require(magrittr)
require(dplyr)

###############################################################################
# WHILE
i <- 1 # sempre definimos o critério de parada fora do loop

while (i < 6) {
  print(i)
  i <- i + 1 # Sempre alteramos o critério 
  # de parada, senão caímos em um loop infinito
}

# BREAK
i <- 1
while (i < 6) {
  print(i)
  i <- i + 1
  if (i == 4) {
    break
  }
}

contador <- 0
i <- 0
while (i < 75) { # rodar números (i) entre 1 e 100 e parar caso i < 75
  i = sample(1:100, size = 1) # size = 1 para i ser apenas um número
  contador = contador + 1 # parar se não aparecer i < 75 mas contador for = 4
  print(contador)
  print(i)
  if (contador == 4)
    break
}

# NEXT, para pular valor específico
i <- 0
while (i < 6) {
  i <- i + 1
  if (i == 3) {
    next
  }
  print(i)
}

# Exercício
set.seed(1234)

dado <- seq(1:6)
n_lancamento = 0
sorteio = 0
df = list()
while (sorteio != 5) {
  sorteio =  sample(dado, 1)
  n_lancamento = n_lancamento + 1
  df [[n_lancamento]] = data.frame(n_sorteio = sorteio,
                                  n_lancamento = n_lancamento)
  
  cat(paste0("\n\nLançamento: ", n_lancamento, "\nValor Sorteado: ", sorteio))
}
df %<>% dplyr::bind_rows() 
df

n_lancamento

n_lancamento = 0
while (sorteio != 7) { # loop infinito
  sorteio =  sample(dado, 1)
  n_lancamento = n_lancamento + 1
  
  cat(paste0("\n\nLançamento: ", n_lancamento, "\nValor Sorteado: ", sorteio))
  
  if(n_lancamento == 100){
    break
  }
}

valor_sorteado = numeric()
n_lancamento = 0
while (sorteio != 7) {
  n_lancamento = n_lancamento + 1
  valor_sorteado[n_lancamento] =  sample(dado, 1)
  
  
  cat(paste0("\n\nLançamento: ", n_lancamento, "\nValor Sorteado: ", sorteio))
  
  if(n_lancamento == 100){
    break
  }
}

valor_sorteado

##############################################################################
# FOR
for(i in 1:10) {
  x1 <- i^2 # para cada valor de 1 a 10, eleve ao quadrado
  print(x1)
}

# Aninhar o for()

## Função
dado = c(1:6)
soma_dois_dados = function(dado1, dado2){
  soma = dado1 + dado2
  # retorno não é necessário, pois apenas uma computação está sendo feita
  # após return a função para de funcionar
}
quadrado_soma = function(soma){
  soma2 = soma^2
  return(soma2)
}

resultado = list()
k = 0 
for(i in dado){
  for(j in dado){
    k = k + 1 # contador
    soma = soma_dois_dados(dado[i], dado[j])
    somaqd = quadrado_soma(soma)
    
    resultado[[k]] = data.frame(dado1 = dado[i], 
                                # [k] é a posição dentro da lista
                                dado2 = dado[j], 
                                soma = soma, 
                                soma2 = somaqd)
  }
}

resultado
resultado %<>% bind_rows() # transforma lista em dataframe
resultado

# APPLY

matriz1 <- matrix(1:6, nrow = 2)
soma_linhas <- apply(matriz1, 1, sum)
soma_colunas <- apply(matriz1, 2, sum)
apply(matriz1, 2, median)
apply(matriz1, 2, mean)

minha_lista <- list(a = c(1, 2, 3), b = c(4, 5, 6))
resultados <- lapply(minha_lista, mean) # l de lista; recebe e retorna listas
resultados

minha_lista <- list(a = c(1, 2, 3), b = c(4, 5, 6), c=c(7,6,8))
resultados <- sapply(minha_lista, mean) # entrada é lista e saída é vetor
resultados

resultado <- mapply(soma_dois_dados, # vários argumentos
                    dado, 
                    dado)

print(resultado)

dois_dados = expand.grid(dado, dado) # fornece todas as combinações possíveis
dois_dados # só funciona com vetores; para dataframe expand_grid() do tidyverse

resultado <- mapply(soma_dois_dados, 
                    dois_dados$Var1, 
                    dois_dados$Var2)

print(resultado)

resultado <- mapply(soma_dois_dados, 
                    dois_dados[,1], 
                    dois_dados[,2])
print(resultado)

###########################################################################
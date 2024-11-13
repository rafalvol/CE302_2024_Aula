require(magrittr)
require(dplyr)
#############################################################################
# FUNÇÕES E EXPRESSÕES
### Boas práticas: criar um código para identificar função própria
data("iris")

media_sepal_len = round(sum(iris$Sepal.Length)/length(iris$Sepal.Length),2)
media_sepal_len

media_sepal_wid = round(sum(iris$Sepal.Width)/length(iris$Sepal.Width),2)
media_sepal_wid

media_petal_len = round(sum(iris$Petal.Length)/length(iris$Petal.Length),2)
media_petal_len

media_petal_wid = round(sum(iris$Petal.Width)/length(iris$Petal.Width),2)
media_petal_wid

minha_media <- function(vetor_de_dados){
  # vetor_de_dados é uma variável/objeto que só existe dentro da função
  # pode ser qualquer nome desejado
  media = sum(vetor_de_dados)/length(vetor_de_dados)
  media = round(media, digitos)
  return(media)
}

minha_media(iris$Sepal.Length)

minha_media <- function(vetor_de_dados, digitos = 2){
  media = sum(vetor_de_dados)/length(vetor_de_dados)
  media = round(media, 2)
  return(media)
}
 minha_media(iris$Sepal.Length, 6)

 minha_media_arredond <- function(vetor_de_dados, arredondamento = 5){
   media = sum(vetor_de_dados)/length(vetor_de_dados)
   media = round(media, arredondamento)
   return(media)
 }
 
 minha_media_arredond(iris$Sepal.Length)
 
 meu_desvio_padrao_amostral <- function(vetor) {
   media <- minha_media_arredond(vetor) 
   diferenca <- vetor - media  # Calcula as diferenças em relação à média
   quadrados <- diferenca^2  # Calcula os quadrados das diferenças
   variancia <- sum(quadrados) / (length(vetor) - 1)  # Calcula a variância
   desvio_padrao <- sqrt(variancia)  # Calcula o desvio padrão
   return(desvio_padrao)
 }
 
 
 meu_desvio_padrao_amostral(iris$Sepal.Length)
 
#' Cálculo do Coeficiente de Variação
#'
#' @param vetor Vetor de dados 
#' @param arredondamento Arredonda o valor para 2 digitos
#'
#' @return
#' @export
#'
#' @examples
 meu_coeficiente_variacao <- function(vetor, arredondamento = 2) {
   
   media <- minha_media_arredond(vetor, arredondamento = arredondamento)  # Calcula a média
   desvio_padrao <- meu_desvio_padrao_amostral(vetor)  # Calcula o desvio padrão
   coeficiente_variacao <- (desvio_padrao / media) * 100  # Calcula o CV em porcentagem
   coeficiente_variacao = round(coeficiente_variacao, arredondamento)
   return(coeficiente_variacao)
 }
 
 meu_coeficiente_variacao(iris$Sepal.Length, arredondamento = 2)

 ## RETIRAR OS NA's
 
 ## Retornos
 meu_coeficiente_variacao3 <- function(vetor, arredondamento = 2) {
   media <- minha_media_arredond(vetor, arredondamento = arredondamento)  # Calcula a média
   desvio_padrao <- meu_desvio_padrao_amostral(vetor)  # Calcula o desvio padrão
   coeficiente_variacao <- (desvio_padrao / media) * 100  # Calcula o CV em porcentagem
   coeficiente_variacao = round(coeficiente_variacao, arredondamento)
   return(list(CV = coeficiente_variacao, 
               média = media, 
               dp = desvio_padrao))
   
 }
 
 meu_coeficiente_variacao3(iris$Sepal.Length, arredondamento = 2)
 
 meu_coeficiente_variacao3 <- function(vetor, arredondamento = 2) {
   media <- minha_media_arredond(vetor, arredondamento = arredondamento)  # Calcula a média
   desvio_padrao <- meu_desvio_padrao_amostral(vetor)  # Calcula o desvio padrão
   coeficiente_variacao <- (desvio_padrao / media) * 100  # Calcula o CV em porcentagem
   coeficiente_variacao = round(coeficiente_variacao, arredondamento)
   out = data.frame(media = media,
                    n = length(vetor),
                    desvio = desvio_padrao,
                    coeficiente = coeficiente_variacao)
   return(out)
 }
 
 meu_coeficiente_variacao3 <- function(vetor, arredondamento = 2) {
   media <- minha_media_arredond(vetor, arredondamento = arredondamento)  # Calcula a média
   desvio_padrao <- meu_desvio_padrao_amostral(vetor)  # Calcula o desvio padrão
   coeficiente_variacao <- (desvio_padrao / media) * 100  # Calcula o CV em porcentagem
   coeficiente_variacao = round(coeficiente_variacao, arredondamento)
   return(list(CV = coeficiente_variacao, 
               média = media, 
               dp = desvio_padrao))
   
 }
 
 meu_coeficiente_variacao3(iris$Sepal.Length, arredondamento = 2)
 
 ## MENSAGENS
 x <- -5
 if (x < 0) {
   message("O valor de x é negativo.") # Mensagem de texto no console/ecrã
   cat("O valor de x é negativo.")
   }
 x <- -5
 if (x < 0) {
   warning("O valor de x é negativo.")
 }
x <- -5
 if (x < 0) {
   stop("O valor de x é negativo. 
        Altere o valor de x para que seja maior que 0.")
 }

# CONTROLE DE FLUXO
## Estruturas Condicionais
idade <- 25

if (idade >= 18) {
  cat("Você é maior de idade.\n")
} else {
  cat("Você é menor de idade.\n")
}

## if aninhado
pontuacao = 90

if (pontuacao >= 90) {
  nota = "A"
} else {
  if (pontuacao >= 80) {
    nota = "B"
  } else {
    if (pontuacao >= 70) {
      nota = "C"
    } else {
      nota = "D"
    }
  }
}

nota


classifica_nota <- function(pontuacao) {
  if (pontuacao >= 90) {
    nota <- "A"
  } else {
    if (pontuacao >= 80) {
      nota <- "B"
    } else {
      if (pontuacao >= 70) {
        nota <- "C"
      } else {
        nota <- "D"
      }
    }
  }
  cat(paste("A nota do aluno é:", nota))
  return(nota)
}

pontuacao_aluno <- 85
nota <- classifica_nota(pontuacao_aluno)

quadrante <- function(x, y) {
  if (x > 0) {
    if (y > 0) {
      quadrante = "Quadrante 1"
      
      cat(paste0("O ponto (", x, ", ", y, ") pertence ao ",  quadrante))
      return(quadrante)
    } else {
      quadrante = "Quadrante 4"
      
      cat(paste0("O ponto (", x, ", ", y, ") pertence ao ",  quadrante))
    }
  } else {
    if (y > 0) {
      quadrante = "Quadrante 2"
      
      cat(paste0("O ponto (", x, ", ", y, ") pertence ao ",  quadrante))
    } else {
      quadrante = "Quadrante 3"
      
      cat(paste0("O ponto (", x, ", ", y, ") pertence ao ",  quadrante))
      print(paste0("O ponto (", x, ", ", y, ") pertence ao ",  quadrante))
    }
  }
}


quadrante(1, 1)

## if_else e ifelse
iris %<>% 
  mutate(cat_petal.len = ifelse(Petal.Length > mean(Petal.Length), "Longa", "Curta"))
  # funciona para vetor mas não para escalar
iris %<>% 
  mutate(cat_petal.len2 = if_else(Petal.Length > mean(Petal.Length), "Longa", "Curta"))
  # funciona para escalar mas não para vetor

# SWITCH
dia_da_semana <- "segunda"

mensagem <- switch(dia_da_semana,
                   "segunda" = "Hoje é segunda-feira.",
                   "terca" = "Hoje é terça-feira.",
                   "quarta" = "Hoje é quarta-feira.",
                   "quinta" = "Hoje é quinta-feira.",
                   "sexta" = "Hoje é sexta-feira.",
                   "sabado" = "Hoje é sábado.",
                   "domingo" = "Hoje é domingo.",
                   "Outro" = "Dia não reconhecido."
)

cat(mensagem)

# case_when() versão do tidyverse; usa-se para letras ou números
iris$cat_sepal = 
  case_when((iris$Sepal.Length < mean(iris$Sepal.Length) - 
               sd(iris$Sepal.Length)) ~ "X < media - 1 sd", 
            (iris$Sepal.Length < mean(iris$Sepal.Length) + 
               sd(iris$Sepal.Length)) ~ "X < media + 1 sd", 
            .default = "X > media + 1 sd")

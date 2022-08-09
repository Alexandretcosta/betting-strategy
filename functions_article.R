library(markovchain)
library(diagram)
library(compiler)


####### Funções

## Criação de Fibonaccin Seq

fibonacci_seq <- function(nterms,n1 = 1,n2 = 2){
  #print("Fibonacci sequence:")
  #print(n1)
  #print(n2)
  sequencia <- c(n1,n2)
  count <- 2
  while(count < nterms) {
    nth = n1 + n2
    #print(nth)
    # update values
    n1 = n2
    n2 = nth
    count = count + 1
    sequencia <- c(sequencia,nth)
  } 
  return(sequencia)
}

## Criação Matriz de Transação

matriz_transacition <- function(nterms, t0 = 1, prob_trans = 0.5){
  if(prob_trans > 1 | nterms < 2 | prob_trans < 0){
    return(print("Error: parâmetros errados 'nterms' or 'prob_trans' "))
  }else{
    nterms <- nterms + t0 - 1
    valor = NULL
    for(i in 1:nterms){
      vetor <- rep(0,nterms)
      if(i == nterms){
        vetor[i] <- 1 - prob_trans
      }else{
        vetor[i+1] <- 1 - prob_trans}
      if(i == 1){
        vetor[i] <- prob_trans
      }else{
        if(i == 2){
          vetor[i-1] <- prob_trans
        }else{
          vetor[i-2] <- prob_trans
        }
      }
      valor <- c(valor,vetor)
    }
    trans_mat <- matrix(valor,nrow = nterms, byrow = TRUE) 
    estados <- as.character(fibonacci_seq(nterms = nterms))
    disc_trans <- new("markovchain",transitionMatrix=trans_mat, 
                      states = estados, name="Markov Chain 1")
    
  return(disc_trans)}
}

## Criação Objeto 'markovchain'

####### TESTE NUMERICO
fibonacci_seq <- cmpfun(fibonacci_seq)
matriz_transacition <- cmpfun(matriz_transacition)

teste_numerico <- function(nsteps = 100,
                           n_matriz = 100,
                           prob = 18/37,
                           time_0 = 1,
                           percent_win = 1,
                           percent_lost = 1,
                           mc1 = 100,
                           mc2 = 10,
                           criterio_parada = 'true',
                           spread = 10000){
  disc_trans <- matriz_transacition(n_matriz, t0 = time_0,prob)

  if(criterio_parada == 'true'){
    ### Monte Carlo 
    df <- data.frame(ponto_inicial = numeric(0), nsteps = numeric(0),numero_vitorias = numeric(0),
                     numero_derrotas = numeric(0),maximo_valor_ganho = numeric(0),  maximo_valor_perda = numeric(0),
                     maximo_aposta = numeric(0),  valor_ganho = numeric(0))
    process <- mc1
    for(j in 1:process){
      
      resultado <- markovchainSequence(n = nsteps ,
                                       t0 = as.numeric(disc_trans@states)[time_0],
                                       markovchain=disc_trans,
                                       include=TRUE)
      
      resultado <- as.numeric(resultado)
      
      valores <- NULL
      nr_lost <- 0
      nr_win <- 0
      
      for(i in 1:(length(resultado)-1)){
        if(resultado[i+1] > resultado[i]){
          nr_lost <- nr_lost + 1
          valores[i] <- resultado[i]*-1*percent_lost
        }else{
          nr_win <- nr_win + 1
          valores[i] <- resultado[i]*percent_win
        }
      }
      values <- c(time_0,nsteps,nr_win,nr_lost,max(cumsum(valores)), min(cumsum(valores)), 
                  max(resultado),sum(valores))
      df[j,] <- values
      ## Começo da Aposta
    }  
    colnames(df) <- c('StartingPoint',
                      'NrBets',
                      'NrWins',
                      'NrLost',
                      'MaximumMoneyLoss',
                      'MaximumMoneyWon',
                      'MaximumBet',
                      'EarnedValue')
    lista <- list(df = df)
    
  }else{

    ### Monte Carlo 
    df <- data.frame(time_0 = numeric(0), nr_jogadas = numeric(0), nr_win = numeric(0),
                     nr_lost = numeric(0), max_lost = numeric(0),
                     percent_lucro = numeric(0),win_value = numeric(0))
    process <- mc2
    for(k in 1:process){
      
    valores <- NULL
    nr_lost <- 0
    nr_win <- 0
    resultado <- NULL
    j = 0
    parada = FALSE
    while(parada == FALSE){
      j = j + 1
      resultado <- as.numeric(markovchainSequence(n = 1 ,
                                                  t0 = tail(c(as.numeric(disc_trans@states)[time_0],resultado),1),
                                                  markovchain=disc_trans,
                                                  include=TRUE))
        if(resultado[2] > resultado[1]){
          nr_lost <- nr_lost + 1
          valores[j] <- resultado[1]*-1*percent_lost
        }else{
          nr_win <- nr_win + 1
          valores[j] <- resultado[1]*percent_win
        }
      
      
      if(sum(valores)/time_0 > spread){
        parada = TRUE
      }else{parada = FALSE}
    }
    valor_ganho <- sum(valores)
    values <- c(time_0, j, nr_win, nr_lost, min(cumsum(valores)), (valor_ganho-time_0)/time_0, valor_ganho)
    df[k,]<- values 
    }
    colnames(df) <- c('StartingPoint',
                     'NrBets',
                     'NrWins',
                     'NrLost',
                     'MaximumMoneyLoss',
                     'PercProfit',
                     'EarnedValue')
    lista <- list(df = df)
  }
  return(lista)
}

#parametros <- list(nsteps = 100,
#                   n_matriz = 100,
#                   prob = 18/37,
#                   time_0 = 1,
#                   percent_win = 1,
#                   percent_lost = 1,
#                   monte_carlo = 100,
#                   criterio_parada = 'FALSE',
#                   spread = 10000)
teste_numerico2 = cmpfun(teste_numerico, options=setCompilerOptions(optimize=3, suppressAll=T))

#system.time(teste_numerico(nsteps=100000, mc1=100))
#
#system.time(teste_numerico2(nsteps=100000, mc1=100))

#aux <- teste_numerico(nsteps = 100,
#               n_matriz = 100,
#               prob = 18/37,
#               time_0 = 1,
#               percent_win = 1,
#               percent_lost = 1,
#               mc1 = 10,
#               mc2 = 10,
#               criterio_parada = 'true',
#               spread = 1000)
#vv <- aux[['df']]$valor_ganho

library(markovchain)
library(diagram)

## Parametros 

n0 <- 100
p <- 0.5

### Creating a transition matrix for Sequence Fibonacci 
## n x n
## p é a probabilidade de ganhar
valor = NULL

for(i in 1:n0){
  vetor <- rep(0,n0)
  if(i == n0){
    vetor[i] <- 1 - p
  }else{
  vetor[i+1] <- 1 - p}
  if(i == 1){
    vetor[i] <- p
  }else{
    if(i == 2){
      vetor[i-1] <- p
    }else{
      vetor[i-2] <- p
    }
  }
  print(vetor)
  valor <- c(valor,vetor)
}
trans_mat <- matrix(valor,nrow = n0, byrow = TRUE) 


###### Sequence Fibonacci

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

sequencia <- fibonacci_seq(nterms = n0)
sequencia[10]
# Creating a transition matrix

trans_mat

# create the Discrete Time Markov Chain
disc_trans <- new("markovchain",transitionMatrix=trans_mat, states=fibonacci_seq(n0), name="Markov Chain 1") 
disc_trans
pi <- steadyStates(disc_trans)
sum(pi[1,]) - sum(pi[1,1:10]) 
Current_state<-c(rep(0,n0-1),1)

steps <- 10
finalState<-Current_state*disc_trans^steps #using power operator
finalState[1,n0]
plot(disc_trans)


###### Simulation with Fibonacci System

start <- 1 
resultado_roleta <- rbinom(1,1,prob = 0.5)





N <- 1000
t <- 1000
cells <- matrix(0,N,t)

### CURRENT PROBLEM: NEED TO DEAL WITH NEGATIVE VALUES! 
D1_Prop <- function(k,d_x,N,t,init_vec){
  cell_mat <- matrix(0,N,t);
  cell_mat[,1] <- init_vec;
for(j in 2:t) {
  for(i in 2:(N-1)){
    cell_mat[i,j] <- cell_mat[i,j-1] + k*(cell_mat[i+1,j-1] + cell_mat[i-1,j-1] - 2*cell_mat[i,j-1])
    if(j<5){
    cell_mat[1,j] <- init_vec[1];
    } else{
     cell_mat[1,j] <- cell_mat[1,j-1] + k*(cell_mat[i+1,j-1] - cell_mat[i,j-1]) - k*cell_mat[i,j-1];
    }
  }
}
  return(cell_mat)
}

k <- 0.5
init_vec <- c(1,rep(,N-1))
D1_sim <- D1_Prop(k,1,N,t,init_vec)

plot(D1_sim[,60])


apply(D1_sim,2,sum)
sum(init_vec)
print(D1_sim[,3])

1-pbinom(50,100,0.5)
dbinom(50,100,0.5)
5/105

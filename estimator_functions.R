# library(caret)

# Is it really necessary to store the coeffs?
produce_estimate = function(X, Y, A, d) {
  # X=feats
  # Y=rewards
  # A=actions+1
  # d=k.num.actions
  # takes in data: X = n by dx matrix of features
  #               Y = n outcomes/rewards
  #               A = n (integer) actions, 1:d
  #               d = number of actions
  # returns: list of two matrices:
  #           return[[1]] = d by dx+1 matrix where each 
  #                         row i represents lm coeffs to estimate means of action i
  #           return[[2]] = d by dx+1 matrix where each 
  #                         row i represents lm coeffs to estimate probabilities of action i
  e_mean = matrix(NA, nrow = d, ncol = ncol(X)+1)
 # e_prob = matrix(NA, nrow = d, ncol = ncol(X)+1)
  for (i in 1:d) {
    if (any(A==i)) {
      e_mean[i, ] = lm(y ~ ., data = data.frame(matrix(X[A == i, ], ncol=ncol(X)), y = Y[A==i]))$coefficients
      if(any(is.na(lm(y ~ ., data = data.frame(matrix(X[A == i, ], ncol=ncol(X)), y = Y[A==i]))$coefficients))) {
        stop(paste('Data is co-dependent (not enough unique data to model regression):',
                   sum(A == i), 'data points with action', i, ',', ncol(X), 'features'))
      }
    }

  # TODO: what if there are no A == i ?
     # e_prob[i, ] = lm(y ~ ., data = data.frame(X, y = (A==i)))$coefficients
     # if(any(is.na(lm(y ~ ., data = data.frame(X, y = (A==i)))$coefficients))) {
     #   stop(paste('Data-probability for action', i, 'is co-dependent (not enough unique data to model regression)'))
     # }
   }
  
  library(nnet)
  model = multinom(A~X)
  e_prob = matrix(model$wts,byrow=TRUE,nrow=d)[,-1]
# return(e_prob)
  return(list(e_mean, e_prob))
}

produce_k_estimates = function(X, Y, A, d, k) {
  # takes in data: X = n by dx matrix of features
  #               Y = n outcomes/rewards
  #               A = n (integer) actions, 1:d
  #               d = number of actions
  #               k = number of folds
  # returns: k-length list of lists, inner has 3 matrices:
  #           return[[j]][[1]] = d by dx+1 matrix where each 
  #                         row i represents lm coeffs to estimate means of action i in -folds[[j]]
  #           return[[j]][[2]] = d by dx+1 matrix where each 
  #                         row i represents lm coeffs to estimate probabilities of action i in -folds[[j]]
  #           return[[j]][[3]] = vector of indices representing folds[[j]] 
  
  estimates = list()
  # uncomment these lines for randomized folds
  # folds = createFolds(Y, k)
  # for (j in 1:k) {
  #   estimate = produce_estimate(X[-folds[[j]], ], Y[-folds[[j]]], A[-folds[[j]]], d)
  #   estimate = append(estimate, list(folds[[j]]))
  #   estimates = append(estimates, list(estimate))
  # }
  
  # uncomment these lines for deterministic folds
  n = length(Y)
  for (i in 1:k) {
    inds = (floor((n/k)*(i-1)) + 1):(floor((n/k)*i))
    estimate = produce_estimate(X[-inds, ], Y[-inds], A[-inds], d)
    estimate = append(estimate, list(inds))
    estimates = append(estimates, list(estimate))
  }
  
  
  return(estimates)
}

calculate_Gamma = function(X, Y, A, d, k, eval = 'DR') {
  # takes in data: X = n by dx matrix of features
  #               Y = n outcomes/rewards
  #               A = n (integer) actions, 1:d
  #               d = number of actions
  #               k = number of folds
  #               eval = type of policy evaluation estimate to use:
  #                       DR = doubly robust, DM = direct method, IPS = inverse propensity score
  
  # returns: k-length list of lists, inner has 3 matrices:
  #           return[[j]][[1]] = d by dx+1 matrix where each 
  #                         row i represents lm coeffs to estimate means of action i in -folds[[j]]
  #           return[[j]][[2]] = d by dx+1 matrix where each 
  #                         row i represents lm coeffs to estimate probabilities of action i in -folds[[j]]
  #           return[[j]][[3]] = vector of indices representing folds[[j]] 
  
  estimates = produce_k_estimates(X, Y, A, d, k)
  
  n = length(Y)
  Gamma = matrix(NA, nrow = n, ncol = d)
  X_aug = cbind(matrix(1, nrow=n, ncol=1), X)
  A_aug = matrix(0, nrow = n, ncol = d)
  A_aug[cbind(X1=1:n, X2=A)] = 1
  
  for (j in 1:k) {
    e_means = estimates[[j]][[1]]
    e_probs = estimates[[j]][[2]]
    k_inds = estimates[[j]][[3]]
    
    
    for (i in k_inds) {
      
      sum_e_probs = sum(exp(e_probs %*% X_aug[i, ]))
      
      
      if (eval == 'DR') {
        mean_xi = e_means %*% X_aug[i, ]
        # prob_xi = e_probs %*% X_aug[i, ]
        prob_xi = exp(e_probs %*% X_aug[i, ])/sum_e_probs
        Gamma[i, ] = t(mean_xi) + (Y[i] - t(mean_xi))/t(prob_xi) * A_aug[i, ]
        
      } else if (eval == 'DM') {
        mean_xi = e_means %*% X_aug[i, ]
        Gamma[i, ] = t(mean_xi) 
        
      } else if (eval == 'IPS') {
        prob_xi = e_probs %*% X_aug[i, ]
        Gamma[i, ] = Y[i]/t(prob_xi) * A_aug[i, ]
        
      } else {
        print('Not a valid policy evaluation estimator')
        return(0)
      }
    }
  }
  return(Gamma)
}



evaluate_policy = function(n, d, Pol, Gamma) {
  # n = nrow(X)
  # d = depth of tree
  # Pol = list of length n, 1 action for each data point
  # Gamma = estimator made from calculate_Gamma
  #
  # returns: E[Y(Pol(X))] = sum over i: Gamma_i[Pol[i]]
  if (length(Pol) != n) {
    stop('Policy length does not match data length')
  }
  
  # X_aug = cbind(matrix(1, nrow=n, ncol=1), X)
  Pol_aug = matrix(0, nrow = n, ncol = d)
  Pol_aug[cbind(X1=1:n, X2=Pol)] = 1
  
  inner_prod = Pol_aug * Gamma
  return(sum(inner_prod)/n)
}



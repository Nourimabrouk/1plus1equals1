# ------------------------------------------------------------------------------
# Optimization.R
# A demonstration of gradient descent with a "duality loss" function that tries
# to force 1+1 = 1.
# ------------------------------------------------------------------------------
library(dplyr)

#' duality_loss
#'
#' Simple function: ( (a + b) - target )^2
#' 
#' @param a numeric
#' @param b numeric
#' @param target numeric default 1
#' @return numeric loss
#' @export
duality_loss <- function(a, b, target = 1) {
  ((a + b) - target)^2
}

#' gradient_descent_duality
#'
#' Minimizes the duality loss by adjusting a, b to converge to 1+1=1.
#'
#' @param a_init numeric
#' @param b_init numeric
#' @param lr Learning rate
#' @param steps Number of iterations
#' @return A list with final a, b, and loss trajectory
#' @export
gradient_descent_duality <- function(a_init = 0.5, b_init = 0.5,
                                     lr = 0.01, steps = 100) {
  a <- a_init
  b <- b_init
  loss_history <- numeric(steps)
  for (i in seq_len(steps)) {
    # Compute gradients (derivative wrt a and b)
    # d/d(a) of ((a + b) - 1)^2 = 2*((a + b) - 1)
    grad_a <- 2 * ((a + b) - 1)
    grad_b <- 2 * ((a + b) - 1)
    
    # Update
    a <- a - lr * grad_a
    b <- b - lr * grad_b
    
    # Store loss
    loss_history[i] <- duality_loss(a, b)
  }
  return(list(a = a, b = b, loss_history = loss_history))
}

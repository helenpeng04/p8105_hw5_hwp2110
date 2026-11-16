#' sim_t_test
#'
#' @param n_obs number of observations
#' @param sigma population standard deviation
#' @param mu true population mean
#' 
#' @returns a tibble containing the simulated data and t-test results
#' @export
#'
#' @examples
sim_t_test = function(n_obs, sigma, mu){
  
  x = rnorm(n = n_obs, mean = mu, sd = sigma)
  test_result = broom::tidy(t.test(x, mu = 0))
  
  return_df = tibble(
    observation = x,
    estimate = test_result |> pull(estimate),
    p_value = test_result |> pull(p.value),
    rejected = p_value < 0.05
  )
  
  return(return_df)
  
}
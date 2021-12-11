envir_diff <- function(day0, day1){ 
  atemp_diff <- day0[["atemp"]] - day1[["atemp"]]
  wind_diff <- day0[["windspeed"]] -  day1[["windspeed"]]
  hum_diff <- day0[["hum"]] - day1[["hum"]]
  norm(as.matrix(c(4*atemp_diff, wind_diff, hum_diff)))
}

envir_diff_ij <- function(i, j){ 
  day0 <- data[i,]; day1 <- data[j,]
  envir_diff(day0, day1)
}

.unique_pairs_lower <- function(N){
  is <- do.call("c", purrr::map(1:N, function(i){rep(i,N)}))
  js <- rep(1:N, N)
  # Helper function: selects elements only if they are upper triangular
  .LowerTri <- function(i, j){if(i > j) { c(i = i, j = j) }}
  pairs <- do.call("rbind", purrr::map2(is, js, .f = .LowerTri))
  data.frame(pairs)
}

growth_ratio <- function(i, j){
  data2011$cnt[i]/data2011$cnt[j]
}
get_rel_ratio <- function(row){
  growth_ratio(i = env_diffs_sset[row, 1], j = env_diffs_sset[row, 2])
}
grr <- get_rel_ratio
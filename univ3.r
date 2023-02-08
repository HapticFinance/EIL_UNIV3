
get_liquidity_0 <- function(x, sa, sb) {
  return (x * sa * sb / (sb - sa))
}

get_liquidity_1 <- function(y, sa, sb) {
  return (y / (sb - sa))
}

get_liquidity <- function(x, y, sp, sa, sb) {
  if (sp <= sa) {
    liquidity <- get_liquidity_0(x, sa, sb)
  } else if (sp < sb) {
    liquidity0 <- get_liquidity_0(x, sp, sb)
    liquidity1 <- get_liquidity_1(y, sa, sp)
    liquidity <- min(liquidity0, liquidity1)
  } else {
    liquidity <- get_liquidity_1(y, sa, sb)
  }
  return (liquidity)
}

# Calculate x and y given liquidity and price range
calculate_x <- function(L, sp, sa, sb) {
  sp <- max(min(sp, sb), sa)     # if the price is outside the range, use the range endpoints instead
  return (L * (sb - sp) / (sp * sb))
}

calculate_y <- function(L, sp, sa, sb) {
  sp <- max(min(sp, sb), sa)     # if the price is outside the range, use the range endpoints instead
  return (L * (sp - sa))
}

calculate_a2 <- function(sp, sb, x, y) {
  sa <- y / (sb * x) + sp - y / (sp * x)
  return(sa^2)
}

calculate_b2 <- function(sp, sa, x, y){
  P <- sp^2
  return ((sp * y)/((sa * sp - P) * x + y))^2
}
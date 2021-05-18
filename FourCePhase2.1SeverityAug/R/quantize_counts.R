

quantize_counts <- function(op, x){
  output_counts = c()
  for (nn in names(x)){
    output_counts = c(output_counts,findInterval(x[[nn]], op[[nn]]))
  }
  names(output_counts) = names(x)
  return(output_counts)
}


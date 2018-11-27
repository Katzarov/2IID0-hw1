remove(datos)
datos = read.csv2('speed-dating-clean.csv', sep=",", dec=".", na.strings = "null", as.is = TRUE)

cols <- colnames(datos)

column_names = character()

for (i in 1:length(cols)){

  # Extract the name of the column
  col_name = cols[i]
  mode_col = mode(datos[[col_name]][1])
  column = datos[[col_name]]
  
  # Case that the type of the variable is numeric
  if(mode_col == "numeric"){
    
    column_names <- c(column_names, col_name)
      
  }
}

res = matrix(ncol = length(column_names), nrow = length(column_names), dimnames = list(column_names, column_names))


for(i in 1:length(column_names)){
  for(j in 1:length(column_names)){
    column = column_names[i]
    row = column_names[j]
    
    correlation = cor(datos[[column]], datos[[row]], use = "na.or.complete")
    
    res[i, j] = round(correlation, 2)
  }
}


write.csv2(res, file = paste(getwd(), "correlation-matrix.csv", sep = ""))
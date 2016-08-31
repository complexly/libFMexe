get_libFM_model <- function(savedmodel) {
  modeltext = readLines(savedmodel)
  line1 = which(modeltext=="#global bias W0")
  line2 = which(modeltext=="#unary interactions Wj")
  line3 = which(modeltext=="#pairwise interactions Vj,f")
  res = list()
  res$w0 = as.numeric(modeltext[line1+1])
  res$w1 = as.numeric(modeltext[(line2+1):(line3-1)])
  res$w2 = t(sapply(strsplit(modeltext[-(1:line3)], split=" "),as.numeric))
  return(res)
}

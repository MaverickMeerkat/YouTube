library(sets)
# seed
x = 1234
already_seen = set()
counter = 0
n = nchar(as.character(x))
while(!(x %in% already_seen)){
  counter = counter + 1
  already_seen = set_union(already_seen, set(x))
  t = as.character(x*x)
  n2 = nchar(t)
  add = 2*n-n2
  for (i in 1:add){
    new = paste0('0', t)
  }
  x = as.integer(substr(new, as.integer(n/2)+1, 2*n-as.integer(n/2)))
  print(c(counter, x))
}

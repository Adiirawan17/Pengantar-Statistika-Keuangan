anuitasmajemuk <- function(num, k, j, t, m=TRUE){
  v=(1/(1+j))
  n=t*m
  j=i/m
  switch(num, 
         anuitasakhir = {
           an = k*(1-v^n)/j
           sn = k*((1+j)^n-1)/j
           print(an)
           print(sn)
         },
         anuitasawal = {
           an = k*(1-v^n)/(j*v)
           sn = k*((1+j)^n-1)/(j*v)
           print(an)
           print(sn)
         }
  )
}
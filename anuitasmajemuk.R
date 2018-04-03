anuitasmajemuk <- function(num, k, i, t, m=TRUE){
  n=t*m
  j=i/m
  v=(1/(1+j))
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

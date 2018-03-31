setwd("G:\\Pengantar statistika keuangan")

anuitas <- function(num, k, i, n){
  v=(1/(1+i))
  switch(num, 
         anuitasakhir = {
           an = k*(1-v^n)/i
           sn = k*((1+i)^n-1)/i
           print(an)
           print(sn)
         },
         anuitasawal = {
           an = k*(1-v^n)/(i*v)
           sn = k*((1+i)^n-1)/(i*v)
           print(an)
           print(sn)
         }
  )
}
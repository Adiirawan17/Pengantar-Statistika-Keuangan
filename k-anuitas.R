angsuran <- function(num, nilai, i, t, m=TRUE){
  n=t*m
  j=i/m
  v=(1/(1+j))
  switch(num, 
         anuitasakhir = {
           kan = nilai/((1-v^n)/j)
           ksn = nilai/(((1+j)^n-1)/j)
           print(kan)
           print(ksn)
         },
         anuitasawal = {
           kan = nilai/((1-v^n)/(j*v))
           ksn = nilai/(((1+j)^n-1)/(j*v))
           print(kan)
           print(ksn)
         }
  )
}
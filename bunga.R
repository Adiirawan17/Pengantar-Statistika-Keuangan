bunga <- function(num, a, k, t, m=TRUE)
  switch(num, 
         bungatunggal = {
           bunga1 = ((a/k)-1)/t
           print(bunga1)
         },
         bunganominal = {
           bunga2 = (((a/k)^(1/(m*t))-1))*m
           print(bunga2)
         },
         bungakontinu = {
           bunga3 = (log(a/k))/t
           print(bunga3)
         }
  )

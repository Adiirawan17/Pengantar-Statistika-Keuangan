PV <- function(num, a, i, t, m=TRUE)#present value
  switch(num, 
         bungatunggal = {
           abunga1 = a/(1+(i*t))
           print(abunga1)
         },
         bunganominal = {
           abunga2 = a/((1+(i/m))^(m*t))
           print(abunga2)
         },
         bungakontinu = {
           abunga3 = a/(exp(i*t))
           print(abunga3)
         }
  )

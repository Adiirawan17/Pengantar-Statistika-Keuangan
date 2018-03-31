setwd("G:\\Pengantar statistika keuangan")
nilaikakumulasiall <- function(num, k, i, t, m=TRUE) #k=modal, i=bunga pertahun, t= tahun, m=banyak pembelian bunga dalam setahun
  switch(num, 
         nilaitunggal = {
           hasil = k*(1+i)^t
           print(hasil)
         },
         nilainominal = {
           hasil1 = k*(1+(i/m))^(m*t)
           print(hasil1)
         },
         nilaikontinu = {
           hasil2 = k*exp(m*t)
           print(hasil2)
         }
         )
        
           
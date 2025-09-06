# ========================================================================
# NOMOR 1
# ========================================================================

# FUNGSI
lcg_int = function(n, a, c, m) {
  rng = vector(length = n)
  x = as.numeric(Sys.time())*1000
  for (i in 1:n){
    x = (a*x+c) %% m
    rng[i] = x %% 21
  }
  return(rng)
}

# KOMBINASI 1 [a=543; c=16; m=7]
hasil1=lcg_int(20,543,34,5)
cat("Berikut bilangan acak dalam interval 0-20 yang dibangkitkan: \n", hasil1 , "\n")
cat("Hasil cek duplikasi bilangan: ", sum(duplicated(hasil1)), "Duplikasi Bilagan", "\n") 

# KOMBINASI 2 [a=6789; c=987; m=6]
hasil2=lcg_int(20,6789,777,96)
cat("Berikut bilangan acak dalam interval 0-20 yang dibangkitkan: \n", hasil2 , "\n")
cat("Hasil cek duplikasi bilangan: ", sum(duplicated(hasil2)), "Duplikasi Bilagan", "\n")

# KOMBINASI 3 [a=6789; c=987; m=6]
hasil3=lcg_int(20,1708,2005,81)
cat("Berikut bilangan acak dalam interval 0-20 yang dibangkitkan: \n", hasil3 , "\n")
cat("Hasil cek duplikasi bilangan: ", sum(duplicated(hasil3)), "Duplikasi Bilagan", "\n")



# ========================================================================
# NOMOR 2
# ========================================================================

# INSTALL PACKAGE YANG DIPERLUKAN
install.packages("scatterplot3d")
library(scatterplot3d) 

n=2000

# FUNGSI
lcg_unif = function(n, a, c, m) {
  rng = vector(length = n)
  x = as.numeric(Sys.time())*1000
  for (i in 1:n){
    x = (a*x+c) %% m
    rng[i] = x/m
  }
  return(rng)
}

# KOMBINASI a, c, DAN m
komb = list(
  list(a=323,   c=2113, m=10,    name="KOMBINASI 1"),
  list(a=69069, c=0,    m=2^32,  name="KOMBINASI 2"),
  list(a=31,    c=1,    m=10,    name="KOMBINASI 3")
)


# Perbandingan Plot 2D
par(mfrow=c(1,3)) 
for (p in komb){ 
  x <- lcg_unif(n, p$a, p$c, p$m) 
  y <- lcg_unif(n, p$a, p$c, p$m) 
  
  plot(x, y, pch=16, cex=0.5, 
       main=p$name, 
       xlab="X", ylab="Y") 
} 

# Perbandingan Plot 3D
par(mfrow=c(1,3)) 
for (p in komb){ 
  x <- lcg_unif(n, p$a, p$c, p$m) 
  y <- lcg_unif(n, p$a, p$c, p$m) 
  z <- lcg_unif(n, p$a, p$c, p$m) 
  
  scatterplot3d(x, y, z, pch=16, cex.symbols=0.5, 
                main=p$name, 
                xlab="X", ylab="Y", zlab="Z") 
} 


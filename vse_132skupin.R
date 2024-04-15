################################################################################
###                    ANALYZA LETU Z NEWYORSKYCH LETIST                     ###
###                          VSECH 132 SKUPIN LETU                           ###
################################################################################

# 1) zobrazeni vsech hustot a jejich rozkladu (viz kapitoly 3.1-3.3 diplomove prace,
#    kde byly zobrazeny pouze hustoty pro 9 vybranych skupin letu)

# 2) kod ke kapitolam 3.4 a 3.5 diplomove prace

##------------------------------------------------------------------------------
##  1) HUSTOTY A JEJICH ROZKLAD PRO VSECH 132 SKUPIN LETU
##------------------------------------------------------------------------------
# vsechny vykreslene obrazky se nachazeji ve slozce "obrazky"

library(nycflights13)
library(grDevices)

data = flights

##  Uprava dat
##------------------------------------------------------------------------------
# 4 vzdalenostni kategorie
distance_category = c()
for(i in 1:length(data$distance)){
  if(data$distance[i] <= 500){
    distance_category[i] = 1
  }else if (data$distance[i] > 500 & data$distance[i] <= 1000){
    distance_category[i] = 2
  }else if (data$distance[i] > 1000 & data$distance[i] <= 2000){
    distance_category[i] = 3
  }else{
    distance_category[i] = 4
  }
}
data$dist_category = distance_category


# Zkratky letist
airports_abbr = sort(unique(data$origin))


# Skupiny
gr_var = as.integer(interaction(as.factor(data$dist_category),
                                as.factor(data$origin),
                                as.factor(data$month), drop = FALSE))
data$group = as.factor(gr_var)
gr = as.numeric(levels(data$group))
group_char = c()
i = 1
for(month in 1:12){
  for(airport in airports_abbr){
    for(dist in 1:4){
      group_char[i] = paste("Měsíc: ",month,", letiště: ",airport,", vzdálenost: ",dist, sep = "")
      i = i + 1
    }
  }
}
rm(list = c("airport","airports_abbr","dist","distance_category","gr_var","month"))


# Vsechna zpozdeni -------------------------------------------------------------
# cairo_pdf(file = "./obrazky/01_vsechna_zpozdeni.pdf", height = 99, width = 14)
# par(mfrow = c(33,4))
# for(i in gr){
#   ddata = data[data$group == i, ]
#   plot(ddata$dep_delay, ddata$arr_delay, xlim = c(-50,1350), ylim = c(-90,1300),
#        main = group_char[i], xlab = "zpoždění při odletu (min)", ylab = "zpoždění při příletu (min)")
# }
# dev.off()


# Zpozdeni v intervalu 5-300 minut ---------------------------------------------
dset = na.omit(data[data$arr_delay <= 300 & data$arr_delay >= 5 &
                    data$dep_delay <= 300 & data$dep_delay >= 5, ])
dset$ad_log = log(dset$arr_delay, base = 5)
dset$dd_log = log(dset$dep_delay, base = 5)

# cairo_pdf(file="./obrazky/02_zpozdeni_5-300_min.pdf", height = 99, width = 14)  
# par(mfrow = c(33,4))
# for(i in gr){
#   ddata = dset[dset$group == i,]
#   plot(ddata$dd_log, ddata$ad_log, main = group_char[i],
#        xlab = expression(log[5]("zpoždění při odletu")),
#        ylab = expression(log[5]("zpoždění při příletu")))
# }
# dev.off()

# rm("ddata")


# Jadrove odhady hustot --------------------------------------------------------
library(KernSmooth)
library(plot3D)

dens = list()
for (i in gr){
  x = as.data.frame(dset[dset$group == i, c("dd_log","ad_log")])
  dens[[i]] = bkde2D(x,
                     bandwidth = c(bw.nrd(x[,1]), bw.nrd(x[,2])),
                     gridsize = c(40,40),
                     range.x = list(range(dset$dd_log), range(dset$ad_log)),
                     truncate = T)
}

x = dens[[1]]$x1
y = dens[[1]]$x2

xx = rep(x,40)
yy = c()
for(i in 1:40){
  yy = c(yy,rep(y[i],40))
}

# cairo_pdf(file = "./obrazky/03_odhady_hustot.pdf", height = 99, width = 14)
# par(mfrow = c(33,4))
# for(i in gr){
#   scatter3D(xx, yy, as.vector(dens[[i]]$fhat),
#             xlab = "log(odlet)", ylab = "log(přílet)", zlab = "odhad hustoty",
#             main = group_char[i], pch = 20, colkey = TRUE,
#             xlim = range(xx)+c(-0.1,0.1), ylim = range(yy)+c(-0.1,0.1), zlim = c(0,1))
# }
# dev.off()


# Clr transformace -------------------------------------------------------------
clr_dens = list()
for (i in gr){
  d = dens[[i]]$fhat
  
  # nahrazeni hodnot mensich nez je prumer 20 nejmensich hodnot vetsich nez 1e-13
  sorted = sort(as.vector(d))
  m = mean(sorted[sorted > 1e-13][1:20])
  for(j in 1:nrow(d)){
    for(k in 1:ncol(d)){
      if((d[j,k]) < m){
        d[j,k] = m
      }
    }
  }
  clr_dens[[i]] = matrix(nrow = length(x), ncol = length(y))
  for(k in 1:length(x)){
    for(j in 1:length(y)){
      clr_dens[[i]][k,j] = log(d[k,j])-sum(log(d))/(length(x)*length(y))
    }
  }
}
rm(list = c("d","i","j","k","m","sorted"))

# cairo_pdf(file = "./obrazky/04_clr_hustoty.pdf", height = 99, width = 14)
# par(mfrow = c(33,4))
# for(i in gr){
#   scatter3D(xx, yy, as.vector(clr_dens[[i]]),
#             xlab = "log(odlet)", ylab = "log(přílet)", zlab = "clr hustota",
#             main = group_char[i], pch = 20, colkey = TRUE,
#             xlim = range(xx)+c(-0.1,0.1), ylim = range(yy)+c(-0.1,0.1), zlim = range(clr_dens))
# }
# dev.off()


# Rozklady hustot --------------------------------------------------------------
clr_int = clr_ind = clr_x = clr_y = list()
comp1 = comp1_x = comp1_y = list()
sing_vals = list()

for(n in gr){
  fc = clr_dens[[n]]
  
  # clr (geometricke) marginalni hustoty
  fxc = fyc = c()
  for(i in 1:length(x)){
    fxc[i] = sum(fc[i,])/length(y)
  }
  for(i in 1:length(y)){
    fyc[i] = sum(fc[,i])/length(x)
  }
  clr_x[[n]] = fxc
  clr_y[[n]] = fyc
  
  # clr nezavisla cast hustoty
  fc_ind = matrix(nrow = length(x), ncol = length(y))
  for(i in 1:length(x)){
    for(j in 1:length(y)){
      fc_ind[i,j] = fxc[i] + fyc[j]
    }
  }
  clr_ind[[n]] = fc_ind
  
  # clr interakcni cast hustoty
  clr_int[[n]] = fc_int = fc - fc_ind
  
  # singularni rozklad interakcni casti hustoty
  fcint_dec = svd(fc_int)
  
  sing_vals[[n]] = fcint_dec$d[1:10]
  
  # prvni komponenty
  comp1_x[[n]] = u1 = fcint_dec$u[,1]
  comp1_y[[n]] = v1 = fcint_dec$v[,1]
  d1 = fcint_dec$d[1]
  
  # aproximace pomoci prvnich komponent
  comp1[[n]] = d1*u1%*%t(v1)
}
rm(list = c("fc","fc_ind","fc_int","fxc","fyc","n","i","j","fcint_dec","u1","v1","d1"))


# Marginalni hustoty -----------------------------------------------------------
# aritmeticke marginalni hustoty (jadrovy odhad) a jejich clr transformace
x_am = y_am = list()
for (i in gr){
  v = dset[dset$group == i, c("dd_log","ad_log")]
  x_am[[i]] = bkde(v$dd_log, bandwidth = bw.nrd(v$dd_log), gridsize = 40,
                   range.x = c(1,log(300,5)), truncate = T)$y
  y_am[[i]] = bkde(v$ad_log, bandwidth = bw.nrd(v$ad_log), gridsize = 40,
                   range.x = c(1,log(300,5)), truncate = T)$y
}
xc_am = yc_am = list()
for(i in gr){
  dx = x_am[[i]]
  dy = y_am[[i]]
  xc_am[[i]] =  yc_am[[i]] = rep(0,length(x))
  for(j in 1:length(x)){
    xc_am[[i]][j] = log(dx[j]) - sum(log(dx))/length(x)
    yc_am[[i]][j] = log(dy[j]) - sum(log(dy))/length(y)
  }
}

# geometricke marginalni hustoty
x_gm = y_gm = list()
h = (x[2]-x[1])
for(i in gr){
  x_gm[[i]] = exp(clr_x[[i]])/sum(h*exp(clr_x[[i]]))
  y_gm[[i]] = exp(clr_y[[i]])/sum(h*exp(clr_y[[i]]))
}

rm(list = c("i","j","v","dx","dy","h"))

# zobrazeni 
# cairo_pdf("./obrazky/05_marginalni_odlet.pdf", height = 3*132, width = 12)
# par(mfrow = c(132,4))
# for(i in gr){
#   plot(x,x_am[[i]], xlab = "log(zpoždění při odletu)", ylab = "aritmetická marg. h.",
#        main = group_char[i], type = "l", ylim = c(0,round(range(x_am)[2],2)))
#   plot(x,xc_am[[i]], xlab = "log(zpoždění při odletu)", ylab = "clr(aritmetická marg. h.)",
#        type = "l", ylim = round(range(xc_am),2))
#   plot(x,x_gm[[i]], xlab = "log(zpoždění při odletu)", ylab = "geometrická marg. h.",
#        type = "l", ylim = c(0,round(range(x_gm)[2],2)))
#   plot(x,clr_x[[i]], xlab = "log(zpoždění při odletu)", ylab = "clr(geometrická marg. h.)",
#        type = "l", ylim = round(range(clr_x),2))
# }
# dev.off()

# cairo_pdf("./obrazky/06_marginalni_prilet.pdf", height = 3*132, width = 12)
# par(mfrow = c(132,4))
# for(i in gr){
#   plot(y,y_am[[i]], xlab = "log(zpoždění při příletu)", ylab = "aritmetická marg. h.",
#        main = group_char[i], type = "l", ylim = c(0,round(range(y_am)[2],2)))
#   plot(y,yc_am[[i]], xlab = "log(zpoždění při příletu)", ylab = "clr(aritmetická marg. h.)",
#        type = "l", ylim = round(range(yc_am),2))
#   plot(y,y_gm[[i]], xlab = "log(zpoždění při příletu)", ylab = "geometrická marg. h.",
#        type = "l", ylim = c(0,round(range(y_gm)[2],2)))
#   plot(y,clr_y[[i]], xlab = "log(zpoždění při příletu)", ylab = "clr(geometrická marg. h.)",
#        type = "l", ylim = round(range(clr_y),2))
# }
# dev.off()


# Zobrazeni rozlozenych hustot -------------------------------------------------

# cairo_pdf(file = "./obrazky/07_nezavisle_casti.pdf", height = 99, width = 14)
# par(mfrow = c(33,4))
# for(i in gr){
#   scatter3D(xx, yy, as.vector(clr_ind[[i]]),
#             xlab = "log(odlet)", ylab = "log(přílet)", zlab = "nezávislá část",
#             main = group_char[i], pch = 20, colkey = TRUE,
#             xlim = range(xx)+c(-0.1,0.1), ylim = range(yy)+c(-0.1,0.1), zlim = range(clr_ind))
# }
# dev.off()

# cairo_pdf(file = "./obrazky/08_interakcni_casti.pdf", height = 99, width = 14)
# par(mfrow = c(33,4))
# for(i in gr){
#   scatter3D(xx, yy, as.vector(clr_int[[i]]),
#             xlab = "log(odlet)", ylab = "log(přílet)", zlab = "interakční část",
#             main = group_char[i], pch = 20, colkey = TRUE,
#             xlim = range(xx)+c(-0.1,0.1), ylim = range(yy)+c(-0.1,0.1), zlim = range(clr_int))
# }
# dev.off()

# cairo_pdf(file = "./obrazky/09_singularni_hodnoty.pdf", height = 99, width = 12)
# par(mfrow = c(33,4))
# for(i in gr){
#   plot(1:10, sing_vals[[i]], xlab = "komponenta", ylab = "singulární hodnota",
#        type = "b", main = group_char[i])
# }
# dev.off()

# cairo_pdf(file = "./obrazky/10_aproximace_interakcni_casti.pdf", height = 99, width = 14)
# par(mfrow = c(33,4))
# for(i in gr){
#   scatter3D(xx, yy, as.vector(comp1[[i]]),
#             xlab = "log(odlet)", ylab = "log(přílet)", zlab = "1. komponenta",
#             main = group_char[i], pch = 20, colkey = TRUE,
#             xlim = range(xx)+c(-0.1,0.1), ylim = range(yy)+c(-0.1,0.1), zlim = range(comp1))
# }
# dev.off()

# cairo_pdf(file = "./obrazky/11_1komponenta_odlet.pdf", height = 99, width = 14)
# par(mfrow = c(33,4))
# for(i in gr){
#   plot(x, comp1_x[[i]], xlab = "log(zpoždění při odletu)", ylab = "1. komponenta",
#        main = group_char[i], type = "l",
#        panel.first = abline(h = 0, col = "lightgrey"))
# }
# dev.off()

# cairo_pdf(file = "./obrazky/12_1komponenta_prilet.pdf", height = 99, width = 14)
# par(mfrow = c(33,4))
# for(i in gr){
#   plot(x, comp1_y[[i]], xlab = "log(zpoždění při příletu)", ylab = "1. komponenta",
#        main = group_char[i], type = "l",
#        panel.first = abline(h = 0, col = "lightgrey"))
# }
# dev.off()


##------------------------------------------------------------------------------
##  2) KOD KE KAPITOLAM 3.4 A 3.5
##------------------------------------------------------------------------------

##  KAPITOLA 3.4 - ANALYZA NEZAVISLYCH CASTI HUSTOT A GEOM. MARG. HUSTOT
##------------------------------------------------------------------------------
# FPCA pro nezavisle casti hustot
ind_table = matrix(nrow = 132, ncol = 1600)
for(i in gr){
  for(k in 1:40){
    for(j in 1:40){
      ind_table[which(gr %in% i),40*(k-1) + j] = clr_ind[[i]][j,k]
    }
  }
}
ind_table = ind_table - matrix(rep(apply(ind_table,2,mean),132), nrow = 132, ncol = 1600, byrow = T)
ind_pca = svd(ind_table)
U_ind = ind_pca$u
D_ind = diag(ind_pca$d)
V_ind = ind_pca$v
ind_scores = U_ind%*%D_ind


# SFPCA pro clr (geometricke) marginalni hustoty
gm_table = matrix(nrow = 132, ncol = 80)
for(i in gr){
  gm_table[which(gr %in% i),] = c(clr_x[[i]],clr_y[[i]])
}
gm_table = gm_table - matrix(rep(apply(gm_table,2,mean),132), nrow = 132, ncol = 80, byrow = T)
gm_pca = svd(gm_table)
U_gm = gm_pca$u
D_gm = diag(gm_pca$d)
V_gm = gm_pca$v
gm_scores = U_gm%*%D_gm


# Zobrazeni skoru
par(mfrow = c(1,2), mar = c(5,5,4,2))
plot(ind_scores[,1],ind_scores[,2], pch = 19,
     xlab = expression(z[s1]^BD), ylab = expression(z[s2]^BD), main = "nezávislé části hustot")
plot(gm_scores[,1],-gm_scores[,2], pch = 19,
     xlab = expression(z[s1]^MV), ylab = expression(z[s2]^MV), main = "clr marginální hustoty")


# Normovane skory a odlehle pozrovani
par(mfrow = c(1,1))
plot(U_ind[,1],U_ind[,2],pch=19,xlab = expression(u[s1]), ylab = expression(u[s2]),
     main = "normované skóry")
points(U_ind[92,1],U_ind[92,2],pch=1,col = c("red"), cex = 2, lwd = 2)


# Clr (geometricke) marginalni hustoty a nezavisla cast hustoty pro odlehle pozorovani
par(mfrow = c(1,3))
plot(x,clr_x[[100]], ylim = range(clr_x), xlab="log(zpoždění při odletu)", ylab="clr hustota",
     type = "l", panel.first = abline(h = 0, col = "lightgrey"))
par(mar = c(3,2,4,3))
scatter3D(xx, yy, as.vector(clr_ind[[100]]),
          xlab = "log(odlet)", ylab = "log(přílet)", zlab = "clr(nezávislá část)",
          main = "", pch = 20, colkey = TRUE, 
          xlim = range(xx)+c(-0.1,0.1), ylim = range(yy)+c(-0.1,0.1), zlim = range(clr_ind))
par(mar = c(5,5,4,2))
plot(y,clr_y[[100]], ylim = range(clr_y), xlab="log(zpoždění při příletu)", ylab="clr hustota",
     type = "l", panel.first = abline(h = 0, col = "lightgrey"))


# Serazeni skupin letu podle hodnot skoru odpovidajicich prvni hlavni komponente
s1 = sort(U_ind[,1])
g1 = c()
for(i in 1:132){
  g1[i] = gr[which(U_ind[,1] %in% s1[i])]
}

# prvnich a poslednich 12 nezavislych casti hustot podle prvni hlavni komponenty
# cairo_pdf(file = "./obrazky/13_nezavisle_casti_podle_1komponenty.pdf", height = 18, width = 14)
# par(mfrow = c(6,4))
# for(i in g1[c(1:12,121:132)]){
#   scatter3D(xx, yy, exp(as.vector(clr_ind[[i]])), main = group_char[i], pch = 20, colkey = TRUE,
#             xlab = "log(odlet)", ylab = "log(přílet)", zlab = "nezávislá část",
#             xlim = range(xx)+c(-0.1,0.1), ylim = range(yy)+c(-0.1,0.1), zlim = exp(range(clr_ind)))
# }
# dev.off()

# prvnich a poslednich 12 clr marginalnich hustot podle prvni hlavni komponenty
# cairo_pdf(file = "./obrazky/14_marg_odlet_podle_1komponenty.pdf", height = 18, width = 14)
# par(mfrow = c(6,4))
# for(i in g1[c(1:12,121:132)]){
#   plot(x, exp(clr_x[[i]]), main = group_char[i], type = "l",
#        xlab = "log(zpoždění při odletu)", ylab = "clr hustota", ylim = exp(range(clr_x)))
# }
# dev.off()

# cairo_pdf(file = "./obrazky/15_marg_prilet_podle_1komponenty.pdf", height = 18, width = 14)
# par(mfrow = c(6,4))
# for(i in g1[c(1:12,121:132)]){
#   plot(y, exp(clr_y[[i]]), main = group_char[i], type = "l",
#        xlab = "log(zpoždění při příletu)", ylab = "clr hustota", ylim = exp(range(clr_y)))
# }
# dev.off()


# Serazeni skupin letu podle hodnot skoru odpovidajicich druhe hlavni komponente
s2 = sort(U_ind[,2])
g2 = c()
for(i in 1:132){
  g2[i] = gr[which(U_ind[,2] %in% s2[i])]
}

# prvnich a poslednich 12 nezavislych casti hustot podle druhe hlavni komponenty
# cairo_pdf(file = "./obrazky/16_nezavisle_casti_podle_2komponenty.pdf", height = 18, width = 14)
# par(mfrow = c(6,4))
# for(i in g2[c(1:12,121:132)]){
#   scatter3D(xx, yy, exp(as.vector(clr_ind[[i]])), main = group_char[i], pch = 20, colkey = TRUE,
#             xlab = "log(odlet)", ylab = "log(přílet)", zlab = "nezávislá část",
#             xlim = range(xx)+c(-0.1,0.1), ylim = range(yy)+c(-0.1,0.1), zlim = exp(range(clr_ind)))
# }
# dev.off()

# prvnich a poslednich 12 clr marginalnich hustot podle druhe hlavni komponenty
# cairo_pdf(file = "./obrazky/17_marg_odlet_podle_2komponenty.pdf", height = 18, width = 14)
# par(mfrow = c(6,4))
# for(i in g2[c(1:12,121:132)]){
#   plot(x, exp(clr_x[[i]]), main = group_char[i], type = "l",
#        xlab = "log(zpoždění při odletu)", ylab = "clr hustota", ylim = exp(range(clr_x)))
# }
# dev.off()

# cairo_pdf(file = "./obrazky/18_marg_prilet_podle_2komponenty.pdf", height = 18, width = 14)
# par(mfrow = c(6,4))
# for(i in g2[c(1:12,121:132)]){
#   plot(y, exp(clr_y[[i]]), main = group_char[i], type = "l",
#        xlab = "log(zpoždění při příletu)", ylab = "clr hustota", ylim = exp(range(clr_y)))
# }
# dev.off()


# Barevne (normovane) skory
dist_col = rep(c(1:4,1:4,1:3), 12)
season_col = c(rep("blue",2*11), rep("green",3*11), rep("red",3*11), rep("orange",3*11), rep("blue",11))
airp_col = c(rep("blue",4), rep("red",4), rep("green",3))

# rocni obdobi
par(mfrow = c(1,1), mar = c(4,4,2,2))
plot(U_ind[,1], U_ind[,2], pch = 19, cex = 1, col = season_col,
     xlab = expression(u[s1]), ylab = expression(u[s2]),  
     panel.first = abline(v = 0, col = "grey"))
legend("bottomright", legend = c("jaro","léto","podzim","zima"),
       col = c("green","red","orange","blue"), pch = 19, cex = 0.9)

# vzdalenostni kategorie
par(mar = c(4,4,2,2))
plot(U_ind[,1], U_ind[,2], pch = 19, cex = 1, col = dist_col,
     xlab = expression(u[s1]), ylab = expression(u[s2]),
     panel.first = abline(h = 0, col = "grey"))
legend("bottomright", 
       legend = c("vzdál. 1","vzdál. 2","vzdál. 3","vzdál. 4"), 
       col = 1:4 , pch = rep(19,4), cex = 0.9)

# letiste
par(mar = c(4,4,2,2))
plot(U_ind[,1], U_ind[,2], pch = 19, cex = 1, col = airp_col,
     xlab = expression(u[s1]), ylab = expression(u[s2]))
legend("bottomright", legend = c("EWR","JFK","LGA"),
       col = c("blue","red","green"), pch = 19, cex = 0.9)


##  KAPITOLA 3.5 - ANALYZA INTERAKCNICH CASTI HUSTOT
##------------------------------------------------------------------------------
# FPCA
int_table = matrix(nrow = 132, ncol = 1600)
for(i in gr){
  for(k in 1:40){
    for(j in 1:40){
      int_table[which(gr %in% i),40*(k-1) + j] = clr_int[[i]][j,k]
    }
  }
}
int_table = int_table - matrix(rep(apply(int_table,2,mean),132), nrow = 132, ncol = 1600, byrow = T)
int_pca = svd(int_table)
U_int = int_pca$u
D_int = diag(int_pca$d)
V_int = int_pca$v
int_scores = U_int%*%D_int


# Serazeni skupin letu podle hodnot skoru odpovidajicich prvni hlavni komponente
s1 = sort(U_int[,1])
g1 = c()
for(i in 1:132){
  g1[i] = gr[which(U_int[,1] %in% s1[i])]
}

# prvnich a poslednich 12 clr interakcnich casti hustot podle prvni hlavni komponenty
# cairo_pdf(file = "./obrazky/19_interakcni_casti_podle_1komponenty.pdf", height = 18, width = 14)
# par(mfrow = c(6,4))
# for(i in g1[c(1:12,121:132)]){
#   scatter3D(xx, yy, as.vector(clr_int[[i]]), main = group_char[i], pch = 20, colkey = TRUE,
#             xlab = "log(odlet)", ylab = "log(přílet)", zlab = "interakční část",
#             xlim = range(xx)+c(-0.1,0.1), ylim = range(yy)+c(-0.1,0.1), zlim = range(clr_int))
# }
# dev.off()


# Serazeni skupin letu podle hodnot skoru odpovidajicich druhe hlavni komponente
s2 = sort(U_int[,2])
g2 = c()
for(i in 1:132){
  g2[i] = gr[which(U_int[,2] %in% s2[i])]
}

# prvnich a poslednich 12 clr interakcnich casti hustot podle druhe hlavni komponenty
# cairo_pdf(file = "./obrazky/20_interakcni_casti_podle_2komponenty.pdf", height = 18, width = 14)
# par(mfrow = c(6,4))
# for(i in g2[c(1:12,121:132)]){
#   scatter3D(xx, yy, as.vector(clr_int[[i]]), main = group_char[i], pch = 20, colkey = TRUE,
#             xlab = "log(odlet)", ylab = "log(přílet)", zlab = "interakční část",
#             xlim = range(xx)+c(-0.1,0.1), ylim = range(yy)+c(-0.1,0.1), zlim = range(clr_int))
# }
# dev.off()


# Barevne (normovane) skory
# vzdalenostni kategorie
par(mar = c(4,4,2,2))
plot(U_int[,1], U_int[,2], pch = 19, cex = 1, col = dist_col,
     xlab = expression(u[s1]), ylab = expression(u[s2]),
     panel.first = abline(a=-0.1, b=3, col = "grey"))
legend("bottomright", legend = c("vzdál. 1","vzdál. 2","vzdál. 3","vzdál. 4"), 
       col = 1:4 , pch = rep(19,4), cex = 0.85)

# rocni obdobi
par(mar = c(4,4,2,2))
plot(U_int[,1], U_int[,2], pch = 19, cex = 1, col = season_col,
     xlab = expression(u[s1]), ylab = expression(u[s2]))  
legend("bottomright", legend = c("jaro","léto","podzim","zima"),
       col = c("green","red","orange","blue"), pch = 19, cex = 0.85)


# letiste
par(mar = c(4,4,2,2))
plot(U_int[,1], U_int[,2], pch = 19, cex = 1, col = airp_col,
     xlab = expression(u[s1]), ylab = expression(u[s2]))
legend("bottomright", legend = c("EWR","JFK","LGA"),
       col = c("blue","red","green"), pch = 19, cex = 0.85)


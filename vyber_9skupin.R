################################################################################
###                    ANALYZA LETU Z NEWYORSKYCH LETIST                     ###
###                         9 VYBRANYCH SKUPIN LETU                          ###
################################################################################

# kod ke kapitolam 3.1-3.3 diplomove prace

#-------------------------------------------------------------------------------
#  KAPITOLA 3.1 - UPRAVA DAT
#-------------------------------------------------------------------------------
library(nycflights13)

data = flights
data$origin = as.factor(data$origin)
summary(na.omit(data[ ,c("month","dep_delay","arr_delay","origin","distance")]))


##  4 vzdalenostni kategorie
##------------------------------------------------------------------------------
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


##  Skupiny
##------------------------------------------------------------------------------
gr_var = as.integer(interaction(as.factor(data$dist_category),
                                as.factor(data$origin),
                                as.factor(data$month), drop = FALSE))
data$group = as.factor(gr_var)
rm("gr_var","distance_category")


##  9 vybranych skupin
##------------------------------------------------------------------------------
sel = sort(c(37,98,16,65,90,113,21,35,83))
sdat = data[data$group %in% sel, ]

gch_sel = c()
a = c("EWR","LGA","LGA","EWR","JFK","LGA","JFK","EWR","JFK")
m = c("únor","únor","březen","duben","červen","červenec","srpen","září","říjen")
v = c(4,1,3,1,1,3,2,2,1)
for(i in 1:9){
  gch_sel[i] = paste0(a[i],", ",m[i],", ","vzdálenost ",v[i])
}
rm(list = c("a","m","v"))


##  Zobrazeni zpozdeni + vyber zpozdeni v intervalu 5-300 minut
##------------------------------------------------------------------------------
par(mfrow = c(3,3))
for(i in sel){
  ind = which(sel %in% i)
  d = sdat[sdat$group == i,]
  plot(d$dep_delay, d$arr_delay, xlim = c(-50,1350), ylim = c(-90,1300),
       main = gch_sel[ind], xlab = "zpoždění při odletu (min)", ylab = "zpoždění při příletu (min)")
}

dset = na.omit(sdat[sdat$arr_delay <= 300 & sdat$arr_delay > 0 &
                    sdat$dep_delay <= 300 & sdat$dep_delay > 0, ])
dset$ad_log = log(dset$arr_delay, base = 5)
dset$dd_log = log(dset$dep_delay, base = 5)


par(mfrow = c(3,3))
for(i in sel){
  ind = which(sel %in% i)
  d = dset[dset$group == i,]
  plot(d$dd_log, d$ad_log, main = gch_sel[ind],
       xlab = expression(log[5]("zpoždění při odletu")),
       ylab = expression(log[5]("zpoždění při příletu")))
}

dset = dset[dset$arr_delay >= 5 & dset$dep_delay >= 5, ]


par(mfrow = c(3,3))
for(i in sel){
  ind = which(sel %in% i)
  d = dset[dset$group == i,]
  plot(d$dd_log, d$ad_log, main = gch_sel[ind],
       xlab = expression(log[5]("zpoždění při odletu")),
       ylab = expression(log[5]("zpoždění při příletu")))
}


#-------------------------------------------------------------------------------
#  KAPITOLA 3.2 - VYTVORENI (ODHAD) HUSTOT A JEJICH CLR TRANSFORMACE
#-------------------------------------------------------------------------------
library(KernSmooth)
library(plot3D)

##  Jadrovy odhad
##------------------------------------------------------------------------------
dens = list()
for(i in sel){
  ind = which(sel %in% i)
  x = as.data.frame(dset[dset$group == i, c("dd_log","ad_log")])
  dens[[ind]] = bkde2D(x, 
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

par(mfrow = c(3,3))
for(i in 1:9){
  scatter3D(xx, yy, as.vector(dens[[i]]$fhat),
            xlab = "log(odlet)", ylab = "log(přílet)", zlab = "odhad hustoty",
            main = gch_sel[i], pch = 20, colkey = TRUE,
            xlim = range(xx)+c(-0.1,0.1), ylim = range(yy)+c(-0.1,0.1), zlim = c(0,1))
}


##  Clr transformace bez nahrazeni zdanlive nenulovych hodnot
##------------------------------------------------------------------------------
clr_dens = list()
for (i in 1:9){
  d = dens[[i]]$fhat

  # nahrazeni nulovych hodnot (abychom mohli pouzit logaritmus)
  m = min(d[d > 0])
  for(j in 1:nrow(d)){
    for(k in 1:ncol(d)){
      if((d[j,k]) <= 0){
        d[j,k] = m
      }
    }
  }
  # clr transformace
  clr_dens[[i]] = matrix(nrow = length(x), ncol = length(y))
  for(k in 1:length(x)){
    for(j in 1:length(y)){
      clr_dens[[i]][k,j] = log(d[k,j])-sum(log(d))/(length(x)*length(y))
    }
  }
}

par(mfrow = c(3,3))
for(i in 1:9){
  scatter3D(xx, yy, as.vector(clr_dens[[i]]),
            xlab = "log(odlet)", ylab = "log(přílet)", zlab = "clr hustota",
            main = gch_sel[i], pch = 20, colkey = TRUE,
            xlim = range(xx)+c(-0.1,0.1), ylim = range(yy)+c(-0.1,0.1), zlim = range(clr_dens))
}

# dev.off()
# plot(NA,NA, xlim = c(1,9), ylim = c(-19,0), xlab = "group", ylab = expression(log[10](fhat)))
# for (i in 1:9){
#   points(rep(i,1600), as.vector(log(dens[[i]]$fhat, base = 10)))
# }
# abline(h = -13, col = "red")


##  Po nahrazeni zdanlive nenulovych hodnot
##------------------------------------------------------------------------------
clr_dens = list()
m = c()  
for (i in 1:9){
  d = dens[[i]]$fhat
  
  # nahrazeni hodnot mensich nez je prumer 20 nejmensich hodnot vetsich nez 1e-13
  sorted = sort(as.vector(d))
  m[i] = mean(sorted[sorted > 1e-13][1:20])
  for(j in 1:nrow(d)){
    for(k in 1:ncol(d)){
      if((d[j,k]) < m[i]){
        d[j,k] = m[i]
      }
    }
  }
  # clr transformace
  clr_dens[[i]] = matrix(nrow = length(x), ncol = length(y))
  for(k in 1:length(x)){
    for(j in 1:length(y)){
      clr_dens[[i]][k,j] = log(d[k,j])-sum(log(d))/(length(x)*length(y))
    }
  }
}
rm(list = c("d","ind","m","sorted","i","j","k"))

par(mfrow = c(3,3))
for(i in 1:9){
  scatter3D(xx, yy, as.vector(clr_dens[[i]]),
            xlab = "log(odlet)", ylab = "log(přílet)", zlab = "clr hustota",
            main = gch_sel[i], pch = 20, colkey = TRUE,
            xlim = range(xx)+c(-0.1,0.1), ylim = range(yy)+c(-0.1,0.1), zlim = range(clr_dens))
}


#-------------------------------------------------------------------------------
#  KAPITOLA 3.3 - MARGINALNI HUSTOTY A ROZKLADY HUSTOT
#-------------------------------------------------------------------------------

##  Ortogonalni rozkad hustot a singularni rozklad interakcni casti hustoty
##------------------------------------------------------------------------------
clr_int = clr_ind = clr_x = clr_y = list()
comp1 = comp1_x = comp1_y = list()
sing_vals = list()

for(n in 1:9){
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
rm(list = c("fcint_dec","fc","fc_ind","fc_int","fxc","fyc","n","u1","v1","d1"))


##  Aritmeticke a geometiricke marginalni hustoty
##------------------------------------------------------------------------------
# aritmeticke marginalni hustoty a jejich clr transformace
x_am = y_am = list()
for (i in sel){
  ind = which(sel %in% i)
  v = dset[dset$group == i, c("dd_log","ad_log")]
  x_am[[ind]] = bkde(v$dd_log, bandwidth = bw.nrd(v$dd_log), gridsize = 40,
                     range.x = c(1,log(300,5)), truncate = T)$y
  y_am[[ind]] = bkde(v$ad_log, bandwidth = bw.nrd(v$ad_log), gridsize = 40,
                     range.x = c(1,log(300,5)), truncate = T)$y
}
xc_am = yc_am = list()
for(i in 1:9){
  dx = x_am[[i]]
  dy = y_am[[i]]
  xc_am[[i]] = yc_am[[i]] = rep(0,length(x))
  for(j in 1:length(x)){
    xc_am[[i]][j] = log(dx[j]) - sum(log(dx))/length(x)
    yc_am[[i]][j] = log(dy[j]) - sum(log(dy))/length(y)
  }
}

# geometricke marginalni hustoty
x_gm = y_gm = list()
h = (x[2]-x[1])
for(i in 1:9){
  x_gm[[i]] = exp(clr_x[[i]])/sum(h*exp(clr_x[[i]]))
  y_gm[[i]] = exp(clr_y[[i]])/sum(h*exp(clr_y[[i]]))
}
rm(list = c("i","j","v","dx","dy","h"))


##  Zobrazeni vysledku
##------------------------------------------------------------------------------
rnbw = c("black",rainbow(18)[c(1,3,4,8,10,11,13,15)])

# marginalni hustoty pro zpozdeni pri odletu
{
  layout(matrix(c(1,2,5,3,4,5), nrow = 2, byrow = T))
  plot(NA,NA, xlim = range(x), ylim = c(0,round(range(x_am)[2],2)),
       xlab = "log(zpoždění při odletu)", ylab = "", main = "aritmetické marg. hustoty")
  for(i in 1:9){
    points(x,x_am[[i]], pch = 20, col = rnbw[i], cex = 0.8)
    lines(x,x_am[[i]], pch = 20, col = rnbw[i])
  }
  plot(NA,NA, xlim = range(x), ylim = round(range(xc_am),2),
       xlab = "log(zpoždění při odletu)", ylab = "", main = "clr(aritmetické marg. hustoty)")
  abline(h = 0, col = "lightgrey")
  for(i in 1:9){
    points(x,xc_am[[i]], pch = 20, col = rnbw[i], cex = 0.8)
    lines(x,xc_am[[i]], pch = 20, col = rnbw[i])
  }
  plot(NA,NA, xlim = range(x), ylim = c(0,round(range(x_gm)[2],2)),
       xlab = "log(zpoždění při odletu)", ylab = "", main = "geometrické marg. hustoty")
  for(i in 1:9){
    points(x,x_gm[[i]], pch = 20, col = rnbw[i], cex = 0.8)
    lines(x,x_gm[[i]], pch = 20, col = rnbw[i])
  }
  plot(NA,NA, xlim = range(x), ylim = round(range(clr_x),2),
       xlab = "log(zpoždění při odletu)", ylab = "", main = "clr(geometrické marg. hustoty)")
  abline(h = 0, col = "lightgrey")
  for(i in 1:9){
    points(x,clr_x[[i]], pch = 20, col = rnbw[i], cex = 0.8)
    lines(x,clr_x[[i]], pch = 20, col = rnbw[i])
  }
  par(mar = c(0,0,0,0))
  plot(1, type = "n", axes = F, xlab = "", ylab = "")
  legend("left", inset = 0.1, legend = gch_sel, col = rnbw, lwd = 5)
}
dev.off()


# marginalni hustoty pro zpozdeni pri priletu
{
  layout(matrix(c(1,2,5,3,4,5), nrow = 2, byrow = T))
  
  plot(NA,NA, xlim = range(y), ylim = c(0,round(range(y_am)[2],2)),
       xlab = "log(zpoždění při příletu)", ylab = "", main = "aritmetické marg. hustoty")
  for(i in 1:9){
    points(y,y_am[[i]], pch = 20, col = rnbw[i], cex = 0.8)
    lines(y,y_am[[i]], pch = 20, col = rnbw[i])
  }
  plot(NA,NA, xlim = range(y), ylim = round(range(yc_am),2),
       xlab = "log(zpoždění při příletu)", ylab = "", main = "clr(aritmetické marg. hustoty)")
  abline(h = 0, col = "lightgrey")
  for(i in 1:9){
    points(y,yc_am[[i]], pch = 20, col = rnbw[i], cex = 0.8)
    lines(y,yc_am[[i]], pch = 20, col = rnbw[i])
  }
  plot(NA,NA, xlim = range(y), ylim = c(0,round(range(y_gm)[2],2)),
       xlab = "log(zpoždění při příletu)", ylab = "", main = "geometrické marg. hustoty")
  for(i in 1:9){
    points(y,y_gm[[i]], pch = 20, col = rnbw[i], cex = 0.8)
    lines(y,y_gm[[i]], pch = 20, col = rnbw[i])
  }
  plot(NA,NA, xlim = range(y), ylim = round(range(clr_y),2),
       xlab = "log(zpoždění při příletu)", ylab = "", main = "clr(geometrické marg. hustoty)")
  abline(h = 0, col = "lightgrey")
  for(i in 1:9){
    points(y,clr_y[[i]], pch = 20, col = rnbw[i], cex = 0.8)
    lines(y,clr_y[[i]], pch = 20, col = rnbw[i])
  }
  par(mar = c(0,0,0,0))
  plot(1, type = "n", axes = F, xlab = "", ylab = "")
  legend("left",inset = 0.1, legend = gch_sel, col = rnbw, lwd = 5)
}
dev.off()


# nezavisle casti hustot
par(mfrow = c(3,3))
for(i in 1:9){
  scatter3D(xx, yy, as.vector(clr_ind[[i]]), main = gch_sel[i], pch = 20, colkey = TRUE,
            xlab = "log(odlet)", ylab = "log(přílet)", zlab = "nezávislá část",
            xlim = range(xx)+c(-0.1,0.1), ylim = range(yy)+c(-0.1,0.1), zlim = range(clr_ind))
}

# interakcni casti hustot
par(mfrow = c(3,3))
for(i in 1:9){
  scatter3D(xx, yy, as.vector(clr_int[[i]]), main = gch_sel[i], pch = 20, colkey = TRUE,
            xlab = "log(odlet)", ylab = "log(přílet)", zlab = "interakční část",
            xlim = range(xx)+c(-0.1,0.1), ylim = range(yy)+c(-0.1,0.1), zlim = range(clr_int))
}

# singularni hodnoty
par(mfrow = c(3,3))
for(i in 1:9){
  plot(1:10, sing_vals[[i]], xlab = "komponenta", ylab = "singulární hodnota", type = "b",
       main = gch_sel[i])
}

# aproximace interakcnich casti hustot
par(mfrow = c(3,3))
for(i in 1:9){
  scatter3D(xx, yy, as.vector(comp1[[i]]),
            xlab = "log(odlet)", ylab = "log(přílet)", zlab = "1. konponenta",
            main = gch_sel[i], pch = 20, colkey = TRUE, 
            xlim = range(xx)+c(-0.1,0.1), ylim = range(yy)+c(-0.1,0.1), zlim = range(comp1))
}

# prvni komonenty singularnich rozkladu
par(mfrow = c(3,3))
for(i in 1:9){
  plot(x, comp1_x[[i]], xlab = "log(zpoždění při odletu)", ylab = "1. komponenta",
       main = gch_sel[i], pch = 20,
       panel.first = abline(h = 0, col = "lightgrey"))
  lines(x, comp1_x[[i]])
}

par(mfrow = c(3,3))
for(i in 1:9){
  plot(y, comp1_y[[i]], xlab = "log(zpoždění při příletu)", ylab = "1. komponenta",
       main = gch_sel[i], pch = 20,
       panel.first = abline(h = 0, col = "lightgrey"))
  lines(y, comp1_y[[i]])
}


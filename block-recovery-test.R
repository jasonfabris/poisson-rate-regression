diags <- c("DepAnx", "MH", "Musculo", "Accident", "Cancer")
office <- c("Mtl","Tor","Wat","Edm")
#expand.grid(diags,office)

#recovery rate
base.rate = 0.03

#diag proportions
#d.prop = .2
#m.prop = .2
#p.prop = .2
#a.prop = .2
#c.prop = .2
#vols <- c(350,200,250,200)

#avg block size
vols <- list("Mtl" = 1350, "Tor" = 1200, "Wat" = 1250, "Edm" = 1200)
#diag proportions
diag.props <- list("DepAnx" = .3, "MH" = .2, "Musculo" = .25, "Accident" = .2, "Cancer" = .05)

rec.rate.blk <- list("Mtl" = 1.2, "Tor" = 1.0, "Wat" = 1.025, "Edm" = .95) 
rec.rate.diag <- list("DepAnx" = 1.15, "MH" = .9, "Musculo" = 1.1, "Accident" = 1.25, "Cancer" = .8) 

#vols.expand <- vapply(names(vols), function(x) {rep(vols[[x]],sum(clms$Off == x))})

#rnorm(vols,vols,10)

# make df -- each diag per office
clms <- expand.grid(diags,office)
colnames(clms) <- c("Diag", "Off")
clms$Blk <- NA
clms$Recs <- NA

blks <- sapply(names(vols), function(x) { 
  #print(x)
  #clms$Blk[which(clms$Off == x)] <- 0
  #clms$Blk[clms$Off == x] <- round(rnorm(length(clms$Off == x), vols[[x]], 10),0)
  round(rnorm(1, vols[[x]], 10),0)
})


#this works
clms$Blk[clms$Off=="Tor"] <- blks[,"Tor"]
clms$Blk[clms$Off=="Mtl"] <- blks[,"Mtl"]

#this fills each office with a vector of all block sizes
clms$Blk <- sapply(clms$Off, function(x){clms$Blk[clms$Off==x] <- blks[,x]})

#this works, but is ugly
for(x in unique(clms$Off)) {
  for (y in unique(clms$Diag)) {
    clms$Blk[clms$Off==x & clms$Diag == y] <- round(blks[x] * diag.props[[y]],0)
    clms$Recs[clms$Off==x & clms$Diag == y] <- round(
      base.rate * rec.rate.blk[[x]] * rec.rate.diag[[y]] * 
      rnorm(1,mean = clms$Blk[clms$Off==x & clms$Diag == y],sd = clms$Blk[clms$Off==x & clms$Diag == y]/50),0)
  }
}

#did it work?
(blk.counts <- xtabs(Blk ~ Diag + Off, clms))
prop.table(blk.counts,margin = 2)

#Poisson Rate Model
model.1 <- glm(Recs ~ Off + Diag, offset = log(Blk), family = poisson(link = "log"), data = clms)

## Results from Poisson
summary(model.1)
model.1$coefficients


#Logistic Model
clms$RecRate <- clms$Recs/clms$Blk
model.2 <- glm(RecRate ~ Off + Diag, weights = Blk, family = binomial(link = "logit"), data = clms)

summary(model.2)
model.2$coefficients


#Normal-Log Model
model.3 <- glm(RecRate ~ Off + Diag, weights = Blk, family = gaussian(link = "log"), data = clms)

summary(model.3)
model.3$coefficients

#Quasipoisson Model
model.4 <- glm(RecRate ~ Off + Diag, weights = Blk, family = quasipoisson(link = "log"), data = clms)

summary(model.4)
model.4$coefficients

## Predict rec per clms (n = 1) for Dep in Mtl
exp(predict(model.1, newdata = data.frame(Off = "Mtl", Diag = "DepAnx", "Blk" = 1)))
exp(predict(model.2, newdata = data.frame(Off = "Mtl", Diag = "DepAnx", "Blk" = 1)))
exp(predict(model.3, newdata = data.frame(Off = "Mtl", Diag = "DepAnx", "Blk" = 1)))
exp(predict(model.4, newdata = data.frame(Off = "Mtl", Diag = "DepAnx", "Blk" = 1)))

## Predict rec per clms (n = 1) for Dep in Tor
exp(predict(model.1, newdata = data.frame(Off = "Tor", Diag = "DepAnx", "Blk" = 1)))
exp(predict(model.2, newdata = data.frame(Off = "Tor", Diag = "DepAnx", "Blk" = 1)))
exp(predict(model.3, newdata = data.frame(Off = "Tor", Diag = "DepAnx", "Blk" = 1)))
exp(predict(model.4, newdata = data.frame(Off = "Tor", Diag = "DepAnx", "Blk" = 1)))

## Predict rec per clms (n = 1) for Can in Edm
exp(predict(model.1, newdata = data.frame(Off = "Edm", Diag = "Cancer", "Blk" = 1)))
exp(predict(model.2, newdata = data.frame(Off = "Edm", Diag = "Cancer", "Blk" = 1)))
exp(predict(model.3, newdata = data.frame(Off = "Edm", Diag = "Cancer", "Blk" = 1)))
exp(predict(model.4, newdata = data.frame(Off = "Edm", Diag = "Cancer", "Blk" = 1)))

# Question 1 R Code

# 2x2 Table (X=marriagestatus, Y=ownership)

m.table<-array(data = c(4579, 2759, 795, 1867), dim = c(2,2), dimnames = list(c("Married", "Not Married"), c("Own","Rent")))
m.table

# Chi-Square test for Independence 
ind.test = chisq.test(m.table)
ind.test
ind.test$residuals

# Relative Risk to Compare Rent Column (Highest Contribution)

Risk.married<-m.table[1,2]/sum(m.table[1,])
Risk.notmarried<-m.table[2,2]/sum(m.table[2,])
Risk.married
Risk.notmarried
Relative.Risk<-Risk.notmarried/Risk.married
Relative.Risk

ln.rr=log(Relative.Risk)
n.married<-sum(m.table[1,])
n.notmarried<-sum(m.table[2,])
n.married
n.notmarried

alpha<-0.05
z<-qnorm(1-alpha/2)

lower<-ln.rr - z*sqrt((1-Risk.married)/(n.married*Risk.married)+(1-Risk.notmarried)/(n.notmarried*Risk.notmarried))
upper<-ln.rr + z*sqrt((1-Risk.married)/(n.married*Risk.married)+(1-Risk.notmarried)/(n.notmarried*Risk.notmarried))
cat("The CI is:", round(lower,4), "<= true ln(RR) <=", round(upper,4))

#Confidence Interval of RR

lower.new<-exp(lower)
upper.new<-exp(upper)
cat("The CI is:", round(lower.new,4), "<= true RR <=", round(upper.new,4))

# 2x2x5 Table (Z=county)
p.table<-array(data = c(358, 150, 42,55,807,502,209,486,2657,1628,398,974,479,327,82,197,278,152,64,155), dim=c(2,2,5), dimnames = list(MarriageStatus = c("Married", "Not Married"), Ownership = c("Own", "Rent"), County = c("Boone", "Fayette", "Jefferson", "Kenton", "Warren")))
p.table

# Check Odds Ratios for Simpson's Paradox
# The odds of married to not married for those who own to those who rent

marginal.odds = (m.table[1,1]*m.table[2,2])/(m.table[2,1]*m.table[1,2])

boone.odds = (p.table[1,1,1]*p.table[2,2,1])/(p.table[2,1,1]*p.table[1,2,1])
fayette.odds = (p.table[1,1,2]*p.table[2,2,2])/(p.table[2,1,2]*p.table[1,2,2])
jefferson.odds = (p.table[1,1,3]*p.table[2,2,3])/(p.table[2,1,3]*p.table[1,2,3])
kenton.odds = (p.table[1,1,4]*p.table[2,2,4])/(p.table[2,1,4]*p.table[1,2,4])
warren.odds = (p.table[1,1,5]*p.table[2,2,5])/(p.table[2,1,5]*p.table[1,2,5])

marginal.odds
boone.odds
fayette.odds
jefferson.odds
kenton.odds
warren.odds

# Simpon's Paradox not a concern, all partial odds facing same direction, CMH test condition met

# CMH Test
mantelhaen.test(p.table, correct=FALSE)

# Theta's =/= 1

# Breslow-Day Test
breslowday.test <- function(x) {
  #Find the common OR based on Mantel-Haenszel
  or.hat.mh <- mantelhaen.test(x)$estimate
  #Number of strata
  K <- dim(x)[3]
  #Value of the Statistic
  X2.HBD <- 0
  #Value of aj, tildeaj and Var.aj
  a <- tildea <- Var.a <- numeric(K)
  
  for (j in 1:K) {
    #Find marginals of table j
    mj <- apply(x[,,j], MARGIN=1, sum)
    nj <- apply(x[,,j], MARGIN=2, sum)
    
    #Solve for tilde(a)_j
    coef <- c(-mj[1]*nj[1] * or.hat.mh, nj[2]-mj[1]+or.hat.mh*(nj[1]+mj[1]),
              1-or.hat.mh)
    sols <- Re(polyroot(coef))
    #Take the root, which fulfills 0 < tilde(a)_j <= min(n1_j, m1_j)
    tildeaj <- sols[(0 < sols) &  (sols <= min(nj[1],mj[1]))]
    #Observed value
    aj <- x[1,1,j]
    
    #Determine other expected cell entries
    tildebj <- mj[1] - tildeaj
    tildecj <- nj[1] - tildeaj
    tildedj <- mj[2] - tildecj
    
    #Compute \hat{\Var}(a_j | \widehat{\OR}_MH)
    Var.aj <- (1/tildeaj + 1/tildebj + 1/tildecj + 1/tildedj)^(-1)
    
    #Compute contribution
    X2.HBD <- X2.HBD + as.numeric((aj - tildeaj)^2 / Var.aj)
    
    #Assign found value for later computations
    a[j] <- aj ;  tildea[j] <- tildeaj ; Var.a[j] <- Var.aj
  }
  
  #Compute Tarone corrected test
  X2.HBDT <-as.numeric( X2.HBD -  (sum(a) - sum(tildea))^2/sum(Var.aj) )
  
  #Compute p-value based on the Tarone corrected test
  p <- 1-pchisq(X2.HBDT, df=K-1)
  
  res <- list(X2.HBD=X2.HBD,X2.HBDT=X2.HBDT,p=p)
  class(res) <- "bdtest"
  return(res)
}

breslowday.test(p.table)

# Thetas are equal, collapse over county

# Confidence Interval for Common Odds Ratio = 3.866663
# 3.1512131 to 4.256912


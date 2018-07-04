#Pruning on Classification tree

library(xlsx)
library(rpart)
library(rpart.plot)

# Promoffers.xlsx
df1 = read.xlsx(file.choose(),1,header = T)
df1 = df1[, !apply(array, margin, ...)]

df1[1:20]
str(df1)

# Grouping the categories of PIN.code
tPIN = tables(as.factor(df1$PIN.Code))
PINnames = dimnames(tPIN)[[1]]

# Count of success class for each PIN code
C_PINcode = NULL
for(x in PINnames){
  C_PINcode = c(C_PINcode, length(which(as.character(df1$PIN.Code) == x &
                                          df1$Promooffer == 1 )))
}
range(C_PINcode)
barplot(C_PINcode, names.arg=PINnames, 
        xlab="PIN Code", las = 3,
        ylab = "#Promotional offers accepted",
        ylim = c(0.13), cex.names=0.6)

table(as.factor(C_PINcode))
# Assign count of PIN code as its label
# PIN Codes having same count will have same label and will be grouped
for(x in PINnames) {
  index = which(as.character(sd1$PIN.Code)==x)
  df1[index, ]$PIN.Code = rep(C_PINcode[which(PINnames == x)], lenght(index))
}

df1$PIN.Code = as.factor(df1$PIN.code)
df1$Promoffer = as.factor(df1$Promoffer)
df1$Online = as.factor(df1$Online)
str(df1)

#Partitioning : Tr:V:Te --> 2500:1500:1000
partidx = sample(1:nrow(df1), 2500, replace=F)
df1train = df1[partidx, ]
partidx1 = sample((1:nrow(df1))[-partidx], 1500, replace = F)
intersect(partidx, partidx1)
df1valid = df1[partidx1, ]
df1test = df1[-c(partidx1, partidx), ]

mod1 = rpart(Promoffer ~., method = "class", data=df1train,
             control = rpart.control(cp=0, minsplit = 2, minbucket = 1,
                                     maxcomplete = 0, maxxsurrogate = 0,
                                     xval = 0),
             parms = list(split="gini"))
# par(mar=c(0,0,0,0), oma=c(0,0,0,0), xpd=NA)
# plot(mod1, uniform=T, branch = 0.1, compress = T,
#     margin = 0, nspace = 1)

# Pruning Process
# Validation partition: Misclassification error vs. no.of. decision nodes
# Total no. of nodes in full grown tree
nrow(mod1$frame)
#No. of Decision nodes
nrow(mod1$splits)
#No.of Terminal nodes
nrow(mod1$frame) - norw(mod1$splits)
#Node numbers
toss1 = as.integer(row.names(mod1$frame)); toss1
DFP = data.frame("toss" = toss1, "Svar" = mod1$frame$var, 
                 "CP" = mod1$frame$complexity); DFP
DFP1 = DFP[DFP$Svar != "<leaf>",]; DFP1

#Nested sequence of splits based on complexity
DFP2 = DFP1[order(DFP1$CP, -DFP1$toss, decreasing = T), ]: DFP2

rownames(DFP2) = 1:nrow(DFP2); DFP2

toss2 = DFP2$toss
#Counter for nodes to be snipped off
i = 1
mod1splitv = list()
mod1strainv = list()
mod1svalidv = list()
ErrTrainv=NULL
Errvalidv = NULL

for(x in DFP2$Svar) {
  if (i <= length(toss2)) {
    toss3 = toss2[i:length(toss2)]
    
    mod1split = snip.rpart(mod1, toss = toss3)
    # Now cut down the CP table
    temp = pmax(mod1$cptable[,1], DPF2$cp[i])
    keep = match(unique(temp), temp)
    mod1split$cptable = mod1$cptable[keep, ,drop = FLASE]
    mod1split$cptable[max(keep),1] = DPF2$cp[i]
    #Reset the variable importance
    mod1split$variable.importance = importance(mod1split)
    
    mod1splitv = list(mod1splitv, mod1split)
    
    mod1strain = predict(mod1split, df1train[ , -c(3)], type = "class")
    mod1strainv = list(mod1strainv, mod1strain)
    mod1svalid = predict(mod1split, df1valid[ , -c(3)], type = "class")
    mod1svalidv = list(mod1svalidv, mod1svalid)
    
    ErrTrain = mean(mod1strain != df1train$promoffer)
    ErrTrainv = c(ErrTrainv, ErrTrain)
    Errvalid = mean(mod1svalid !=df1valid$promoffer)
    Errvalidv = c(Errvalidv, Errvalid)
  }
  i = i+1
}

mod1train = predict(mod1, df1train[ , -c(3)], type = "class")
mod1test = predict(mod1, df1test[ , -c(3)], type = "class")
mod1valid = predict(mod1, df1valid[ , -c(3)], type = "class")

#  Error rate vs no.of.splits
DF = data.frame("Decision Nodes" = 0:(nrow(mod1$splits)-1),
                "Error Training" = ErrTrainv,
                "Error validation" = Errvalidv, check.names = F);
DF[nrow(mod1$splits),1] = nrow(mod1$splits)
DF[nrow(mod1$splits),2] = mean(mod1train != df1train$Promoffer)
DF[nrow(mod1$splits),3] = mean(mod1valid != df1valid$Promoffer)
DF

#Tree after last snip
prp(mod1split, varlen = 0, cex = 0.7, extra = 0, compress = T,
    Margin = 0, digits = 0)
nrow(mod1split$frame)
nrow(mod1split$splits)
nrow(mod1split$frame) - nrow(mod1split$splits)

# Tree after first snip
prp(mod1splitv[[1]], varlen = 0, cex = 0.7, extra = 1, compress = T,
    Margin = 0, digits = 0)
nrow(mod1splitv[[1]]$frame)
nrow(mod1splitv[[1]]$splits)
nrow(mod1splitv[[1]]$frame) - nrow(mod1splitv[[1]]$splits)

# Plot of error rate vs. no.of splits
range(100*DF[,2])
range(100*DF[,3])
plot(smooth.spline(DF[ ,1], 100*DF[,2]), type = "l",
     xlab = "Number of splits", ylab = "Error Rate")
lines(smooth.spline(DF[,1], 100*DF[,3]))

# Minimum error tree & Best pruned tree
min(DF[,3])
MET = min(DF[which(DF[,3] == min(DF[,3])),1]); MET

#Std err
sqrt(var(DF[,3])/length(DF[,3]))

# Best pruned tree near first minima: within 1 std. err
met1std = min(DF[,3]) + sqrt(var(DF[,3])/length(DF[,3])); met1std

BPT = DF[which(DF[,3] > min(DF[,3]) &
                 DF[,3] < met1std &
                 DF[,1]<MET), ][1,1];
if(is.na(BPT)) BPT=MET; BPT

toss3 = toss2[(BPT + 1):length(toss2)]
mod1best = snip.rpart(mod1, toss = toss3)
prp(mod1best, type = 1, extra = 1, under = T, varlen = 0, cex = 0.7,
    compress = T, Margin = 0, digits = 0,
    split.cex = 0.8, under.cex = 0.8)

# Change xval (default value = 10)
# Pruning using rpart's prune
mod2 = rpart(Promoffer ~., method = "class", data = df1train,
             control = rpart.control(cp = 0, minsplit = 2, minbucket = 1,
                                     maxcompete = 0, maxsurrogate = 0,
                                     xval = 10))

mod2$cptable

# xerror contains estimates of cross_validated prediction error
# for different numbers of splits (nsplit)
# Find CP value corresponding to minimum xerror value

mod2$sptable[which.min(mod2$cptable[, "xerror"]), ]
cp1 = mod2$cptable[which.min(mod2$cptable[,"xerror"]), "CP"]; cp1
plotcp(mod2)

pmod=prune(mod2, cp=cp1)











importance = function(fit)
{
  ff = fit$frame
  fpri = which(ff$var != "<leaf>") #points to primary splits in ff
  spri = 1 + cumsum(c(0,1 + ff$ncompete[fpri] + ff$nsurrigate[fpri]))
  spri = spri[seq_along(fpri)] # points to primaries in the splits matrix
  nsurr = ff$nsurrogate[fpri] # number of surrogates each has
  
  sname = vector("list", length = (fpri))
  sval = sname
  
  ##The importance for primary splits needs to be scaled
  ## it was a printout choice for the anova method to list % improvement in
  ## the sum of squares, an importance calculation needs the total ss
  ## All the other methods report an unscaled change.
  scaled.imp = if(fit$method == "anova")
    fit$splits[spri, "improve"] * ff$dev[fpri]
  else  fit$splits[spri, "improve"]
  
  sdim = rownames(fit$splits)
  for(i in seq_along(fpri)) {
    ##points to surrogates
    if (nsurr[i] > OL) {
      indx = spri[i] + ff$ncompete[fpri[i]] + seq_len(nsurr[i])
      sname[[i]] = sdim[indx]
      sval[[i]] = scaled.imp[i] * fit$splits[indx, "adj"]
      
    }
  }
  
  import = tapply(c(scaled.imp, unlist(sval)),
                  c(as.character(ff$var[fpri]), unlist(sname)),
                  sum)
  sort(c(import), decreasing = TRUE) # a named vector
  
}













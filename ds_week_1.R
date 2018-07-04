typeof() #object data type
is.integer() #The type of variable
as.integer() #To convert data type 

X = c(2,3,4) #Example of a vector

subject = c("sci","mat","eng")
Ram = c(65,30,67)
Geetha=c(87,89,75) 

mylist = list(subject,Ram,Geetha)
mylist[[2]][3] #accesing component of a list
mylist  = c(mylist, list(Priya = c(89,56,75)))
print(mylist)

data.frame() #creates a data frame
subset()#Extracts te subset based on condtions 

as.data.frame(mylist)
mytab = edit(mylist) #used to edit a df
rbind() #adds a row to df
cbind() #adds a col to df

A  = matrix(c(1,2,3,4,5,6,7,8,9),nrow =3,ncol=3, byrow = TRUE) #creates a matrix
B = matrix(c(1,2,3,4,5,6,7,8,9),nrow =3,ncol=3)
print(A,B)
matrix(4,3,2) #MAtrix where all and rows and cols are filled by a single constant 4-> value,3->row,2->col

diag(c(1,2,3),4,4) #diagonal ele 3,3 is rows,columns 
diag(1,4,4) #identity matrix

dim(A) #dimensions of a matrix
C =matrix(c(1:9), nrow = 3, ncol = 3, byrow = F)
D = matrix(c(51,52,53,54,55,56,57,58,59),nrow = 3,ncol=3)
D[2,3]
diag(D) = c (45,75,69)

f = function(arguments) {  } #creating a function

a = seq(from =1, to=10, by =2)
b=seq(from =1, to=16, length =8)

n= 5
sum =0 
for(i in seq(1,n,1)) {
  sum = sum + i
  print(c(i,sum))
  
}

x = c(1,2,3)
y = c(4,5,6)
XY = cbind(x,y)
print(XY)
 
rank(A) #rank of a matrix 
library(pracma)
install.packages("pracma") 
Rank(A)

#Linear algebra for DS
#Null space of a matrix
 #Topics needed to be explored
 #Rank of a matrix
 #Nullity
 #Null Space  

#Linear Algebra
  #Full row Rank
 #Full Col Rank
Z = matrix(c(1,-3,-5,3,0,-2,1,3,-4,1),nrow = 2, ncol = 5, byrow = TRUE)
nullspace(Z)

Rank(matrix(1,3,3))
C = matrix(c(1,2,2,3,4,7,-2,0,6),nrow = 3, ncol = 3, byrow = TRUE)
Rank(C)
eigen(C)

v1 = c(1,-2,4)
v2 = c(2,5,2)
N = t(v1)%*%v2# Matrix Multiplication 
N

 
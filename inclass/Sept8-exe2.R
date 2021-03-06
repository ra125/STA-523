##read the data file
d = read.csv("~cr173/Sta523/data/evals.csv")

#typeof(d)
typeof(d)

typeof(d[,1])
typeof(d[,2])
typeof(d[,3])
typeof(d[,4])
typeof(d[,5])

#assign character to number
d[,3][d[,3]==0]='teaching'
d[,3][d[,3]==1]='tenure track'
d[,3][d[,3]==2]='tenured'
d[,4][d[,4]==0]='male'
d[,4][d[,4]==1]='female'
d[,5][d[,5]==0]='lower division'
d[,5][d[,5]==1]='upper division'

#sub NA to -999.0
d[d==-999.0]=NA

#mean of evalutation
#if use (d[,1][d[,1]==NA]), it wouldn't work, becasuse there is a miss value there. In this form, all the value return would be "missing value", i.e. NA.
mean(d[,1][!is.na(d[,1])])
mean(d[,2][!is.na(d[,2])])

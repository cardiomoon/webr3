#  http://www.cultureplex.ca/wp-content/uploads/2018/01/1-s2.0-S1751157717300500-main.pdf

library(bibliometrix)

files='http://www.bibliometrix.org/datasets/bibliometric_management_business_pa.txt'
D<-convert2df(file=files)
D<-M
results=biblioAnalysis(M,sep=";")
summary(results)
plot(results,k=10)
CR <- citations(M, field = "article", sep = ";")
str(CR)
CR <- localCitations(M, results, sep = ";")
str(CR)
CR$Authors

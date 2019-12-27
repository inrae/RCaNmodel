setwd("~/Documents/Bordeaux/equipe estuaire/CaN/can/RCaN/")

library(Matrix) #to build sparse matrices and vector
library(symengine) #https://github.com/symengine/symengine.R
rm(list=ls())
library(xlsx)
library(cpgsR)
library(lpSolveAPI)
library(utils)
library(ggplot2)


file="CaN_template_mini.xlsx"
#file="CaN_input_template3.xlsx"










fitCaN<-function(myCaNmod){
  nbparam<-ncol(myCaNmod$A)
  nbcontr<-nrow(myCaNmod$C)+nrow(myCaNmod$A)
  lp_model<-defineLPMod(myCaNmod)
  ncontr<-length(get.constr.value(lp_model))
  set.objfn(lp_model,rep(1,nbparam))
  res<-solve.lpExtPtr(lp_model)
  x0<-(get.primal.solution(lp_model,orig=TRUE)[(ncontr+1):(ncontr+nbparam)])
}




myCaNmod=build_CaNmod(file)

checkPolytopeStatus(myCaNmod)
findingIncompatibleConstraints(myCaNmod)


getAllBoundsParam(myCaNmod)
plotPolytope2D(myCaNmod,params=c("F01_1","F01_2"))

gets


write.table(myCaNmod$N,"/tmp/N.csv",sep=";")
write.table(myCaNmod$H,"/tmp/H.csv",sep=";")
write.table(as.matrix(myCaNmod$A),"/tmp/A.csv",sep=";")
write.table(as.matrix(myCaNmod$C),"/tmp/C.csv",sep=";")
write.table(myCaNmod$b,"/tmp/b.csv",sep=";")
write.table(myCaNmod$v,"/tmp/v.csv",sep=";")
write.table(as.matrix(myCaNmod$L),"/tmp/L.csv",sep=";")
write.table(myCaNmod$M,"/tmp/M.csv",sep=";")


setwd("~/Documents/Bordeaux/equipe estuaire/CaN/can/RCaN/")

library(Matrix) #to build sparse matrices and vector
library(symengine) #https://github.com/symengine/symengine.R
rm(list=ls())
library(xlsx)
library(cgpsR)
library(lpSolveAPI)
library(utils)

file="CaN_input_template3.xlsx"

#this function can be useful to use relative abundance indices
mean.VecBasic<-function(x,...){
  sum(x)/length(x)
}


#function that convert a less than constraint into a linear equation a%*%x=0
treat_constraint<-function(myconstraint,yr=NULL,years=NULL){
  sign<-ifelse (length(grep("<=",myconstraint))>0,"<=",ifelse (length(grep(">=",myconstraint))>0,">=","="))
  tmp<-strsplit(myconstraint,sign)[[1]]
  if(sign=="<=" | sign=="="){
    left<-tmp[1]
    right<-tmp[2]
  } else if(sign==">="){
    left<-tmp[2]
    right<-tmp[1]
  }
  ###check for division
  left_numerator<-left
  left_denominator<-1
  right_numerator<-right
  right_denominator<-1
  if(length(grep("/",left))>0){
    left_numerator<-strsplit(left,"/")[[1]][1]
    left_denominator<-strsplit(left,"/")[[1]][2]
  }
  if(length(grep("/",right))>0){
    right_numerator<-strsplit(right,"/")[[1]][1]
    right_denominator<-strsplit(right,"/")[[1]][2]
  }
  
  symbolic_constraint<-symengine::expand(eval(parse(text=left_numerator))*eval(parse(text=right_denominator))-eval(parse(text=right_numerator))*eval(parse(text=left_denominator)))
  mat<-do.call(rbind,lapply(as.vector(symbolic_constraint),build_vector_constraint))
  if (is.null(yr)){
    yr<-1:nrow(mat)
  } else{
    yr<-years %in% as.character(eval(parse(text=yr)))
  }
  
  mat<-mat[yr,]
  rownames(mat)<-paste(yr,myconstraint,sep=" : ")
  mat
}



##now we put it into a (sparse) matrix format
build_vector_constraint<-function(eq_constraint){
  coeff_const<-Matrix::Matrix(0,1,length(param),sparse=TRUE)
  colnames(coeff_const)<-as.character(param)
  basic_constraint<-(eq_constraint)
  all_elements=get_str(basic_constraint)
  myelem=strsplit(gsub(" ","",gsub(" +",",+1*",gsub(" -",",-1*",all_elements,fixed=TRUE),fixed=TRUE)),",",fixed=TRUE)[[1]]
  param_used=sort(as.character(free_symbols(basic_constraint)))
  if (length(myelem)>length(param_used)){ #there is an intercept
    param_used=c("1",param_used)
  }
  coeff_const[,param_used]<-mapply(function(expr,par) eval(parse(text=gsub(paste("\\*?",par,"$",sep=""),"",expr))),myelem,param_used)
  coeff_const
}




###function that builds all generic objects
generate_symbolic_objects <- function (flow,species,ntstep,H,N,B0,series){
  nbspec <- length(species)
  Ie<-diag(nbspec) #diagonal_matrix
  IE_H<-symengine::Matrix(Ie-H)
  n<-symengine::Matrix(N)
  B_0<-Vector(B0)
  for (f in flow)  assign(paste(f,0,sep="_"),S(paste(f,0,sep='_'))) #symbolic flow for time step 0
  F_0<-eval(parse(text=paste("Vector(",paste(flow,0,sep="_",collapse=","),")"))) #symbolic vector F_0 (all fluxes for time step 0)
  list_F<-list(F_0)
  list_B<-list(B_0)
  for(t in 1:(ntstep-1)){
    for (f in flow){
      assign(paste(f,t,sep="_"),S(paste(f,t,sep='_'))) #creation of symbolic fluxes for time step t
      
    }
    assign(paste("F",t,sep="_"),eval(parse(text=paste("Vector(",paste(flow,t,sep="_",collapse=","),")")))) #vector of fluxes for time step t
    assign(paste("B",t,sep="_"),(IE_H%*%eval(parse(text=paste("B",t-1,sep="_"))))[,1]+(n%*%eval(parse(text=paste("F",t-1,sep="_"))))[,1]) # biomass at time t+1 is B_t+1=(Ie-H)%*%B_t+N%*%F_t
    list_F=c(list_F,eval(parse(text=paste("F",t,sep="_"))))
    list_B=c(list_B,eval(parse(text=paste("B",t,sep="_"))))
  } 
  
  assign("Fmat",do.call('cbind',list_F))
  assign("param",do.call('c',list_F)) #vector of flows on which we will have to sample
  assign("Bmat",do.call('cbind',list_B)) 
  param<-c(V(1),param) #we add an intercept
  
  for(is in 1:nbspec){
    assign(species[is],Bmat[is,]) #vectors of biomass named by species name
  }
  
  for(f in 1:length(flow)){
    assign(flow[f],Fmat[f,]) #vectors of flow named by flow name
  }
  
  for (s in names(series)[-1]){
    assign(s,series[,s])
  }
  rm(list=c("H","N","flow","species","ntstep","series","B0","f","s","t","is"))
  return(environment())
}



###function that reads the template and return build objects
build_CaNmod<-function(file){
  #Components & input parameter
  components_param<-read.xlsx(file,sheetName="Components & input parameter",de)
  index_species<-which(components_param$in_out=="In")
  components<-components_param$Component
  species<-as.character(components_param$Component[index_species])
  nbspecies<-length(species)
  
  #read Fluxes
  fluxes_def<-read.xlsx(file,sheetName="Fluxes")
  flow<-as.character(fluxes_def$Flux)
  nbfluxes<-nrow(fluxes_def)
  fluxes_from<-match(fluxes_def$From,species)
  fluxes_to<-match(fluxes_def$To,species)
  is_trophic_flux <- fluxes_def$Trophic==1
  
  #read Times series
  series<-read.xlsx(file,sheetName="Input time-series")
  ntstep<-nrow(series)
  data_series_name<-names(series)[-1]
  
  
  #read constraints
  constraints<-read.xlsx(file,sheetName="Constraints")
  lessthan <- grep("<",constraints$Constraint)
  greaterthan <- grep(">",constraints$Constraint)
  equality <- grep("^[^<>]+$",constraints$Constraint)
  
  
  #build matrices H and N
  H<-diag(1-exp(-components_param$OtherLosses[index_species]))
  N<-matrix(0,nbspecies,nbfluxes)
  N[cbind(fluxes_from,1:nbfluxes)]<- -1 #this is an outgoing flow
  N[na.omit(cbind(fluxes_to,1:nbfluxes))]<-na.omit(N[cbind(fluxes_to,1:nbfluxes)]+ifelse(is_trophic_flux,components_param$AssimilationE[match(fluxes_def$To,components)]*components_param$Digestibility[match(fluxes_def$From,components)],1)) #if it is not a trophic flow, we do not take into account assimilation and digestibility
  N<-sweep(N,1,STATS=diag(H)/(components_param$OtherLosses[index_species]),"*")
  
  
  #build symbolic objects in a specific environment
  symbolic_enviro <- generate_symbolic_objects(flow,species,ntstep,H,N,components_param$InitialBiomass[index_species],series)
  
  
  #build A matrix and b corresponding to constraints A.x<=b
  nbparam<-length(symbolic_enviro$param)
  A<-Matrix::Matrix(0,0,length(symbolic_enviro$param),sparse=TRUE)
  colnames(A)<-as.character(symbolic_enviro$param)
  
  ####add flow positiveness
  A<-rbind(A,cbind(rep(0,nbparam-1),diag(-1,nbparam-1,nbparam-1)))
  
  ####add refuge biomasses/biomass positiveness
  attach(symbolic_enviro)
  A<-rbind(A,do.call(rbind,lapply(components_param$Component[components_param$Component %in%species],function(sp) treat_constraint(paste(sp,">=",ifelse(is.na(components_param$RefugeBiomass[components_param$Component==sp]),0,components_param$RefugeBiomass[components_param$Component==sp]))))))
  
  ####add satiation
  species_flow_to<-unique(as.character(fluxes_def$To[fluxes_def$To%in%species & is_trophic_flux]))
  A<-rbind(A,do.call(rbind,lapply(species_flow_to[!is.na(components_param$Satiation[match(species_flow_to,components_param$Component)])],
                                  function(sp) treat_constraint(paste(paste(fluxes_def$Flux[fluxes_def$To==sp & is_trophic_flux],collapse="+"),"<=",components_param$Satiation[components_param$Component==sp],"*",sp)))))
  ####add inertia 
  ####to be corrected, we should only take into account tropic flows, i.e. remove non trophic flows
  A<-rbind(A,do.call(rbind,lapply(components_param$Component[components_param$Component %in%species & !is.na(components_param$Inertia)],
                                  function(sp) { #increase
                                    emigrants <- as.character(fluxes_def$Flux)[as.character(fluxes_def$From)==sp & !fluxes_def$Trophic]
                                    treat_constraint(paste(sp,"[-1] >=",sp,"[1:(length(",sp,")-1)]*exp(-",components_param$Inertia[components_param$Component==sp],")",
                                                           ifelse(length(emigrants)>0,paste("-",paste(emigrants,collapse="-","[1:(length(",sp,")-1)]",sep=""),sep=""),""), #we do not take into account emigrants
                                                           sep=""))
                                  })))
  A<-rbind(A,do.call(rbind,lapply(components_param$Component[components_param$Component %in%species & !is.na(components_param$Inertia)],
                                  function(sp) { #decrease
                                    immigrants <- as.character(fluxes_def$Flux)[as.character(fluxes_def$To)==sp & !fluxes_def$Trophic]
                                    treat_constraint(paste(sp,"[-1] <=",sp,"[1:(length(",sp,")-1)]*exp(",components_param$Inertia[components_param$Component==sp],")",
                                                           ifelse(length(immigrants)>0,paste("+",paste(immigrants,collapse="+","[1:(length(",sp,")-1)]",sep=""),sep=""),""), #we do not take into account imemigrants
                                                           sep=""))
                                  })))
  
  
  ####add constraint provided by user
  if (length(lessthan)+length(greaterthan)>0){
    A<-rbind(A,do.call(rbind,mapply(function(c,yr) treat_constraint(c,yr,as.character(series$Year)),
                                    as.character(constraints$Constraint[c(lessthan,greaterthan)]),as.character(constraints$Time.range[c(lessthan,greaterthan)]))))
    
  }
  b<- -A[,1]
  A <- A [,-1]
  
  
  ####build matrix for equality constraint  C x = v and fill it
  C<-Matrix::Matrix(0,0,length(symbolic_enviro$param),sparse=TRUE)
  colnames(C)<-as.character(symbolic_enviro$param)
  if (length(equality)>0){
    C<-rbind(C,do.call(rbind,lapply(as.character(constraints$Constraint[equality]),
                                    function(c) treat_constraint(c))))
  }
  v<- -C[,1]
  C <- C[,-1]
  detach(symbolic_enviro)
  
  return (list(components_param=components_param,
               species=species,
               fluxes_def=fluxes_def,
               flow=flow,
               series=series,
               ntstep=ntstep,
               data_series_name=data_series_name,
               constraints=constraints,
               H=H,
               N=N,
               A=A,
               C=C,
               v=v,
               b=b,symbolic_enviro=symbolic_enviro))
}


positiveness=1:572
refuge=573:754
satiation=755:936
inertia=937:1111
inertia2=1112:1286
user=1287:1702

getBoundsParam<-function(myCaNmod,i=1){
  a<-matrix(rep(0,ncol(myCaNmod$A)),1)
  a[1,i]<-1
  lsei(a,0,as.matrix(myCaNmod$C),myCaNmod$v,-as.matrix(myCaNmod$A)[c(positiveness,refuge,satiation,inertia,inertia2,user),],-myCaNmod$b[c(positiveness,refuge,satiation,inertia,inertia2,user)])
}

a<-matrix(rep(0,ncol(myCaNmod$A)),1)
a[1,i]<-1
nbc=122
lsei(a,0,e=-as.matrix(myCaNmod$A),f=myCaNmod$b)


myCaNmod=build_CaNmod(file)







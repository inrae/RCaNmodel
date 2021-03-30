  library(RCaN) #the main package
library(ggplot2) #to draw results
library(coda) #to explore mcmc 
library(dplyr) #to manipulate data frame
library(xtable) #to create latex tables
library(xlsx) # to import excel files

#  NAMEFILE <- '/Users/christianmullon/Desktop/Ocean/BarentsSeaReconstructions_01_02_21.xlsx'
NAMEFILE <- '/Users/christianmullon/Desktop/Ocean/CaN_template_mini.xlsx'

COMPONENTS <- read.xlsx(NAMEFILE, 2) 
FLUXES <- read.xlsx(NAMEFILE, 3) 
OBSERVATIONS <- read.xlsx(NAMEFILE, 5) 
CONSTRAINTS <- read.xlsx(NAMEFILE, 4) 

begin <- Sys.time()
POLYTOPE <- buildCaN(NAMEFILE)
end <- Sys.time()
end-begin
summary(POLYTOPE)

dim(POLYTOPE$A)
length(POLYTOPE$b)
dim(POLYTOPE$C)
length(POLYTOPE$v)

checkPolytopeStatus(POLYTOPE)

BOUNDS <- getAllBoundsParam(POLYTOPE)
summary(BOUNDS)

begin = Sys.time()
fluxX <- paste(FLUXES[1,1],'[1990]',sep="")
fluxY <- paste(FLUXES[2,1],'[1990]',sep="")
plotPolytope2D(POLYTOPE, c(fluxX, fluxY))
end=Sys.time()
end-begin

begin = Sys.time()
SAMPLE <- sampleCaN(POLYTOPE, 
                    N=100,thin=100, 
                    nchain=2,
                    ncore=2)
end=Sys.time()
end-begin

nchain(SAMPLE$mcmc)
summary(SAMPLE$mcmc)

fluxY <- paste(FLUXES[2,1],'[1990]',sep="")
gelman.diag(SAMPLE$mcmc[,fluxY])

fluxZ <- paste(FLUXES[3,1],'[1990]',sep="")
thinned_SAMPLE <- window(SAMPLE$mcmc,thin=2)
thin(thinned_SAMPLE)
acfplot(thinned_SAMPLE[,fluxZ])

fluxX <- FLUXES[1,1]
fluxY <- FLUXES[2,1]
compA <- COMPONENTS[2,1] 
c(fluxX,fluxY,compA)

g <- ggSeries(SAMPLE, c(fluxX,fluxY,compA), TRUE)
g + scale_y_log10() + guides(color = FALSE, fill = FALSE)

ggViolin(SAMPLE,c(fluxX,fluxY,compA),year=1990,TRUE)


ggNetwork(POLYTOPE)
ggTrophicRelation(SAMPLE)
ggSatiation(SAMPLE)

ggGrowth(SAMPLE)

ggPairsBiomass(SAMPLE)

compA <- COMPONENTS[2,1] 
ggDiet(SAMPLE, compA)
  

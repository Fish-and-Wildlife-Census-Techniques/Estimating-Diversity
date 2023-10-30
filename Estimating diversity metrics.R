##Biodiversity
#Assignment 1. learn to calculate species richness and diversity from a community data
# use package vegan 
library(vegan)
data(BCI) #read in data, embedded in vegan. 
str(BCI)#take a look on the data. The data consist of species and their abundances on different stands
#you see they are integers (0,1,2,...)
H <- diversity(BCI)# calculate Shannon index (default)
print(H)
range(H)
#prints the values of Shannon index for each site
invsimp <- diversity(BCI, "invsimpson") #calculate simpson index.
#"simpson" returns 1-D and "invsimpson" returns 1/D. See the lecture
print(simp)#Prints the values of Simpson index

S <- specnumber(BCI) ## counts species richness
print(S)
Div_table=cbind(H, simp, S) #combine the values to a table
print(Div_table)
##########################################################
#Now do the same with another dataset.
data(dune)# the data contain species and their abundances.
S1=specnumber(dune)
#1) which of the stands has highest species richness?
#2) which of the stands has lowest Shannon diversity index?
#3) Which of the stands has lowest Simpsons index (1-D)?  
##############

#Assignment2
#Learn to count beta and gamma-diversity in a very simple way
data(dune.env)# Data on sites. Measured values,manage types, treatments.
#everything you measure from sites, should be in this type of file
str(dune.env)#check how it looks like
###############
#dune.env is a data frame of 20 observations on the following 5 variables:
  #A1: a numeric vector of thickness of soil A1 horizon.
#Moisture: an ordered factor with levels: 1 < 2 < 4 < 5.
#Management:a factor with levels: BF (Biological farming), HF (Hobby farming), NM (Nature Conservation Management), and SF (Standard Farming).
#Use: an ordered factor of land-use with levels: Hayfield < Haypastu < Pasture.
#Manure: an ordered factor with levels: 0 < 1 < 2 < 3 < 4.#explanations on dune.env
##############
alpha <- with(dune.env, tapply(specnumber(dune), Management, mean))
print(alpha)
#you count the alpha diversity (species richness) and create a table with mean values
#for each Management.
#YOu can even use the other environmental factors (classifiers), such as Manure
alpha_Manure <- with(dune.env, tapply(diversity(dune,"simpson"), Manure, mean))
#tapply calculates mean values for each management level.
#or Moisture
alpha_Moisture<- with(dune.env, tapply(specnumber(dune), Moisture, mean))
##or any other FACTOR

####Then gamma diversity (Total species richness) for each treatment (Management)
gamma <- with(dune.env, specnumber(dune, Management))#again, you can do the same for other factors

#Beta-diversity, the differences in species composition between the management types
#beta-diversity can be calculated as additive or multiplicative (gamma=alpha*beta)
beta=gamma/alpha - 1
#What do the values mean?

#illustrate the differences between the Management types using Nonmetric 
nmds_dune<-metaMDS(dune)
nmds_dune
plot(nmds_dune,"species", type="t")#plot species in the communities
plot(nmds_dune,"sites", type="n")#makes a "layer" for plotting the sites, but plots nothing

#levels(dune.env$Management) #check the levels
colors=c("black", "blue", "green", "pink") #you can change the colors!
with(dune.env, points(nmds_dune, col=colors[Management], pch=16)) #pch=symbols, 16 is a filled circle
with(dune.env, ordispider(nmds_dune, groups=Management, col="black", label=T ))
#orwith ordihulls, which outline the points 
with(dune.env, ordihull(nmds_dune, groups=Management, col="black", label=T ))
#################
# now you should use another data to do the same.
#You need two datasets: bee.mat and env
#fetch the datasets from Canvas and place them in to the folder you are working in
#read in the datasets
bee_mat=read.csv2(file.choose(), head=T, row.names=1)
str(bee_mat)#check it out first!
env=read.csv2(file.choose(), head=T, stringsAsFactors = T)
str(env)
#Assignment:
#1) calculate the mean species richness for each treatment.
alpha_bee <- with(env, tapply(specnumber(bee_mat), Treatment, mean))
print(alpha_bee)

#2) Calculate the gamma diversity for each treatment
#3)illustrate the species assemblage structures on different treatments with nmds
#what can you say about the species assemblages in these three forest types?

####insects collected by students
insect_mat=read.csv2(file.choose(), head=T, row.names = 1)
insect_env=read.csv2(file.choose(), head=T, stringsAsFactors = T)
str(insect_env)

alpha_insects<- with(insect_env, tapply(specnumber(insect_mat), landscape.type, mean))
nmds_insects=metaMDS(insect_mat, trymax = 500)
colors=c("blue", "red")
plot(nmds_insects, "sites",type="n")
with(insect_env,points(nmds_insects, col=colors[landscape.type], pch=16))
with(insect_env, ordihull(nmds_insects, groups=landscape.type, label=T))

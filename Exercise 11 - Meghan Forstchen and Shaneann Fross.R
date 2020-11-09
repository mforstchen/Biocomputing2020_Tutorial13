#Exercise 11 - Meghan Forstchen and Shaneann Fross

library(ggplot2)

#Set the initial parameters of the models
N0=1
M0=1
r=0.1
K=1000000
######Need to change timestep number
timesteps=100

#Create a dataframe to store output and initial values
NS=data.frame(time=1:timesteps, sim1=rep(0,timesteps), sim2=rep(0,timesteps))


ggplot(data=NS, aes(x=time, y=sim1))+
  geom_line()+
  theme_classic()

NS$sim1[1]=N0
NS$sim2[1]=M0

####Need 1 population to start with not two
#### STUCK ON Loop
###Need to index a single number not whole 100
for(t in 2:timesteps){
  if (NS$sim1 || NS$sim2 <100){
    NS$sim1[t] <- NS$sim1[t-1]+r*NS$sim1[t-1]*(1-((NS$sim1[t-1]+ NS$sim2[t-1])/K))
    NS$sim2[t] <- NS$sim2[t-1]+r*NS$sim2[t-1]*(1-((NS$sim2[t-1]+ NS$sim1[t-1])/K))  
  }else{
    NS$sim1[t] <- NS$sim1[t-1]+(r*NS$sim1[t-1]*(1-((NS$sim1[t-1]+ NS$sim2[t-1])/K))/2)
    NS$sim2[t] <- NS$sim2[t-1]+(-r*NS$sim2[t-1]*(1-((NS$sim2[t-1]+ NS$sim1[t-1])/K))) 
  }
}
  
  
  
#######TEST RUN
for(t in 2:timesteps){
  if (t>49){
    NS$sim1[t] <- NS$sim1[t-1]+r*NS$sim1[t-1]*(1-((NS$sim1[t-1]+ NS$sim2[t-1])/K))
    NS$sim2[t] <- NS$sim2[t-1]+r*NS$sim2[t-1]*(1-((NS$sim2[t-1]+ NS$sim1[t-1])/K))  
  }else{
    NS$sim1[t] <- NS$sim1[t-1]+(r*NS$sim1[t-1]*(1-((NS$sim1[t-1]+ NS$sim2[t-1])/K))/2)
    NS$sim2[t] <- NS$sim2[t-1]+(-r*NS$sim2[t-1]*(1-((NS$sim2[t-1]+ NS$sim1[t-1])/K))) 
  }
}  
  
  


####Trying after stuart

for(t in 2:timesteps){
  if (NS[,2]<100){
    NS$sim1[t] <- NS$sim1[t-1]+r*NS$sim1[t-1]*(1-(NS$sim1[t-1])/K)
  }
}

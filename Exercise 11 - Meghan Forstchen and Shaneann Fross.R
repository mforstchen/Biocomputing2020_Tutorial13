#Exercise 11 Meghan Forstchen and Shaneann Fross

#Set the parameters
N0=1
M0=0
r=0.1
K=1000000
timesteps=500

#Create a dataframe to store output and initial values
NS=data.frame(time=1:timesteps, sim1=rep(0,timesteps), sim2=rep(0,timesteps))

NS$sim1[1]=N0
NS$sim2[1]=M0


#Run a for loop for the two cancer cell populations

for(t in 2:timesteps){
  if(t<50){ ##Normal Growth -- No mutations, there is one population
    NS$sim1[t] <- NS$sim1[t-1]+r*NS$sim1[t-1]*(1-((NS$sim1[t-1]+ NS$sim2[t-1])/K))
    NS$sim2[t] <- 0 
  }else if (t==50){ ## Mutation occured 
    NS$sim1[t] = 99
    NS$sim2[t] = 1
  }else if (t < 150){ ##Both populations grow normally
    NS$sim1[t] <- NS$sim1[t-1]+r*NS$sim1[t-1]*(1-((NS$sim1[t-1]+ NS$sim2[t-1])/K))
    NS$sim2[t] <- NS$sim2[t-1]+r*NS$sim2[t-1]*(1-((NS$sim2[t-1]+ NS$sim1[t-1])/K)) 
  }else{ ## After drug treatment, non-mutant declines and mutant grows slower
    NS$sim1[t] <- NS$sim1[t-1]+(-r*NS$sim1[t-1]*(1-((NS$sim1[t-1]+ NS$sim2[t-1])/K))) 
    NS$sim2[t] <- NS$sim2[t-1]+(r*NS$sim2[t-1]*(1-((NS$sim2[t-1]+ NS$sim1[t-1])/K))/2)
  }
}

NS1<-data.frame(time=c(NS$time), N1=c(NS$sim1), sim=rep(c("normal"), each=timesteps))
NS2<-data.frame(time=c(NS$time), N1=c(NS$sim2), sim=rep(c("mutant"), each=timesteps))


#Plot the models
#Load ggplot library
library(ggplot2)

ggplot()+
  geom_line(data=NS2, aes(x=time, y=N1, color=sim))+
  geom_line(data=NS1, aes(x=time, y=N1, color=sim))+
  xlab("Time")+
  ylab("Number of Cells")+
  theme_classic()



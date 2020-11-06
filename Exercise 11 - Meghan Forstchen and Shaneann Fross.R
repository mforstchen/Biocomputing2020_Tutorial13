#Exercise 11 - Meghan Forstchen and Shaneann Fross


#Set the initial parameters of the models
N0=1
M0=1
r=0.1
K=1000000
timesteps=100

#Create a dataframe to store output and initial values
NS=data.frame(time=1:timesteps, sim1=rep(0,timesteps), sim2=rep(0,timesteps))
NS$sim1[1]=N0
NS$sim2[1]=M0

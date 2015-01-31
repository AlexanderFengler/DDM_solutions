# remove old variables
################
rm(list=ls())

# Initialize mean and sd
mean = 0.00
sd = 0.20

# how coarse do you want the steps
stateStep = 0.1

# how much time do you want to give the process -- defined in steps
timeMax = 200

barrierUp = 1
barrierDown = -1

barrierTimeUp = rep(barrierUp,timeMax)
barrierTimeDown = rep(barrierDown,timeMax)

decay = 0

for (t in 2:timeMax){
  #barrierTimeUp[t]=barrierTimeUp[t-1] * decay
  #barrierTimeDown[t]=barrierTimeDown[t-1] * decay
  barrierTimeUp[t] = barrierUp /(1+decay*t)
  barrierTimeDown[t] = barrierDown /(1+decay*t)
}

# Defining grid vertically
states = seq(barrierDown, barrierUp, stateStep)

# Initialize all probability states with 1
prStates = rep(0,length(states))
prStates[states==0]=1

# Initialize count for barrier crossings -- up and down
upCrossing = rep(0,timeMax)
downCrossing = rep(0,timeMax)


for (t in 1:timeMax) {
  print(t)
  
  # Set next states
  PrStatesNew = rep(0,length(states))
  
  # Over all possible destination states
  for (s in 1:length(states)) {
    # define current destination
    to = states[s]
    if (to>barrierTimeDown[t] & to<barrierTimeUp[t]) {
      # calculate change from every possible state to current destination state
      change = to - states
      
      # calculate new probability for current destination state 
      PrStatesNew[s] = stateStep * sum(prStates * dnorm(change,mean,sd)) # why dnorm?
    }
  }
  
  # DONT UNDERSTAND
  changeUp = barrierTimeUp[t] - states
  
  # Why 1-pnorm instead of dnorm
  tempUpCross = sum(prStates * (1-pnorm(changeUp,mean,sd)))
  changeDown = barrierTimeDown[t] - states
  tempDownCross = sum(prStates * pnorm(changeDown,mean,sd))
  
  # renormalize (to cope w/ numerical approximation)
  sumIn = sum(prStates)
  sumCurrent = sum(PrStatesNew) + tempUpCross + tempDownCross
  PrStatesNew = PrStatesNew * sumIn/sumCurrent
  tempUpCross = tempUpCross * sumIn/sumCurrent
  tempDownCross = tempDownCross * sumIn/sumCurrent
  #
  prStates = PrStatesNew
  upCrossing[t]=tempUpCross
  downCrossing[t]=tempDownCross
}

sum(upCrossing)
sum(downCrossing)
sum(upCrossing) + sum(downCrossing)
plot(1:timeMax,upCrossing,type="line",col=1)
points(1:timeMax,downCrossing,type="line",col=2)
sum((1:timeMax)*upCrossing)/sum(upCrossing)
sum((1:timeMax)*downCrossing)/sum(downCrossing)

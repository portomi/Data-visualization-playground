#simulaton_length
days <- 3*365

#Environment parameters
o2_level <- 0.16
temperature <- 22
rain <- 22
daytime_hours <- 12

#Plants and animals can be franctions meaning eg. junger animals

#Plants
grass <- c(3500) #let's start with 1 ton of grass
grass_preferred_temp <- 25
#Grazers
cow=c(200)
cow_preferred_temp <- 24
cp <- 1 #parameter related cow population growth
#Predators
wolf=c(1)
wolf_preferred_temp <- 18
wop <- 1

#time flies
for (time in 2:days)
{
  #plant growth
  grass <- append(grass, (grass[time-1] + grass[time-1]*o2_level*dnorm(temperature, mean=grass_preferred_temp, sd=5)*(rain/10)*(daytime_hours*100)))

  #cows live and eat grass
  #a cow needs 20kg of grass a day
  if (grass[time] < 20*(cow[time-1]+1)){ #if there is not enough grass for at leat one more cow
    #if there is enough grass for every cow
    if (grass[time] >= 20*cow[time-1]){
      cow <- append(cow, cow[time-1]) #number of cows remain the same
      grass[time] <- grass[time] - 20*cow[time]
    }
    else{ #not enough grass for every cows
      cow <- append(cow, grass[time]/20) #only that many cows remains that has enough food
      grass[time] <- grass[time] - 20*cow[time]
    }
  }
  else{ #if there is enough grass
    cow <- append(cow, cow[time-1]*(1+cp*dnorm(temperature, mean=cow_preferred_temp, sd=10)))
    grass[time] <- grass[time] - 20*cow[time]
  }
  
  #wolf live and hunt
  #a wolf needs a quarter cow every 5th day let it be 1/20 cows per day
  if (cow[time] < (1/20)*(wolf[time-1]+1)){ #if there is not enough cow for at leat one more wolf
    #if there is enough cow for every wolf
    if (cow[time] >= (1/20)*wolf[time-1]){
      wolf <- append(wolf, wolf[time-1]) #number of wolves remain the same
      cow[time] <- cow[time] - (1/20)*cow[time]
    }
    else{ #not enough cow for every wolves
      wolf <- append(wolf, cow[time]/(1/20)) #only that many wolves remain that has enough food
      cow[time] <- cow[time] - (1/20)*wolf[time]
    }
  }
  else{ #if there is enough cow
    wolf <- append(wolf, wolf[time-1]*(1+wop*dnorm(temperature, mean=wolf_preferred_temp, sd=15)))
    cow[time] <- cow[time] - (1/20)*wolf[time]
  }
  
if (grass[time] < 1) {
  cat("no more grass at", time)
  break}
else if (cow[time] < 1) {
  cat("no more cow at", time)
  break}
else if (wolf[time] < 1) {
  cat("no more wolves at ", time)
}
  
#adjusting cp and WOP
  
  #if grass can feed 10 times as much cows let's double cp
  if (grass[time] >= 10*cow[time]){
    cp <- 2*cp
  }
  #if grass is not enough for all the cows plus one more cow let's devide cp by 10
  else if (grass[time] < cow[time]+1){
    cp <- cp/10
  }
  #else cp stays the same
  
  #if cows can feed 4 times the number of wolves let's increase wop
  if (cow[time] >= 4*wolf[time]){
    wop <- 4.75*wop
  }
  #if cows are not enogh for all the wolves let's divide wop by 1
  else if (cow[time] < wolf[time]+1){
    wop <- wop/10
  }
  #else wop stays the same
  
} #end of time, the world and everything


#Plot
plot(grass, type='l', col='red', xlab='Time (Days)')
lines(cow, col='green')
lines(wolf, col='blue')
#Monty hall simulation 

##Initialise some variable
sum_of_gain_switch <- 0
sum_of_gain_keep <- 0
niter <- 10000 #number of iteration

for(i in 1:niter) 
{
  prices <- c(1000000,0,0) #These are the possible price. 1 price is a million dollar, the two other are nothing
  
  doors <- sample(prices, 3, replace = F) #We randomly put the price behind one of the three door
  
  door_numbers <- c(1,2,3) #We assign numbers to door, it's just to make it easier to manipulate
  
  door_choice <- sample(door_numbers, size = 1) #We simulate the random choice of the door. We have 1/3 chances of picking the 1000000 dollar door at this point, and 2/3 chance of being wrong.
  
  if(doors[door_choice] != prices[1]) #Now we have two possible case scenario. First, what happens if i didn't pick the 1000000 dollar door? This scenario happens in 2 out of 3 cases.
  {
    show_door_0 <- door_numbers[door_numbers != door_choice & doors != prices[1]] #Then the host doesn't have a choice and have to show the other door (beside the one we picked) that has no money behind.
    #By doing so, the host is essentially revealing the 1000000 dollar door, because it's the last remaining door, beside our first choice and the one he just revealed.
    
  } else #The second case, which happens in 1/3 cases, the host can simply show one of the two remaing door with no price behind.
  {
    show_door_0 <- door_numbers[sample(door_numbers[doors != prices[1]], 1)] #show randomly one of the two remaing loosing doors.
    #This case happens only with 1/3 chances. Here, the host didn't reveal the the right door either, because both of the door he could have shown where loosing door anyway.
  }
  
  new_door_choice <- door_numbers[door_numbers != door_choice & door_numbers != show_door_0] #This is the situation where we switch.
  keep_first_door_choice <- door_choice #This is the situation where we stick to our first choice. 
  
  #This is were the trick is. Remember that if we switch, the scenario where we loose is the one where we had picked the winning door in the first choice. This happens in only 1/3 cases.
  #This means that we only loose by switching in 1/3 cases, not 1/2 !! Because in 2/3 of cases, we picked the wrong door, leading to scenario 1 where the host doesn't have a choice in which door he 
  #can show us, thereby indirectly revealing the 1000000 door !
  
  price_switch <- doors[new_door_choice] #gain if we switched
  price_keep <- doors[keep_first_door_choice] #gain if we kept the first choice
  
  sum_of_gain_switch <- sum_of_gain_switch + price_switch #cumulate the switch gain of the multiple runs
  sum_of_gain_keep <- sum_of_gain_keep + price_keep #cumulate the keep gain of the multiple runs
}

avg_gain_switch <- sum_of_gain_switch/niter #average the switch gains
avg_gain_keep <- sum_of_gain_keep/niter #average the keeph gains

#The switch gain are twice as much as the keep gains !!!!
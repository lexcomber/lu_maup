rbga.lu <-  function (size = 100, suggestions = NULL, fixed = NULL, popSize = 200, iters = 200, 
    mutationChance = NA, elitism = NA, zeroToOneRatio = 10, monitorFunc = NULL, 
    evalFunc = NULL, showSettings = FALSE, verbose = FALSE) 
{
	breed <- function(p1,p2) {
	  chop <- sample(1:length(p1),1)
	  c(p1[1:chop],p2[! p2 %in% p1[1:chop]]) }
	
	mutate <- function(p1) {
	  chops <- sample(1:length(p1),2)
	  temp <- p1[chops[1]]
	  p1[chops[1]] <- p1[chops[2]]
	  p1[chops[2]] <- temp
	  p1}	
	
    vars = size #subsize
    if (is.na(mutationChance)) {
        mutationChance = 1/(vars + 1)
    }
    if (is.na(elitism)) {				# prop of chromosomes kept to next generation
        elitism = floor(popSize/5)
    }
    # create an initial population (from whihc chromosome are extracted)
    population = matrix(nrow = popSize, ncol = vars)
    for (child in 1:popSize) {
    	#population[child, ] = sample(size,subsize)
 		population[child, ] = sample(size)
    }
	bestEvals = rep(NA, iters)
    meanEvals = rep(NA, iters)
    evalVals = rep(NA, popSize)
    for (iter in 1:iters) {
    	#iter = 1
        # in each iteration (actually is this the first iteration? evalVals is na once)
        # evaluate each row in population
        for (object in 1:popSize) {
        	if (is.na(evalVals[object])) {
        		evalVals[object] = evalFunc(population[object,])
                }
         }      
        #bestEvals[iter] = min(evalVals)
        bestEvals[iter] = max(evalVals)
        meanEvals[iter] = mean(evalVals)
        result = list(type = "permutation chromosome", size = size, 
        	popSize = popSize, iter = iter, iters = iters, 
        	population = population, elitism = elitism, 
        	mutationChance = mutationChance, evaluations = evalVals, 
        	best = bestEvals, mean = meanEvals)
        class(result) = "rbga"
        monitorFunc(result)
        
        if (iter < iters) {
            newPopulation = matrix(nrow = popSize, ncol = vars)
            newEvalVals = rep(NA, popSize)
            # sortedEvaluations = sort(evalVals, index = TRUE)
            sortedEvaluations = sort(evalVals, decreasing = T, index = TRUE)
            sortedPopulation = matrix(population[sortedEvaluations$ix,], ncol = vars)
            if (elitism > 0) {
                newPopulation[1:elitism, ] = sortedPopulation[1:elitism,]
                newEvalVals[1:elitism] = sortedEvaluations$x[1:elitism]
                }
            if (vars > 1) {
               for (child in (elitism + 1):popSize) {
                    parentProb = dnorm(1:popSize, mean = 0, sd = (popSize/3))
                    parentIDs = sample(1:popSize, 2, prob = parentProb)
                    parents = sortedPopulation[parentIDs, ]
                    newPopulation[child,] <- breed(parents[1,],parents[2,])        
               	}
               } else {
               		newPopulation[(elitism + 1):popSize, ] = sortedPopulation[sample(1:popSize, 
                     popSize - elitism), ]
             }
             population = newPopulation
             evalVals = newEvalVals
             if (mutationChance > 0) {
                  mutationCount = 0
                  for (object in (elitism + 1):popSize) {
                    for (var in 1:vars) {
                      if (runif(1) < mutationChance) {
                        population[object,] = mutate(population[object,])                       
                        mutationCount = mutationCount + 1

                      }
                    }
                  }
              }
            }
        }
    result = list(type = "permutation chromosome", size = size, popSize = popSize, 
        iters = iters, suggestions = suggestions, population = population, 
        elitism = elitism, mutationChance = mutationChance, evaluations = evalVals, 
        best = bestEvals, mean = meanEvals)
    class(result) = "rbga"
    return(result)
}

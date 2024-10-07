# set model for diagnostic checks - https://www.rensvandeschoot.com/brms-wambs/
model <- burn2_p2

# step 2 - check trace plots for convergence
stanplot(model, type = "trace") #check for divergence

summary(burn2) # Check that Rhats are all close to 1

modelposterior <- as.mcmc(model) #with the as.mcmc() command we can use all the CODA package convergence statistics and plotting options
coda::gelman.plot(modelposterior[, 1:5]) #shows the Gelman-Rubin Diagnostic with PSRF values (within and between chain variability) - check that upper CI shrinks to 1, if not more iterations are needed
coda::geweke.diag(modelposterior[, 1:5]) #"Scores above 1.96 or below -1.96 mean that the two portions of the chain significantly differ and full chain convergence was not obtained."
coda::geweke.plot(modelposterior[, 1:5]) #might need to do something about b_trees100_9020?

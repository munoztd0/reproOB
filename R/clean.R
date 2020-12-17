#----clean----
#subset only pretest
tables <- c("PAV","INST","PIT","HED", "intern")
dflist <- lapply(mget(tables),function(x)subset(x, session == 'second'))
list2env(dflist, envir=.GlobalEnv)

#exclude participants (242 really outlier everywhere, 256 can't do the task, 114 & 228 REALLY hated the solution and thus didn't "do" the conditioning) & 123 and 124 have imcomplete data
`%notin%` <- Negate(`%in%`)
dflist <- lapply(mget(tables),function(x)filter(x, id %notin% c(242, 256, 114, 228, 123, 124)))
list2env(dflist, envir=.GlobalEnv)

#merge with info
tables = tables[-length(tables)] # remove intern
dflist <- lapply(mget(tables),function(x)merge(x, info, by = "id"))
list2env(dflist, envir=.GlobalEnv)

# creates internal states variables for each data
listA = 2:5
def = function(data, number){
  baseINTERN = subset(intern, phase == number)
  data = merge(x = get(data), y = baseINTERN[ , c("thirsty", 'hungry', 'id')], by = "id", all.x=TRUE)
  # diffINTERN = subset(intern, phase == number | phase == number+1) #before and after 
  # before = subset(diffINTERN, phase == number); after = subset(diffINTERN, phase == number+1); diff = after
  # diff$diff_piss = diff$piss - before$piss
  # diff$diff_thirsty = diff$thirsty - before$thirsty
  # diff$diff_hungry = diff$hungry - before$hungry
  # data= merge(data, y = diff[ , c("diff_piss", "diff_thirsty", 'diff_hungry', 'id')], by = "id", all.x=TRUE)
  return(data)
}
dflist = mapply(def,tables,listA)
list2env(dflist, envir=.GlobalEnv)


#center covariates
numer <- c("thirsty", "hungry", "age")
tables <- c("PAV","INST","PIT","HED")
dflist <- lapply(mget(tables),function(x) x %>% group_by %>% mutate_at(numer, scale))
list2env(dflist, envir=.GlobalEnv)


#imput mean (0 since its mean centered) for the two participant that have missing covariate (MAR) data so we can still use them in ANCOVA
tables <- c("PAV","INST","PIT","HED")
dflist <- lapply(mget(tables),function(x) imput(x))
list2env(dflist, envir=.GlobalEnv)



# prepro RT PAv -----------------------------------------------------------


# get times in milliseconds 
PAV$RT               <- PAV$RT * 1000

#Preprocessing
PAV$condition <- droplevels(PAV$condition, exclude = "Baseline")
acc_bef = mean(PAV$ACC, na.rm = TRUE) #0.93
full = length(PAV$RT)

##shorter than 100ms and longer than 3sd+mean
PAV.clean <- filter(PAV, RT >= 100) # min RT is 
PAV.clean <- ddply(PAV.clean, .(id), transform, RTm = mean(RT))
PAV.clean <- ddply(PAV.clean, .(id), transform, RTsd = sd(RT))
PAV.clean <- filter(PAV.clean, RT <= RTm+3*RTsd) 

# calculate the dropped data in the preprocessing
clean = length(PAV.clean$RT)
dropped = full-clean
(dropped*100)/full

PAV = PAV.clean

# define as.factors
fac <- c("id", "trial", "condition", "group" ,"trialxcondition", "gender")
PAV.clean[fac] <- lapply(PAV.clean[fac], factor)

#revalue all catego
PAV.clean$group = as.factor(revalue(PAV.clean$group, c(control="-1", obese="1"))) #change value of group
PAV.clean$condition = as.factor(revalue(PAV.clean$condition, c(CSminus="-1", CSplus="1"))); PAV.clean$condition <- factor(PAV.clean$condition, levels = c("1", "-1"))#change value of condition


PAV.means <- aggregate(PAV.clean$RT, by = list(PAV.clean$id, PAV.clean$condition, PAV.clean$liking, PAV.clean$group, PAV.clean$age, PAV.clean$gender, PAV.clean$group), FUN='mean') # extract means
colnames(PAV.means) <- c('id','condition','liking','group', 'RT')



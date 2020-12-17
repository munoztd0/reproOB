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


# clean PAV --------------------------------------------------------------

# define as.factors
fac <- c("id", "trial", "condition", "group" ,"trialxcondition", "gender")
PAV.clean[fac] <- lapply(PAV.clean[fac], factor)

#revalue all catego
PAV.clean$group = as.factor(revalue(PAV.clean$group, c(control="0", obese="1"))) #change value of group
PAV.clean$condition = as.factor(revalue(PAV.clean$condition, c(CSminus="-1", CSplus="1"))); PAV.clean$condition <- factor(PAV.clean$condition, levels = c("1", "-1"))#change value of condition

PAV.means <- aggregate(PAV.clean$RT, by = list(PAV.clean$id, PAV.clean$condition, PAV.clean$liking, PAV.clean$group, PAV.clean$age, PAV.clean$gender, PAV.clean$thirsty,PAV.clean$hungry), FUN='mean') # extract means
colnames(PAV.means) <- c('id','condition','liking','group', 'age','gender', 'thirsty', 'hungry', 'RT')


# clean INST -------------------------------------------------------------

#defne factors
fac <- c("id", "gender", "group")
INST[fac] <- lapply(INST[fac], factor)
#revalue all catego
INST$group = as.factor(revalue(INST$group, c(control="0", obese="1"))) #change value of group


# get the averaged dataset
INST.means <- aggregate(INST$grips, by = list(INST$id, INST$trial, INST$group, INST$age, INST$gender, INST$thirsty, INST$hungry), FUN='mean') # extract means
colnames(INST.means) <- c('id','trial','group', 'age','gender', 'thirsty', 'hungry', 'grips')
tmp = lspline(INST.means$trial, 5); INST.means$ls1 = tmp[,1] ; INST.means$ls2 = tmp[,2]


# clean PIT --------------------------------------------------------------

# define as factors
fac <- c("id", "trial", "condition", "trialxcondition", "gender", "group")
PIT[fac] <- lapply(PIT[fac], factor)
#remove the baseline (we just use it for fMRI analysis)
PIT.clean =  subset(PIT, condition != 'BL') 
#revalue all catego
PIT.clean$group = as.factor(revalue(PIT.clean$group, c(control="0", obese="1"))) #change value of group
PIT.clean$condition = as.factor(revalue(PIT.clean$condition, c(CSminus="-1", CSplus="1"))); PIT.clean$condition <- factor(PIT.clean$condition, levels = c("1", "-1"))#change value of condition

PIT.means <- aggregate(PIT.clean$AUC, by = list(PIT.clean$id, PIT.clean$condition, PIT.clean$group, PIT.clean$age, PIT.clean$gender, PIT.clean$thirsty, PIT.clean$hungry), FUN='mean') # extract means
colnames(PIT.means) <- c('id','condition', 'group', 'age', 'gender', 'thirsty', 'hungry','AUC')


# clean HED ---------------------------------------------------------------

# define as.factors
fac <- c("id", "trial", "condition", "trialxcondition", "gender", "group")
HED[fac] <- lapply(HED[fac], factor)

#revalue all catego
HED$condition = as.factor(revalue(HED$condition, c(MilkShake="1", Empty="-1"))) #change value of condition
HED$condition <- relevel(HED$condition, "1") # Make MilkShake first
HED$group = as.factor(revalue(HED$group, c(obese="1", control="0"))) #change value of group

# create Intensity and Familiarity diff
bs = ddply(HED, .(id, condition), summarise, int = mean(perceived_intensity, na.rm = TRUE), fam = mean(perceived_familiarity, na.rm = TRUE)) 
Empty = subset(bs, condition == "-1"); Milkshake = subset(bs, condition == "1"); diff = Empty;
diff$int = Milkshake$int - Empty$int; diff$fam = Milkshake$fam - Empty$fam;
HED = merge(x = HED, y = diff[ , c("int", "fam", 'id')], by = "id", all.x=TRUE)

#center covariates
numer <- c("fam", "int")
HED = HED %>% group_by %>% mutate_at(numer, scale)
HED$intensity = HED$int; HED$familiarity = HED$fam

HED.means <- aggregate(HED$perceived_liking, by = list(HED$id, HED$condition, HED$group, HED$age, HED$gender, HED$thirsty, HED$hungry, HED$intensity, HED$familiarity), FUN='mean') # extract means
colnames(HED.means) <- c('id','condition','group', 'age', 'gender', 'thirsty', 'hungry', 'intensity', 'familiarity', 'perceived_liking')


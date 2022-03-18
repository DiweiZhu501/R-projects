df <- read.csv("C:/Users/admin/Desktop/animal_shelter.csv")

# ！！！！！！！！！！！！！！！！！！！！！！！！！！
# install.packages("tree")
# install.packages("rpart.plot")
# install.packages("randomForest") 
# ！！！！！！！！！！！！！！！！！！！！！！！！！！
library(tree)
library(rpart)
library(rpart.plot)
library(randomForest)
# ！！！！！！！！！！！！！！！！！！！！！！！！！！

attach(df)

# cp=0.5
mytree1=rpart(outcome~age+number_animals_in_shelter+animal_type+is_sterilized ,control=rpart.control(cp=0.5))
rpart.plot(mytree1)

# cp=0.1
mytree2=rpart(outcome~age+number_animals_in_shelter+animal_type+is_sterilized ,control=rpart.control(cp=0.1))
rpart.plot(mytree2)

# cp=0.0003
mytree3=rpart(outcome~age+number_animals_in_shelter+animal_type+is_sterilized ,control=rpart.control(cp=0.0003))
rpart.plot(mytree3)


# 2C
printcp(mytree3)
plotcp(mytree3)

# 2D
opt_cp = mytree3$cptable[which.min(mytree3$cptable[,"xerror"]),"CP"]
print(opt_cp)

# 2E
mytree4 = rpart(outcome~age+number_animals_in_shelter+animal_type+is_sterilized,control=rpart.control(cp=opt_cp))
rpart.plot(mytree4)


## QUESTION 3
# 3G
df$outcome = factor(df$outcome) 
myforest = randomForest(outcome~age+number_animals_in_shelter+animal_type+is_sterilized+sex, ntree=10000, data=df, importance=TRUE, na.action=na.omit)
myforest

myforest_trace = randomForest(outcome~age+number_animals_in_shelter+animal_type+is_sterilized+sex, ntree=10000, data=df, importance=TRUE, na.action=na.omit, do.trace=1000)
myforest_trace
                                                          
importance(myforest)
varImpPlot(myforest)

                                                          
                                                          
                                                          
                                                          
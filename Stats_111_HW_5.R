#load necessary libraries
install.packages("stringr")
install.packages("ciTools")
library(stringr) # for string manipulation within dataset
library(ciTools) # for GLM confidence interval calculations
#-------------------------------------------------------------------------------
# load given data set into R
student.mat = read.csv("pathname{student-mat.csv}", sep = ";")

# Create bare GLM with only traveltime term and yield p-value to evaluate
# efficiency of a minimal model
model.bare = glm(absences~traveltime, family = poisson, data = student.mat)
summary(model.bare)

# traveltime coefficient only has p = 0.389, so proceed to step() method in 
# order to rule out traveltime as a significant predictor
model.full = glm(absences ~ .-absences, family = poisson, data = student.mat)
summary(model.full)
forward = step(glm(absences ~ 1, family = poisson, data = student.mat), scope = 
                 list(upper=model.full), direction = "forward")
model.max = glm(absences~reason+age+school+Walc+Pstatus+romantic+guardian+
                  internet+sex+studytime+G3+G2+address+Medu+Mjob+famrel+ 
                  schoolsup+Fjob+goout+nursery+paid+health+famsize+freetime+
                  activities, family = poisson, data =student.mat)
summary(model.max)

# Proceed to interaction between sex and traveltime on absences. Construct the
# model given by g(\lambda) = b0 + b1(traveltime) + b2(absences) +
# b3(traveltime * absences)
model.interaction = glm(absences~traveltime + traveltime*sex, family = poisson,
                        data = student.mat)

# yield significance values to evaluate above model
summary(model.interaction)

#-------------------------------------------------------------------------------
# Now for question 2. We are in this question modeling a binary response and
# will therefore be using a t test, a chi squared test, and a logistic GLM.

# Start with the t test to determine if a significant relationship exists
# between absences and desire to attend college.
student.mat.higher = student.mat[student.mat$absences == "0",]$higher
student.mat.higher = ifelse(student.mat.higher == "yes", 1, 0)
t.test(student.mat.higher, mu = 0.5, alternative = "g")

# Now for the Chi Squared Test, to evaluate whether absence level is 
# (in)dependent of desire to attend college.
no.absences.high = as.numeric(sum(str_count(student.mat[student.mat$absences == 
                                                          "0",]$higher, "yes")))
no.absences.nohigh = as.numeric(sum(str_count(student.mat[student.mat$absences==
                                                          "0",]$higher, "no")))
yes.absences = 395 - (no.absences.high + no.absences.nohigh)
yes.absences.high = (as.numeric(sum(str_count(student.mat$higher, "yes")))) -
  no.absences.high
yes.absences.nohigh = (as.numeric(sum(str_count(student.mat$higher, "no")))) -
  no.absences.nohigh
high.vector = c(no.absences.high, yes.absences.high)
nohigh.vector = c(no.absences.nohigh, yes.absences.nohigh)
high.frame = data.frame(high.vector, nohigh.vector)

# rename column and row names for convenience
colnames(high.frame) = c("Yes Higher Ed", "No Higher Ed")
rownames(high.frame) = c("No Absences", "Yes Absences")

# conduct Chi Sq. Test
chisq.test(high.frame)

# proceed to prediction for 17 y.o. female student with no absences and with two
# parents who attended college
# convert "higher" and "sex" variables in student.mat from strings of "yes" or
# "no" to numerics with "yes" = 1, "no" = 0 and "M" = 1, "F" = 0, respectively,
# for ease of integration of information with necessary programs
student.mat$higher = ifelse(student.mat$higher == "yes", 1, 0)
student.mat$sex = ifelse(student.mat$sex == "M", 1, 0)

# create the glm
predict.model = glm(higher~sex+age+absences+Medu+Fedu, family = binomial, data =
                      student.mat)
# evaluate model efficiency via its summary
summary(predict.model)

# In order to evaluate the desire a student with the stated characteristics to 
# pursue higher education, we would like a 95% CI of their probability of 
# desiring so. 
char = t(c(0, 17, 0, 4, 4))
colnames(char) = c("sex", "age", "absences", "Medu", "Fedu")
char = as.data.frame(char)
add_ci(char, predict.model)

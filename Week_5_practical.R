# Question 5

x_vals = seq(8,12,0.1)
probs = dnorm(x_vals, mean = 10, sd = 0.5)

ggplot() + 
  geom_line(aes(x=x_vals, y=probs)) +
  theme(axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10)) + 
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10))

print(sum(probs*0.1))


# Quesion 4 

x_vals = seq(8,12,0.1)
probs_1 = dnorm(x_vals, mean = 10, sd = 0.5)
probs_2 = dnorm(x_vals, mean = 10.2, sd = 0.5)

ggplot()+
  +     geom_line(aes(x=x_vals, y=probs_1), color = "red", size = 2)+
  +     geom_line(aes(x=x_vals, y=probs_2), color = "blue", size = 2)
  

x_sample <- sample(probs_1, size = 40)
y_sample <- sample(probs_2, size = 40)

t.test(x_sample, y_sample)

# Welch Two Sample t-test

data:  x_sample and y_sample
t = 0.00028005, df = 78, p-value = 0.9998
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
  -0.1215072  0.1215414
sample estimates:
  mean of x mean of y 
0.2304378 0.2304207 
 
# t.test is not statistically significant 


# Question 3 
fish_samples= c(0:340)
probs = mapply(dbinom, fish_samples, size+340, prob = 0.43)
sum(probs)


# Question 2

get_all_perms <- function(size){
  rep(list(0:9), size)%>%
    expand.grid() %>%
    nrow()
}

get_all_perms(size = 4)

# Question 1

get_all_perms <- function(x){
  rep(list(0:9), 4)%>%
    expand.grid()%>%
    nrow()
}
get_all_perms()

# Question 0 

list_na <- function(x){
  replace(x, is.na(list_na), mean(list_na, na.rm = TRUE))
}
replace_na_mean(c(1, NA, 2, 3, 2, 2, NA))

}

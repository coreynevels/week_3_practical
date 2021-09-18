# Question 1

x <- c(1,2,3)
sd_x <- function(x) {sqrt(sum((x - mean(x))^2)/(length(x)-1))}

sd_x(x)

# Question 2

coral_pop = c("Montipora capitata",
              "Porites compressa",
              "Porites lobata")

coral <- sample(coral_pop, size = 100, replace = TRUE)

coral_health_logic = c("TRUE", "FALSE")

health_status = sample(coral_health_logic, size = 100, replace = TRUE, prob = c(0.8,0.2))

coral_health <- data.frame(coral, health_status)

coral_health_tbl <- as_tibble(coral_health)

print(coral_health_tbl)

# Question 3

return_cover <-function(x){if(x==TRUE){
  (rnorm(n=1,mean=9,sd=2))
} else if(x==FALSE) {
  (rnorm(n=1,mean=1,sd=3))
} else stop("error")}


coral_cover <- mapply(return_cover, coral_health_tbl$health_status)

coral_health_tbl_2 <- add_column(coral_health_tbl,coral_cover)
coral_health_tbl_2

# Question 4

coral_health_tbl$binary_health_status = as.integer(as.factor(coral_health_tbl$health_status))-1

coral_health_tbl

# Question 5
coral_health_tbl_2$health_status = as.factor(coral_health_tbl_2$health_status)
ggplot(coral_health_tbl_2, aes(x=coral_cover, fill= health_status))+scale_fill_manual(values=c("grey","white"))+ geom_histogram(aes(y=..count../sum(..count..)), binwidth= 1)

#Question 6

ggplot(coral_health_tbl_2, aes(x=coral_cover, fill=coral))+scale_fill_manual(values = c("cyan1","chartreuse1", "goldenrod1"))+
  geom_density(aes(y=..count../sum(..count..)),adjust=2, colour = "black")+xlim(c(-15,20))






library(dplyr) 
library(ggplot2)

my_function_name <- function(my_args) {
  # code to run
  # return (some value)
}
#actvity 1----
add_one <- function(input_data) {
  return(input_data + 1)}
number_series <- c(1,5,10)
add_one(number_series)

#activity 2----
# Use the instructions above to complete the function below
variance <- function(input_data){
  square <- sqrt(input_data)
  ss <- sum(square)
  variance <- (ss/length(input_data-1))
  return(variance)
}

# Use vector (or make a new one) and try out your new function

number_series <- c(1,5,10)
variance(number_series)

say_hello <- function(){
  paste("Hello World") 
}

say_hello()

#activity 3----
say_morning <- function(x){
  paste("Good morning", x)
}

#  what about this one?
say_morning("Phil")

say_morning_default <- function(name = "you"){
  paste("Good morning", name)
}

say_morning_default()

report_p <- function(p, digits = 3) 
  {roundp <- round(p, digits)
  reported <- if_else(p< 0.001, "p< 0.001",
                      paste("p =", roundp))
  return(reported)}

report_p <- function(p, digits = 3) {
  
  if (!is.numeric(p)) stop("p must be a number")
  if (p <= 0) warning("p-values cannot less 0")
  if (p >= 1) warning("p-values cannot be greater than 1")
  
  reported <- if_else(p < 0.001,
                      "p < 0.001",
                      paste("p=", round(p, digits)))
  return(reported)
}
report_p(0.5)

function(input_data) {
  return(input_data + 1)
  
#activity 3----
  # Make some fake data into a tibble
  
  vial <- (c((1:10),(1:10)))
  sex <- (c(rep("male",10),rep("female", 10)))
  weight_mg <- c(rnorm(10, mean=0.2, sd=0.02), rnorm(10, mean=0.21, sd=0.01))
  
  dros_weight <- tibble(vial, sex, weight_mg)
}

dros_weight %>% 
  filter(sex == "male") %>% 
  arrange(., desc(weight_mg)) %>% 
  head(., n=1)

find_largest_fly <- function(df,  n=1, s=c("male", "female") ){ 
  df %>% 
    filter(sex == s) %>% 
    arrange(., desc(weight_mg)) %>% 
    head(., n=n)
}

#activity 4----

plot <- dros_weight %>% 
  ggplot(aes(x=sex,
             y=weight_mg))+
  geom_jitter(width = 0.1) +
  ggtitle("Comparison of weights (mg) between \nmale and female Drosophila")+
  theme_classic()

# custom theme sets defaults for font and size, but these can be changed without changing the function
theme_custom <- function(base_size=12, base_family="serif"){
  theme_classic(base_size = base_size, 
                base_family = base_family,
  ) +
    # update theme minimal 
    theme(
      # specify default settings for plot titles - use rel to set titles relative to base size
      plot.title=element_text(size=rel(1.5),
                              face="bold",
                              family=base_family),
      #specify defaults for axis titles
      axis.title=element_text(
        size=rel(1),
        family=base_family),
      # specify position for y axis title
      axis.title.y=element_text(margin = margin(r = 10, l= 10)),
      # specify position for x axis title
      axis.title.x = element_text(margin = margin( t = 10, b = 10)),
      # set major y grid lines
      panel.grid.major.y = element_line(colour="gray", size=0.5),
      # add axis lines
      axis.line=element_line(),
      # Adding a 0.5cm margin around the plot
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),    
      # Setting the font for the legend text
      legend.text = element_text(face = "italic"),   
      # Removing the legend title
      legend.title = element_blank(),    
      # Setting the position for the legend - 0 is left/bottom, 1 is top/right
      legend.position = c(0.9, 0.8)             
    )
  
}
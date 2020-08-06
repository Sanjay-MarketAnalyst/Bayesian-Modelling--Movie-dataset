
### Load packages

library(ggplot2)
library(dplyr)
library(statsr)
library(BAS)
library(kableExtra)
library(ggpubr)
library(BAS)
library(tidyr)
library(lubridate)


### Load data

load("C:/Users/sanjay/Desktop/Extras/Bayesian Modelling/movies.RData")
attach(movies)
str(movies)


## Part 1: Data

# Generalizability : 

# 651 randomly sampled movies data from IMDB and Rotten Tomatoes with 32 variables were to be analysed. 
# As it is a random sample, the data thus can be regarded as the population data.

# Causability: 

# No causation can be established as there is no random assignment in the data provided is observed 


## Part 2: Data manipulation

# Step1:

# Well before recreating variables, I here am removing the duplicate data from the set by,


movies_dup <- movies %>% select(title) %>% 
  table() %>%
  data.frame() %>%
  filter(Freq >1)
movies_dup %>% rename(title='.') %>%
  inner_join(movies,by="title") %>%
  select(title, studio, thtr_rel_year) %>%
  arrange(title, thtr_rel_year) %>%
  kable(align = "c", caption ="TABLE2.1 Movies with duplicated title")  %>%
  kable_styling(bootstrap_options = c("striped","hover","condensed"),full_width = TRUE)

# Checking for the duplicate datas by comparing the "title" variable, and thus was able to find out that 8 entries are present with the redundant titles, but since the theatre release year are different for all except for the movie "Man on Wire"(2008)  meaning it to not be considered as a remake. Hence we shall remove that specific entry.


movies <- movies[!duplicated(movies),]
dim(movies)


# The total number of entries has been reduced to 650 from 651.

# Step 2:

# Recreation of Variables to simplify the analysis, in this step I reduced the number of columns of release date from three to one, removed unncessary details from genre variable, title type variable, mpaa rating variable and also have comprehended the informations of best_pic_nom|win|actor_win|dir_win within one factor variable which is oscar_choice.


movies_new<-movies%>%mutate(rel_date = make_date(thtr_rel_year,thtr_rel_month,thtr_rel_day))
movies_new <- movies_new %>% mutate(feature_film = as.factor(ifelse(title_type == "Feature Film", "yes", "no")))
movies_new <- movies_new %>% mutate(mpaa_rating_R = as.factor(ifelse(mpaa_rating == "R", "yes", "no")))
movies_new <- movies_new %>% mutate(oscar_season = as.factor(ifelse(thtr_rel_month %in% 10:12, "yes", "no")))
movies_new <- movies_new %>% mutate(drama = as.factor(ifelse(genre == "Drama", "yes", "no")))
movies_new <- movies_new %>% mutate(summer_season = as.factor(ifelse(thtr_rel_month %in% 5:8, "yes", "no")))
movies_new <- movies_new%>%mutate(oscar_choice=as.factor(ifelse(best_pic_nom=="yes"|best_pic_win=="yes"|best_actor_win=="yes"|best_actress_win=="yes"|best_dir_win=="yes","yes","no")))



# And for the analysis to be not so complicated I selected only those variables that are considered to be necessary, which eventually led the number of variables to reduce from 32 to just 12

# Upon setting up the data frame with only the neccassary variables removal of NA's was performed and the total number of observation was brought down from 650 to 649.
# Therefore, the amount of data to analyse is, 649 obs with 12 Variables.


movies_new<-movies_new %>%
  select(audience_score, feature_film, drama, rel_date, runtime, mpaa_rating_R, oscar_season, summer_season,imdb_rating, imdb_num_votes, critics_score, oscar_choice)
movies_new %>% sapply(is.na) %>% colSums()
movies_new <- movies_new %>% filter(!is.na(runtime))
dim(movies_new)





## Part 3: Exploratory data analysis

# The Distribution of audience score with all the categorical variables to b eanalysed is as follows,


p1<-ggplot(movies_new, 
           aes(x= audience_score, y = rel_date)) +
  geom_point()
p2<-ggplot(movies_new, aes(x = audience_score)) + geom_histogram(aes(fill= mpaa_rating_R),binwidth = 2)+scale_fill_manual(values = c("#C0C0C0", "gold")) + facet_grid(mpaa_rating_R ~.)+
  geom_vline(aes(xintercept=mean(audience_score)), color="blue", linetype="dashed", size=1)
p3<-ggplot(movies_new, aes(x=audience_score, color=oscar_season)) +
  geom_histogram(fill="white", alpha=0.5, position="identity", binwidth = 4)+
  geom_vline(aes(xintercept=mean(audience_score)), color="blue", linetype="dashed", size=1)
p4<-ggplot(movies_new, aes(x=audience_score, color=summer_season)) +
  geom_histogram(fill="white", alpha=0.5, position="identity", binwidth = 4)+
  geom_vline(aes(xintercept=mean(audience_score)), color="blue", linetype="dashed", size=1)
p5 <- ggplot(movies_new, aes(x = oscar_choice, y = audience_score))
bxplotcolr <- c("#FC4E07", "blue")
p6 <- p5 + geom_boxplot(aes(color = oscar_choice)) +
  scale_color_manual(values = bxplotcolr)
figure <- ggarrange(p1,p2,p3,p4,p6,
                    labels = c("A", "B", "C","D","E"),
                    ncol = 2, nrow = 3)
figure


# The Distribution of audience score with numerical variables is as follows, 

b1<-ggplot(data = movies_new, aes(x = imdb_rating, y = audience_score)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm")
b2<-ggplot(movies_new, aes(x = audience_score, y = critics_score))+
  geom_point(aes(color = critics_score), size = 3) + labs(title = "Audience Score vs Critics Score") +
  scale_color_gradientn(colors = c("#00AFBB", "#E7B800", "#FC4E07"))+geom_smooth(method = "lm")
b3<-ggplot(movies_new,aes(runtime,audience_score, color=drama))+geom_quantile(quantiles=0.5)+geom_point(size=2)
figure1 <- ggarrange(b1,b2,b3,
                     labels = c("A", "B", "C"),
                     ncol = 2, nrow = 2)
figure1



# Although the data are not normally distributed, we can clearly see the existence of positive, identical and independent relation of audience score with the rest of explanatory variables and also that the mean of audience score in regards with oscar_season and summer_season are very much closer to each other which are 61.75|63.64 and 62.54|61.83 respectively. Hence, we can proceed with modelling without having to modify any variables.  


## Part 4: Modeling


movie_BayesModel <- bas.lm(audience_score ~ ., data = movies_new, prior = "BIC", modelprior = uniform(), method = "BAS")
summary(movie_BayesModel)


# Looking at the summary we can conclude that the top three models have a probability of around 13% which is a good value due to the presence of 2^12 model combinations for the analysis. And also imdb_rating, critics_score, runtime are established as the most prominent variables for our modelling which we will confirm in the next few steps

image(movie_BayesModel, rotate=F)


# The visualization above depicts the model rank, as mentioned earlier imdb_rating, critics_score, runtime are the variables that are included in most of the models and hence their marginal inclusion probability is expected to be more


coefs1 <- coef(movie_BayesModel, estimator = "BMA")
coefs_bas <- data.frame(parameter = coefs1$namesx, post_mean = coefs1$postmean, post_SD = coefs1$postsd, post_pne0 = coefs1$probne0) %>% arrange(post_pne0) %>% filter(parameter != "Intercept")
coefs_bas$parameter <- factor(coefs_bas$parameter, levels = coefs_bas$parameter[order(coefs_bas$post_pne0, decreasing = TRUE)])
high_pne0 <- data.frame(parameter = coefs_bas$parameter, post_pne0 = coefs_bas$post_pne0) %>% filter(post_pne0 > 0.5)
ggplot(coefs_bas, aes(x = parameter, y = post_pne0)) + 
  geom_pointrange(aes(ymax = post_pne0), ymin = 0) +
  geom_pointrange(data=high_pne0, aes(x = parameter, y = post_pne0, ymax = post_pne0), ymin = 0, color = "red") +
  geom_hline(yintercept = 0.5, color = "red") +
  labs(title = "Posterior Marginal Inclusion Probabilities of Explanatory Variables",x="Explanatory Variable",y = "Marginal Inclusion Probability") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), plot.title = element_text(hjust = 0.5))


# As per the visualization we hereby confirm that except for imdb_rating, critics_score, runtime and oscar_choice no other explanatory variable has any significant on predicting the audience score. 



par(mfrow=c(2,2))
plot(coefficients(movie_BayesModel), subset=c(5,9,11), ask=FALSE)


# Plots of the posterior distributions of runtime, imdb_rating and critics_score averaging over all of the models are plotted and that of the three prominent variables only runtime's coefficient is 0 and the rest two variables are strong explanatory for audience_score. 


# Now moving on to model diagonistics,


plot(movie_BayesModel, which = 1, ask = F)


# Plotting of the residual plot clarifies that the residuals are randomly distributed around zero hence the model seems to have no problem. But to furthur confirm about the residual distribution, I hereby plotted residual vd imdb_rating(the highest posterior marginal inclusive variable of all)


residual_fit <- data.frame(resid = movies_new$audience_score - predict(movie_BayesModel,estimator = "BMA")$fit) 
ggplot(residual_fit, aes(x=movies_new$imdb_rating, y = resid))+ 
  geom_point() + 
  geom_smooth(color="red", lwd = 0.5, se = FALSE, span = 0.5)+
  geom_vline(xintercept = 4.6, color = "blue")+
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Residuals vs IMDB Rating Plot",x="IMDB Rating",y = "Residuals") +
  theme_bw()



# And again the plot seems to be similar to the prior plot, and thus I deduce that the model is clear off ambiguities.


## Part 5: Prediction

# audience_score prediction for the movie "Manchester by the sea"


movie_predictframe1<-movies_new
manchester_by_the_sea<-data.frame(mpaa_rating_R="yes",rel_date="2016-11-14",oscar_season="yes",summer_season="no",
                                  feature_film="yes",drama="yes",runtime=137, audience_score=100,
                                  imdb_rating=7.8,imdb_num_votes=237196,critics_score=96,oscar_choice="yes")
movie_predictframe1<-rbind(movie_predictframe1,manchester_by_the_sea)
manchester_by_the_sea<-tail(movie_predictframe1,1)
manchester_by_the_sea_bayes<-predict(movie_BayesModel, manchester_by_the_sea, estimator="BMA", top = 15, se.fit = TRUE)
manchester_by_the_sea_bayes$fit
ci<-confint(manchester_by_the_sea_bayes, estimator="BMA")
ci


# Rotten tomatoes audience score is 80, and with the information available the model designed has predicted 83 containing within the confidence interval limit.


## Part 6: Conclusion

# Although the model might not have predicted the exact value, it is due to insufficient information. But still the model was able to predict the confidence interval exactly. Provided adequate information the model can be updated and put to use efficiently. 
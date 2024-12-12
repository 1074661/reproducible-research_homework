# Reproducible research: version control and R

## Questions 1, 2 and 3

Questions 1, 2 and 3 were answered in the README.md file of the logistic_growth repo forked during the practical. The link to these answers is shown here: https://github.com/1074661/logistic_growth. 

## Question 4)

i) Executing the provided code gives two side-by-side random walk plots that are the result of two independent iterations of the function 'random_walk'. Each plot shows a unique trajectory of 500 steps even though each plot was produced by running the same function (random_walk) for 500 steps. Time is shown by a gradient colour scheme (ranging from dark blue to light blue), allowing visualisation of how random walks changed temporally; earlier steps correspond with darker blue and later steps correspond with light blue. While each step on the random walk has a set distance (0.25 units), a random angle between 0 and 2pi is generated for each step to produce a random path over the 500 iterations. The combination of angle and step length information is used to compute (x,y) coordinates after each step. While both paths begin from the origin (0,0), the random nature of the walk results in the following of distinct trajectories. Production of a unique random_walk plot each time the random_walk function is iterated during code re-running (due to path reliance on random generation of an angle for each step) means that this simulation lacks reproducibility.

ADD IMAGE of the output of random_walk function after two independent iterations to the README.md of the fork.

#### Output (in form of random walk trajectories) of random_walk function after two independent iterations 
![image](https://github.com/user-attachments/assets/0321d0c1-15ad-4cca-a6b2-f720e49fbe95)

ii) A random seed is an initial number used as a fixed starting point to initialise a pseudo-random number generator (PRNG) in R. Use of a random seed ensures replicability and reproducibility of results of a random process (such as Brownian motion). This is because the random seed allows researchers to reproduce the same sequence of numbers generated by a PRNG in R. 

iii) I edited the script to make a reproducible simulation of Brownian motion. **The edited script can be found in my forked reproducible-research-homework repo**. To provide a reproducible simulation of Brownian motion, I added the set.seed() function, using an arbitrary random seed, inside the random_walk function. 

iv) Code edited to ensure reproducibility.

ADD IMAGE OF LATEST COMMIT HISTORY to the README.md of the fork

#### Commit history

## Question 5)

i) The code below informs that there are 33 rows and 13 columns in the table. 

```{r}
dsDNAviruses_data <- read.csv("Cui_etal2014.csv")
ncol(dsDNAviruses_data)
nrow(dsDNAviruses_data)
```

ii) In order that a linear model (in the form y = a + bx) can be fitted to the data, a log transformation can be applied such that:

$$V = αL^β$$ Is transformed to $$ln(V) = ln(α) + βln(L)$$

```{r}
# Cleaning data and applying transformation to data
dsDNAviruses_data$log_volume <- log(dsDNAviruses_data$Virion.volume..nm.nm.nm.)
dsDNAviruses_data$bases <- (dsDNAviruses_data$Genome.length..kb.)
dsDNAviruses_data$log_bases <- log(dsDNAviruses_data$bases)

# Applying a linear model (in the form y = a + bx) to data
linearmodel <- lm(log_volume ~ log_bases, data = dsDNAviruses_data)
summary(linearmodel)
```

iii) The linear model is $$ln(V) = ln(α) + βln(L)$$

ADD IMAGE OF OUTPUT OF SUMMARY(LINEARMODEL)

#### Output of summary(linearmodel)

Per the summary table, the estimate of the intercept (ln(α)) of the linear model is 7.0748. Therefore, the scaling factor (α) is 1181.81 (since e^7.0748 is 1181.807116). Per the summary table, the P-value for the intercept is 2.28 x 10^-10, which is highly statistically significant (<<< 0.01 significance level threshold). 

Per the summary table, the estimate of the gradient (β) of the linear model is 1.5152. Therefore, the exponent (β) is 1.52. Per the summary table, the P-value for the gradient is 6.44 x 10^-10, which is highly statistically significant (<<< 0.01 significance level threshold). 

In Table 2 from Cui et al. (2014), for the allometric relationship between dsDNA virion volume and dsDNA virion genome length, the allometric exponent is 1.52 and the scaling factor is 1182.

Therefore, my scaling factor is the same as that found by Cui et al. (2014) when rounded to the nearest whole number, and my allometric exponent is the same as that found by Cui et al. (2014).

iv) The code to reproduce the figure is shown below.

```{r}
ggplot(data = dsDNAviruses_data, aes(x=log_bases, y=log_volume)) + 
  geom_point() +
  xlab("log [Genome length (kb)]") + 
  ylab("log [Virion volume (nm3)]") +
  geom_smooth(method = 'lm') +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))
```
ADD IMAGE OF OUTPUT OF CODE

#### Output of code

v) $$V = αL^β$$

V = (1181.807116)*((300)^1.5152) = 6,700,000 nm^3 (rounded to 3 s.f.) 

Therefore, the estimated volume of a 300 kb dsDNA virus is 6,700,000 nm^3.

## Instructions

The homework for this Computer skills practical is divided into 5 questions for a total of 100 points. First, fork this repo and make sure your fork is made **Public** for marking. Answers should be added to the # INSERT ANSWERS HERE # section above in the **README.md** file of your forked repository.

Questions 1, 2 and 3 should be answered in the **README.md** file of the `logistic_growth` repo that you forked during the practical. To answer those questions here, simply include a link to your logistic_growth repo.

**Submission**: Please submit a single **PDF** file with your candidate number (and no other identifying information), and a link to your fork of the `reproducible-research_homework` repo with the completed answers (also make sure that your username has been anonymised). All answers should be on the `main` branch.

## Assignment questions 

1) (**10 points**) Annotate the **README.md** file in your `logistic_growth` repo with more detailed information about the analysis. Add a section on the results and include the estimates for $N_0$, $r$ and $K$ (mention which *.csv file you used).
   
2) (**10 points**) Use your estimates of $N_0$ and $r$ to calculate the population size at $t$ = 4980 min, assuming that the population grows exponentially. How does it compare to the population size predicted under logistic growth? 

3) (**20 points**) Add an R script to your repository that makes a graph comparing the exponential and logistic growth curves (using the same parameter estimates you found). Upload this graph to your repo and include it in the **README.md** file so it can be viewed in the repo homepage.
   
4) (**30 points**) Sometimes we are interested in modelling a process that involves randomness. A good example is Brownian motion. We will explore how to simulate a random process in a way that it is reproducible:

   a) A script for simulating a random_walk is provided in the `question-4-code` folder of this repo. Execute the code to produce the paths of two random walks. What do you observe? (10 points) \
   b) Investigate the term **random seeds**. What is a random seed and how does it work? (5 points) \
   c) Edit the script to make a reproducible simulation of Brownian motion. Commit the file and push it to your forked `reproducible-research_homework` repo. (10 points) \
   d) Go to your commit history and click on the latest commit. Show the edit you made to the code in the comparison view (add this image to the **README.md** of the fork). (5 points) 

5) (**30 points**) In 2014, Cui, Schlub and Holmes published an article in the *Journal of Virology* (doi: https://doi.org/10.1128/jvi.00362-14) showing that the size of viral particles, more specifically their volume, could be predicted from their genome size (length). They found that this relationship can be modelled using an allometric equation of the form **$`V = \alpha L^{\beta}`$**, where $`V`$ is the virion volume in nm<sup>3</sup> and $`L`$ is the genome length in nucleotides.

   a) Import the data for double-stranded DNA (dsDNA) viruses taken from the Supplementary Materials of the original paper into Posit Cloud (the csv file is in the `question-5-data` folder). How many rows and columns does the table have? (3 points)\
   b) What transformation can you use to fit a linear model to the data? Apply the transformation. (3 points) \
   c) Find the exponent ($\beta$) and scaling factor ($\alpha$) of the allometric law for dsDNA viruses and write the p-values from the model you obtained, are they statistically significant? Compare the values you found to those shown in **Table 2** of the paper, did you find the same values? (10 points) \
   d) Write the code to reproduce the figure shown below. (10 points) 

  <p align="center">
     <img src="https://github.com/josegabrielnb/reproducible-research_homework/blob/main/question-5-data/allometric_scaling.png" width="600" height="500">
  </p>

  e) What is the estimated volume of a 300 kb dsDNA virus? (4 points) 

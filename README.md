# Reproducible research: version control and R

## Question 4)

a) Executing the provided code gives two side-by-side random walk plots that are the result of two independent iterations of the function 'random_walk'. Each plot shows a unique trajectory of 500 steps even though each plot was produced by running the same function (random_walk) for 500 steps. Time is shown by a gradient colour scheme (ranging from dark blue to light blue), allowing visualisation of how random walks changed over time; earlier steps correspond with darker blue and later steps correspond with light blue. Each step on the random walk has a set distance (0.25 units); however, a random angle between 0 and 2pi is generated for each step to produce a random path over the 500 iterations. The combination of angle and step length information is used to compute (x,y) coordinates after each step. While both paths begin from the origin (0,0), the random nature of the walk results in following of distinct trajectories. Since the path relies on random generation of an angle for each step, each iteration of the random_walk function produces a different output, leading to a unique random_walk plot each time the code is re-run. This means that the simulation lacks reproducibility.

ADD IMAGE of the output of random_walk function after two independent iterations to the README.md of the fork.

### Output of random_walk function after two independent iterations in the form of random walk trajectories

b) A random seed is an initial number or vector used as a fixed starting point to initialise a pseudo-random number generator (PRNG). Use of a random seed ensures replicability and reproducibility of results of a random process (such as Brownian motion) since the random seed allows researchers to reproduce the same sequence of numbers generated by a PRNG in R. 

c) I edited the script to make a reproducible simulation of Brownian motion and I committed the file and pushed it to my forked reproducible-research-homework repo. To give a reproducible simulation of Brownian motion, I added the set.seed() function to use an arbitrary random seed inside the random_walk function. 

d) ADD IMAGE to the README.md of the fork

# Question 5)

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

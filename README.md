# ADSY Cycle 2 Distances
This repo contains the code I used to create distances ADSY cycle 2. I'll break down the basic code workflow here and then describe the method I'm using here. For each grade = (3, 4, 5) and subject = (glmath, readng), I produce `{grade}_{subject}_distances.rds` in the `distances` directory. These are ISMs from optmatch. The treatment schools are a subset of the treatment schools that exist in the `adsy_model$CAMPUS` column from `adsy_models21_22.Rdata`. Any treatment school that doesn't appear in a given ISM must not have served that grade in 2021-2022.

## Where are the distances?
Originally they were generated in this repo, but i've moved them to `/tea/bernado/cycle2-distances/`

## Workflow
I'll describe what each of the files in the pipeline does. The number here corresponds to the number of the script in scripts.
1. First, I download relevant records from `TX2019_DRV.dta` in `tea/data/current` as well as `TX2021_DRV.dta`, these are all students in a given grade with present outcome values for the supplied subject.
2. Then, I fit linear mixed models predicting test scores from student demographic/attendance variables (and lagged outcome for grades 4 and 5)
3. I use these models to calculate calipers that will be used in the distance procedure
4. I use the fitted models from `3` to predict test scores on students in `2021-2022`
5. Using these predicted scores and the caliper, I calculate a balance measure and an effective sample size measure for each pair of schools.
6. I stitch the parallel-computed distances together, collapse the two measures into a single distance, and save them as an ISM.

All of this is done for each grade, subject pair; The Makefile and Git track versioning of data products and scripts respectively. You can see details for the most recent run of the scripts in the `logs/` directory.

## matchAhead Distances at a glance
The main idea behind the distance procedure is that if we're doing matching in this hierarchical setting where we match schools first, but we're interested in later student-level matches, then we don't want to naively match schools together that look similar in aggregate but differ at the student-level. 

For an simple toy example, imagine a setting where we have one covariate $X_{ij}$ for all students $i$ in school $j$. Then, fix $j$ and let $X_{ij} \sim \mathcal{N}(0, 1)$. Then, fix some $k \neq j$ and imagine $X_{ik}$ is a Gaussian mixture with means -10 and 10 respectively. We expect in this case $mean(X_{\cdot j}) = mean({X}_{\cdot k})$, but clearly schools $j$ and $k$ would form pretty unbalanced groups.  

Current approaches in the literature avoid naive school-level matches by actually just doing the downstream matching for each pair of schools and then selecting the optimal arrangement of schools.  

We replace this expensive min-cost flow calculation for a cheaper max-flow calculation. Instead of producing an actual matching for each pair of schools, we calculate the maximum allowable number of pair matches that could be created for a given treatment and control pair when only allowing within-caliper pairs.

### Full detailing of distance calculation
It's slightly more involved than this glance, I'll describe it here. Imagine that we've fixed treatment school $j$ and control $k$. Now, using the hierarchical models fit, we get average test scores for each school. To form a balance measure, we take the absolute difference between the two schools' fitted average test score.  

Then, the effective sample size measure comes from a flowchart check of the two schools comparability. First, we check how many treatment units have some control unit that are within a caliper distance of at least one control unit (call this $e_1$). If there are some treatment units with no control units within a caliper, we return distance $\frac{N_t}{e_1}$. In this case, this value will always be greater than 1. On the other hand, if all treatment units are within a caliper of at least one control unit, then we run the maxflow calculation above to get the maximum number of pairs able to be matched in this configuration and call this $e_2$. Then, we return the distance $\frac{1}{e_2}$.  

This is how these distances were constructed. If we know ahead of time what the maximum ratio of control to treatment that would be allowed, then we could calculate $e_3$, the maximum number of control values able to be matched in this configuration. And if $e_2 = N_t$, then we can return $\frac{1}{e_3}$. 

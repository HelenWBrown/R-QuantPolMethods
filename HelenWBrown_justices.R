## -------------
## QUESTION 1
## -------------

## calculate the median ideal point for the Court during each term

ideal.med <- tapply(justices$idealpt, justices$term, median)

## create a line plot with term on the horizontal axis (xlab labels this "term") and ideal point on the vertical axis (ylab labels this "ideal point"). 'Main' titles the plot.

plot(names(ideal.med),
     ideal.med,
     main = "The Court's Median Ideal\nPoint During Each Term",
     type = "l",
     xlab = "Term",
     ylab = "Ideal Point")

## Add a dashed horizontal line, coloured dark green, at zero to indicate a “neutral” ideal point
abline(h=0, lty = "dashed", col = "darkgreen")

## Label the dashed line, coloured red, for additional clarity
text(x = 1990, y = -0.1, "Neutral Ideal Point", col = "red")

## -------------
## QUESTION 2
## -------------
## Use length and unique to find the number of unique terms - 64.
justice_term <- length(unique(justices$term))

## create an empty container called term.ideal to be filled during the loop's iterations. The 64 empty spaces represents the 64 unique terms found above.

term.ideal <- rep(NA, 64)

## set up a for loop to find the median justice's name for each term

for (i in 1:64) {
  
  ## subset the justice data by term. I added 1944 to i so that it would equal the term year during each iteration. I found setting up the for loop and subset this way was easier because my container has 64 spaces.
  
  justice.term <- subset(justices, subset = (term == (i+1944)))
  
  ## use question 1's ideal.med data to subset justice.term to find the name of the justice that has the median ideal point for the term
  
  justice.ideal <- subset(justice.term, subset = (idealpt == ideal.med[i]))
  
  ## store the name of the median justice in the container term.ideal
  
  term.ideal[i] <- justice.ideal$justice
}

## I use the table function on term.ideal to visualise how many times each justice's name was found to be the median justice. The sort function then puts the table in ascending order, making it easier for me to identify the justice who was the median justice the most amount of times.

sort(table(term.ideal))
## White was the justice with the medial ideal point in the most terms, 13 times in total.

## medianpt.justice.name <- cbind(term.ideal, ideal.med)

## The table alone shows us the year terms that Justice White served on the Court. The sum then adds these terms up.

sum(table(justices$term[justices$justice == "White"]))
## Justice White  served on the Court for 32 years overall.

## Here, I took the mean of all ideal points where the associated justice was Justice White

mean(justices$idealpt[justices$justice == "White"])
## Justice White's average ideal point over their entire tenure on the Court was 0.4401563.

## I could also the same calculation and get the same result (under True) using tapply

tapply(justices$idealpt, justices$justice == "White", mean)

## -------------
## QUESTION 3
## -------------
## We now turn to the relationship between Supreme Court ideology and the president. Specifically, we want to see how the ideology of the Supreme Court changes over the course of each president’s time in office. Begin by creating two empty ‘container’ vectors: one to hold Democratic presidents, and another for Republican presidents. Label each vector with the presidents’ names.

## I used the code below to determine how large to make the two empty vector. This summed up all of the unique instances of a Democrat president and Republican president, respectively

sum(table(unique(justices$pres[justices$pparty == "D"])))
## [1] 5

sum(table(unique(justices$pres[justices$pparty == "R"])))
## [1] 6

## I then created the empty container vectors for the Democrat and Republican presidents, using the lengths from above.

dem.president <- rep(NA, 5)
gop.president <- rep(NA, 6)

## The names function enabled me to label each vector with a unique presidents' name

names(dem.president) <- unique(justices$pres[justices$pparty == "D"])

names(gop.president) <- unique(justices$pres[justices$pparty == "R"])

## -------------
## QUESTION 4
## -------------
## Next, for each Democratic president, calculate the shift in Supreme Court ideology by subtracting the Court’s median ideal point in the president’s first term from its median ideal point in the president’s last term. Use a loop to store these values in your Democratic container vector. Repeat the same process for Republican presidents.

## You have to loop over Presidents. Within the loop, you are required to calculate the difference between the median ideal points of the last term and the first term. Hint: tapply within the loop might help.

## names of the unique Democratic presidents
dem.names <- unique(justices$pres[justices$pparty == "D"])

## names of the unique Republican presidents
rep.names <- unique(justices$pres[justices$pparty == "R"])

## loop for Democratic presidents

## for each of the unique president names
for(i in dem.names) {
  
## subset justices for the unique name of each president during each iteration
  
## For example dem.names[2] is Kennedy so will subset justices to only include justices during the Kennedy presidency (where pres == "Kennedy")
  
  Dem.data<- subset(justices, pres == i)
  
  ## calculate the median ideal point of each Court term during the presidency
  
  median.admin <- tapply(Dem.data$idealpt, Dem.data$term, median)
  
  ## obtain first and last element of median.admin, representing the median ideal point of the presidency's first and last Court term
  
  element.first <- median.admin[1]
  
  element.last <- median.admin[length(median.admin)]
  
  ## subtract the Court’s median ideal point in the president’s first term (element.first) from its median ideal point in the president’s last term (element.last)
  ## put the result in the container
  
  dem.president[i] <- element.last - element.first
  
}

dem.president

## loop for Republican presidents

for(i in rep.names) {
  
  Rep.data<- subset(justices, pres == i)
  
  r.median.out <- tapply(Rep.data$idealpt, Rep.data$term, median)
  
  ## obtain first and last element of median.admin, representing the median ideal point of the presidency's first and last Court term
  ## last term varies between presidents so use length of r.median.out to signify the last term
  
  first.term <- r.median.out[1]
  
 last.term <- r.median.out[length(r.median.out)]
  
  ## subtract the Court’s median ideal point in the president’s first term (element.first) from its median ideal point in the president’s last term (element.last)
 
  ## put the result in the container
  
  gop.president[i] <- last.term - first.term 
}

gop.president
## -------------
## QUESTION 5
## -------------
## What was the mean and standard deviation of the Supreme Court ideology shifts you just calculated when looking only at the Democratic presidencies? What about the Republican presidencies? Which Republican president’s tenure had the largest conservative (positive) shift on the Court? Which Democratic president’s tenure had the largest liberal (negative) shift?
  
mean(gop.president)
## [1] 0.272

sd(gop.president)
## [1] 0.4403894

mean(dem.president)
## [1] -0.1052

sd(dem.president)
## [1] 0.7384542

sort(table(gop.president))
## President Reagan had the largest conservative (positive) shift on the Court during his tenure, with a 1.01399999298156 difference between the median ideal point of his first and last term.

sort(table(dem.president))
## President Kennedy had the largest liberal (negative) shift on the Court during his tenure, with a -0.837000008672476 difference between the median ideal point of his first and last term.

## -------------
## QUESTION 6
## -------------
## Create a plot that shows the median Supreme Court ideal point over time. Then, for each unique justice add a line (to the same plot) for the ideal points of that justice. The color of each line should be red if the justice was appointed by a Republican and blue if he or she was appointed by a Democrat. (You can assume that when a Justice first appears in the data, they were appointed by the president sitting during that term.) Briefly comment on the resulting plot.

## Creates a plot with the terms on the x-axis, labelled "term", the ideal points on the y-axis, labelled "Ideal Point", with the main title "Median Supreme Court Ideal Point Over Time." Ylim designates the y-axis range, type l designates a line plot.

plot(names(ideal.med),
     ideal.med,
     main = 'Median Supreme Court\nIdeal Point Over Time',
     xlab = 'Term',
     ylab = 'Ideal Point',
     ylim = c(-7,7),
     type = "l")

## Add a dashed horizontal line at zero to indicate a “neutral” ideal point
abline(h=0, lty = "dotted")

## find the number of unique justices
sc.justice <- length(unique(justices$justice))
## 35 unique justices

## make a variable with the unique names of justices
jus.names <- unique(justices$justice)

## create
ideal.justice <- rep(NA, 35)

## For each unique justice add a line (to the same plot) for the ideal points of that justice

for(i in 1:35) {
  
  sc.ideal <- subset(justices, subset = (justice == jus.names[i]))
  
## if the justice's first term has a democratic president give party.col the value of "blue", if not give it the value of "red"
  
  party.col <- ifelse(sc.ideal$pparty[1] == "D", "blue", "red")
  
  ## add a line on the plot, which shows each unique justice's ideal points over time. Col = party.col uses the above variable to colour the line blue if the first term president was a Dem, and red if they were GOP.
  
lines(sc.ideal$term,
        sc.ideal$idealpt,
      col = party.col)
}

## The graph shows how each justice's idea points have changed over time. Negative ideal points indicate liberal preferences. Many of the justices' - both Democrat and Republican appointed - have ideal points that seem to be trending downward over time, indicating that many became more liberal. Justices appointed by Republican presidents, indicated by the red line colour, tend to be hold more Conservative preferences, as shown by their positive ideal points.
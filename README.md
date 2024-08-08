Team106 README

Our Project is divided up into 3 distinct sections, and each one of our members worked on one of those sections.

Section1 - Code1/Results1 - Josh Southwick

The results of this section are discussed in the "Farm System's Almanic" section of our paper. This code is the Team106-code1.R and the CSV of these results are saved in the Team106-results1.csv file (Showcases the number of series each player has played at each level).

Section2 - Code2/Results2 - Douglas Fenwick

The results of this section are discussed in the "Wait, this pitcher is a lefty right?" section of our paper. This code is the Team106-code2.ipynb and the CSV of these results are saved in the Team106-results2.csv file (Showcases pitcher hand data per pitch).

Section3 - Code3/Results3 - Kendall Ruth

The results of this section are discussed in the rest of the paper, and this section is the most complicated. There are three R Files, they are ran in the following order and they each have the following CSV associated with them:
1) Team106-code3-batter_hand.R <br />
   a) Team106-results3-batter_hand1-batter_hits.csv (Showcases the batter's hand for each ball hit)<br />
   b) Team106-results3-batter_hand2-grouped_batter_hits.csv (Showcases the number of ball's hit for each batter seperated by batting hand, and also has the average batter's box location) <br />
2) Team106-code3-ball_release.R <br />
   a) Team106-results3-ball_release1-batter_ball_hits.csv (Similar to 1a, but this includes the hit locations at the first bounce) <br />
   b) Team106-results3-ball_release2-hit_outcomes.csv (Has each batter seperated by batting hand, and calculates how many push/pull hits they have had) <br />
3) Team106-code-batter_pitcher_hits.R <br />
   a) Team106-results3-batter_pitcher_hits1-batter_pitcher_hits.csv (Joins the Team106-results1.csv file to the Team106-results3-ball_release1-batter_ball_hits.csv and allows us to see pitcher hand for each ball hit) <br />
   b) Team106-results3-batter_pitcher_hits2-batter_pitcher_outcomes.csv (Dataframe 3a grouped by batter hand and pitcher hand) <br />
   c) Team106-results3-batter_pitcher_hits3-anova_results.csv (Most significant P-value in the anova results on batter distance) <br />

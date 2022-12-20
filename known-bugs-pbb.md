
## MENUS ##

#### Team Management / Playing Time ####
-can't enter number with decimal
	-uses INPUT command
	-stored to array with % (integer)
-calculated as decimal on the fly

#### User Saved Boxscores ####
- lets us select boxscore but then nothing!!! 
- this is because we're not supporting it
- We need to notify the user!!!

####  View / Print Stat Comparisons ####

#### COMPARE.BAS ####
- code is missing for Send to Printer function
- eval CBasket; how similar to Screen Print?
- based on results, add to PBasket

#### Expanded Leaders --> View Top 10 ####
- issues with printing
- lining up of headers / clear screen timing
- issue is present in original

#### Draft / Trade Players ####
- TRADE.BAS - Overflow Error
- Ony happens if stats don't exist 
- This even if you say stats exist for the team!!!
- There is no check to see if it exists!!!

#### Scheduler ####
-Logic overall isn't solid 
-(ie, global changes doesn't free up after you input your option)

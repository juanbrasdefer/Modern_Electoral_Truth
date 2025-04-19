### attributes_vars_2020 - manual annotation of var types, descriptions, scales -------------------------------
# manually defining 
var_attributes_2020 <- tibble::tibble(
  # 'variable', 'numerical_type', 'category', 'description', 'scale' 
  variable = c(
    "V202014", #V202014 POST: R go to any political meetings, rallies, speeches, dinners, # 1 yes, 2 no
    "V202025", #V202025 POST: Has R in past 12 months: joined a protest march, rally, or demonstration, # 1 yes, 2 no
    "V202406", #V202406 POST: CSES5‐Q01: How interested in politics is R, #1. Very interested, 2. Somewhat interested, 3. Not very interested, 4. Not at all interested
    "V202214", #V202214 POST: [REV] Politics/government too complicated to understand, #1. Always, 2. Most of the time, 3. About half the time, 4. Some of the time, 5. Never
    "V202439", #V202439 POST: CSES5‐Q18: Left‐right‐self, #0. Left, 10. Right
    "V202216", #V202216 POST: Important differences in what major parties stand for, #1. Yes, differences, 2. No, no differences
    "V202431", #V202431 POST: CSES5‐Q14a: 5pt scale: Does it make a difference who is in power, #5-point scale, 1. It doesn’t make any difference 5. It makes a big difference l
    
    "V202138y", #V202138y POST: Office recall: Vice‐President ‐ Mike Pence [coded], #0. Incorrect, 1. Correct
    "V202139y1_V202139y2_summary", # Joint V202139y1 and V202139y2 POST: Office recall: Speaker of the House ‐ Nancy Pelosi,  #0. Incorrect, 1. Correct
    "V202140y1_V202140y2_summary", # Joint V202140y1 and V202140y2 POST: Office recall: German Chancellor ‐ Angela Merkel, #0. Incorrect, 1. Correct
    "V202142y1_V202142y2_summary", # Joint V202142y1 and V202139y2, POST: Office recall: SCOTUS Chief Justice ‐ John Roberts,  #0. Incorrect, 1. Correct
    
    "V202158", #V202158 POST: Feeling Favourability Rating: Dr. Anthony Fauci, 0-100 scale: 0 Terrible, 100 Great,#998. Don’t know, 999. Don’t recognize
    "V202160", #V202160 POST: Feeling Favourability Rating: feminists
    "V202159",#V202159 POST: Feeling Favourability Rating: Christian fundamentalists
    "V202162", #V202162 POST: Feeling Favourability Rating: labor unions
    "V202265", #V202265 POST: Fewer problems if there was more emphasis on traditional family values, #1. Agree strongly, 2. Agree somewhat, 3. Neither agree nor disagree, 4. Disagree somewhat, 5. Disagree strongly
    "V202224", #V202224 POST: How important that more women get elected to political office, #1. Extremely important, 2. Very important, 3. Moderately important, 4. A little important, 5. Not at all important
    
    "V202427", #V202427 POST: CSES5‐Q09: How good/bad a job has government done in last 4 years, #1. Very good job, 2. Good job, 3. Bad job, 4. Very bad job
    "V202430", #V202430 POST: CSES5‐Q11: State of economy better or worse over past 12 months, #1. Gotten much better, 2. Gotten somewhat better, 3. Stayed about the same, 4. Gotten somewhat worse, 5. Gotten much worse
    "V202317", #V202317 POST: How much opportunity in America for average person to get ahead, #1. A great deal, 2. A lot, 3. A moderate amount, 4. A little, 5. None
    "V202271", #V202271 POST: Is the US better or worse than most other countries, #1. Better, 2. Worse, 3. The same
    "V202212", #V202212 POST: [STD] Public officials don't care what people think, #1. Agree strongly, 2. Agree somewhat, 3. Neither agree nor disagree, 4. Disagree somewhat, 5. Disagree strongly
    "V202411", #V202411 POST: CSES5‐Q04c: Attitudes about elites: most politicians are trustworthy, #1. Agree strongly, 2. Agree somewhat, 3. Neither agree nor disagree, 4. Disagree somewhat, 5. Disagree strongly
    "V202304", #V202304 POST: Our political system only works for insiders with money and power, #how well does the statement describe your views #1. Not at all well, 2. Not very well, 3. Somewhat well, 4. Very well, 5. Extremely well
    
    "V202355", #V202355 POST: Does R currently live in a rural or urban area, #1. Rural area, 2. Small town, 3. Suburb, 4. City
    "V202468x",#V202468x PRE‐POST: SUMMARY: Total (family) income
#NEW    
    "V202173", #V202173 POST: Feeling Favourability Rating: scientists
    "V202213", #V202213 POST: [STD] Have no say about what goverment does #1. Agree strongly 2. Agree somewhat 3. Neither agree nor disagree 4. Disagree somewhat 5. Disagree strongly
    "V202253", #V202253 POST: Less government better OR more that government should be doing, #1. The less government the better, 2. More things government should be doing
    "V202259x",#V202259x POST: SUMMARY: Favor/oppose government trying to reduce income inequality 1. Favor a great deal 2. Favor a moderate amount 3. Favor a little 4. Neither favor nor oppose 5. Oppose a little 6. Oppose a moderate amount 7. Oppose a great deal
    "V202260", #V202260 POST: Society should make sure everyone has equal opportunity #1. Agree strongly 2. Agree somewhat 3. Neither agree nor disagree 4. Disagree somewhat 5. Disagree strongly
    "V202264", ##V202264 POST: The world is changing & we should adjust view of moral behavior 1. Agree strongly 2. Agree somewhat 3. Neither agree nor disagree 4. Disagree somewhat 5. Disagree strongly
    "V202290x",#V202290x POST: SUMMARY: Better/worse if man works and woman takes care of home, #1. Much better 2. Somewhat better 3. Slightly better 4. Makes no difference 5. Slightly worse 6. Somewhat worse 7. Much worse
    "V202305", #V202305 POST: Because of rich and powerful it's difficult for the rest to get ahead-- describes your view 1. Not at all well 2. Not very well 3. Somewhat well 4. Very well 5. Extremely well
    "V202308x",#V202308x POST: SUMMARY: Trust ordinary people/experts for public policy, #1. Trust ordinary people much more 2. Trust ordinary people somewhat more 3. Trust both the same 4. Trust experts somwhat more 5. Trust experts much more
    "V202309", #V202309 POST: How much do people need help from experts to understand science #1. Not at all 2. A little 3. A moderate amount 4. A lot 5. A great deal
    "V202320x",#V202320x POST: SUMMARY: Economic mobility, 1. A great deal easier 2. A moderate amount easier 3. A little easier 4. The same 5. A litte harder 6. A moderate amount harder 7. A great deal harder
    "V202332", #V202332 POST: How much is climate change affecting severe weather/temperatures in US, 1. Not at all 2. A little 3. A moderate amount 4. A lot 5. A great deal
    "V202333", #V202333 POST: How important is issue of climate change to R, 1. Not at all important 2. A little important 3. Moderately important 4. Very important 5. Extremely important
    "V202361x",#V202361x POST: SUMMARY: Favor/oppose free trade agreement, 1. Favor a great deal 2. Favor moderately 3. Favor a little 4. Neither favor nor oppose 5. Oppose a little 6. Oppose moderately 7. Oppose a great deal
    "V202400", #V202400 POST: How much is China a threat to the United States, 1. Not at all 2. A little 3. A moderate amount 4. A lot 5. A great deal
    "V202407", #V202407 POST: CSES5‐Q02: How closely does R follow politics in media 1. Very closely 2. Fairly closely 3. Not very closely 4. Not at all
    "V202410", #V202410 POST: CSES5‐Q04b: Attitudes about elites: politicians do not care about people 1. Agree strongly 2. Agree somewhat 3. Neither agree nor disagree  4. Disagree somewhat 5. Disagree strongly
    "V202412", #V202412 POST: CSES5‐Q04d: Attitudes about elites: politicians are main problem in US 1. Agree strongly 2. Agree somewhat 3. Neither agree nor disagree  4. Disagree somewhat 5. Disagree strongly
    "V202413", #V202413 POST: CSES5‐Q04e: Attitudes about elites: strong leader in government is good 1. Agree strongly 2. Agree somewhat 3. Neither agree nor disagree 4. Disagree somewhat 5. Disagree strongly
    "V202414", #V202414 POST: CSES5‐Q04f: Attitudes about elites: people should make policy decisions 1. Agree strongly 2. Agree somewhat 3. Neither agree nor disagree  4. Disagree somewhat 5. Disagree strongly
    "V202424", #V202424 POST: CSES5‐Q06d: National identity: how important to follow America's customs 1. Very important 2. Fairly important 3. Not very important 4. Not important at all
    "V202440", #V202440 POST: CSES5‐Q21: Satisfaction with democratic process 1. Very satisfied 2. Fairly satisfied 4. Not very satisfied 5. Not at all satisfied
#NEW NEW
    "V201115", #V201115 PRE: How hopeful R feels about how things are going in the country
    "V201116", #V201116 PRE: How afraid R feels about how things are going in the country
    "V201117", #V201117 PRE: How outraged R feels about how things are going in the country
    "V201118", #V201118 PRE: How angry R feels about how things are going in the country
    "V201119", #V201119 PRE: How happy R feels about how things are going in the country
    "V201120", #V201120 PRE: How worried R feels about how things are going in the country
    "V201121", #V201121 PRE: How proud R feels about how things are going in the country
    "V201122", #V201122 PRE: How irritated R feels about how things are going in the country
    "V201123", #V201123 PRE: How nervous R feels about how things are going in the country
    
    "V201208", #V201208 PRE: Democratic Presidential candidate trait: strong leadership
    "V201209", #V201209 PRE: Democratic Presidential candidate trait: really cares
    "V201210", #V201210 PRE: Democratic Presidential candidate trait: strong knowledgeable
    "V201211", #V201211 PRE: Democratic Presidential candidate trait: strong honest

    "V201225x",#V201225xPRE: SUMMARY: Voting as duty or choice
    "V201366", #V201366 PRE: How important that news organizations free to criticize
    "V201367", #V201367 PRE: How important branches of government keep one another from too much power 
    "V201368" #V201368 PRE: How important elected officials face serious consequences for misconduct
),
  numerical_type = c(
    "binary", #V202014 POST: R go to any political meetings, rallies, speeches, dinners, # 1 yes, 2 no
    "binary", #V202025 POST: Has R in past 12 months: joined a protest march, rally, or demonstration, # 1 yes, 2 no
    "ordinal", #V202406 POST: CSES5‐Q01: How interested in politics is R, #1. Very interested, 2. Somewhat interested, 3. Not very interested, 4. Not at all interested
    "ordinal", #V202214 POST: [REV] Politics/government too complicated to understand, #1. Always, 2. Most of the time, 3. About half the time, 4. Some of the time, 5. Never
    "ordinal", #V202439 POST: CSES5‐Q18: Left‐right‐self, #0. Left <-> 10. Right
    "binary", #V202216 POST: Important differences in what major parties stand for, #1. Yes, differences, 2. No, no differences
    "ordinal", #V202431 POST: CSES5‐Q14a: 5pt scale: Does it make a difference who is in power, #5-point scale, 1. It doesn’t make any difference 5. It makes a big difference l
    
    "binary", #V202138y POST: Office recall: Vice‐President ‐ Mike Pence [coded], #0. Incorrect, 1. Correct
    "binary", # Joint V202139y1 and V202139y2 POST: Office recall: Speaker of the House ‐ Nancy Pelosi,  #0. Incorrect, 1. Correct
    "binary", # Joint V202140y1 and V202140y2 POST: Office recall: German Chancellor ‐ Angela Merkel, #0. Incorrect, 1. Correct
    "binary", # Joint V202142y1 and V202139y2, POST: Office recall: SCOTUS Chief Justice ‐ John Roberts,  #0. Incorrect, 1. Correct
    
    "continuous", #V202158 POST: Feeling Favourability Rating: Dr. Anthony Fauci, 0-100 scale: 0 Terrible, 100 Great,#998. Don’t know, 999. Don’t recognize
    "continuous", #V202160 POST: Feeling Favourability Rating: feminists
    "continuous",#V202159 POST: Feeling Favourability Rating: Christian fundamentalists
    "continuous", #V202162 POST: Feeling Favourability Rating: labor unions
    "ordinal", #V202265 POST: Fewer problems if there was more emphasis on traditional family values, #1. Agree strongly, 2. Agree somewhat, 3. Neither agree nor disagree, 4. Disagree somewhat, 5. Disagree strongly
    "ordinal", #V202224 POST: How important that more women get elected to political office, #1. Extremely important, 2. Very important, 3. Moderately important, 4. A little important, 5. Not at all important
    
    "ordinal", #V202427 POST: CSES5‐Q09: How good/bad a job has government done in last 4 years, #1. Very good job, 2. Good job, 3. Bad job, 4. Very bad job
    "ordinal", #V202430 POST: CSES5‐Q11: State of economy better or worse over past 12 months, #1. Gotten much better, 2. Gotten somewhat better, 3. Stayed about the same, 4. Gotten somewhat worse, 5. Gotten much worse
    "ordinal", #V202317 POST: How much opportunity in America for average person to get ahead, #1. A great deal, 2. A lot, 3. A moderate amount, 4. A little, 5. None
    "ordinal", #V202271 POST: Is the US better or worse than most other countries, #1. Better, 2. Worse, 3. The same
    "ordinal", #V202212 POST: [STD] Public officials don't care what people think, #1. Agree strongly, 2. Agree somewhat, 3. Neither agree nor disagree, 4. Disagree somewhat, 5. Disagree strongly
    "ordinal", #V202411 POST: CSES5‐Q04c: Attitudes about elites: most politicians are trustworthy, #1. Agree strongly, 2. Agree somewhat, 3. Neither agree nor disagree, 4. Disagree somewhat, 5. Disagree strongly
    "ordinal", #V202304 POST: Our political system only works for insiders with money and power, #how well does the statement describe your views #1. Not at all well, 2. Not very well, 3. Somewhat well, 4. Very well, 5. Extremely well
    
    "ordinal", #V202355 POST: Does R currently live in a rural or urban area, #1. Rural area, 2. Small town, 3. Suburb, 4. City
    "continuous", #V202468x PRE‐POST: SUMMARY: Total (family) income, #1. <-> 21.
# NEW    
    "continuous", #V202173 POST: Feeling Favourability Rating: scientists
    "ordinal", #V202213 POST: [STD] Have no say about what goverment does #1. Agree strongly 2. Agree somewhat 3. Neither agree nor disagree 4. Disagree somewhat 5. Disagree strongly
    "binary", #V202253 POST: Less government better OR more that government should be doing, #1. The less government the better, 2. More things government should be doing
    "ordinal",#V202259x POST: SUMMARY: Favor/oppose government trying to reduce income inequality 1. Favor a great deal 2. Favor a moderate amount 3. Favor a little 4. Neither favor nor oppose 5. Oppose a little 6. Oppose a moderate amount 7. Oppose a great deal
    "ordinal", #V202260 POST: Society should make sure everyone has equal opportunity #1. Agree strongly 2. Agree somewhat 3. Neither agree nor disagree 4. Disagree somewhat 5. Disagree strongly
    "ordinal", #V202264 POST: The world is changing & we should adjust view of moral behavior 1. Agree strongly 2. Agree somewhat 3. Neither agree nor disagree 4. Disagree somewhat 5. Disagree strongly
    "ordinal",#V202290x POST: SUMMARY: Better/worse if man works and woman takes care of home, #1. Much better 2. Somewhat better 3. Slightly better 4. Makes no difference 5. Slightly worse 6. Somewhat worse 7. Much worse
    "ordinal", #V202305 POST: Because of rich and powerful it's difficult for the rest to get ahead-- describes your view 1. Not at all well 2. Not very well 3. Somewhat well 4. Very well 5. Extremely well
    "ordinal",#V202308x POST: SUMMARY: Trust ordinary people/experts for public policy, #1. Trust ordinary people much more 2. Trust ordinary people somewhat more 3. Trust both the same 4. Trust experts somwhat more 5. Trust experts much more
    "ordinal", #V202309 POST: How much do people need help from experts to understand science #1. Not at all 2. A little 3. A moderate amount 4. A lot 5. A great deal
    "ordinal",#V202320x POST: SUMMARY: Economic mobility, 1. A great deal easier 2. A moderate amount easier 3. A little easier 4. The same 5. A litte harder 6. A moderate amount harder 7. A great deal harder
    "ordinal", #V202332 POST: How much is climate change affecting severe weather/temperatures in US, 1. Not at all 2. A little 3. A moderate amount 4. A lot 5. A great deal
    "ordinal", #V202333 POST: How important is issue of climate change to R, 1. Not at all important 2. A little important 3. Moderately important 4. Very important 5. Extremely important
    "ordinal",#V202361x POST: SUMMARY: Favor/oppose free trade agreement, 1. Favor a great deal 2. Favor moderately 3. Favor a little 4. Neither favor nor oppose 5. Oppose a little 6. Oppose moderately 7. Oppose a great deal
    "ordinal", #V202400 POST: How much is China a threat to the United States, 1. Not at all 2. A little 3. A moderate amount 4. A lot 5. A great deal
    "ordinal", #V202407 POST: CSES5‐Q02: How closely does R follow politics in media 1. Very closely 2. Fairly closely 3. Not very closely 4. Not at all
    "ordinal", #V202410 POST: CSES5‐Q04b: Attitudes about elites: politicians do not care about people 1. Agree strongly 2. Agree somewhat 3. Neither agree nor disagree  4. Disagree somewhat 5. Disagree strongly
    "ordinal", #V202412 POST: CSES5‐Q04d: Attitudes about elites: politicians are main problem in US 1. Agree strongly 2. Agree somewhat 3. Neither agree nor disagree  4. Disagree somewhat 5. Disagree strongly
    "ordinal", #V202413 POST: CSES5‐Q04e: Attitudes about elites: strong leader in government is good 1. Agree strongly 2. Agree somewhat 3. Neither agree nor disagree 4. Disagree somewhat 5. Disagree strongly
    "ordinal", #V202414 POST: CSES5‐Q04f: Attitudes about elites: people should make policy decisions 1. Agree strongly 2. Agree somewhat 3. Neither agree nor disagree  4. Disagree somewhat 5. Disagree strongly
    "ordinal", #V202424 POST: CSES5‐Q06d: National identity: how important to follow America's customs 1. Very important 2. Fairly important 3. Not very important 4. Not important at all
    "ordinal", #V202440 POST: CSES5‐Q21: Satisfaction with democratic process 1. Very satisfied 2. Fairly satisfied 4. Not very satisfied 5. Not at all satisfied
# NEW NEW
    "ordinal", #V201115 PRE: How hopeful R feels about how things are going in the country 1. Not at all 2. A little 3. Somewhat 4. Very 5. Extremely
    "ordinal", #V201116 PRE: How afraid R feels about how things are going in the country 1. Not at all 2. A little 3. Somewhat 4. Very 5. Extremely
    "ordinal", #V201117 PRE: How outraged R feels about how things are going in the country 1. Not at all 2. A little 3. Somewhat 4. Very 5. Extremely
    "ordinal", #V201118 PRE: How angry R feels about how things are going in the country 1. Not at all 2. A little 3. Somewhat 4. Very 5. Extremely
    "ordinal", #V201119 PRE: How happy R feels about how things are going in the country 1. Not at all 2. A little 3. Somewhat 4. Very 5. Extremely
    "ordinal", #V201120 PRE: How worried R feels about how things are going in the country 1. Not at all 2. A little 3. Somewhat 4. Very 5. Extremely
    "ordinal", #V201121 PRE: How proud R feels about how things are going in the country 1. Not at all 2. A little 3. Somewhat 4. Very 5. Extremely
    "ordinal", #V201122 PRE: How irritated R feels about how things are going in the country 1. Not at all 2. A little 3. Somewhat 4. Very 5. Extremely
    "ordinal", #V201123 PRE: How nervous R feels about how things are going in the country 1. Not at all 2. A little 3. Somewhat 4. Very 5. Extremely
    
    "ordinal", #V201208 PRE: Democratic Presidential candidate trait: strong leadership 1. Extremely well 2. Very well 3. Moderately well 4. Slightly well 5. Not well at all
    "ordinal", #V201209 PRE: Democratic Presidential candidate trait: really cares 1. Extremely well 2. Very well 3. Moderately well 4. Slightly well 5. Not well at all
    "ordinal", #V201210 PRE: Democratic Presidential candidate trait: strong knowledgeable 1. Extremely well 2. Very well 3. Moderately well 4. Slightly well 5. Not well at all
    "ordinal", #V201211 PRE: Democratic Presidential candidate trait: strong honest 1. Extremely well 2. Very well 3. Moderately well 4. Slightly well 5. Not well at all
    
    "ordinal",#V201225xPRE: SUMMARY: Voting as duty or choice 1. Very strongly a duty 2. Moderately strongly a duty 3. A little strongly a duty 4. Neither a duty nor a choice 5. A little strongly a choice 6. Moderately strongly a choice 7. Very strongly a choice
    "ordinal", #V201366 PRE: How important that news organizations free to criticize 1. Not important at all 2. A little important 3. Moderately important 4. Very important 5. Extremely important
    "ordinal", #V201367 PRE: How important branches of government keep one another from too much power 1. Not important at all 2. A little important 3. Moderately important 4. Very important 5. Extremely important
    "ordinal" #V201368 PRE: How important elected officials face serious consequences for misconduct 1. Not important at all 2. A little important 3. Moderately important 4. Very important 5. Extremely important
    ),
  category = c(
    rep("Political Involvement/ View of Politics", 7),
    rep("Information Level", 4),
    rep("Beliefs", 6),
    rep("Review of State of Union/ Government's Job", 7),
    rep("Demographic", 2),
    rep("New Variables", 22),
    rep("New New Variables", 17)
  ),
  description = c(
      "Attended political events", 
      "Joined protest/demonstration",
      "Interest in politics", 
      "Politics too complicated", 
      "Left-Right self-placement",
      "Differences in Major Parties", 
      "Matters who is in power",
      "Recall: VP Pence", 
      "Recall: Speaker Pelosi", 
      "Recall: Chancellor Merkel", 
      "Recall: SCOTUS Chief Roberts",
      "Favourability Rating: Fauci", 
      "Favourability Rating: Feminists", 
      "Favourability Rating: Christian Fundamentalists", 
      "Favourability Rating: Labor Unions",
      "Fewer problems if more Traditional Family Values", 
      "Importance of more Women in public office",
      "Government Performance - 4yrs", 
      "State of Economy - 12months", 
      "Opportunity in America", 
      "United States vs other countries", 
      "Public Officials don't care what people think", 
      "Most Politicians Trustworthy", 
      "System only works for pol and financial insiders",
      "Self: Rural vs. Urban location", 
      "Family income",
      "Favourability Rating: Scientists", 
      "Have no say in government decisions", 
      "Government: more or less better", 
      "Government should do more to reduce inequality", 
      "Society should ensure Equal Opportunity", 
      "As world changes, morals should change", 
      "Better/worse if Man works and woman at home", 
      "Because of rich and powerful, hard to get ahead",
      "Trust people vs experts for public policy", 
      "Need experts to understand science", "Econ. mobility",
      "Climate: impact level on United States", 
      "Climate: issue importance", 
      "Free trade stance", 
      "China threat level to United States",
      "How closely follow politics in media", 
      "Politicians don't care about people", 
      "Politicians main problem in United States",
      "Strong leader in government is good", 
      "People should make policy decisions", 
      "Important to follow American customs", 
      "Satisfaction with how Democracy works",
      
      "How Hopeful about how things are going in US",
      "How Afraid about how things are going in US",
      "How Outraged about how things are going in US",
      "How Angry about how things are going in US",
      "How Happy about how things are going in US",
      "How Worried about how things are going in US",
      "How Proud about how things are going in US",
      "How Irritated about how things are going in US",
      "How Nervous about how things are going in US",
      
      "Dem. Candidate Trait: Strong Leadership",
      "Dem. Candidate Trait: Really Cares",
      "Dem. Candidate Trait: Knowledgeable",
      "Dem. Candidate Trait: Honest",
      
      "Is Voting a Duty or a Choice",
      "Importance: News Orgs. be free to criticize",
      "Importance: Branches of Govt check one another",
      "Importance: Elected Officials consequences if misconduct"

  ),
  description_original = c(
    "POST: R go to any political meetings, rallies, speeches, dinners",
    "POST: Has R in past 12 months: joined a protest march, rally, or demonstration",
    "POST: CSES5‐Q01: How interested in politics is R",
    "POST: [REV] Politics/government too complicated to understand",
    "POST: CSES5‐Q18: Left‐right‐self",
    "POST: Important differences in what major parties stand for",
    "POST: CSES5‐Q14a: 5pt scale: Does it make a difference who is in power",
    "POST: Office recall: Vice‐President ‐ Mike Pence [coded]",
    "POST: Office recall: Speaker of the House ‐ Nancy Pelosi",
    "POST: Office recall: German Chancellor ‐ Angela Merkel",
    "POST: Office recall: SCOTUS Chief Justice ‐ John Roberts",
    "POST: Feeling Favourability Rating: Dr. Anthony Fauci",
    "POST: Feeling Favourability Rating: feminists",
    "POST: Feeling Favourability Rating: Christian fundamentalists",
    "POST: Feeling Favourability Rating: labor unions",
    "POST: Fewer problems if there was more emphasis on traditional family values",
    "POST: How important that more women get elected to political office",
    "POST: CSES5‐Q09: How good/bad a job has government done in last 4 years",
    "POST: CSES5‐Q11: State of economy better or worse over past 12 months",
    "POST: How much opportunity in America for average person to get ahead",
    "POST: Is the US better or worse than most other countries",
    "POST: [STD] Public officials don't care what people think",
    "POST: CSES5‐Q04c: Attitudes about elites: most politicians are trustworthy",
    "POST: Our political system only works for insiders with money and power",
    "POST: Does R currently live in a rural or urban area",
    "PRE‐POST: SUMMARY: Total (family) income",
    "POST: Feeling Favourability Rating: scientists",
    "POST: [STD] Have no say about what goverment does",
    "POST: Less government better OR more that government should be doing",
    "POST: SUMMARY: Favor/oppose government trying to reduce income inequality",
    "POST: Society should make sure everyone has equal opportunity",
    "POST: The world is changing & we should adjust view of moral behavior",
    "POST: SUMMARY: Better/worse if man works and woman takes care of home",
    "POST: Because of rich and powerful it's difficult for the rest to get ahead-- describes your view",
    "POST: SUMMARY: Trust ordinary people/experts for public policy",
    "POST: How much do people need help from experts to understand science",
    "POST: SUMMARY: Economic mobility",
    "POST: How much is climate change affecting severe weather/temperatures in US",
    "POST: How important is issue of climate change to R",
    "POST: SUMMARY: Favor/oppose free trade agreement",
    "POST: How much is China a threat to the United States",
    "POST: CSES5‐Q02: How closely does R follow politics in media",
    "POST: CSES5‐Q04b: Attitudes about elites: politicians do not care about people",
    "POST: CSES5‐Q04d: Attitudes about elites: politicians are main problem in US",
    "POST: CSES5‐Q04e: Attitudes about elites: strong leader in government is good",
    "POST: CSES5‐Q04f: Attitudes about elites: people should make policy decisions",
    "POST: CSES5‐Q06d: National identity: how important to follow America's customs",
    "POST: CSES5‐Q21: Satisfaction with democratic process",
    
    "PRE: How hopeful R feels about how things are going in the country",
    "PRE: How afraid R feels about how things are going in the country",
    "PRE: How outraged R feels about how things are going in the country",
    "PRE: How angry R feels about how things are going in the country",
    "PRE: How happy R feels about how things are going in the country",
    "PRE: How worried R feels about how things are going in the country",
    "PRE: How proud R feels about how things are going in the country",
    "PRE: How irritated R feels about how things are going in the country",
    "PRE: How nervous R feels about how things are going in the country",
    
    "PRE: Democratic Presidential candidate trait: strong leadership",
    "PRE: Democratic Presidential candidate trait: really cares",
    "PRE: Democratic Presidential candidate trait: knowledgeable",
    "PRE: Democratic Presidential candidate trait: honest",
    
    "PRE: SUMMARY: Voting as duty or choice",
    "PRE: How important that news organizations free to criticize",
    "PRE: How important branches of government keep one another from too much power",
    "PRE: How important elected officials face serious consequences for misconduct"
  ),
  scale = c(
    "1 yes, 2 no",
    "1 yes, 2 no",
    "1. Very interested, 2. Somewhat interested, 3. Not very interested, 4. Not at all interested",
    "1. Always, 2. Most of the time, 3. About half the time, 4. Some of the time, 5. Never",
    "0. Left <-> 10. Right",
    "1. Yes, differences, 2. No, no differences",
    "5-point scale, 1. It doesn’t make any difference 5. It makes a big difference",
    "0. Incorrect, 1. Correct",
    "0. Incorrect, 1. Correct",
    "0. Incorrect, 1. Correct",
    "0. Incorrect, 1. Correct",
    "0-100 scale: 0 Terrible, 100 Great",
    "0-100 scale: 0 Terrible, 100 Great",
    "0-100 scale: 0 Terrible, 100 Great",
    "0-100 scale: 0 Terrible, 100 Great",
    "1. Agree strongly, 2. Agree somewhat, 3. Neither agree nor disagree, 4. Disagree somewhat, 5. Disagree strongly",
    "1. Extremely important, 2. Very important, 3. Moderately important, 4. A little important, 5. Not at all important",
    "1. Very good job, 2. Good job, 3. Bad job, 4. Very bad job",
    "1. Gotten much better, 2. Gotten somewhat better, 3. Stayed about the same, 4. Gotten somewhat worse, 5. Gotten much worse",
    "1. A great deal, 2. A lot, 3. A moderate amount, 4. A little, 5. None",
    "1. Better, 2. Worse, 3. The same",
    "1. Agree strongly, 2. Agree somewhat, 3. Neither agree nor disagree, 4. Disagree somewhat, 5. Disagree strongly",
    "1. Agree strongly, 2. Agree somewhat, 3. Neither agree nor disagree, 4. Disagree somewhat, 5. Disagree strongly",
    "1. Not at all well, 2. Not very well, 3. Somewhat well, 4. Very well, 5. Extremely well",
    "1. Rural area, 2. Small town, 3. Suburb, 4. City",
    "1. <-> 22.: 1. Under $9,999, 22. $250,000+",
    "0-100 scale: 0 Terrible, 100 Great",
    "1. Agree strongly, 2. Agree somewhat, 3. Neither agree nor disagree, 4. Disagree somewhat, 5. Disagree strongly",
    "1. The less government the better, 2. More things government should be doing",
    "1. Favor a great deal 2. Favor a moderate amount 3. Favor a little 4. Neither favor nor oppose 5. Oppose a little 6. Oppose a moderate amount 7. Oppose a great deal",
    "1. Agree strongly, 2. Agree somewhat, 3. Neither agree nor disagree, 4. Disagree somewhat, 5. Disagree strongly",
    "1. Agree strongly, 2. Agree somewhat, 3. Neither agree nor disagree, 4. Disagree somewhat, 5. Disagree strongly",
    "1. Much better 2. Somewhat better 3. Slightly better 4. Makes no difference 5. Slightly worse 6. Somewhat worse 7. Much worse",
    "1. Not at all well 2. Not very well 3. Somewhat well 4. Very well 5. Extremely well",
    "1. Trust ordinary people much more 2. Trust ordinary people somewhat more 3. Trust both the same 4. Trust experts somwhat more 5. Trust experts much more",
    "1. Not at all 2. A little 3. A moderate amount 4. A lot 5. A great deal",
    "1. A great deal easier 2. A moderate amount easier 3. A little easier 4. The same 5. A litte harder 6. A moderate amount harder 7. A great deal harder",
    "1. Not at all 2. A little 3. A moderate amount 4. A lot 5. A great deal",
    "1. Not at all important 2. A little important 3. Moderately important 4. Very important 5. Extremely important",
    "1. Favor a great deal 2. Favor moderately 3. Favor a little 4. Neither favor nor oppose 5. Oppose a little 6. Oppose moderately 7. Oppose a great deal",
    "1. Not at all 2. A little 3. A moderate amount 4. A lot 5. A great deal",
    "1. Very closely 2. Fairly closely 3. Not very closely 4. Not at all",
    "1. Agree strongly, 2. Agree somewhat, 3. Neither agree nor disagree, 4. Disagree somewhat, 5. Disagree strongly",
    "1. Agree strongly, 2. Agree somewhat, 3. Neither agree nor disagree, 4. Disagree somewhat, 5. Disagree strongly",
    "1. Agree strongly, 2. Agree somewhat, 3. Neither agree nor disagree, 4. Disagree somewhat, 5. Disagree strongly",
    "1. Agree strongly, 2. Agree somewhat, 3. Neither agree nor disagree, 4. Disagree somewhat, 5. Disagree strongly",
    "1. Very important 2. Fairly important 3. Not very important 4. Not important at all",
    "1. Very satisfied 2. Fairly satisfied 4. Not very satisfied 5. Not at all satisfied",
    
    "1. Not at all 2. A little 3. Somewhat 4. Very 5. Extremely",
    "1. Not at all 2. A little 3. Somewhat 4. Very 5. Extremely",
    "1. Not at all 2. A little 3. Somewhat 4. Very 5. Extremely",
    "1. Not at all 2. A little 3. Somewhat 4. Very 5. Extremely",
    "1. Not at all 2. A little 3. Somewhat 4. Very 5. Extremely",
    "1. Not at all 2. A little 3. Somewhat 4. Very 5. Extremely",
    "1. Not at all 2. A little 3. Somewhat 4. Very 5. Extremely",
    "1. Not at all 2. A little 3. Somewhat 4. Very 5. Extremely",
    "1. Not at all 2. A little 3. Somewhat 4. Very 5. Extremely",
    
    " 1. Extremely well 2. Very well 3. Moderately well 4. Slightly well 5. Not well at all",
    " 1. Extremely well 2. Very well 3. Moderately well 4. Slightly well 5. Not well at all",
    " 1. Extremely well 2. Very well 3. Moderately well 4. Slightly well 5. Not well at all",
    " 1. Extremely well 2. Very well 3. Moderately well 4. Slightly well 5. Not well at all",
    
    "1. Very strongly a duty 2. Moderately strongly a duty 3. A little strongly a duty 4. Neither a duty nor a choice 5. A little strongly a choice 6. Moderately strongly a choice 7. Very strongly a choice",
    "1. Not important at all 2. A little important 3. Moderately important 4. Very important 5. Extremely important",
    "1. Not important at all 2. A little important 3. Moderately important 4. Very important 5. Extremely important",
    "1. Not important at all 2. A little important 3. Moderately important 4. Very important 5. Extremely important"
    )
)
# so it begins

# libraries, directory ------------------------------------------------------------
library(tidyverse)
library(here)

here::i_am("code/exploring_ANES.R")


# load data -----------------------------------------------------------------------

ANES_2020_raw <- read_csv(here("data/ANES/2020/anes_timeseries_2020_csv_20220210/anes_timeseries_2020_csv_20220210.csv"))


ANES_2016_raw <- read.delim(here("data/ANES/2016/anes_timeseries_2016/anes_timeseries_2016_rawdata.txt"),
                            sep = "|", header = TRUE)  #a tab-separated

ANES_2012_raw <- read.delim(here("data/ANES/2012/anes_timeseries_2012/anes_timeseries_2012_rawdata.txt"), 
                            sep = "|", header = TRUE)  # tab-separated

ANES_2008_raw <- read.delim(here("data/ANES/2008/anes_timeseries_2008/anes_timeseries_2008_rawdata.txt"), 
                            sep = "|", header = TRUE)  # tab-separated

ANES_2004_raw <- read.delim(here("data/ANES/2004/anes2004/nes04dat.txt"), 
                            sep = ",", header = TRUE)  # comma separated

ANES_2000_raw <- read.delim(here("data/ANES/2000/anes_2000prepost/anes_2000prepost_dat.txt"), 
                            sep = ",", header = TRUE)  # comma separated







# Experiment: Voted for Change in Pres, 2020  --------------------------------------------

change_cand_20 <- 1 # Joe Biden is coded as 1 in this dataset
# dataset reduction (filtering)
voted_change_20 <- ANES_2020_raw %>%
  filter(V202072 %in% c(1,2)) %>%           # V202072 - POST: Did R vote for President
                                            # Filters out: refused q, didnt do post vote survey...
                                            # Filter: 8280->6029
  filter(V202072 == 1) %>%                  # Filter: voted for president, 6029->5952
  filter(V202073 == change_cand_20)         # V202073 - POST: For whom did R vote for President
                                            # Filter: Voted for change (Biden) 5952->3267 is 55% voted Biden


# need to figure out what variables are interesting for clustering these people
  # State? not too helpful... unless they are all in swing states...
  #V202014 POST: R go to any political meetings, rallies, speeches, dinners
  #V202025 POST: Has R in past 12 months: joined a protest march, rally, or demonstration
  #V202074 POST: Preference strong for Presidential candidate for whom R vote

  #V202138y POST: Office recall: Vice‐President ‐ Mike Pence [coded]
  #V202139y1 POST: Office recall: Speaker of the House ‐ Nancy Pelosi [coded/scheme 1]
  #V202139y2 POST: Office recall: Speaker of the House ‐ Nancy Pelosi [coded/scheme 2]
  #V202158 POST: Feeling thermometer: Dr. Anthony Fauci
  #V202160 POST: Feeling thermometer: feminists
  #V202162 POST: Feeling thermometer: labor unions
  #V202163 POST: Feeling thermometer: big business
  #V202168 POST: Feeling thermometer: Muslims
  #V202177 POST: Feeling thermometer: United Nations (UN)
      #V202205 POST: MOST IMPORTANT PROBLEMS FACING THE COUNTRY - MENTION 1 [TEXT]
      # and its derivatives sound nice on paper but the responses are a mess. impossible for use
  #V202212 POST: [STD] Public officials don't care what people think
  #V202214 POST: [REV] Politics/government too complicated to understand
  #V202216 POST: Important differences in what major parties stand for
  #V202224 POST: How important that more women get elected to political office
  #V202232 POST: What should immigration levels be (coding is phrased very change-y)
  #V202265 POST: Fewer problems if there was more emphasis on traditional family values
  #V202271 POST: Is the US better or worse than most other countries
  #V202304 POST: Our political system only works for insiders with money and power
  #V202317 POST: How much opportunity in America for average person to get ahead
  #V202355 POST: Does R currently live in a rural or urban area
  #V202406 POST: CSES5‐Q01: How interested in politics is R
  #V202411 POST: CSES5‐Q04c: Attitudes about elites: most politicians are trustworthy
  #V202425 POST: CSES5‐Q07: How widespread is corruption among politicians in US
###V202427 POST: CSES5‐Q09: How good/bad a job has government done in last 4 years
  #V202430 POST: CSES5‐Q11: State of economy better or worse over past 12 months
  #V202431 POST: CSES5‐Q14a: 5pt scale: Does it make a difference who is in power
  #V202439 POST: CSES5‐Q18: Left‐right‐self
  #V202468xPRE‐POST: SUMMARY: Total (family) income
  #V202504 POST: How important is being American to R's identity
  #V202569 POST: Life experience: has R ever been bitten by a shark
    #V202575 POST: GSS: How often does R pay attention to politics and elections
    ## NONE OF THE GSS VARIABLES ARE USEFUL
    ## THEY LITERALLY HAVE ONLY STUPID CODINGS LIKE INTERVIEW DROPPED
#V202637 POST: IWR OBS: respondent's gender
#V202638 POST: IWR OBS: respondent's estimated age
#V202640 POST: IWR OBS: respondent's level of information
#V202641 POST: IWR OBS: respondent's intelligence






### 2020 Presidential PREV ------------------------------------------
# V201101 - PRE: Did R vote for President in 2016 [revised][basically intimidation q]
# -9. Refused
# -8. Don’t know 
# -1. Inapplicable 
# 1. Yes, voted
# 2. No, didn’t vote

# V201102 - PRE: Did R vote for President in 2016
# -9. Refused
# -8. Don’t know 
# -1. Inapplicable 
# 1. Yes, voted
# 2. No, didn’t vote

# V201103 - PRE: Recall of last (2016) Presidential vote choice
# -9. Refused
# -8. Don’t know
# -1. Inapplicable
# 1. Hillary Clinton
# 2. Donald Trump
# 5. Other {SPECIFY}

# V201104 - PRE: Did R vote for president in 2012 election
# -9. Refused
# -8. Don’t know
# 1. Yes, voted
# 2. No, didn’t vote

# V201105 - PRE: recall of 2012 presidential vote choice
# -9. Refused
# -8. Don’t know
# -1. Inapplicable
# 1. Barack Obama
# 2. Mitt Romney
# 5. Other {SPECIFY}





















# Appendix - Identifying Variable Codes in ANES -------------------------------------------------------------
# Annoyingly, variable codes are not the same across years...
# even the variable-naming scheme is completely different


## 2000 \------------------------- ------------------------------

### 2000 Presidential  ------------------------------------
# V001241 - Did R Vote
  #1. I DID NOT VOTE (IN THE ELECTION THIS NOVEMBER).
  #2. I THOUGHT ABOUT VOTING THIS TIME, BUT DIDN'T.
  #3. I USUALLY VOTE, BUT DIDN'T THIS TIME.
  #4. I AM SURE I VOTED

# V001248 - Did R vote for a presidential candidate
  #1. YES, VOTED FOR PRESIDENT
  #5. NO, DIDN'T VOTE FOR PRESIDENT

# V001249 - Who did R vote for in Presidential race
  #1. AL GORE
  #3. GEORGE W. BUSH
  #5. PAT BUCHANAN
  #6. RALPH NADER
  #2. HOWARD PHILLIPS - CONSTITUTION PARTY CANDIDATE 4. HARRY BROWN - LIBERTARIAN CANDIDATE
  #7. R REPORTS VOTING FOR SELF

### 2000 Primary -------------------------------------------
# 2000 seems to have no data on primaries





## 2004 \------------------------- ----------------------------------------------

### 2004 Presidential PREV ------------------------------------
# V043002 - Did R vote in previous election (2000 election)
  #1. Yes, voted
  #5. No, didn't vote

# V043003 - Recall of presidential vote in previous election (2000 election)
  #1. Al Gore
  #3. George W. Bush
  #5. Pat Buchanan
  #6. Ralph Nader
  #7. Other {SPECIFY}

### 2004 Presidential ------------------------------------
# V045017a - Did R vote in this election (standard q)
  #1. Yes, voted
  #5. No, didn't vote

# V045017b - Did R vote in this election (experimental q)
  #1. I did not vote (in the election this November)
  #2. I thought about voting this time, but didn't
  #3. I usually vote, but didn't this time
  #4. I am sure I voted

# V045025 - Did R vote for a presidential candidate
  # 1. Yes, voted for President
  # 5. No, didn't vote for President

# V045026 - R's vote for President
  # 1. John Kerry
  # 3. George W. Bush
  # 5. Ralph Nader
  # 7. Other {SPECIFY}

### 2004 Primary -------------------------------------------
# also doesnt seem like they have any data on primaries...


### 2004 Respondent Attributes ----------------------------------
# V041201a - Sampling.1a. Postal abbreviation of state
  # Written as a 2-character string abbrevation

# V041202 - Sampling.2. FIPS state code
  # coded according to regular FIPS codes 
  # ex: Colorado is FIPS 08





## 2008 \------------------------- ----------------------------------------------

### 2008 Presidential PREV  ------------------------------------

# V083007 - Did R vote for President in (previous election) 2004
  # 1. Yes, voted
  # 5. No, didn't vote

# V083007a - Recall of last (2004) Presidential vote choice
  # 1. John Kerry
  # 3. George W. Bush
  # 7. Other (SPECIFY)

### 2008 Presidential  ------------------------------------

# V085044 - Did R vote for candidate for President
  # 1. Yes, voted for President
  # 5. No, didn't vote for President

#V085044a - For whom did R vote for President
  # 1. Barack Obama
  # 3. John McCain
  # 7. Other {SPECIFY}




### 2008 Primary ----------------------------------------
# V083077 - Did R vote in the Presidential primary or caucus
  # 1. Yes, voted in primary or caucus
  # 5. No, didn't vote in primary or caucus

# V083077a - For which candidate did R vote in Presidential primary
  # 01. Joe Biden
  # 02. Hillary Clinton
  # 03. Chris Dodd
  # 04. John Edwards
  # 05. Rudy Giuliani
  # 06. Mike Gravel
  # 07. Mike Huckabee
  # 08. Duncan Hunter
  # 09. Alan Keyes
  # 10. Dennis Kucinich
  # 11. John McCain
  # 12. Barack Obama
  # 13. Ron Paul
  # 14. Bill Richardson
  # 15. Mitt Romney
  # 16. Tom Tancredo
  # 17. Fred Thompson
  # 30. Someone else {SPECIFY}


### 2008 Respondent Attributes -------------------------------------

# V081201a - State Postal abbreviation
  # 2 character string 

# V081201b - State FIPS code
  # follows standard FIPS conventions







## 2012 \------------------------- ----------------------------------------------

### 2012 Presidential PREV ------------------------------------

# interest_voted2008 - Did R vote for President in 2008
  #1. Yes, voted
  #2. No, didnt vote

# interest_whovote2008 - Recall of last (2008) Presidential vote choice
  #1. Barack obama
  #2. John mccain
  #5. Other (SPECIFY)

### 2012 Presidential  ------------------------------------
# postvote_regpty - which party is R registered to vote for
  #1. Democratic party
  #2. Republican party
  #4. None or 'independent'
  #5. Other party (SPECIFY)

# postvote_rvote - Did R vote
  # 1. I did not vote (in the election this november)
  # 2. I thought about voting this time, but didn't
  # 3. I usually vote, but didn't this time
  # 4. I am sure I voted

# postvote_presvt - Did R vote for President
  # 1. Yes, voted for President
  # 2. No, didn't vote for President

# postvote_presvtwho - For whom did R vote for President
  # 1. <preload: dem_pcname>
  # 2. <preload: rep_pcname
  # 5. Other candidate (SPECIFY)


### 2012 Primary  ------------------------------------

# prevote_primv - Did R vote in the Presidential primary or caucus
  #1. Yes, voted in primary or caucus
  #2. No, did not vote in primary or caucus

# prevote_primvwho - For which candidate did R vote in Presidential prim
  # 01. Mitt romney 
  # 02. Barack obama 
  # 03. Rick santorum
  # 04. Newt gingrich 
  # 05. Ron paul 
  # 06. Rick perry 
  # 07. Michele bachmann 
  # 08. Jon huntsman 
  # 09. Herman cain 
  # 95. Someone else (SPECIFY)


### 2012 Respondent Attributes -------------------------------------





## 2016 \------------------------- -----------------------------

#length(unique(df$V160001)) 
# gives 4270
# which means this is definitely our unique identifier column

#Pre-Election Variables


### 2016 Presidential PREV ----------------------------------------------
# V161005 - PRE: Did R vote for President in 2012
  # 1. Yes, voted 
  # 2. No, didn’t vote

# V161006 - PRE: Recall of last (2012) Presidential vote choice
  # 1. Barack Obama
  # 2. Mitt Romney 
  # 5. Other SPECIFY 
  # 6. Other specify - specified as: Did not vote/did not vote for President in 2012

### 2016 Presidential ----------------------------------------------
# V161019 - PRE: Party of registration
  # 1. Democratic party 
  # 2. Republican party
  # 4. None or ‘independent’
  # 5. Other SPECIFY

# V162031x - PRE-POST: SUMMARY -Did R vote in 2016
  # 0 did not vote in 2016
  # 1 did vote in 2016

# V162034 - POST: Did R vote for President
  #1. Yes, voted for President 
  # 2. No, didn’t vote for President

# V162034a - POST: For whom did R vote for President
  # 1. Hillary Clinton
  # 2. Donald Trump
  # 3. Gary Johnson
  # 4. Jill Steiin
  # 5. Other candidate SPECIFY
  # 7. Other specify given as: none
  # 9. Other specify given as: RF


### 2016 Primary ---------------------------------------------------

# V161021 - PRE: Did R vote in a Presidential primary or caucus
  # 1. Yes, voted in primary or caucus
  # 2. No, didn’t vote in primary or caucus

# V161021a - PRE: For which candidate did R vote in Presidential primary
  # 1. Hillary Clinton
  # 2. Bernie Sanders 
  # 3. Another Democrat 
  # 4. Donald Trump 
  # 5. Ted Cruz 
  # 6. John Kasich
  # 7. Marco Rubio 
  # 8. Another Republican
  # 9. Someone else who is not a Republican or Democrat

### 2016 Respondent Attributes -------------------------------------
# V161010d - PRE: Vote section- FIPS state code for sample address
# V163001a - SAMPLE: Sample location FIPS state
# V163001b - SAMPLE: Sample location state postal abbreviation
# V161003 - PRE: How often does R pay attn to politics and elections
# V161004 - PRE: How interested in following campaigns
# V161008 - PRE: Days in week watch/listen/read news on any media
# V161009 - PRE: Attention to news on any media

### 2016 Other Vars -------------------------------------------------

# V161029b - PRE: Placeholder Code- How long before election R made decision Pres vote
# V161030 - PRE: Does R intend to vote for President
# V161031 - PRE: For whom does R intend to vote for President
# V161032 - PRE: Pref strng for Pres cand for whom R intends to vote
# V161033 - PRE: Does R prefer Pres cand (no intent to register)
# V161034 - PRE: Preference for Pres cand (no intent to register)
# V161036 - PRE: Did R vote for U.S. House of Representatives
# V161039 - PRE: Does R intend to vote for U.S. House
# V161046 - PRE: Did R vote for U.S. Senate
# V161049 - PRE: Does R intend to vote for U.S. Senate
# V161055 - PRE: Did R vote for governor
# V161058 - PRE: Does R intend to vote for governor
# V161064x - PRE: SUMMARY - party of Pre-election Presidential vote/intent/preference
# V161065x - PRE: SUMMARY - party of Pre-election U.S. House vote/intent/preference
# V161066x - PRE: SUMMARY - party of Pre-election U.S. Senate vote/intent/preference
# V161067x - PRE: SUMMARY - party of Pre-election Gubernatorial vote/intent/preference
# V161080 - PRE: Approval of Congress handling its job
# V161081 - PRE: Are things in the country on right track
# V161082 - PRE: Approve or disapprove President handling job as Pres
# V161083 - PRE: Approve or disapprove President handling economy
# V161084 - PRE: Approve or disapprove President handling foreign rel
# V161086 - PRE: Feeling Thermometer: Democratic Presidential cand
# V161087 - PRE: Feeling Thermometer: Republican Presidential cand
# V161095 - PRE: Feeling Thermometer: Democratic Party
# V161096 - PRE: Feeling Thermometer: Republican Party
# V161097 - PRE: Is there anything R likes about Democratic Party
# V161103 - PRE: Is there anything R likes about Republican Party
# V161110 - PRE: R how much better worse off than 1 year ago
# V161111 - PRE: R how much better worse off next year
# V161126 - PRE: 7pt scale Liberal conservative self-placement
# V161128 - PRE: 7pt scale liberal conservative - Dem Pres cand
# V161129 - PRE: 7pt scale liberal conservative - Rep Pres cand
# V161137 - PRE: Income gap today more or less than 20 years ago
# V161139 - PRE: Current economy good or bad
# V161140 - PRE: National economy better worse in last year
# V161144 - PRE: Which party better: handling nations economy
# V161145 - PRE: Care who wins Presidential Election revised version
# V161150a - PRE: VERSION 1A placement- Does R consider voting a duty or choice
# V161150b - PRE: VERSION 1B placement- Does R consider voting a choice or duty
# V161151x - PRE: SUMMARY - Voting as duty or choice
# V161155 - PRE: Party ID: Does R think of self as Dem, Rep, Ind or what
# V161173 - PRE: Rep and Dem adequate parties
# V161215 - PRE: REV How often trust govt in Wash to do what is right
# V161216 - PRE: Govt run by a few big interests or for benefit of all
# V161218 - PRE: How many in government are corrupt
# V161219 - PRE: How often can people be trusted
# V161220 - PRE: Elections make govt pay attention
# V161221 - PRE: Is global warming happening or not
# V161224 - PRE: Govt action about rising temperatures
# V161231 - PRE: R position on gay marriage
# V161234 - PRE: U.S. more or less secure than when Pres took office
# V161235 - PRE: Economy better since 2008
# V161241 - PRE: Is religion important part of R life
# V161267 - PRE: Respondent age
# V161267x - PRE: SUMMARY - Respondent age group
# V161268 - PRE: R marital status
# V161270 - PRE: Highest level of Education
# V161274a - PRE: Previously served on active duty in armed forces
# V161277 - PRE: Initial R employment status, start of occupation module (EMPLOYMENT general)
# V161302 - PRE: Anyone in HH belong to labor union
# V161310x - PRE: SUMMARY - R self-identified race
# V161342 - PRE FTF CASI / WEB: R self-identified gender
# V161513 - PRE FTF CASI / WEB: Years Senator Elected
# V161514 - PRE FTF CASI / WEB: Political knowledge: program Fed govt spends
# V161522 - PRE: How satisfied is R with life

#Post-Election Variables
# V162002 - POST: How many programs about 2016 campaign did R watch on TV
# V162003 - POST: How many speeches about 2016 campaign did R listen to on radio
# V162004 - POST: How many times R got info about 2016 campaign on the Internet
# V162005 - POST: How many stories R read about 2016 campaign in any newspaper
# V162007 - POST: Did party contact R about 2016 campaign
# V162007a - POST: Which party contacted R about 2016 campaign
# V162011 - POST: R go to any political meetings, rallies, speeches
# V162036a - POST: Code- How long before election R made decision Pres vote
# V162038x - POST: SUMMARY- Preference for Pres cand (did not vote)
# V162058x - POST: SUMMARY -Post-election Presidential vote/pref
# V162062x - 2016 PRE-POST VOTE SUMMARY: 2016 Presidential vote
# V162066x - 2016 PRE-POST VOTE SUMMARY: 2016 Presidential vote w/strength
# V162072 - POST: Office recall: Vice-President Biden
# V162074a - POST: Office recall: Chancellor of Germany Merkel
# V162074b - POST: Office recall: Chancellor of Germany Merkel [Scheme 2]
# V162075a - POST: Office recall: President of Russia Putin
# V162075b - POST: Office recall: President of Russia Putin [Scheme 2]
# V162078 - POST: Feeling thermometer: Democratic Presidential candidate
# V162079 - POST: Feeling thermometer: Republican Presidential candidate
# V162100 - POST: Feeling thermometer: BIG BUSINESS
# V162103 - POST: Feeling thermometer: GAY MEN AND LESBIANS
# V162105 - POST: Feeling thermometer: RICH PEOPLE
# V162106 - POST: Feeling thermometer: MUSLIMS
# V162113 - POST: Feeling thermometer: BLACK LIVES MATTER
# V162116a_1 - POST: Most Important Problem - Mention 1, Idea 1 (meaning just their #1)
# V162117 - POST: Party to deal with mention 1 MIP
# V162118a_1 - POST: Most Important Problem - Mention 2, Idea 1
# V162119 - POST: Party to deal with mention 2 MIP
# V162123 - POST: Better if rest of world more like America
# V162126 - POST: Heard about Rep Presidential cand Trump 2005 video about women
# V162127 - POST: Does Rep Presidential cand Trump 2005 video about women matter
# V162134 - POST: How much opportunity in America to get ahead
# V162135 - POST: Economic mobility compared to 20 yrs ago
# V162136x - POST: SUMMARY- Economic mobility easier/harder compared to 20 yrs ago
#

# from V162139 to V162295x is all issue question; super good ones
#



## 2020 \------------------------- -----------------------------

### 2020 Presidential PREV ------------------------------------------
# V201101 - PRE: Did R vote for President in 2016 [revised][basically intimidation q]
  # -9. Refused
  # -8. Don’t know 
  # -1. Inapplicable 
  # 1. Yes, voted
  # 2. No, didn’t vote

# V201102 - PRE: Did R vote for President in 2016
  # -9. Refused
  # -8. Don’t know 
  # -1. Inapplicable 
  # 1. Yes, voted
  # 2. No, didn’t vote

# V201103 - PRE: Recall of last (2016) Presidential vote choice
  # -9. Refused
  # -8. Don’t know
  # -1. Inapplicable
  # 1. Hillary Clinton
  # 2. Donald Trump
  # 5. Other {SPECIFY}

# V201104 - PRE: Did R vote for president in 2012 election
  # -9. Refused
  # -8. Don’t know
  # 1. Yes, voted
  # 2. No, didn’t vote

# V201105 - PRE: recall of 2012 presidential vote choice
  # -9. Refused
  # -8. Don’t know
  # -1. Inapplicable
  # 1. Barack Obama
  # 2. Mitt Romney
  # 5. Other {SPECIFY}

### 2020 Presidential ------------------------------------------
# V201228 - PRE: PARTY ID: DOES R THINK OF SELF AS DEMOCRAT, REPUBLICAN, OR INDEPENDENT
  # -9. Refused
  # -8. Don’t know
  # -4. Technical error
  # 0. No preference {VOL - video/phone only} 
  # 1. Democrat
  # 2. Republican
  # 3. Independent
  # 5. Other party {SPECIFY}

# V201229 - PRE: Party Identification strong ‐ Democrat Republican
  # 1. Strong
  # 2. Not very strong

# V202065x - PRE‐POST: SUMMARY: Party of registration
  # -9. Refused
  # -8. Don’t know
  # -1. Inapplicable
  # 1. Democratic Party
  # 2. Republican Party
  # 4. None or ’independent 
  # 5. Other

# V202072 - POST: Did R vote for President
  # -9. Refused
  # -7. No post-election data, deleted due to incomplete interview 
  # -6. No post-election interview
  # -1. Inapplicable
  # 1. Yes, voted for President
  # 2. No, didn’t vote for President

# V202073 - POST: For whom did R vote for President
  # 1. Joe Biden
  # 2. Donald Trump
  # 3. Jo Jorgensen
  # 4. Howie Hawkins
  # 7. Specified as Republican candidate
  # 8. Specified as Libertarian candidate




### 2020 Primary ---------------------------------------------------
# V201020 - PRE: Did R vote in a Presidential primary or caucus
  # 1. Yes, voted in primary or caucus
  # 2. No, didn’t vote in primary or caucus

# V201021 - PRE: For which candidate did R vote in Presidential primary
  # 1. Joe Biden
  # 2. Michael Bloomberg
  # 3. Pete Buttigieg
  # 4. Amy Klobuchar
  # 5. Bernie Sanders
  # 6. Elizabeth Warren
  # 7. Another Democrat
  # 8. Donald Trump
  # 9. Another Republican
  # 10. Someone else who is not a Republican or a Democrat

### 2020 Respondent Attributes ---------------------------------------------------



## 2024 \------------------------- -----------------------------

### 2024 Presidential ------------------------------------------

# V241106x - PRE: SUMMARY: RECALL OF LAST (2020) PRESIDENTIAL VOTE
  # -4. Error
  # -2. DK/RF in V241103, V241104, or V241105 1. No, didn’t vote for president in 2020
  # 2. Yes, for Joe Biden
  # 3. Yes, for Donald Trump
  # 4. Yes, for another candidate

# V241107 - PRE: DID R VOTE FOR PRESIDENT IN 2016 ELEC‐ TION
  # 1. Yes, voted
  # 2. No, didn’t vote

# V241108 - PRE: RECALL OF 2016 PRESIDENTIAL VOTE CHOICE
# 1. Hillary Clinton
# 2. Donald Trump
# 5. Another candidate {SPECIFY}


# REMEMBER THIS IS 2024 PRE SO NOT ALL VOTING HAS BEEN ACCOUNTED FOR
# V241036 - PRE: CONFIRMATION VOTED (EARLY) IN NOVEMBER 5 ELECTION
# 1. Yes, voted
# 2. No, have not voted

# V241038 - PRE: DID R VOTE FOR PRESIDENT
# 1. Yes, voted for President
# 2. No, didn’t vote for President

# V241039 - PRE: FOR WHOM DID R VOTE FOR PRESIDENT
# 1. Kamala Harris
# 2. Donald Trump
# 3. Robert F. Kennedy, Jr.
# 4. Cornel West
# 5. Jill Stein
# 6. Another candidate {SPECIFY}

### 2024 Primary -----------------------------------------------

# V241031 - PRE: DID R VOTE IN A PRESIDENTIAL PRIMARY OR CAUCUS
  # 1. Yes, voted in Democratic primary or caucus 
  # 2. Yes, voted in Republican primary or caucus 
  # 3. No, didn’t vote in a primary or caucus

# V241032 - PRE: FOR WHICH CANDIDATE DID R VOTE IN THE DEMOCRATIC PRESIDENTIAL PRIMARY
  # 1. Joe Biden
  # 2. Dean Phillips
  # 3. Another Democrat
  # 9. None of these candidates/uncommitted/left blank

# V241033 - PRE: FOR WHICH CANDIDATE DID R VOTE IN THE REPUBLICAN PRESIDENTIAL PRIMARY
  # 1. Donald Trump
  # 2. Ron DeSantis
  # 3. Nikki Haley
  # 4. Vivek Ramaswamy
  # 5. Another Republican
  # 9. None of these candidates/uncommitted/left blank


### 2024 Respondent Attributes -----------------------------------------

# V241008x - PRE: SUMMARY: APPROVE/DISAPPROVE DEMOCRATIC PARTY CHOOSING NEW CANDIDATE
  # 1. Approve strongly
  # 2. Approve not strongly
  # 3. Disapprove not strongly 
  # 4. Disapprove strongly

















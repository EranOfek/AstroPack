# LCS - 2026-06-07


https://chatgpt.com/c/6a2528d6-080c-83eb-a787-87b1fc78841b

## General

- Act as expert astro scientist for ULTRASAT project. 
- This chat is about low cadence survey planning. 
- I want to make sure that i understand everything correctly. 
- You will help me to understand and build my mind map about it.

- Because i am not astro and i do not yet understand everything, and also I do not understand the astro meaning of the numbers.

- Currently we have algorithm to schedule lcs targets.

- First, I want you to help me understand the meaning of evrything, lets go baby steps. 


## Inputs files

- CSV - List of 240 fields based on ULTRASAT field, these 240 fields represent the entire sky to observe.
- PDF - Document created by claude that describes the MATLAB code
- MATLAB class - the code that we have now.

## Details

- Current MATLAB class is original algorithm deeveloped by Yossi, which is not good enough. 
- Yossi now has new version NOT attached here with Claude improvements, based on 'pair matching' whatever it means (I am not familiar with it).

## TOP Goal

- My top goal is to understand if its worth spending time to convert this MATLAB code to Python and use Google OR-Tools CP-SAT.

## Requirements

What I understand is this - corret my if I am wrong, ask questions if you are not sure about something.

- ULTRASAT exposure is 5 minutes per image

- 3 exposures per target = 15 minutes per target

- We want 11 LCS targets everyday, not less, not more

- There is variable SLEW time between targets

- Every day (24 hours), we allocate 15 minutes x 12 (11 targets + total estimated slew time for all 11 targets) = 180 minutes = 3 hours per day

- LCS targets need to start every day on the approximated same utc time of the day, for 3 hours

- Fields must have A_U <= 1 and a usable continuous visibility window of at least 45 days.

- There is also per-day data which is the angle of the sun related to SOLAR PANELS CHARGING, we have this table in advance (I dont have it here, but its per day number)

- There are 4 groups of targets (see the PDF): A, B, C, D

- Out of the 240 targets in the CSV file, only 80-82 targets match the initial requirements (EXPLAIN WHY)

- Total required LCS plan length: 240 days

- Out of the 420 days mentioned in the PDF, we actually want to create LCS plan of 360 days (420 is extra buffer above 360)


## Visibility - Old Version

The old assumption was apparently: Maximum Sun distance for power = 130°

The current code already considers solar-panel constraints indirectly.
In Step 1 it calls: ULTRASAT_restricted_visibility(...), and then uses:

PowerLimits, SunLimits, MoonLimits, EarthLimits - and combines them.

The output is simply (for example):

Field 57 - Day 123 - Visible = Yes
Field 57 - Day 124 - Visible = No

The scheduler never sees the actual solar angle.

It only sees: Allowed or Not Allowed.


## Visiblity - New Version

Now PowerLimits() calculates the maximum allowed Sun distance dynamically.


## Soft TOO

A Soft ToO is one that can be observed while maintaining normal spacecraft power balance.

In practice: Sun angle < SoftMaxSunAngDist

which means: Solar panels still generate enough power for continuous operations. No battery sacrifice is needed.

Think: "Easy ToO"


## Hard TOO

The target is so far from the Sun direction that: Sun angle > SoftMaxSunAngDist

meaning: Solar panels are no longer producing enough power to sustain the observation. The spacecraft can still do it, but now it must: consume battery instead of operating in power balance.

Think: "I really want this observation, even though it costs battery."


## DoD = Depth Of Discharge

This is battery terminology.

0.0 = battery full
0.5 = 50% discharged
0.8 = 80% discharged

Args.maxDOD = 0.8 - meaning: ULTRASAT is allowed to use up to 80% of the battery capacity.

Example:

If battery capacity is 100 units: Start: 100
After observation: 40
DOD = 60%


## Eclipse

During eclipse: Earth blocks sunlight, so: Solar panels generate zero power.


## Set meaning

- A = 48 fields, 45 days, daily cadence.
- B = 16 fields, 45 days daily + 90 days every 4 days.
- C = 16 fields, 135 days every 4 days.
- D = 4 special high-extinction fields, 45 days daily.


## A Group Fields

48 fields total.

Each field gets:

- 45 consecutive days
- Observed every day (1-day cadence)

Example:

Day	Observe?
1	Yes
2	Yes
3	Yes
...	...
45	Yes

Then the field is finished.


## C Group Fields

16 fields total.

Each field gets:

- One 135-day window
- Observed every 4 days

Example:

Day	Observe?
1	Yes
5	Yes
9	Yes
13	Yes
...	...
133	Yes

About 34 observations over the 135-day period.

The code actually uses: mod(day-start_day, 4) to determine which days are observed.



## B Group Fields

Each field gets:

- One 45-day block at daily cadence
- Two 45-day blocks at 4-day cadence

Total: 45 + 45 + 45 = 135 days

Internally can split the 90 days into TWO separate 45-day windows.

So each B field occupies three 45-day windows.

Example:

Window 2 : daily
Window 3 : every 4 days
Window 4 : every 4 days

or

Window 5 : daily
Window 3 : every 4 days
Window 4 : every 4 days


## D Group Fields

These are fields that failed the extinction requirement: A_U > 1
while A/B/C require: A_U <= 1

Why observe them at all? Because the science working group (WG5) explicitly wants them.

Someone already ranked them manually: [79 12 48 28 16 88 55 32 213 26]

Observation pattern - Each D field gets: 45 days daily cadence, exactly like an A field.

## 




## Questions

### Is the order scientifically important?

There are two possibilities:

Interpretation 1 - THIS IS THE REQUIREMENT

The science only cares that a field gets:

45 days daily
90 days every 4 days


Interpretation 2

The science wants a transient light curve:

high cadence first
then low cadence follow-up


## Why only ~80–82 fields

Not because the CSV has only 80 good-extinction fields. I inspected the CSV: it has 240 fields, and many more than 80 have AU <= 1. The reduction to ~80–81 happens after combining extinction + visibility-window requirements: fields must have A_U <= 1 and a usable continuous visibility window of at least 45 days. The PDF says the scheduler then needs exactly 48 + 16 + 16 = 80 good fields for A/B/C, leaving almost no slack.

## Rules

- Do not write any code yet. 

- I want you to first analyze the files and tell me if its worth migrating this MATLAB code (or its newer claude version also in MATLAB) to Python using Google OR-Tools CP-SAT - will this give more flexibility and better solutions?


## Glossary (for me, correct me if something is wrong)

### Greedy Scheduler

A greedy scheduler is a task-allocation strategy that makes immediate, locally optimal choices at every step—like assigning the next available task to the first idle processor—without considering the long-term global consequences.


### Pair Matching

Pair matching (or matched-pair analysis/design) is a technique used in research and statistics where subjects are grouped into pairs based on shared characteristics (e.g., age, weight, or background). One subject receives the treatment, and the other receives the control. This method eliminates individual differences to isolate the true effect of a variable.


END OF FILE

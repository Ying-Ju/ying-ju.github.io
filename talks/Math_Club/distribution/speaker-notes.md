# Speaker notes

40-minute talk + 5-minute questions.

## Slide 2: Professor “Close Enough”

Allow 2 minutes. Invite students to say where they have seen a bell curve. Introduce the playful Professor Close Enough character verbally. Shape alone does not uniquely identify a distribution. The purpose of today is to connect fitting with decisions, not to memorize distribution names.

## Slide 3: A distribution answers a question

1 minute. A physical mechanism can be causal while our predictions remain uncertain. Distinguish empirical distribution from a proposed parametric model. All examples today use observed data.

## Slide 4: Is 94 mph fast? {background-color="#183b49"}

2 minutes. Four-seam fastballs in regular-season Statcast records, March through September 2026. Ask for a show of hands about 94 mph. Speeds and counts are pitch-level observations, not independent player samples. Source: https://baseballsavant.mlb.com/statcast_search and https://baseballsavant.mlb.com/csv-docs

## Slide 5: Three pitchers, three fitted distributions

3 minutes. Ask whether bell-shaped means Normal. The t has an estimated location, scale and degrees of freedom, not the fixed standard t distribution. Weibull fixes the lower endpoint at zero. This is a descriptive pooled comparison: game-level dependence and changing baselines limit iid likelihood interpretation.

## Slide 6: What does the comparison say?

1 minute. AIC does not certify adequacy. Normal and t are close. Fitted t degrees of freedom are approximately 19.33, 47.95, and 31.27. Abbott has a modest preference for t (delta AIC 3.07); the other two prefer Normal by only 1.53 and 0.45. Avoid announcing a decisive universal winner. Weibull is a valid candidate but does not win here. Do not compare absolute AIC across different pitchers or datasets.

## Slide 7: A closer look at the quantiles

2 minutes. Both axes use mph and equal scale. Points near y=x suggest agreement at the corresponding quantile. A few extreme pitches can depart. Models fit the rounded reported speeds as continuous measurements. No diagnostic injury conclusion follows from a slow pitch.

## Slide 8: A personal baseline changes the answer

2 minutes. Strictly greater than 94, not greater than or equal. Monitoring supports questions about performance, not causal diagnosis. Abbott's mean in March–May was about 92.68 mph versus 92.10 in August–September, so pooling the season can obscure a shifting baseline. A game mean has a different uncertainty from an individual pitch.

## Slide 9: Cincinnati repair work {background-color="#183b49"}

2 minutes. Fleet equipment, not verified individual passenger vehicles. An order is the observation; equipment can contribute several orders. Work-order year defines the cohort, not necessarily completion year. Latest public observations in the downloaded source stop in August 2023 despite metadata suggesting daily updates. Source: https://data.cincinnati-oh.gov/Thriving-Neighborhoods/Fleet-Preventative-Maintenance-Repair-Work-Orders/2a8x-bxjm . Labor hours are recorded aggregate labor; they do not measure elapsed downtime. Downtime definitions were too vague to support the earlier time-to-return story.

## Slide 10: Typical work and total work

1 minute. Median and mean here include the 32 zeros. Maximum is 88 hours, retained. Around 0.70% of all orders exceed 20 hours. Some common exact values may reflect recording practices or standard tasks. Do not interpret labor as customer waiting time or one mechanic's shift.

## Slide 11: Three models for positive labor

2 minutes. Compare the peak and body. A density height is not a probability; area over a range gives probability. Rounded work hours create ties that a smooth density cannot reproduce as point masses. All models include the 88-hour observation.

## Slide 12: Lognormal fits best among these candidates {.qq-slide}

2 minutes. Equal axis scaling. AIC: Gamma 12147.59, Lognormal 11526.07, Weibull 12166.14. Lognormal has lowest AIC/BIC and lower KS/CvM/AD descriptive discrepancies in the user's R run. This is a relative comparison, not an adequacy certificate or future validation.

## Slide 13: Which model gets a large job right?

3 minutes. Ask whether lowest AIC makes every estimate most accurate. All estimates come from fitting and evaluating the same sample, so this is an illustration of task-specific fit rather than a forecasting contest. Eight labor hours is an illustrative staffing threshold, not an official service standard. Tail frequency alone is not total workload; job sizes and arrival volume also matter.

## Slide 14: Why does fit matter?

1 minute. No empirical claim that Cincinnati actually uses these models or suffered understaffing. These are teaching applications. Gamma MLE matches the positive sample mean closely even though it misses other features; use that fact to distinguish mean estimation from tail estimation. Future planning needs volume and validation across time or equipment.

## Slide 15: Flood claims after Hurricane Helene {background-color="#183b49"}

2 minutes. Source: https://www.fema.gov/openfema-data-page/nfip-redacted-claims-v3 and https://www.fema.gov/api/open/v3/NfipClaims . Filter occupancyType=11, floodEvent='Hurricane Helene', state NC, yearOfLoss 2024, totalBuildingInsuranceCoverage=250000. Variable netBuildingPaymentAmount. Snapshot asOfDate September 8 2026. Claims may develop; no closed-status field establishing finalization. Paid amount differs from total damage or latent loss. One storm creates dependence across claims.

## Slide 16: The policy helps shape the distribution

3 minutes. Equal $10,000 bins including 0 and cap. Histogram endpoints are not smooth tails. 140 zeros and 63 exactly at cap. Zero payment does not establish zero damage or claim denial. Mean about $67,137 and median $26,453. We do not infer policy-level claim probability from a sample containing claims only.

## Slide 17: A mixed distribution for payments

2 minutes. Rounded percentages may sum to 100.01. This is a mixed discrete–continuous payment model, not a discovered mixture of latent customer groups. Endpoint masses are empirical. Interior distribution is explicitly conditioned on lying between zero and cap. A cap atom does not reveal the distribution of losses beyond the cap.

## Slide 18: One curve for the interior?

2 minutes. Truncated Lognormal has lower AIC than truncated Gamma in this exploratory likelihood comparison, delta AIC about 32.78. The normalization is F(cap)-F(0). Do not present ordinary fits after simply deleting capped claims. Generalized Pareto splicing or latent mixtures remain candidates for future work, not tested findings. No uncapped-loss tail is identified from the cap values.

## Slide 19: Ordinary claims and unusually large claims

1 minute. A Lognormal body and GPD tail is a possible example, not a fitted model here. Threshold choice, sufficient exceedances and treatment of the policy cap need scrutiny. More flexible models can overfit. For paid amount, the cap and zero masses remain part of the model. Avoid claiming this dataset proves a mixture mechanism.

## Slide 20: Insurance decisions need frequency and severity

2 minutes. Formula assumes at most one claim or a binary indicator over a specified period. With multiple claims, use expected count times appropriate average severity under the needed conditions. Pricing, reserves and policy design are applications, not advice to price from this storm alone. Shared storms matter for portfolio totals.

## Slide 21: Professor “Close Enough” revisited

2 minutes. Ask students which fit measure they would use for each case. Evidence can support different answers for different tasks. All candidates can be poor. Check support, recording, dependence, time changes, and decision-specific errors without overwhelming the introductory audience.

## Slide 22: You can make a presentation like this

1 minute. User supplied the course connection. Avoid promising a specific current Quarto syllabus. Mention data and source code are available to explore. Questions can use the final five minutes; adjust discussion to the 45-minute slot.

## Slide 23: All models are wrong, but some are useful {background-color="#183b49"}

1 minute. Attribution: George E. P. Box, Robustness in the Strategy of Scientific Model Building, 1979. The wording is the familiar quotation. Return to the decision before concluding that a better AIC alone is enough.

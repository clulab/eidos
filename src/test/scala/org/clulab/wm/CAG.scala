package org.clulab.wm

object CAG {
  val SENTENCE_SEPARATOR = " "
  val PARAGRAPH_SEPARATOR = "\n"
  
  // Note the two &ndash;
  val p1s1 = "Food insecurity levels are extremely alarming throughout the country due to conflict, a collapsing economy, low cereal production, poor rainfall in southeastern areas, and the exhaustion of coping capacities after several years of crisis."
  val p1s2 = "In Eastern Equatoria \u2013 one of the drought-hit areas \u2013 the latest food security and nutrition assessments in November 2016 revealed that households experiencing poor food consumption nearly doubled from 19 to 37 percent when compared with the same time in 2015."
  val p1s3 = "Rainfall deficits were widely reported in the area, with 93 percent of households indicating this was a major shock during the last agricultural season, impacting pasture and water availability and local food production."
  val p1s4 = "Climatic forecasting indicates these areas are likely to experience depressed rainfall between March to May 2017."
  val p1 = Array(p1s1, p1s2, p1s3, p1s4).mkString(SENTENCE_SEPARATOR)

  // Note the &rsquo;
  val p2s1 = "South Sudan\u2019s economic crisis has been driven by the rapidly depreciating value of the South Sudanese Pound (SSP), shortages of hard currency, global declines in oil prices and significant dependence on imports."
  val p2s2 = "Conflict, insecurity, market disruption, economic downturn and localized crop failures have caused record high food prices and hunger has spread to locations that were previously stable."
  val p2s3 = "Conflict and economic decline have led to violence and displacement."
  val p2s4 = "Violence has caused livestock to be looted, killed and disease-prone and crops destroyed, and displacement has caused delayed planting."
  val p2s5 = "These impacts on livestock and crops have resulted in livelihoods being decimated."
  val p2 = Array(p2s1, p2s2, p2s3, p2s4, p2s5).mkString(SENTENCE_SEPARATOR)
  
  val p3s1 = "Food insecurity is becoming more severe across a wider area and is deepening for people already made vulnerable by displacement and conflict."
  val p3s2 = "In 2017, food insecurity in Unity, Jonglei and parts of Greater Equatoria and Greater Bahr el Ghazal remained critical as spikes in conflict, economic collapse and impacts of flooding reduced agricultural production."
  val p3s3 = "About three quarters of counties countrywide are expected to face severe food insecurity in the first quarter of 2018."
  val p3s4 = "Even after the harvest in late 2017, food prices remained high, and the 2018 lean season is projected to begin early and become worse than in 2017."
  val p3s5 = "Particularly for people living in market-dependent urban areas, economic decline has meant a reduction in access to staple food and clean water, and to a variety of foods."
  val p3 = Array(p3s1, p3s2, p3s3, p3s4, p3s5).mkString(SENTENCE_SEPARATOR)
  
  // Note the &rsquo;
  val p4s1 = "The rising cost of living and impact of the conflict have also undermined people\u2019s ability to access safe water."
  val p4s2 = "It is estimated that only 13 per cent of South Sudanese people have access to improved sanitation, while 85 per cent of the population practice open defecation and only 41 per cent have access to safe water."
  val p4s3 = "Families in urban centres have had to spend an increasing portion of their income to obtain clean water, while water trucking has decreased due to the cost of fuel."
  val p4s4 = "Borehole repairs have not been possible in areas hardest hit by conflict, including large swathes of Upper Nile, due to lack of access due to insecurity and lack of technical expertise and supplies."
  val p4 = Array(p4s1, p4s2, p4s3, p4s4).mkString(SENTENCE_SEPARATOR)
  
  val p5s1 = "Due to conflict and food shortages, more than one in five people in South Sudan have been forced to flee their homes in the past 22 months, including over 1.66 million people who are currently internally displaced and nearly 646,000 people who have fled to neighbouring countries as refugees."
  val p5s2 = "Many have been displaced multiple times because of repeated attacks, particularly in counties such as Leer, Koch, Mayendit and Rubkona in Unity State, Fangak and Pigi County in Jonglei, and Malakal and surrounding areas in Upper Nile."
  val p5 = Array(p5s1, p5s2).mkString(SENTENCE_SEPARATOR)
  
  val p6s1 = "Persistent insecurity and armed conflict have disrupted livelihood activities, affected market functionality and limited physical access to markets."
  val p6s2 = "Acute malnutrition has worsened compared to the same period in 2016 due largely to the unprecedented high levels of food insecurity, widespread fighting, displacement causing poor access to services, high morbidity, extremely poor diet (in terms of both quality and quantity), low coverage of sanitation facilities and poor hygiene practices."
  val p6s3 = "While marginal improvements in levels of acute malnutrition are expected up to December 2017 due to consumption of household production, forecasts for 2018 are deeply concerning with over 1.1 million children under five expected to be acutely malnourished and 269 000 children likely to be severely malnourished."
  val p6 = Array(p6s1, p6s2, p6s3).mkString(SENTENCE_SEPARATOR)
  
  val fullText = Array(p1, p2, p3, p4, p5, p6).mkString(PARAGRAPH_SEPARATOR)
}

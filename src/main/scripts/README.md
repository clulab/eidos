Simple scripts to convert the Intervention corpus from BBN (present at: https://drive.google.com/drive/folders/1-qSu5HKUU2dpvBD9XKUDOEYwccNct3cQ) into the Stanford NER system format. 
-----

1. read_intervention_dataset.py --> simple script to read the json format data from BBN into simple text files which are tab separated
2. create_tok_stanfordNER.py --> simple script to create a random train/test split of the 742 datapoints in Stanford NER format (description: see https://nlp.stanford.edu/software/crf-faq.html#a .. question 1: "How can I train my own NER model?"). Also the scripts genrates various different percentages of train dataset (25, 50, 75, 90) to create different amounts of train dataset (to generate the learning curves) 

The data generated using 2 is given as input to the Stanford NER system as described in https://nlp.stanford.edu/software/crf-faq.html#a 

Results of this exercise present at: https://docs.google.com/spreadsheets/d/1jm1grWQoCmrhUs2yPOpxR3iV38ueNkwp3i5LeEvgq1k/edit#gid=901477829

#!/usr/bin/python

import nltk
import random
random.seed(1)

fw1 = open("../intervention_examples/event_trig_data_train.tok", "w")
fw2 = open("../intervention_examples/event_trig_data_test.tok", "w")
fw1_25p = open("../intervention_examples/event_trig_data_train_25p.tok", "w")
fw1_50p = open("../intervention_examples/event_trig_data_train_50p.tok", "w")
fw1_75p = open("../intervention_examples/event_trig_data_train_75p.tok", "w")
fw1_90p = open("../intervention_examples/event_trig_data_train_90p.tok", "w")

TRAIN_FRACTION = 0.8

tr_num = 0
ts_num = 0
tr_25p_num = 0
tr_50p_num = 0
tr_75p_num = 0
tr_90p_num = 0

# =======
# NOTE:
# =======
# Train test split while creating the data for Stanford NER dataset (tokenized)
# ----
# TRAIN NUM 587
# TRAIN NUM (25p) 143
# TRAIN NUM (50p) 295
# TRAIN NUM (75p) 439
# TRAIN NUM (90p) 530
# TEST NUM 155
# TOTAL 742
    # ----

with open('../intervention_examples/event_trig_data.tsv') as fh:
    for idx, line in enumerate(fh):
        if idx == 0:
            continue

        fields = line.rstrip("\n").split("\t")
        label = fields[0]
        sentence_tokens = nltk.word_tokenize(fields[1])
        trig = fields[2]

        sentence_labels = []
        for tok in sentence_tokens:
            match = False
            for ttok in nltk.word_tokenize(trig):
                if tok == ttok:
                    match = True
            if match:
                sentence_labels.append(label)
            else:
                sentence_labels.append("O")

        assert len(sentence_tokens) == len(sentence_labels), "Something is wrong in line " + line

        rand = random.random()
        if rand < 0.8:
            tr_num += 1
            for i, tok in enumerate(sentence_tokens):
                fw1.write(tok + "\t" + sentence_labels[i] + "\n")
            if rand < 0.2:  ## 25% of data
                tr_25p_num += 1
                for i, tok in enumerate(sentence_tokens):
                    fw1_25p.write(tok + "\t" + sentence_labels[i] + "\n")
            if rand < 0.4:  ## 50% of data
                tr_50p_num += 1
                for i, tok in enumerate(sentence_tokens):
                    fw1_50p.write(tok + "\t" + sentence_labels[i] + "\n")
            if rand < 0.6:  ## 75% of data
                tr_75p_num += 1
                for i, tok in enumerate(sentence_tokens):
                    fw1_75p.write(tok + "\t" + sentence_labels[i] + "\n")
            if rand < 0.72:  ## 90% of data
                tr_90p_num += 1
                for i, tok in enumerate(sentence_tokens):
                    fw1_90p.write(tok + "\t" + sentence_labels[i] + "\n")
        else:
            ts_num += 1
            for i, tok in enumerate(sentence_tokens):
                fw2.write(tok + "\t" + sentence_labels[i] + "\n")

print("TRAIN NUM " + str(tr_num))
print("TRAIN NUM (25p) " + str(tr_25p_num))
print("TRAIN NUM (50p) " + str(tr_50p_num))
print("TRAIN NUM (75p) " + str(tr_75p_num))
print("TRAIN NUM (90p) " + str(tr_90p_num))
print("TEST NUM " + str(ts_num))
print("TOTAL " + str(tr_num + ts_num))
fw1.close()
fw2.close()
fw1_25p.close()
fw1_50p.close()
fw1_75p.close()
fw1_90p.close()
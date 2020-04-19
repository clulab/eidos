# Major changes to the rule grammars

+ **03-30-2020** - Removed a set of rules which had low precision (<=50%) in this 
domain, as determined from the EstoniaRun5 evaluation:
    + reverse_causal-noun-3, dueToSyntax4-Causal, 
    syntax_explicit_favorable, reverse_causal-noun-2, 
    ported_syntax_2_noun-Causal, dueTo-caseSyntax-dobj_effect-Causal,
    ported_token_2_noun-Causal, report-as-reason-Causal, 
    reasons-Causal, dueToSyntax3-Causal, token_1_verb-Causal

+ **04-16-2020** - Marked legacy changes with "# Removed for LUM" so that it is
easier to interpret the diffs.  All other changes involve substitution of "causeex"
for "wm".
    + stops.txt
    + transparent.txt
    + causal.yml

#!/usr/bin/python

with open('../intervention_examples/wm_intervention_event_trigger.json') as fh:
    int_event_trig_data = json.load(fh)

with open('../intervention_examples/wm_intervention_event_argument.json') as fh:
    int_event_arg_data = json.load(fh)

print("EVENT TRIGGER DATA SIZE -- " + str(len(int_event_trig_data)))

print("EVENT ARGUMENT DATA SIZE -- " + str(len(int_event_arg_data)))

fw = open('event_arg_data.txt', 'w')
fw.write("ARGUMENT\tARG.ROLE\tEVENT.TYPE\tTEXT\tTRIG\n")
for datum in int_event_arg_data:
    fw.write(datum['argument'] + "\t" + datum['argumentRole'] + '\t' + datum['eventType'] + '\t' + datum['text'] + '\t' + datum['trigger'] + '\n')
fw.close()


fw = open('event_trig_data.txt', 'w')
fw.write("EVENT.TYPE\tTEXT\tTRIG\n")
for datum in int_event_trig_data:
    fw.write(datum['eventType'] + '\t' + datum['text'] + '\t' + datum['trigger'] + '\n')
fw.close()

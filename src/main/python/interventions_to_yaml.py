from mk_yaml_ontology import ont_node, dump_yaml

import sys
from collections import defaultdict

# todo: check??
syn_relations = {"Synonym", "B is a subset of A", "A is a subset of B", "Rough synonym"}

def is_valid_synonym(s):
    return s in syn_relations

def load_intervention_types(fn, syn_file):
    # ont_node_dict = defaultdict(dict)
    # ocha_dict = defaultdict(ont_node_dict)

    synonyms = defaultdict(list)
    with open(syn_file) as f:
        _ = f.readline()
        for line in f:
            fields = line.split("\t")
            print(fields)
            reln = fields[2]
            if is_valid_synonym(reln):
                node_name = fields[1].strip().lower()
                syn = fields[3].strip().lower()
                synonyms[node_name].append(syn)


    interventions = defaultdict(lambda: defaultdict(list))
    # interventions = {}

    with open(fn) as f:
        header = f.readline().rstrip().split("\t")
        # intervention_class = header[0]
        # OCHA = header[1]
        # intervention_type_name = header[2]
        # keywords = header[4]


        # Provision of Goods and Services:
        #   Education:
        #       OntologyNode:
        #       Name: (Provision of) Child friendly learning spaces
        #       Examples:
        #           "education"
        #           "services"
        #           "provision of child friendly learning spaces"
        for line in f:
            fields = line.rstrip().split("\t")
            print(fields)
            curr_class = fields[0].strip().lower()
            curr_ocha = fields[1].strip().lower()
            curr_name = fields[2].strip().lower()
            if curr_class == 'provision of goods and services':
                curr_name = "provision of " + curr_name
            if len(fields) >= 5:
                curr_keywords = fields[4].strip().lower()
                # Sometimes we get '' examples -- filter them out!
                if curr_keywords == '':
                    curr_keywords = None
                # curr_keywords = [kw for kw in kws if kw != '']
            else:
                curr_keywords = None




            # interventions[curr_class][curr_ocha].append(ont_node(curr_name, curr_keywords))
            # if curr_class in interventions:
            #     if curr_ocha in interventions[curr_class]:
            #         interventions[curr_keywords]
            #     else:
            #         interventions[curr_class][curr_ocha] = [ont_node(curr_name, curr_keywords)]
            # else:
            #     interventions.append({curr_class:[]})

            interventions[curr_class][curr_ocha].append(ont_node(curr_name, synonyms[curr_name], curr_keywords))
            # interventions[curr_class][curr_ocha]['OntologyNode']['name'] = curr_name
            # interventions[curr_class][curr_ocha]['OntologyNode']['examples'] = curr_keywords
            # interventions[curr_class][curr_ocha]['OntologyNode']['polarity'] = 1.0

    # Convert back to a regular nested dict for later yaml fun
    final_interventions = []
    for j_class in interventions:
        j_class_oshas = []
        for k_osha in interventions[j_class]:
            j_class_oshas.append({k_osha:interventions[j_class][k_osha]})
        final_interventions.append({j_class: j_class_oshas})



    # interventions = dict(interventions)
    # for class_j in interventions:
    #     interventions[class_j] = dict(interventions[class_j])
    #     for k in interventions[class_j]:
    #         interventions[class_j][k] = dict(interventions[class_j][k])
    # interventions = {'Interventions': interventions}

    # return interventions
    return final_interventions

def main():
    intervention_classes_file = sys.argv[1]
    synonyms_file = sys.argv[2]
    ont_output_file = sys.argv[3]
    info = load_intervention_types(intervention_classes_file, synonyms_file)
    #print(info)
    dump_yaml(info, ont_output_file)

main()

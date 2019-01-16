"""
This script takes a flat list of indicators (a text file with one column)
and converts it into a YAML ontology suitable for usage with Eidos.

Usage:
    python mk_yaml_ontology.py flat_list.txt ontology_filename.yml ontology_name
"""

import yaml
import sys

# Since we seem to want a None for OntologyNode, but don't want to display it :)
def represent_none(self, _):
    return self.represent_scalar('tag:yaml.org,2002:null', '')

yaml.add_representer(type(None), represent_none)


def ont_node(name, examples, keywords):
    # Make sure the node name is added to the examples to be used for grounding
    examples.append(name)
    d = {'OntologyNode': None, "name": name, 'examples': examples, 'polarity': 1.0}
    if keywords is not None:
        d['keywords'] = keywords
    return d


def dump_yaml(d, fn, ont_name):
    super_list = [{ont_name: d}]
    with open(fn, 'w') as yaml_file:
        yaml.dump(super_list, yaml_file, default_flow_style=False)


def main():
    flat_file = sys.argv[1]
    ont_file = sys.argv[2]
    ont_name = sys.argv[3]
    with open(flat_file, "r") as f:
        lines = [line.rstrip() for line in f.readlines()]
    nodes = [ont_node(line, [], None) for line in lines]
    dump_yaml(nodes, ont_file, ont_name)

if __name__ == "__main__":
    main()

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


def dump_yaml(d, fn):
    with open(fn, 'w') as yaml_file:
        yaml.dump(d, yaml_file, default_flow_style=False)


def main():
    flat_file = sys.argv[1]
    ont_file = sys.argv[2]
    lines = [line.rstrip() for line in open(flat_file, 'r').readlines()]
    nodes = [ont_node(line, [], None) for line in lines]
    dump_yaml(nodes, ont_file)

main()
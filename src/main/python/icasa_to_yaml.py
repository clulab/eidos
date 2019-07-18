import sys, re
from mk_yaml_ontology import ont_node, dump_yaml

def replace_token(pattern, replacement, s):
    replacement = f' {replacement} '
    s = re.sub(f' ?{pattern} ', replacement, s)
    s = re.sub(f' {pattern} ?', replacement, s)
    return s

def mk_ont_node(line_string):
    fields = line_string.split("\t")
    assert(len(fields) >= 4)
    var_name = fields[0].strip()
    description = fields[3].strip()
    description = replace_token("C", "carbon", description)
    description = replace_token("CO2", "carbon dioxide", description)
    description = replace_token("CH2O", "formaldehyde", description)
    description = replace_token("N", "nitrogen", description)
    description = replace_token("NH3", "ammonia", description)
    description = replace_token("NH4", "ammonium", description)
    description = replace_token("NO3", "nitrate", description)
    description = replace_token("P", "phosphorus", description)
    return ont_node(var_name, [description], None, add_name = False)    # the name isn't in a format we can use


def main():
    flat_file = sys.argv[1]
    ont_file = sys.argv[2]
    ont_name = sys.argv[3]
    with open(flat_file, "r") as f:
        _ = f.readline()    # read header
        lines = [line.rstrip() for line in f.readlines()]
    nodes = [mk_ont_node(line) for line in lines]
    dump_yaml(nodes, ont_file, ont_name)

main()
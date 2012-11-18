from os import walk
import os.path
import os

# For some reason, althoug the prolog parser checks for built-ins, it doesn't 
# exclude them all, so we check again here for a few. I think it's because these 
# aren't built-ins, they are SWI library predicates.
prolog_built_ins = set(['!/0', 'retractall/1', 'append/3', 'member/2'])
t = 0

def is_prolog_file(filename):
    name, ext = os.path.splitext(filename)
    return ((ext == '.pl') and (name != 'prolog_graph'))

def process_file(filename):

    global prolog_builtins
    # This temporal file will be used for 'communication' with SWI-Prolog.
    # SWI will output its results in it, and we will read it back in.
    # Afterwards it will be deleted.
    global t
    t += 1
    tmpfilename = "./tmp" + str(t) + ".txt"
    call_subgraph = {}

    # Ask Prolog to parse the file for us, since Prolog is so friendly to 
    # Prolog code. Ahhh, the wonders of homoiconicity. ;)
    os.system("""swipl -f prolog_graph.pl -g "process_file('""" + filename + """','""" + tmpfilename + """')",halt -t "halt(1)" """)

    # Process Prolog's output in the temporal file.
    tmpfile = open(tmpfilename, 'r')
    for line in tmpfile:

        # The results from Prolog are in a certain format. 
        # Each line corresponds to one top-level clause in the source file.
        # Every line consists of comma-separated values.
        # The first two values are the clause head and arity.
        # Every pair of values after that are the terms in the clause body.
        # The very last item is a newline, so it is discarded.
        l = line.split(',')[:-1]
        head = l[0]
        body = set([])

        # It may have been the case that in a previous line this predicate had 
        # another clause defined, so the call subgraph for this file will 
        # already contain a 'head' key.
        # So as to not overwrite it's value, we check for membership before 
        # initalizing.
        if (not (head in call_subgraph)):
            call_subgraph[head] = set([])

        # Iterate over the values in the line.
        i = 1
        n = len(l)
        while (i < n): 
            pred = l[i]
            if (not (pred in prolog_built_ins)):
                body.add(pred)
            i += 1
        call_subgraph[head] = call_subgraph[head].union(body)
    tmpfile.close()

    # Delete the temporal file.
    os.system("rm " + tmpfilename)
    
    # Generate output for this particular file
    #output_call_subgraph(call_subgraph, filename)
    return call_subgraph

def output_call_subgraph(call_subgraph, output_filename):
    cluster_i = 0
    dotfilename = "./prolog_explorer_output/" + output_filename
    tmpfile = open(dotfilename, 'w')
    tmpfile.write('digraph G {')
    tmpfile.write('subgraph cluster_0 {')
    tmpfile.write('style=filled; color=lightgrey;')
    for k, v in call_subgraph.items():
        pass
        # unfinished

def output_call_graph(call_graph, output_filename):
    cluster_i = 0

    dotfilename = "./prolog_explorer_output/" + output_filename
    dotfile = open(dotfilename, 'w')

    dotfile.write('digraph G {')
    for fn, csg in call_graph.items():
        dotfile.write('subgraph cluster_' + str(cluster_i) + ' {')
        dotfile.write('style=filled; color=lightgrey;')
        cluster_i += 1
        for k, v in csg.items():
            dotfile.write('"' + k + '";')
            for p in v:
                dotfile.write('"' + k + '"' + ' -> "' + p + '";')
        dotfile.write('label = "' + fn + '";')
        dotfile.write('}')

    dotfile.write('}')
    dotfile.close()

    os.system("dot -Tpdf -O " + dotfilename)
    os.system("rm " + dotfilename)

if (__name__ == "__main__"):
    
    prolog_files = []
    call_graph   = {}

    os.system("mkdir -p ./prolog_explorer_output/")

    # Obtain all program source files.
    for path, subdirs, files in os.walk("."):
        prolog_files += ["%s/%s" % (path,f) for f in files if is_prolog_file(f)]

    # Build call graph.
    for f in prolog_files:
        print("processing %s" % f)
        call_graph[f] = process_file(f)

    # Convert call graph to graphviz dot language and call graphviz on the result.
    output_call_graph(call_graph, "call_graph")


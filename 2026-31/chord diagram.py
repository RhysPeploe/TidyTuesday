import os
import pandas as pd
import holoviews as hv
from holoviews import opts, dim
from bokeh.sampledata.les_mis import data
from bokeh.plotting import show
from bokeh.io import output_file

os.chdir("C:\\Users\\rpeploe\\Documents\\TidyTuesday\\2026-31")

hv.extension("bokeh")

nodes = hv.Dataset(pd.DataFrame(data["nodes"]), "index")
links = pd.DataFrame(data["links"])

chord = hv.Chord((links, nodes))

chord = chord.opts(
    opts.Chord(
        cmap="Category20",
        edge_cmap="Category20",
        edge_color=dim("source").str(),
        labels="name",
        node_color=dim("index").str(),
        width=800,
        height=800,
    )
)
output_file("chord.html")
show(hv.render(chord))
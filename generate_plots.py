import matplotlib as mpl
import matplotlib.pyplot as plt
import json
import os
import sys
from cycler import cycler
import argparse

plt.rcParams.update({'font.size': 16})

parser = argparse.ArgumentParser()
parser.add_argument("--file", required=False, default=None)
parser.add_argument(
	"--dims", required=False, default="6x4", help="Dimensions of the figure in widthxheight inches"
)
args = parser.parse_args()
args.dims = list(map(int, args.dims.split("x")))

plot_data_file = args.file
print(plot_data_file)
PLOT_DATA_DIR="plot-data-v2/"
PLOT_DIR="plots-v2/"

if __name__ == "__main__":
	data_files = os.listdir(PLOT_DATA_DIR)
	for data_file in data_files:
		basename, ext = os.path.splitext(data_file)
		if plot_data_file != None and PLOT_DATA_DIR+data_file != plot_data_file: continue
		with open(PLOT_DATA_DIR+data_file) as inf:
			try:
				plot_details = json.load(inf)
				plt.clf()
				plt.figure(figsize=args.dims)
				markercycle = cycler(marker=['o', '^', 's', '*', 'P', 'd'])
				colorcycle = cycler(color=plt.rcParams['axes.prop_cycle'].by_key()['color'][:6])
				plt.gca().set_prop_cycle(colorcycle + markercycle)
				mpl.rcParams["lines.markersize"] = 8

				plt.title(plot_details["title"])
				plt.xlabel(plot_details["xlabel"])
				plt.ylabel(plot_details["ylabel"])

				bottom, top = plot_details["ylim"]
				if top is not None and bottom is not None: plt.ylim(bottom=bottom, top=top)
				elif top is not None: plt.ylim(top = top)
				elif bottom is not None: plt.ylim(bottom = bottom)

				plotfun = plot_details["plot_type"]
				data = plot_details["data"]
				for label in data.keys():
					d = data[label]
					if plotfun == "plot":
						plt.plot(d[0], d[1], label=label, linewidth=2)
					elif plotfun == "errorbar":
						if len(d) == 3:
							plt.errorbar(x=d[0], y=d[1], yerr=d[2], label=label, linewidth=2)
						elif len(d) == 4:
							plt.errorbar(x=d[0], y=d[1], yerr=d[2], label=label, fmt=d[3], linewidth=2)
						else:
							raise Exception("Unknown data line format: " + str(d))
					elif plotfun == "scatter":
						plt.scatter(x=d[0], y=d[1], label=label)
					elif plotfun == "bar":
						if type(d) == dict:
							plt.bar(
								x=d["x"],
								height=d["height"],
								tick_label=d["tick_label"],
								yerr=d["yerr"],
								width=d["width"],
							)
						elif type(d) == list:
							plt.bar(x = d[0], height=d[1])
					else:
						raise Exception("Don't know how to plot " + plotfun)
				if len(data.keys()) > 1: plt.legend()
				plt.savefig(PLOT_DIR+basename+".png", bbox_inches='tight', dpi=300)
			except Exception as e:
				print("Error while plotting from", PLOT_DATA_DIR+data_file, "\n ", str(e))
			finally:
				plt.close()


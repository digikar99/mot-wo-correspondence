import matplotlib
import matplotlib as mpl
import matplotlib.pyplot as plt
import json
import os
import sys
from cycler import cycler
import argparse

matplotlib.use("pgf")
matplotlib.rcParams.update({
	"pgf.texsystem": "pdflatex",
	'font.family': 'sans',
	'text.usetex': True,
	'pgf.rcfonts': False,
})


# For latex pgf export
print(plt.rcParams["legend.fontsize"])
plt.rcParams.update({
	"font.size": 8,
	"lines.markersize": 3,
	"lines.linewidth": 1,
	"axes.linewidth": 1,
	"legend.borderpad": 0.2,
	"legend.fontsize": 7
})

# plt.rcParams.update({
# 	'font.size': 16,
# 	"lines.markersize": 12,
# 	"lines.linewidth": 3,
# 	"axes.linewidth": 3
# })

# plt.rcParams['lines.markersize'] = 10.0

parser = argparse.ArgumentParser()
parser.add_argument("--file", required=False, default=None)
parser.add_argument("--type", required=False, default="png")
parser.add_argument(
	"--dims", required=False, default="6x4", help="Dimensions of the figure in widthxheight inches"
)
args = parser.parse_args()
args.dims = list(map(float, args.dims.split("x")))

plot_data_file    = args.file
plot_image_format = args.type
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
				# mpl.rcParams["lines.markersize"] = 12
				# mpl.rcParams["lines.linewidth"] = 3
				# mpl.rcParams["axes.linewidth"] = mpl.rcParams["lines.linewidth"]
				plt.clf()
				plt.figure(figsize=args.dims)
				markercycle = cycler(marker=['o', '^', 's', '*', 'P', 'd'])
				colorcycle = cycler(color=plt.rcParams['axes.prop_cycle'].by_key()['color'][:6])
				plt.gca().set_prop_cycle(colorcycle + markercycle)

				plt.title(plot_details["title"])
				plt.xlabel(plot_details["xlabel"])
				plt.ylabel(plot_details["ylabel"])

				bottom, top = plot_details["ylim"]
				if top is not None and bottom is not None: plt.ylim(bottom=bottom, top=top)
				elif top is not None: plt.ylim(top = top)
				elif bottom is not None: plt.ylim(bottom = bottom)

				plotfun_data = plot_details["plot_type"].split()
				plotfun = plotfun_data[0]
				istwin  = (len(plotfun_data)>1 and plotfun_data[1] == "twin")
				data = plot_details["data"]
				for label in data.keys():
					d = data[label]
					if plotfun == "plot":
						if len(d) == 2:
							plt.plot(d[0], d[1], label=label)
						elif len(d) == 3:
							if " " in d[2]:
								plt.scatter(d[0], d[1], marker=d[2][1], label=label)
							else:
								plt.plot(d[0], d[1], d[2], label=label)
					elif plotfun == "errorbar":
						if len(d) == 3:
							plt.errorbar(x=d[0], y=d[1], yerr=d[2], label=label)
						elif len(d) == 4:
							plt.errorbar(x=d[0], y=d[1], yerr=d[2], label=label, fmt=d[3])
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
				if plot_image_format == "pgf":
					plt.tight_layout(pad=0)
					plt.savefig(PLOT_DIR+basename+"."+plot_image_format, bbox_inches="tight")
				else:
					plt.savefig(PLOT_DIR+basename+"."+plot_image_format, bbox_inches='tight', dpi=300)
			except Exception as e:
				print("Error while plotting from", PLOT_DATA_DIR+data_file, "\n ", str(e))
			finally:
				plt.close()


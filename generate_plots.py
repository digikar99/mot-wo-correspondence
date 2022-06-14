import matplotlib.pyplot as plt
import json
import os

plt.rcParams.update({'font.size': 16})

PLOT_DATA_DIR="plot-data/"
PLOT_DIR="plots/"

if __name__ == "__main__":
	data_files = os.listdir(PLOT_DATA_DIR)
	for data_file in data_files:
		basename = data_file.rsplit(".", maxsplit=1)[0]
		with open(PLOT_DATA_DIR+data_file) as inf:
			plot_details = json.load(inf)
			plt.clf()
			plt.title(plot_details["title"])
			plt.xlabel(plot_details["xlabel"])
			plt.ylabel(plot_details["ylabel"])
			plt.ylim(*plot_details["ylim"])

			plotfun = plot_details["plot_type"]
			data = plot_details["data"]
			for label in data.keys():
				d = data[label]
				if plotfun == "plot":
					plt.plot(d[0], d[1], label=label)
				elif plotfun == "errorbar":
					plt.errorbar(x=d[0], y=d[1], yerr=d[2], label=label)
				elif plotfun == "scatter":
					plt.scatter(x=d[0], y=d[1], label=label)
				else:
					raise Exception("Don't know how to plot " + plotfun)
			plt.savefig(PLOT_DIR+basename+".png", bbox_inches='tight', dpi=300)

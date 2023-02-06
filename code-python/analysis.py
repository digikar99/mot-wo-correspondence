import matplotlib.pyplot as plt
# from mpl_toolkits.axisartist.parasite_axes import HostAxes, ParasiteAxes
import argparse
import os
import sys
import json
import math
import csv
import pandas as pd
import numpy as np
import seaborn as sns
from generate_plot_data import write_plot_file
from tasks import is_bad_trial, NUM_BAD_TRIALS, evaluate_tracking, evaluate_id

from Environment import OrnsteinUhlenbeckEnvironment, ExperimentalEnvironment
from OurMOTModel import OurMOTModel

np.set_printoptions(precision=3, suppress=False)

PLOT_DIR = "plots-data-v2/"
DATA_DIR = "human-experiments/data/"

def plot_tracking_vs_id(num_targets, json_filename=None):
	files = os.listdir(DATA_DIR)

	pp_tracking_perf    = {} # per participant: list for each condition
	pp_tracking_perf_se = {}
	pp_id_perf    = {}
	pp_id_perf_se = {}
	for i in range(1, num_targets+1):
		pp_tracking_perf[i]    = []
		pp_tracking_perf_se[i] = []
		pp_id_perf[i]    = []
		pp_id_perf_se[i] = []

	for f in files:
		if (json_filename is None and not f.endswith(".json"))\
		   or (json_filename is not None and DATA_DIR+f != json_filename):
			continue
		with open(DATA_DIR+f) as fp: json_data = json.load(fp)
		num_practice_trials = int(json_data["session_details"]["num_practice_trials"])
		all_trials = json_data["all_trial_data"]
		non_practice_trials = all_trials[num_practice_trials:]

		tracking_accuracies = {}
		id_accuracies       = {}
		for i in range(1, num_targets+1):
			tracking_accuracies[i] = []
			id_accuracies[i] = []

		for trial in non_practice_trials:
			trial_num_targets = trial["num_targets"]
			responses   = trial["responses"]
			# if is_bad_trial(trial, trial_num_targets): continue
			num_correct_tracking_responses = 0
			num_correct_id_responses       = 0
			for response in responses:
				r = response
				if r["true_id"] <= trial_num_targets:
					num_correct_tracking_responses += 1
					if  r["true_id"] == r["response_id"]:
						num_correct_id_responses += 1
			tracking_accuracy = 100 * num_correct_tracking_responses / trial_num_targets
			id_accuracy = 100 * num_correct_id_responses / trial_num_targets

			tracking_accuracies[trial_num_targets].append(tracking_accuracy)
			# if tracking_accuracy == 100:
			# 	id_accuracies[trial_num_targets].append(id_accuracy)
			id_accuracies[trial_num_targets].append(id_accuracy)

		for i in range(1, num_targets+1):
			pp_tracking_perf[i].append(np.mean(tracking_accuracies[i]))
			pp_tracking_perf_se[i].append(
				np.std(tracking_accuracies[i], ddof=1)/np.sqrt(len(tracking_accuracies[i]))
			)
			pp_id_perf[i].append(np.mean(id_accuracies[i]))
			pp_id_perf_se[i].append(
				np.std(id_accuracies[i], ddof=1)/np.sqrt(len(id_accuracies[i]))
			)

	print(NUM_BAD_TRIALS, "bad trials found")

	tracking_perf    = []
	tracking_perf_se = []
	id_perf    = []
	id_perf_se = []

	if json_filename is None:
		for i in range(1, num_targets+1):
			tracking_perf.append(np.mean(pp_tracking_perf[i]))
			tracking_perf_se.append(
				np.std(pp_tracking_perf[i], ddof=1) / np.sqrt(len(pp_tracking_perf[i]))
			)
			id_perf.append(np.mean(pp_id_perf[i]))
			id_perf_se.append(
				np.std(pp_id_perf[i], ddof=1) / np.sqrt(len(pp_id_perf[i]))
			)
	else:
		for i in range(1, num_targets+1):
			tracking_perf.append(pp_tracking_perf[i][0])
			tracking_perf_se.append(pp_tracking_perf_se[i][0])
			id_perf.append(np.mean(pp_id_perf[i][0]))
			id_perf_se.append(pp_id_perf_se[i][0])

	print(tracking_perf)

	tracking_correct_count    = [0]*num_targets
	tracking_correct_count_se = [0]*num_targets
	id_correct_count    = [0]*num_targets
	id_correct_count_se = [0]*num_targets
	for i in range(1, num_targets+1):
		tracking_correct_count[i-1]    = tracking_perf[i-1]*i/100
		tracking_correct_count_se[i-1] = tracking_perf_se[i-1]*i/100
		id_correct_count[i-1]    = id_perf[i-1]*i/100
		id_correct_count_se[i-1] = id_perf_se[i-1]*i/100

	plt.clf()
	title = ("Human Performance:\nAccuracy vs Number of targets\n(sigma=1.5, " +\
			   ("combined" if json_filename is None else json_filename) + ")")
	plt.title(title)

	plt.errorbar(
		x=np.arange(1, num_targets+1),
		y=tracking_perf,
		yerr = tracking_perf_se,
		label="Tracking Performance"
	)
	plt.errorbar(
		x=np.arange(1, num_targets+1),
		y=id_perf,
		yerr=id_perf_se,
		label="ID Performance"
	)

	plt.ylim(0, 100)
	plt.xlabel("Number of targets")
	plt.ylabel("Accuracy")

	filename = ("" if json_filename is None else "individual-plots/") \
		+ "exp-human-accuracy-targets-id-" \
		+ ("combined" if json_filename is None else
		   os.path.splitext(os.path.basename(json_filename))[0])
	plt.legend()
	write_plot_file(
		filename = filename,
		title = title,
		xlabel = "Number of targets",
		ylabel = "Accuracy",
		ylim = [0, 100],
		plot_type="errorbar",
		data = {
			"Tracking Performance": [np.arange(1, num_targets+1), tracking_perf, tracking_perf_se],
			"ID Performance": [np.arange(1, num_targets+1), id_perf, id_perf_se],

		}
	)
	# plt.show()

	plt.clf()
	title = ("Human Performance:\nCorrect response counts vs Number of targets\n(sigma=1.5, " +\
			 ("combined" if json_filename is None else json_filename) + ")")
	plt.title(title)

	plt.errorbar(
		x=np.arange(1, num_targets+1),
		y=tracking_correct_count,
		yerr = tracking_correct_count_se,
		label="Tracking Performance"
	)
	plt.errorbar(
		x=np.arange(1, num_targets+1),
		y=id_correct_count,
		yerr=id_correct_count_se,
		label="ID Performance"
	)
	plt.ylim(0, 8)
	plt.xlabel("Number of targets")
	plt.ylabel("Number of correct responses")
	plt.legend()
	# plt.show()
	filename = ("" if json_filename is None else "individual-plots/")\
		+ "exp-human-correct-targets-id-" \
		+ ("combined" if json_filename is None else
		   os.path.splitext(os.path.basename(json_filename))[0])

	write_plot_file(
		filename = filename,
		title = title,
		xlabel = "Number of targets",
		ylabel = "Number of correct responses",
		ylim = [0, 8],
		plot_type="errorbar",
		data = {
			"Tracking Performance": [
				np.arange(1, num_targets+1), tracking_correct_count, tracking_correct_count_se
			],
			"ID Performance": [
				np.arange(1, num_targets+1), id_correct_count, id_correct_count_se
			],
		}
	)

	# plt.show()

	# host.legend()
	# plt.show()
	# plt.clf()
	# # https://matplotlib.org/stable/gallery/axisartist/demo_parasite_axes.html
	# fig = plt.figure()
	# host = fig.add_axes([0.15, 0.1, 0.65, 0.8], axes_class=HostAxes)
	# par = ParasiteAxes(host, sharex=host)
	# host.parasites.append(par)
	# host.axis["right"].set_visible(False)
	# par.axis["right"].set_visible(True)
	# par.axis["right"].major_ticklabels.set_visible(True)
	# par.axis["right"].label.set_visible(True)
	# host.set_ylim(0,100)
	# par.set_ylim(0,8)

	# host.errorbar(
	# 	x=np.arange(1, num_targets+1),
	# 	y=tracking_perf,
	# 	yerr = tracking_perf_se,
	# 	label="Tracking Performance"
	# )
	# par.errorbar(
	# 	x=np.arange(1, num_targets+1),
	# 	y=id_perf,
	# 	yerr=id_perf_se,
	# 	label="ID Performance"
	# )

	# host.legend()
	# plt.show()
	# plt.clf()


def plot_correct_id_count(num_targets, json_filename=None):
	files = os.listdir(DATA_DIR)

	correct_id_count = [0]*(num_targets+1)

	for f in files:
		if (json_filename is None and not f.endswith(".json"))\
		   or (json_filename is not None and DATA_DIR+f != json_filename):
			continue
		with open(DATA_DIR+f) as fp: json_data = json.load(fp)
		num_practice_trials = int(json_data["session_details"]["num_practice_trials"])
		all_trials = json_data["all_trial_data"]
		non_practice_trials = all_trials[num_practice_trials:]

		tracking_accuracies = {}
		id_accuracies       = {}
		for i in range(1, num_targets+1):
			tracking_accuracies[i] = []
			id_accuracies[i] = []

		for trial in non_practice_trials:
			trial_num_targets = trial["num_targets"]
			# if trial_num_targets < 3:
			responses   = trial["responses"]
			num_correct_tracking_responses = 0
			num_correct_id_responses       = 0
			for response in responses:
				r = response
				if r["true_id"] <= trial_num_targets:
					num_correct_tracking_responses += 1
					if  r["true_id"] == r["response_id"]:
						num_correct_id_responses += 1
			correct_id_count[num_correct_id_responses] += 1

	plt.clf()
	plt.title(("Combined" if json_filename is None else json_filename))
	plt.bar(
		x=np.arange(0, num_targets+1),
		height=correct_id_count
	)
	plt.show()



def human_model_accuracy_correlation(num_targets=None, json_filename=None):
	# TODO: Incorporate json_filename
	human_file = "plot-data-v2/exp-human-accuracy-targets-id-combined.json"
	model_file = "plot-data-v2/exp-model-accuracy-targets-id-20-custom-bounded-lowest-activation.json"
	with open(human_file) as f:	human_acc = json.load(f)["data"]["Tracking Performance"][1]
	with open(model_file) as f: model_acc = json.load(f)["data"]["Our Tracking Accuracy"][1]

	corr = np.corrcoef(human_acc, model_acc)
	print("Human performance:", list(human_acc))
	print("Model performance:", list(model_acc))
	print("Correlation is: ", corr)




def human_model_kl_divergence(max_num_targets=None, json_filename=None):

	human_performance_distribution = {}
	model_performance_distribution = {}

	def get_human_performance_distribution(num_targets):
		if num_targets in human_performance_distribution:
			return human_performance_distribution[num_targets]
		for i in range(1, max_num_targets+1):
			human_performance_distribution[i] = np.zeros(i+1)
		files = os.listdir(DATA_DIR)

		for f in files:
			if (json_filename is None and not f.endswith(".json"))\
			   or (json_filename is not None and DATA_DIR+f != json_filename):
				continue
			with open(DATA_DIR+f) as fp: json_data = json.load(fp)
			num_practice_trials = int(json_data["session_details"]["num_practice_trials"])
			all_trials = json_data["all_trial_data"]
			non_practice_trials = all_trials[num_practice_trials:]

			for trial in non_practice_trials:
				trial_num_targets = trial["num_targets"]
				responses   = trial["responses"]
				if is_bad_trial(trial, trial_num_targets): continue
				num_correct_tracking_responses = 0
				num_correct_id_responses       = 0
				for response in responses:
					r = response
					if r["true_id"] <= trial_num_targets:
						num_correct_tracking_responses += 1
						if  r["true_id"] == r["response_id"]:
							num_correct_id_responses += 1
				human_performance_distribution[trial_num_targets][num_correct_tracking_responses] += 1
				# id_response_count[trial_num_targets][num_correct_id_responses] += 1

		for i in range(1, max_num_targets+1):
			human_performance_distribution[i] = \
				human_performance_distribution[i] / np.sum(human_performance_distribution[i])

		return human_performance_distribution[num_targets]



	def get_model_performance_distribution(num_targets):
		if num_targets in model_performance_distribution:
			return model_performance_distribution[num_targets]

		model_updates_per_time_step = 2
		grid_side = 720
		per_target_attention = None
		nearest_object_bound = 30
		update_strategy = "lowest"

		for i in range(1, max_num_targets+1):
			model_performance_distribution[i] = np.zeros(i+1)

		files = os.listdir(DATA_DIR)
		for f in files:
			if (json_filename is None and not f.endswith(".json"))\
			   or (json_filename is not None and DATA_DIR+f != json_filename):
				continue
			with open(DATA_DIR+f) as f_: json_data = json.load(f_)
			all_trial_data  = json_data["all_trial_data"]
			num_simulations = len(all_trial_data)

			# IMPORTANT: We want to maintain the semantics of time_step
			# One time_step denotes one OU update; thus per_ou_update and per_time_step are synonymous

			session_details = json_data["session_details"]
			refreshes_per_ou_update = 1 / session_details["ou_updates_per_refresh"]

			# print("Will update model every {0} time steps".format(model_updates_per_time_step))
			for simulation_idx in range(num_simulations):
			# for simulation_idx in range(2):

				trial_data     = all_trial_data[simulation_idx]
				num_time_steps = trial_data["num_time_steps"]
				trial_num_targets = trial_data["num_targets"]
				# if is_bad_trial(trial_data, trial_num_targets): continue

				env = ExperimentalEnvironment(
					shape = (grid_side, grid_side),
					trial_data = trial_data,
				)
				env.initialize_random()
				our_model = OurMOTModel(
					trial_num_targets,
					per_target_attention = per_target_attention,
					nearest_object_bound = nearest_object_bound
				)
				our_model.process_env(env, observe_targets=True)
				model_updates_so_far = 0

				for t in range(num_time_steps):
					for _ in range(math.ceil(max(refreshes_per_ou_update, model_updates_per_time_step))):
						if env.is_trial_done(): continue
						if (env.time_elapsed < (t + 1) * refreshes_per_ou_update):
							env.update_object_map()
						if model_updates_so_far < (t + 1) * model_updates_per_time_step:
							our_model.process_env(env, strategy=update_strategy)
							model_updates_so_far += 1

				num_correct_targets = round(trial_num_targets * evaluate_tracking(env, our_model))
				num_correct_ids = round(trial_num_targets * evaluate_id(env, our_model))

				model_performance_distribution[trial_num_targets][num_correct_targets] += 1
				# id_response_counts[trial_num_targets][num_correct_ids] += 1

		for i in range(1, max_num_targets+1):
			model_performance_distribution[i] = \
				model_performance_distribution[i] / np.sum(model_performance_distribution[i])

		return model_performance_distribution[num_targets]



	for num_targets in range(1, max_num_targets+1):
		human_distribution = get_human_performance_distribution(num_targets)
		model_distribution = get_model_performance_distribution(num_targets)
		kl_score = np.sum(
			np.nan_to_num(
				model_distribution * np.log(model_distribution / human_distribution),
				nan=0.0,
				posinf=0,
				neginf=0,
			),
		)
		print("""Num targets: {0}
  Human Performance Distribution: {1}
  Model Performance Distribution: {2}
  KL Divergence Score: {3}
		""".format(
			num_targets,
			human_distribution,
			model_distribution,
			kl_score
		))


def human_boxplot(num_targets, json_filename=None):
	data = pd.read_csv("data_summary.csv")
	tracking_data = dict()
	id_data = dict()
	for i in range(1,num_targets+1):
		d = data[data["trial_num_targets"]==i]
		tracking_data[i] = 100*d["tracking_accuracy"]
		id_data[i] = 100*d["id_accuracy"]

	plt.rcParams.update({
		"font.size": 8,
		"lines.markersize": 3,
		"lines.linewidth": 1,
		"axes.linewidth": 1,
		"legend.borderpad": 0.2,
		"legend.fontsize": 7
	})
	# plt.figure(figsize=(3,2))
	plt.figure(figsize=(2,2))
	# for i in range(num_targets):
	def plot_box(data, color=None, displacement=None):
		if displacement==None:
			positions = list(np.arange(1, 1+len(data)))
		else:
			positions = list(np.arange(1+displacement, 1+len(data)+displacement))
		box = plt.boxplot(
			data,
			patch_artist=True, widths=0.3,
			positions=positions, manage_ticks=False
		)
		if color == None: return
		for item in ['boxes', 'whiskers', 'fliers', 'medians', 'caps']:
			plt.setp(box[item], color=color)
		plt.setp(box["boxes"], facecolor=color)
		plt.setp(box["fliers"], markeredgecolor=color)
		return box

	b1 = plot_box(tracking_data.values(), "C0", -0.15)
	# plot_box(id_data.values(), "#ffa756", +0.15)
	# b2 = plot_box(id_data.values(), "#d8863b", +0.15)
	b2 = plot_box(id_data.values(), "C1", +0.15)
	# sns.boxplot(
	# 	data = list(tracking_data.values()),
	# 	color="C0", width=0.2, positions=range(1,9)
	# )
	# sns.boxplot(
	# 	data = list(id_data.values()),
	# 	color="C1", width=0.2
	# )

	plt.legend(
		[b1["boxes"][0], b2["boxes"][0]],
		["Tracking", "ID"]
	)
	plt.ylabel("Accuracy")
	plt.xlabel("Number of Targets")
	plt.xticks((2,4,6,8))
	plt.ylim(0,100)
	plt.tight_layout(pad=0)
	plt.savefig("exp-boxplot.pgf", bbox_inches="tight")
	plt.show()


if __name__ == "__main__":

	PLOT_NAMES = [
		"plot_tracking_vs_id",
		"plot_correct_id_count",
		"human_model_accuracy_correlation",
		"human_model_kl_divergence",
		"human_boxplot"
	]

	parser = argparse.ArgumentParser()
	parser.add_argument(
		"--file",
		help="path of the json file to plot; considers all the json files in {0} if the argument is unsupplied".format(DATA_DIR),
		default=None,
		required=False
	)
	parser.add_argument(
		"--plot",
		help="name of the plots to plot; plots all if unsupplied",
		choices=PLOT_NAMES,
		default=None,
		required=False
	)

	args = parser.parse_args()
	if args.plot is not None:
		PLOT_NAMES = [args.plot]
	for plot in PLOT_NAMES:
		globals()[plot](8, (args.file if hasattr(args, "file") else None))

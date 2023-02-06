
import os
import numpy as np
import random
import matplotlib as mpl
import matplotlib.pyplot as plt
from cycler import cycler

from tasks import simulate_mot, simulate_mit, simulate_mot_using_experimental_data, NUM_BAD_TRIALS
# from OUSpeedOnSameGrid import simulate
import json

PLOT_DIR = "plot-data-v2/"

"""

Structure of the plot data file:
- title
- xlabel
- ylabel
- plot_type
- ylim
- data

"""

def write_plot_file(filename, title, xlabel, ylabel, ylim, plot_type, data, ):
	"data: a dictionary mapping label to a list containing x and y data, and optionally errorbar data"
	# print(PLOT_DIR+filename+".json")
	with open(PLOT_DIR+filename+".json", "w") as f:
		json.dump({
			"title": title,
			"xlabel": xlabel,
			"ylabel": ylabel,
			"plot_type": plot_type,
			"ylim": ylim,
			"data": data
		}, f, indent=True, default = lambda x : x.tolist())
	return

def get_filename(base_name,
				 accuracy=None,
				 time=None,
				 sigma=None,
				 per_target_attention=None,
				 last_step_uses_nearest_object=True,
				 nearest_object_bound=None,
				 update_strategy=""):

	prefix = ("custom" if per_target_attention is None else "constant")

	# ignore bounded varying for now
	if per_target_attention is not None: update_scheme = ""
	elif last_step_uses_nearest_object \
	   and nearest_object_bound is None: update_scheme = "nearest"
	elif last_step_uses_nearest_object: update_scheme = "bounded"
	else: update_scheme = "random"

	accuracy = ("" if accuracy is None else str(int(accuracy)))
	sigma    = ("" if sigma is None else "sigma-"+str(int(sigma*10)))
	time     = ("" if time is None else "time-"+str(int(time)))

	parts = [base_name, prefix, update_scheme, accuracy, sigma, time, update_strategy, "activation"]
	parts = list(part for part in parts if part != "")

	return "-".join(parts)


def plot_acc_wrt_targets(
		grid_side, num_simulations, num_time_steps, num_objects, max_num_targets,
		k, lm, sigma, per_target_attention=None,
		nearest_object_bound=None, update_strategy="random"
):
	our_accuracy_list      = []
	our_accuracy_list_se   = []
	chance_accuracy_list  = []
	chance_accuracy_list_se = []
	num_target_list = []
	for num_targets in range(1, max_num_targets+1):
		_, our_accuracies = \
			simulate_mot(grid_side, num_simulations, num_time_steps, num_objects, num_targets,
						 k = k, lm = lm, sigma = sigma,
						 per_target_attention = per_target_attention,
						 nearest_object_bound = nearest_object_bound,
						 update_strategy = update_strategy)
		_, chance_accuracies = \
			simulate_mot(grid_side, num_simulations, num_time_steps, num_objects, num_targets,
						 k = k, lm = lm, sigma = sigma,
						 per_target_attention = per_target_attention,
						 nearest_object_bound = 0,
						 update_strategy = update_strategy)
		# chance_accuracies = [0]*num_simulations

		# our_accuracies = 1 - np.asarray(our_accuracies)
		our_accuracy_list.append(np.mean(our_accuracies))
		our_accuracy_list_se.append(np.std(our_accuracies, ddof=1)/np.sqrt(num_simulations))

		# chance_accuracies = 1 - np.asarray(chance_accuracies)
		chance_accuracy_list.append(np.mean(chance_accuracies))
		chance_accuracy_list_se.append(np.std(chance_accuracies, ddof=1)/np.sqrt(num_simulations))

		num_target_list.append(num_targets)

	num_target_list     = np.asarray(num_target_list)
	our_accuracy_list   = 100*np.asarray(our_accuracy_list)
	our_accuracy_list_se = 100*np.asarray(our_accuracy_list_se)
	chance_accuracy_list = 100*np.asarray(chance_accuracy_list)
	chance_accuracy_list_se = 100*np.asarray(chance_accuracy_list_se)

	filename = get_filename(
		"accuracy-targets",
		per_target_attention = per_target_attention,
		nearest_object_bound = nearest_object_bound,
		update_strategy = update_strategy
	)
	print(filename)

	plt.clf()
	plt.title("Accuracy vs Number of targets (sigma={0})".format(sigma))

	plt.errorbar(num_target_list, our_accuracy_list, our_accuracy_list_se,
				 label="Our Accuracy")
	plt.errorbar(num_target_list, chance_accuracy_list, chance_accuracy_list_se,
				 label="Chance Accuracy")
	plt.xlabel("Number of targets ({0} objects)".format(num_objects))
	plt.ylabel("Accuracy")
	plt.ylim(0,100)
	plt.legend()
	plt.show()

	write_plot_file(
		filename = filename,
		title = "Accuracy vs Number of targets (sigma={0})".format(sigma),
		xlabel = "Number of targets ({0} objects)".format(num_objects),
		ylabel = "Accuracy",
		ylim = [0,100],
		plot_type = "errorbar",
		data = {
			"Our Accuracy": [num_target_list, our_accuracy_list, our_accuracy_list_se],
			"Chance Accuracy": [num_target_list, chance_accuracy_list, chance_accuracy_list_se]
		}
	)



def plot_acc_wrt_time(
		grid_side, num_simulations, max_time_steps, num_objects, num_targets,
		k, lm, sigma,
		per_target_attention=None,
		nearest_object_bound=None,
		update_strategy="random"
):
	"""
	Plots percentage of targets that were tracked wrt time
	"""
	our_accuracy_list      = []
	our_accuracy_list_se   = []
	num_time_steps_list    = []
	for num_time_steps in range(10, max_time_steps, (max_time_steps-10)//5):
		_, our_accuracies = \
			simulate_mot(grid_side, num_simulations, num_time_steps, num_objects, num_targets,
						 k = k, lm = lm, sigma = sigma,
						 nearest_object_bound = nearest_object_bound,
						 per_target_attention = per_target_attention,
						 update_strategy=update_strategy)

		our_accuracy_list.append(np.mean(our_accuracies))
		our_accuracy_list_se.append(np.std(our_accuracies, ddof=1)/np.sqrt(num_simulations))

		num_time_steps_list.append(num_time_steps)

	our_accuracy_list        = 100*np.asarray(our_accuracy_list)
	our_accuracy_list_se     = 100*np.asarray(our_accuracy_list_se)

	filename = get_filename(
		"accuracy-time",
		sigma = sigma,
		per_target_attention = per_target_attention,
		nearest_object_bound = nearest_object_bound,
		update_strategy = update_strategy
	)
	print(filename)
	plt.clf()
	plt.errorbar(num_time_steps_list, our_accuracy_list, our_accuracy_list_se,
				 label="Our Model")
	plt.title("Accuracy vs Time of tracking\n({0} objects, {1} targets, sigma={2})"
			  .format(num_objects, num_targets, sigma))
	plt.xlabel("Number of time steps")
	plt.ylabel("Accuracy")
	plt.ylim(0,100)
	plt.legend()
	plt.show()
	write_plot_file(
		filename = filename,
		title = "Accuracy vs Time of tracking\n({0} objects, {1} targets, sigma={2})"
			.format(num_objects, num_targets, sigma),
		xlabel = "Number of time steps",
		ylabel = "Accuracy",
		ylim = [0,100],
		plot_type = "errorbar",
		data = {
			"Our Model": [
				num_time_steps_list,
				our_accuracy_list,
				our_accuracy_list_se,
			],
		}
	)

	return our_accuracy_list


def plot_acc_wrt_sigma(
		grid_side, num_simulations, num_time_steps, num_objects, num_targets,
		k, lm, max_sigma=None, sigma_list=None,
		per_target_attention=None,
		nearest_object_bound=None,
		update_strategy="random"
):
	"""
	Plots percentage of targets that were tracked wrt time
	"""
	our_accuracy_list_large    = []
	our_accuracy_list_large_se = []
	our_accuracy_list_small    = []
	our_accuracy_list_small_se = []
	chance_accuracy_list       = []
	chance_accuracy_list_se    = []
	min_sigma = 0.1
	int_sigma = ((max_sigma - min_sigma)/10 if max_sigma is not None else None)
	sigma_list = (np.arange(max_sigma,min_sigma,-int_sigma) if sigma_list is None else sigma_list)
	for sigma in sigma_list:
		_, our_accuracies_large = \
			simulate_mot(grid_side, num_simulations, num_time_steps, num_objects, num_targets,
						 k = k, lm = lm, sigma = sigma,
						 nearest_object_bound = nearest_object_bound,
						 per_target_attention = per_target_attention,
						 update_strategy = update_strategy)
		_, our_accuracies_small = \
			simulate_mot(grid_side//4, num_simulations, num_time_steps, num_objects, num_targets,
						 k = k, lm = lm, sigma = sigma,
						 nearest_object_bound = nearest_object_bound,
						 per_target_attention = per_target_attention,
						 update_strategy = update_strategy)
		_, chance_accuracies = \
			simulate_mot(grid_side, num_simulations, num_time_steps, num_objects, num_targets,
						 k = k, lm = lm, sigma = sigma,
						 nearest_object_bound = 0,
						 per_target_attention = per_target_attention,
						 update_strategy = update_strategy)

		our_accuracy_list_large.append(np.mean(our_accuracies_large))
		our_accuracy_list_large_se.append(np.std(our_accuracies_large, ddof=1)/np.sqrt(num_simulations))

		our_accuracy_list_small.append(np.mean(our_accuracies_small))
		our_accuracy_list_small_se.append(np.std(our_accuracies_small, ddof=1)/np.sqrt(num_simulations))

		chance_accuracy_list.append(np.mean(chance_accuracies))
		chance_accuracy_list_se.append(np.std(chance_accuracies, ddof=1)/np.sqrt(num_simulations))


	our_accuracy_list_large      = 100*np.asarray(our_accuracy_list_large)
	our_accuracy_list_large_se   = 100*np.asarray(our_accuracy_list_large_se)
	our_accuracy_list_small      = 100*np.asarray(our_accuracy_list_small)
	our_accuracy_list_small_se   = 100*np.asarray(our_accuracy_list_small_se)
	chance_accuracy_list         = 100*np.asarray(chance_accuracy_list)
	chance_accuracy_list_se      = 100*np.asarray(chance_accuracy_list_se)

	filename = get_filename(
		"accuracy-sigma",
		sigma = sigma,
		per_target_attention = per_target_attention,
		nearest_object_bound = nearest_object_bound,
		update_strategy = update_strategy
	)
	print(filename)
	plt.clf()
	plt.errorbar(sigma_list, our_accuracy_list_large, our_accuracy_list_large_se,
				 label="Our Model ({0}x{0})".format(grid_side))
	plt.errorbar(sigma_list, our_accuracy_list_small, our_accuracy_list_small_se,
				 label="Our Model ({0}x{0})".format(grid_side//4))
	plt.errorbar(sigma_list, chance_accuracy_list, chance_accuracy_list_se,
				 label="Chance Performance")
	plt.title("Accuracy vs Object Speeds\n({0} objects, {1} targets)"
			  .format(num_objects, num_targets))
	plt.xlabel("Object Speed (sigma)")
	plt.ylabel("Accuracy")
	plt.ylim(0,100)
	plt.legend()
	plt.show()
	write_plot_file(
		filename = filename,
		title = "Accuracy vs Object Speeds\n({0} objects, {1} targets)"\
				.format(num_objects, num_targets),
		xlabel = "Object Speed (sigma)",
		ylabel = "Accuracy",
		ylim = [0,100],
		plot_type = "errorbar",
		data = {
			"Our Model ({0}x{0})".format(grid_side): [
				sigma_list,
				our_accuracy_list_large,
				our_accuracy_list_large_se,
			],
			"Our Model ({0}x{0})".format(grid_side//4): [
				sigma_list,
				our_accuracy_list_small,
				our_accuracy_list_small_se,
			],
			"Chance Performance": [
				sigma_list,
				chance_accuracy_list,
				chance_accuracy_list_se,
			],
		}
	)


def plot_momit_acc_wrt_sigma(
		grid_side, num_simulations, num_time_steps, num_objects, num_targets,
		k, lm, max_sigma=None, sigma_list=None,
		per_target_attention=None,
		nearest_object_bound=None,
		update_strategy="random"
):
	"""
	Plots percentage of targets that were tracked wrt time
	"""
	momit_accuracy_list    = []
	momit_accuracy_list_se = []
	our_accuracy_list      = []
	our_accuracy_list_se   = []
	our_nob_accuracy_list     = []
	our_nob_accuracy_list_se  = []
	chance_accuracy_list      = []
	chance_accuracy_list_se   = []
	min_sigma = 0.1
	int_sigma = ((max_sigma - min_sigma)/10 if max_sigma is not None else None)
	sigma_list = (np.arange(max_sigma,min_sigma,-int_sigma) if sigma_list is None else sigma_list)
	for sigma in sigma_list:
		momit_accuracies, our_nob_accuracies = \
			simulate_mot(grid_side, num_simulations, num_time_steps, num_objects, num_targets,
						 k = k, lm = lm, sigma = sigma,
						 nearest_object_bound = nearest_object_bound,
						 per_target_attention = per_target_attention,
						 update_strategy = update_strategy)
		_, our_accuracies = \
			simulate_mot(grid_side, num_simulations, num_time_steps, num_objects, num_targets,
						 k = k, lm = lm, sigma = sigma,
						 nearest_object_bound = None,
						 per_target_attention = per_target_attention,
						 update_strategy = update_strategy)
		_, chance_accuracies = \
			simulate_mot(grid_side, num_simulations, num_time_steps, num_objects, num_targets,
						 k = k, lm = lm, sigma = sigma,
						 nearest_object_bound = 0,
						 per_target_attention = per_target_attention,
						 update_strategy = update_strategy)

		momit_accuracy_list.append(np.mean(momit_accuracies))
		momit_accuracy_list_se.append(np.std(momit_accuracies, ddof=1)/np.sqrt(num_simulations))

		our_accuracy_list.append(np.mean(our_accuracies))
		our_accuracy_list_se.append(np.std(our_accuracies, ddof=1)/np.sqrt(num_simulations))

		our_nob_accuracy_list.append(np.mean(our_nob_accuracies))
		our_nob_accuracy_list_se.append(np.std(our_nob_accuracies, ddof=1)/np.sqrt(num_simulations))

		chance_accuracy_list.append(np.mean(chance_accuracies))
		chance_accuracy_list_se.append(np.std(chance_accuracies, ddof=1)/np.sqrt(num_simulations))


	momit_accuracy_list      = 100*np.asarray(momit_accuracy_list)
	momit_accuracy_list_se   = 100*np.asarray(momit_accuracy_list_se)
	our_accuracy_list        = 100*np.asarray(our_accuracy_list)
	our_accuracy_list_se     = 100*np.asarray(our_accuracy_list_se)
	our_nob_accuracy_list    = 100*np.asarray(our_nob_accuracy_list)
	our_nob_accuracy_list_se = 100*np.asarray(our_nob_accuracy_list_se)
	chance_accuracy_list     = 100*np.asarray(chance_accuracy_list)
	chance_accuracy_list_se  = 100*np.asarray(chance_accuracy_list_se)

	filename = get_filename(
		"accuracy-sigma-momit",
		sigma = sigma,
		per_target_attention = per_target_attention,
		nearest_object_bound = nearest_object_bound,
		update_strategy = update_strategy
	)
	print(filename)
	plt.clf()
	plt.errorbar(sigma_list, momit_accuracy_list, momit_accuracy_list_se,
				 label="MOMIT")
	plt.errorbar(sigma_list, our_accuracy_list, our_accuracy_list_se,
				 label="Our Model (w/o nob)")
	plt.errorbar(sigma_list, our_nob_accuracy_list, our_nob_accuracy_list_se,
				 label="Our Model (with nob)")
	plt.errorbar(sigma_list, chance_accuracy_list, chance_accuracy_list_se,
				 label="Chance Performance")
	plt.title("Accuracy vs Object Speeds\n({0} objects, {1} targets)"
			  .format(num_objects, num_targets))
	plt.xlabel("Object Speed (sigma)")
	plt.ylabel("Accuracy")
	plt.ylim(0,100)
	plt.legend()
	plt.show()
	write_plot_file(
		filename = filename,
		title = "Accuracy vs Object Speeds\n({0} objects, {1} targets)"\
				.format(num_objects, num_targets),
		xlabel = "Object Speed (sigma)",
		ylabel = "Accuracy",
		ylim = [0,100],
		plot_type = "errorbar",
		data = {
			"MOMIT": [
				sigma_list,
				momit_accuracy_list,
				momit_accuracy_list_se,
			],
			"Our Model (w/o nob)": [
				sigma_list,
				our_accuracy_list,
				our_accuracy_list_se,
			],
			"Our Model (with nob)": [
				sigma_list,
				our_nob_accuracy_list,
				our_nob_accuracy_list_se,
			],
			"Chance Performance": [
				sigma_list,
				chance_accuracy_list,
				chance_accuracy_list_se,
			],
		}
	)

	return our_accuracy_list


def plot_momit_acc_wrt_targets(
		grid_side, num_simulations, num_time_steps, num_objects, max_num_targets,
		k, lm, sigma, per_target_attention=None, last_step_uses_nearest_object=True,
		nearest_object_bound=None, update_strategy="random"
):
	momit_accuracy_list    = []
	momit_accuracy_list_se = []
	our_accuracy_list      = []
	our_accuracy_list_se   = []
	our_nob_accuracy_list  = []
	our_nob_accuracy_list_se = []
	num_target_list = []
	for num_targets in range(1, max_num_targets+1):
		momit_accuracies, our_accuracies = \
			simulate_mot(grid_side, num_simulations, num_time_steps, num_objects, num_targets,
						 k = k, lm = lm, sigma = sigma,
						 per_target_attention = per_target_attention,
						 nearest_object_bound = None,
						 update_strategy = update_strategy)
		_, our_nob_accuracies = \
			simulate_mot(grid_side, num_simulations, num_time_steps, num_objects, num_targets,
						 k = k, lm = lm, sigma = sigma,
						 per_target_attention = per_target_attention,
						 nearest_object_bound = nearest_object_bound,
						 update_strategy = update_strategy)

		momit_accuracy_list.append(np.mean(momit_accuracies))
		momit_accuracy_list_se.append(np.std(momit_accuracies, ddof=1)/np.sqrt(num_simulations))

		our_accuracy_list.append(np.mean(our_accuracies))
		our_accuracy_list_se.append(np.std(our_accuracies, ddof=1)/np.sqrt(num_simulations))

		our_nob_accuracy_list.append(np.mean(our_nob_accuracies))
		our_nob_accuracy_list_se.append(np.std(our_nob_accuracies, ddof=1)/np.sqrt(num_simulations))

		num_target_list.append(num_targets)

	num_target_list     = np.asarray(num_target_list)
	momit_accuracy_list = 100*np.asarray(momit_accuracy_list)
	momit_accuracy_list_se = 100*np.asarray(momit_accuracy_list_se)
	our_accuracy_list   = 100*np.asarray(our_accuracy_list)
	our_accuracy_list_se = 100*np.asarray(our_accuracy_list_se)
	our_nob_accuracy_list = 100*np.asarray(our_nob_accuracy_list)
	our_nob_accuracy_list_se = 100*np.asarray(our_nob_accuracy_list_se)

	filename = get_filename(
		"accuracy-targets-momit",
		per_target_attention = per_target_attention,
		nearest_object_bound = nearest_object_bound,
		update_strategy = update_strategy
	)
	print(filename)

	plt.clf()
	plt.errorbar(num_target_list, momit_accuracy_list, momit_accuracy_list_se,
				 label="MOMIT Accuracy")
	plt.errorbar(num_target_list, our_accuracy_list, our_accuracy_list_se,
				 label="Our Accuracy (w/o nob)")
	plt.errorbar(num_target_list, our_nob_accuracy_list, our_nob_accuracy_list_se,
				 label="Our Accuracy (with nob)")
	plt.xlabel("Number of targets ({0} objects)".format(num_objects))
	plt.ylabel("Accuracy")
	plt.ylim(0,100)
	plt.legend()
	plt.show()

	write_plot_file(
		filename = filename,
		title = "Accuracy vs Number of targets",
		xlabel = "Number of targets ({0} objects)".format(num_objects),
		ylabel = "Accuracy",
		ylim = [0,100],
		plot_type = "errorbar",
		data = {
			"MOMIT Accuracy": [num_target_list, momit_accuracy_list, momit_accuracy_list_se],
			"Our Accuracy (w/o nob)": [num_target_list, our_accuracy_list, our_accuracy_list_se],
			"Our Accuracy (with nob)": [num_target_list, our_nob_accuracy_list, our_nob_accuracy_list_se]
		}
	)

	return momit_accuracy_list, our_accuracy_list


def plot_momit_acc_wrt_time(
		grid_side, num_simulations, max_time_steps, num_objects, num_targets,
		k, lm, sigma,
		per_target_attention=None,
		nearest_object_bound=None,
		update_strategy="random"
):
	"""
	Plots percentage of targets that were tracked wrt time
	"""
	momit_accuracy_list    = []
	momit_accuracy_list_se = []
	our_accuracy_list      = []
	our_accuracy_list_se   = []
	our_nob_accuracy_list  = []
	our_nob_accuracy_list_se = []
	num_time_steps_list    = []
	for num_time_steps in range(10, max_time_steps, (max_time_steps-10)//5):
		momit_accuracies, our_accuracies = \
			simulate_mot(grid_side, num_simulations, num_time_steps, num_objects, num_targets,
						 k = k, lm = lm, sigma = sigma,
						 nearest_object_bound = None,
						 per_target_attention = per_target_attention,
						 update_strategy=update_strategy)
		_, our_nob_accuracies = \
			simulate_mot(grid_side, num_simulations, num_time_steps, num_objects, num_targets,
						 k = k, lm = lm, sigma = sigma,
						 nearest_object_bound = nearest_object_bound,
						 per_target_attention = per_target_attention,
						 update_strategy=update_strategy)

		momit_accuracy_list.append(np.mean(momit_accuracies))
		momit_accuracy_list_se.append(np.std(momit_accuracies, ddof=1)/np.sqrt(num_simulations))

		our_accuracy_list.append(np.mean(our_accuracies))
		our_accuracy_list_se.append(np.std(our_accuracies, ddof=1)/np.sqrt(num_simulations))

		our_nob_accuracy_list.append(np.mean(our_nob_accuracies))
		our_nob_accuracy_list_se.append(np.std(our_nob_accuracies, ddof=1)/np.sqrt(num_simulations))

		num_time_steps_list.append(num_time_steps)

	momit_accuracy_list      = 100*np.asarray(momit_accuracy_list)
	momit_accuracy_list_se   = 100*np.asarray(momit_accuracy_list_se)
	our_accuracy_list        = 100*np.asarray(our_accuracy_list)
	our_accuracy_list_se     = 100*np.asarray(our_accuracy_list_se)
	our_nob_accuracy_list    = 100*np.asarray(our_nob_accuracy_list)
	our_nob_accuracy_list_se = 100*np.asarray(our_nob_accuracy_list_se)

	filename = get_filename(
		"accuracy-time-momit",
		sigma = sigma,
		per_target_attention = per_target_attention,
		nearest_object_bound = nearest_object_bound,
		update_strategy = update_strategy
	)
	print(filename)
	plt.clf()
	plt.errorbar(num_time_steps_list, momit_accuracy_list, momit_accuracy_list_se, label="MOMIT")
	plt.errorbar(num_time_steps_list, our_accuracy_list, our_accuracy_list_se,
				 label="Our Model (w/o nob)")
	plt.errorbar(num_time_steps_list, our_nob_accuracy_list, our_nob_accuracy_list_se,
				 label="Our Model (with nob)")
	plt.title("Accuracy vs Time of tracking\n({0} objects, {1} targets, sigma={2})"
			  .format(num_objects, num_targets, sigma))
	plt.xlabel("Number of time steps")
	plt.ylabel("Accuracy")
	plt.ylim(0,100)
	plt.legend()
	plt.show()
	write_plot_file(
		filename = filename,
		title = "Accuracy vs Time of tracking\n({0} objects, {1} targets, sigma={2})"
			.format(num_objects, num_targets, sigma),
		xlabel = "Number of time steps",
		ylabel = "Accuracy",
		ylim = [0,100],
		plot_type = "errorbar",
		data = {
			"MOMIT": [
				num_time_steps_list,
				momit_accuracy_list,
				momit_accuracy_list_se,
			],
			"Our Model (w/o nob)": [
				num_time_steps_list,
				our_accuracy_list,
				our_accuracy_list_se,
			],
			"Our Model (with nob)": [
				num_time_steps_list,
				our_nob_accuracy_list,
				our_nob_accuracy_list_se,
			]
		}
	)

	return momit_accuracy_list, our_accuracy_list


def plot_both_acc_wrt_targets(
		grid_side, num_simulations, num_time_steps, num_objects, max_num_targets,
		k, lm, sigma, per_target_attention=None, last_step_uses_nearest_object=True,
		nearest_object_bound=None
):
	seq_accuracy_list      = []
	seq_accuracy_list_se   = []
	parallel_accuracy_list  = []
	parallel_accuracy_list_se = []
	chance_accuracy_list    = []
	chance_accuracy_list_se = []
	num_target_list = []
	for num_targets in range(1, max_num_targets+1):
		_, seq_accuracies = \
			simulate_mot(grid_side, num_simulations, num_time_steps, num_objects, num_targets,
						 k = k, lm = lm, sigma = sigma,
						 per_target_attention = per_target_attention,
						 nearest_object_bound = nearest_object_bound,
						 update_strategy = "lowest")
		_, parallel_accuracies = \
			simulate_mot(grid_side, num_simulations, num_time_steps, num_objects, num_targets,
						 k = k, lm = lm, sigma = sigma,
						 per_target_attention = per_target_attention,
						 nearest_object_bound = nearest_object_bound,
						 update_strategy = "random")
		_, chance_accuracies = \
			simulate_mot(grid_side, num_simulations, num_time_steps, num_objects, num_targets,
						 k = k, lm = lm, sigma = sigma,
						 per_target_attention = per_target_attention,
						 nearest_object_bound = 0,
						 update_strategy = "random")

		seq_accuracy_list.append(np.mean(seq_accuracies))
		seq_accuracy_list_se.append(np.std(seq_accuracies, ddof=1)/np.sqrt(num_simulations))

		parallel_accuracy_list.append(np.mean(parallel_accuracies))
		parallel_accuracy_list_se.append(np.std(parallel_accuracies, ddof=1)/np.sqrt(num_simulations))

		chance_accuracy_list.append(np.mean(chance_accuracies))
		chance_accuracy_list_se.append(np.std(chance_accuracies, ddof=1)/np.sqrt(num_simulations))

		num_target_list.append(num_targets)

	num_target_list     = np.asarray(num_target_list)
	seq_accuracy_list   = 100*np.asarray(seq_accuracy_list)
	seq_accuracy_list_se = 100*np.asarray(seq_accuracy_list_se)
	parallel_accuracy_list = 100*np.asarray(parallel_accuracy_list)
	parallel_accuracy_list_se = 100*np.asarray(parallel_accuracy_list_se)
	chance_accuracy_list = 100*np.asarray(chance_accuracy_list)
	chance_accuracy_list_se = 100*np.asarray(chance_accuracy_list_se)

	filename = get_filename(
		"accuracy-targets-both",
		per_target_attention = per_target_attention,
		nearest_object_bound = nearest_object_bound,
	)
	print(filename)

	plt.clf()
	plt.errorbar(num_target_list, seq_accuracy_list, seq_accuracy_list_se,
				 label="Sequential Strategy")
	plt.errorbar(num_target_list, parallel_accuracy_list, parallel_accuracy_list_se,
				 label="Parallel Strategy")
	plt.errorbar(num_target_list, chance_accuracy_list, chance_accuracy_list_se,
				 label="Chance Performance")
	plt.xlabel("Number of targets ({0} objects)".format(num_objects))
	plt.ylabel("Accuracy")
	plt.ylim(0,100)
	plt.legend()
	plt.show()

	write_plot_file(
		filename = filename,
		title = "Accuracy vs Number of targets",
		xlabel = "Number of targets ({0} objects)".format(num_objects),
		ylabel = "Accuracy",
		ylim = [0,100],
		plot_type = "errorbar",
		data = {
			"Sequential Strategy": [num_target_list, seq_accuracy_list, seq_accuracy_list_se],
			"Parallel Strategy": [num_target_list, parallel_accuracy_list, parallel_accuracy_list_se],
			"Chance Performance": [num_target_list, chance_accuracy_list, chance_accuracy_list_se]
		}
	)


def plot_both_acc_wrt_time(
		grid_side, num_simulations, max_time_steps, num_objects, num_targets,
		k, lm, sigma, per_target_attention=None, last_step_uses_nearest_object=True,
		nearest_object_bound=None
):
	seq_accuracy_list      = []
	seq_accuracy_list_se   = []
	parallel_accuracy_list  = []
	parallel_accuracy_list_se = []
	num_time_steps_list    = []
	for num_time_steps in range(10, max_time_steps, (max_time_steps-10)//5):
		_, seq_accuracies = \
			simulate_mot(grid_side, num_simulations, num_time_steps, num_objects, num_targets,
						 k = k, lm = lm, sigma = sigma,
						 per_target_attention = per_target_attention,
						 nearest_object_bound = nearest_object_bound,
						 update_strategy = "lowest")
		_, parallel_accuracies = \
			simulate_mot(grid_side, num_simulations, num_time_steps, num_objects, num_targets,
						 k = k, lm = lm, sigma = sigma,
						 per_target_attention = per_target_attention,
						 nearest_object_bound = nearest_object_bound,
						 update_strategy = "random")

		seq_accuracy_list.append(np.mean(seq_accuracies))
		seq_accuracy_list_se.append(np.std(seq_accuracies, ddof=1)/np.sqrt(num_simulations))

		parallel_accuracy_list.append(np.mean(parallel_accuracies))
		parallel_accuracy_list_se.append(np.std(parallel_accuracies, ddof=1)/np.sqrt(num_simulations))

		num_time_steps_list.append(num_time_steps)

	num_time_steps_list     = np.asarray(num_time_steps_list)
	seq_accuracy_list   = 100*np.asarray(seq_accuracy_list)
	seq_accuracy_list_se = 100*np.asarray(seq_accuracy_list_se)
	parallel_accuracy_list = 100*np.asarray(parallel_accuracy_list)
	parallel_accuracy_list_se = 100*np.asarray(parallel_accuracy_list_se)

	filename = get_filename(
		"accuracy-time-both",
		sigma = sigma,
		per_target_attention = per_target_attention,
		nearest_object_bound = nearest_object_bound,
	)
	print(filename)
	plt.clf()
	plt.errorbar(num_time_steps_list, seq_accuracy_list, seq_accuracy_list_se,
				 label="Sequential Strategy")
	plt.errorbar(num_time_steps_list, parallel_accuracy_list, parallel_accuracy_list_se,
				 label="Parallel Strategy")
	plt.title("Accuracy vs Time of tracking\n({0} objects, {1} targets, sigma={2})"
			  .format(num_objects, num_targets, sigma))
	plt.xlabel("Number of time steps")
	plt.ylabel("Accuracy")
	plt.ylim(0,100)
	plt.legend()
	plt.show()
	write_plot_file(
		filename = filename,
		title = "Accuracy vs Time of tracking\n({0} objects, {1} targets, sigma={2})"
			.format(num_objects, num_targets, sigma),
		xlabel = "Number of time steps",
		ylabel = "Accuracy",
		ylim = [0,100],
		plot_type = "errorbar",
		data = {
			"Sequential Strategy": [
				num_time_steps_list,
				seq_accuracy_list,
				seq_accuracy_list_se,
			],
			"Parallel Strategy": [
				num_time_steps_list,
				parallel_accuracy_list,
				parallel_accuracy_list_se,
			],
		}
	)


def plot_both_sigma_wrt_targets(
		base_grid_side,
		num_simulations,
		num_time_steps,
		num_objects,
		max_num_targets,
		k, lm, max_sigma=None, sigma_list=None,
		accuracy_threshold = 80,
		per_target_attention=None,
		nearest_object_bound=None
):
	grid_side = base_grid_side
	min_sigma = 0.1
	int_sigma = ((max_sigma - min_sigma)/10 if max_sigma is not None else None)
	sigma_list = (np.arange(max_sigma,min_sigma,-int_sigma) if sigma_list is None else sigma_list)
	seq_sigma_threshold_list     = []
	parallel_sigma_threshold_list = []
	num_targets_list = np.arange(1, max_num_targets+1)
	if type(nearest_object_bound) == list: nob_list = nearest_object_bound
	else: nob_list = [nearest_object_bound] * max_num_targets
	print("num_targets sigma seq_accuracy parallel_accuracy")
	for i in range(max_num_targets):
		num_targets = num_targets_list[i]
		nearest_object_bound = nob_list[i]
		for sigma in sigma_list:
			_, seq_accuracies = \
				simulate_mot(grid_side, num_simulations, num_time_steps, num_objects,
							 num_targets, k, lm, sigma,
							 per_target_attention = per_target_attention,
							 nearest_object_bound = nearest_object_bound,
							 update_strategy = "lowest")
			_, parallel_accuracies = \
				simulate_mot(grid_side, num_simulations, num_time_steps, num_objects,
							 num_targets, k, lm, sigma,
							 per_target_attention = per_target_attention,
							 nearest_object_bound = nearest_object_bound,
							 update_strategy = "random")

			seq_accuracy     = np.mean(seq_accuracies)*100
			parallel_accuracy = np.mean(parallel_accuracies)*100

			print(num_targets, sigma, seq_accuracy, parallel_accuracy)

			if len(seq_sigma_threshold_list)-1 < i and \
			   ((seq_accuracy >= accuracy_threshold) or (sigma == sigma_list[-1])):
				sigma_threshold = sigma
				seq_sigma_threshold_list.append(sigma_threshold)
			if len(parallel_sigma_threshold_list)-1 < i and \
			   ((parallel_accuracy >= accuracy_threshold) or (sigma == sigma_list[-1])):
				sigma_threshold = sigma
				parallel_sigma_threshold_list.append(sigma_threshold)
			if len(seq_sigma_threshold_list) == i+1\
			   and len(parallel_sigma_threshold_list) == i+1:
				break

	plt.plot(np.arange(1,max_num_targets+1), seq_sigma_threshold_list, label="Sequential Strategy")
	plt.plot(np.arange(1,max_num_targets+1), parallel_sigma_threshold_list, label="Parallel Strategy")
	plt.title("Velocity (sigma) Threshold vs Number of Targets\n({0} Simulations, {1} Time Steps)"\
			  .format(num_simulations, num_time_steps))
	plt.xlabel("Number of targets ({0} objects)".format(num_objects),)
	plt.ylabel("Sigma threshold ({0}% accuracy)".format(accuracy_threshold),)
	plt.ylim(0, max(sigma_list))
	plt.legend()
	plt.show()

	filename = get_filename(
		"sigma-targets-both",
		accuracy = accuracy_threshold,
		time = num_time_steps,
		per_target_attention = per_target_attention,
		nearest_object_bound = nearest_object_bound,
	)
	write_plot_file(
		filename = filename,
		title = "Velocity (sigma) Threshold vs Number of Targets\n({0} Simulations, {1} Time Steps)"\
			.format(
				num_simulations,
				num_time_steps
			),
		ylim = [0, max(sigma_list)],
		plot_type = "plot",
		xlabel = "Number of targets ({0} objects)".format(num_objects),
		ylabel = "Sigma threshold ({0}% accuracy)".format(accuracy_threshold),
		data = {
			"Sequential Strategy": [np.arange(1,max_num_targets+1), seq_sigma_threshold_list],
			"Parallel Strategy": [np.arange(1,max_num_targets+1), parallel_sigma_threshold_list],
		}
	)


def plot_both_acc_wrt_sigma(
		grid_side, num_simulations, num_time_steps, num_objects, num_targets,
		k, lm, max_sigma=None, sigma_list=None,
		per_target_attention=None, last_step_uses_nearest_object=True,
		nearest_object_bound=None
):
	seq_accuracy_list      = []
	seq_accuracy_list_se   = []
	parallel_accuracy_list  = []
	parallel_accuracy_list_se = []
	chance_accuracy_list  = []
	chance_accuracy_list_se = []
	min_sigma = 0.1
	int_sigma = ((max_sigma - min_sigma)/10 if max_sigma is not None else None)
	sigma_list = (np.arange(max_sigma,min_sigma,-int_sigma) if sigma_list is None else sigma_list)
	for sigma in sigma_list:
		_, seq_accuracies = \
			simulate_mot(grid_side, num_simulations, num_time_steps, num_objects, num_targets,
						 k = k, lm = lm, sigma = sigma,
						 per_target_attention = per_target_attention,
						 nearest_object_bound = nearest_object_bound,
						 update_strategy = "lowest")
		_, parallel_accuracies = \
			simulate_mot(grid_side, num_simulations, num_time_steps, num_objects, num_targets,
						 k = k, lm = lm, sigma = sigma,
						 per_target_attention = per_target_attention,
						 nearest_object_bound = nearest_object_bound,
						 update_strategy = "random")
		_, chance_accuracies = \
			simulate_mot(grid_side, num_simulations, num_time_steps, num_objects, num_targets,
						 k = k, lm = lm, sigma = sigma,
						 per_target_attention = per_target_attention,
						 nearest_object_bound = 0,
						 update_strategy = "random")

		seq_accuracy_list.append(np.mean(seq_accuracies))
		seq_accuracy_list_se.append(np.std(seq_accuracies, ddof=1)/np.sqrt(num_simulations))

		parallel_accuracy_list.append(np.mean(parallel_accuracies))
		parallel_accuracy_list_se.append(np.std(parallel_accuracies, ddof=1)/np.sqrt(num_simulations))

		chance_accuracy_list.append(np.mean(chance_accuracies))
		chance_accuracy_list_se.append(np.std(chance_accuracies, ddof=1)/np.sqrt(num_simulations))

	seq_accuracy_list         = 100*np.asarray(seq_accuracy_list)
	seq_accuracy_list_se      = 100*np.asarray(seq_accuracy_list_se)
	parallel_accuracy_list    = 100*np.asarray(parallel_accuracy_list)
	parallel_accuracy_list_se = 100*np.asarray(parallel_accuracy_list_se)
	chance_accuracy_list      = 100*np.asarray(chance_accuracy_list)
	chance_accuracy_list_se   = 100*np.asarray(chance_accuracy_list_se)


	filename = get_filename(
		"accuracy-sigma-both",
		sigma = sigma,
		per_target_attention = per_target_attention,
		nearest_object_bound = nearest_object_bound,
	)
	print(filename)
	plt.clf()
	plt.errorbar(sigma_list, seq_accuracy_list, seq_accuracy_list_se,
				 label="Sequential Strategy")
	plt.errorbar(sigma_list, parallel_accuracy_list, parallel_accuracy_list_se,
				 label="Parallel Strategy")
	plt.errorbar(sigma_list, chance_accuracy_list, chance_accuracy_list_se,
				 label="Chance Performance")
	plt.title("Accuracy vs Object Speeds\n({0} objects, {1} targets)"
			  .format(num_objects, num_targets))
	plt.xlabel("Object Speed (sigma)")
	plt.ylabel("Accuracy")
	plt.ylim(0,100)
	plt.legend()
	plt.show()
	write_plot_file(
		filename = filename,
		title = "Accuracy vs Object Speeds\n({0} objects, {1} targets)"\
				.format(num_objects, num_targets),
		xlabel = "Object Speed (sigma)",
		ylabel = "Accuracy",
		ylim = [0,100],
		plot_type = "errorbar",
		data = {
			"Sequential Strategy": [
				sigma_list,
				seq_accuracy_list,
				seq_accuracy_list_se,
			],
			"Parallel Strategy": [
				sigma_list,
				parallel_accuracy_list,
				parallel_accuracy_list_se,
			],
			"Chance Performance": [
				sigma_list,
				chance_accuracy_list,
				chance_accuracy_list_se,
			],
		}
	)


def plot_mot_mit_wrt_targets(
		grid_side, num_simulations, num_time_steps, num_objects, max_num_targets,
		k, lm, sigma,
		per_target_attention=None,
		nearest_object_bound=None,
		update_strategy="random"
):
	"""
	Plots percentage of targets that were tracked wrt time
	"""
	momit_mot_accuracy_list      = []
	momit_mot_accuracy_list_se   = []
	our_nob_mot_accuracy_list    = []
	our_nob_mot_accuracy_list_se = []
	momit_mit_accuracy_list      = []
	momit_mit_accuracy_list_se   = []
	our_nob_mit_accuracy_list    = []
	our_nob_mit_accuracy_list_se = []
	num_target_list    = []

	for num_targets in range(1, max_num_targets+1):
		momit_mot_accuracies, our_nob_mot_accuracies = \
			simulate_mot(grid_side, num_simulations, num_time_steps, num_objects, num_targets,
						 k = k, lm = lm, sigma = sigma,
						 per_target_attention = per_target_attention,
						 nearest_object_bound = None,
						 update_strategy = update_strategy)
		momit_mit_accuracies, our_nob_mit_accuracies = \
			simulate_mit(grid_side, num_simulations, num_time_steps, num_objects, num_targets,
						 k = k, lm = lm, sigma = sigma,
						 per_target_attention = per_target_attention,
						 nearest_object_bound = None,
						 update_strategy = update_strategy)

		momit_mot_accuracy_list.append(np.mean(momit_mot_accuracies))
		momit_mot_accuracy_list_se.append(np.std(momit_mot_accuracies, ddof=1)/np.sqrt(num_simulations))

		our_nob_mot_accuracy_list.append(np.mean(our_nob_mot_accuracies))
		our_nob_mot_accuracy_list_se.append(np.std(our_nob_mot_accuracies, ddof=1)/np.sqrt(num_simulations))

		momit_mit_accuracy_list.append(np.mean(momit_mit_accuracies))
		momit_mit_accuracy_list_se.append(np.std(momit_mit_accuracies, ddof=1)/np.sqrt(num_simulations))

		our_nob_mit_accuracy_list.append(np.mean(our_nob_mit_accuracies))
		our_nob_mit_accuracy_list_se.append(np.std(our_nob_mit_accuracies, ddof=1)/np.sqrt(num_simulations))


		num_target_list.append(num_targets)

	num_target_list     = np.asarray(num_target_list)
	momit_mot_accuracy_list = 100*np.asarray(momit_mot_accuracy_list)
	momit_mot_accuracy_list_se = 100*np.asarray(momit_mot_accuracy_list_se)
	our_nob_mot_accuracy_list = 100*np.asarray(our_nob_mot_accuracy_list)
	our_nob_mot_accuracy_list_se = 100*np.asarray(our_nob_mot_accuracy_list_se)
	momit_mit_accuracy_list = 100*np.asarray(momit_mit_accuracy_list)
	momit_mit_accuracy_list_se = 100*np.asarray(momit_mit_accuracy_list_se)
	our_nob_mit_accuracy_list = 100*np.asarray(our_nob_mit_accuracy_list)
	our_nob_mit_accuracy_list_se = 100*np.asarray(our_nob_mit_accuracy_list_se)

	filename = get_filename(
		"accuracy-targets-mot-mit",
		per_target_attention = per_target_attention,
		nearest_object_bound = nearest_object_bound,
		update_strategy = update_strategy
	)
	print(filename)

	plt.clf()
	plt.errorbar(num_target_list, momit_mot_accuracy_list, momit_mot_accuracy_list_se,
				 label="MOMIT Accuracy for MOT")
	plt.errorbar(num_target_list, our_nob_mot_accuracy_list, our_nob_mot_accuracy_list_se,
				 label="Our Accuracy for MOT (with nob)")
	plt.errorbar(num_target_list, momit_mit_accuracy_list, momit_mit_accuracy_list_se,
				 label="MOMIT Accuracy for MIT")
	plt.errorbar(num_target_list, our_nob_mit_accuracy_list, our_nob_mit_accuracy_list_se,
				 label="Our Accuracy for MIT (with nob)")
	plt.xlabel("Number of targets ({0} objects)".format(num_objects))
	plt.ylabel("Accuracy")
	plt.ylim(0,100)
	plt.legend()
	plt.show()

	write_plot_file(
		filename = filename,
		title = "Accuracy vs Number of targets",
		xlabel = "Number of targets ({0} objects)".format(num_objects),
		ylabel = "Accuracy",
		ylim = [0,100],
		plot_type = "errorbar",
		data = {
			"MOMIT Accuracy for MOT": [num_target_list, momit_mot_accuracy_list, momit_mot_accuracy_list_se],
			"Our Accuracy for MOT (with nob)": [
				num_target_list, our_nob_mot_accuracy_list, our_nob_mot_accuracy_list_se
			],
			"MOMIT Accuracy for MIT": [num_target_list, momit_mit_accuracy_list, momit_mit_accuracy_list_se],
			"Our Accuracy for MIT (with nob)": [
				num_target_list, our_nob_mit_accuracy_list, our_nob_mit_accuracy_list_se
			]
		}
	)


def plot_mot_id_wrt_targets(
		grid_side, num_simulations, max_num_targets, num_objects, num_time_steps,
		k, lm, sigma,
		per_target_attention=None,
		nearest_object_bound=None,
		update_strategy="random"
):
	"""
	Plots percentage of targets that were tracked wrt time
	"""
	momit_mot_accuracy_list      = []
	momit_mot_accuracy_list_se   = []
	our_nob_mot_accuracy_list    = []
	our_nob_mot_accuracy_list_se = []

	momit_static_id_accuracy_list      = []
	momit_static_id_accuracy_list_se   = []

	momit_nonstatic_id_accuracy_list    = []
	momit_nonstatic_id_accuracy_list_se = []

	our_nob_id_accuracy_list    = []
	our_nob_id_accuracy_list_se = []

	num_targets_list = []

	for num_targets in range(1, max_num_targets+1):
		momit_mot_accuracies, our_nob_mot_accuracies, momit_static_id_accuracies, our_nob_id_accuracies = \
			simulate_mot(grid_side, num_simulations, num_time_steps, num_objects, num_targets,
						 k = k, lm = lm, sigma = sigma,
						 per_target_attention = per_target_attention,
						 nearest_object_bound = nearest_object_bound,
						 update_strategy = update_strategy,
						 return_id_accuracy = True,
						 use_static_indices = True)

		_, _ , momit_nonstatic_id_accuracies, _ = \
			simulate_mot(grid_side, num_simulations, num_time_steps, num_objects, num_targets,
						 k = k, lm = lm, sigma = sigma,
						 per_target_attention = per_target_attention,
						 nearest_object_bound = nearest_object_bound,
						 update_strategy = update_strategy,
						 return_id_accuracy = True,
						 use_static_indices = False)


		momit_mot_accuracy_list.append(np.mean(momit_mot_accuracies))
		momit_mot_accuracy_list_se.append(np.std(momit_mot_accuracies, ddof=1)/np.sqrt(num_simulations))

		our_nob_mot_accuracy_list.append(np.mean(our_nob_mot_accuracies))
		our_nob_mot_accuracy_list_se.append(
			np.std(our_nob_mot_accuracies, ddof=1)/np.sqrt(num_simulations)
		)

		momit_static_id_accuracy_list.append(np.mean(momit_static_id_accuracies))
		momit_static_id_accuracy_list_se.append(
			np.std(momit_static_id_accuracies, ddof=1)/np.sqrt(num_simulations)
		)

		momit_nonstatic_id_accuracy_list.append(np.mean(momit_nonstatic_id_accuracies))
		momit_nonstatic_id_accuracy_list_se.append(
			np.std(momit_nonstatic_id_accuracies, ddof=1)/np.sqrt(num_simulations)
		)

		our_nob_id_accuracy_list.append(np.mean(our_nob_id_accuracies))
		our_nob_id_accuracy_list_se.append(
			np.std(our_nob_id_accuracies, ddof=1)/np.sqrt(num_simulations)
		)

		num_targets_list.append(num_targets)

	num_targets_list                    = np.asarray(num_targets_list)
	momit_mot_accuracy_list             = 100*np.asarray(momit_mot_accuracy_list)
	momit_mot_accuracy_list_se          = 100*np.asarray(momit_mot_accuracy_list_se)
	our_nob_mot_accuracy_list           = 100*np.asarray(our_nob_mot_accuracy_list)
	our_nob_mot_accuracy_list_se        = 100*np.asarray(our_nob_mot_accuracy_list_se)
	momit_static_id_accuracy_list       = 100*np.asarray(momit_static_id_accuracy_list)
	momit_static_id_accuracy_list_se    = 100*np.asarray(momit_static_id_accuracy_list_se)
	momit_nonstatic_id_accuracy_list    = 100*np.asarray(momit_nonstatic_id_accuracy_list)
	momit_nonstatic_id_accuracy_list_se = 100*np.asarray(momit_nonstatic_id_accuracy_list_se)
	our_nob_id_accuracy_list            = 100*np.asarray(our_nob_id_accuracy_list)
	our_nob_id_accuracy_list_se         = 100*np.asarray(our_nob_id_accuracy_list_se)

	filename = get_filename(
		"accuracy-targets-mot-id",
		per_target_attention = per_target_attention,
		nearest_object_bound = nearest_object_bound,
		update_strategy = update_strategy
	)
	print(filename)

	plt.clf()
	markercycle = cycler(marker=['o', '^', 's', '*', 'P', 'd'])
	colorcycle = cycler(color=plt.rcParams['axes.prop_cycle'].by_key()['color'][:6])
	plt.gca().set_prop_cycle(colorcycle + markercycle)
	mpl.rcParams["lines.markersize"] = 10
	plt.errorbar(num_targets_list, momit_mot_accuracy_list, momit_mot_accuracy_list_se,
				 label="MOMIT Tracking Accuracy")
	plt.errorbar(num_targets_list, our_nob_mot_accuracy_list, our_nob_mot_accuracy_list_se,
				 label="Our Tracking Accuracy (with nob)")
	plt.errorbar(num_targets_list, momit_static_id_accuracy_list, momit_static_id_accuracy_list_se,
				 label="MOMIT ID Accuracy (static indices)")
	plt.errorbar(num_targets_list, momit_nonstatic_id_accuracy_list,
				 momit_nonstatic_id_accuracy_list_se,
				 label="MOMIT ID Accuracy (nonstatic indices)")
	plt.errorbar(num_targets_list, our_nob_id_accuracy_list, our_nob_id_accuracy_list_se,
				 label="Our ID Accuracy (with nob)")
	plt.xlabel("Number of Targets ({0} objects)".format(num_objects))
	plt.ylabel("Accuracy")
	plt.ylim(0,100)
	plt.legend()
	plt.show()

	write_plot_file(
		filename = filename,
		title  = "Accuracy vs Number of targets",
		xlabel = "Number of Targets ({0} objects)".format(num_objects),
		ylabel = "Accuracy",
		ylim = [0,100],
		plot_type = "errorbar",
		data = {
			"MOMIT Tracking Accuracy": [
				num_targets_list, momit_mot_accuracy_list, momit_mot_accuracy_list_se
			],
			"Our Tracking Accuracy (with nob)": [
				num_targets_list, our_nob_mot_accuracy_list, our_nob_mot_accuracy_list_se
			],
			"MOMIT ID Accuracy (static indices)": [
				num_targets_list, momit_static_id_accuracy_list, momit_static_id_accuracy_list_se
			],
			"MOMIT ID Accuracy (nonstatic indices)": [
				num_targets_list, momit_nonstatic_id_accuracy_list,
				momit_nonstatic_id_accuracy_list_se
			],
			"Our ID Accuracy (with nob)": [
				num_targets_list, our_nob_id_accuracy_list, our_nob_id_accuracy_list_se
			]
		}
	)


def plot_id_wrt_time(
		grid_side, num_simulations, max_time_steps, num_objects, num_targets,
		k, lm, sigma,
		per_target_attention=None,
		nearest_object_bound=None,
		update_strategy="random"
):
	"""
	Plots percentage of targets that were tracked wrt time
	"""
	our_mot_accuracy_list    = []
	our_mot_accuracy_list_se = []

	our_id_accuracy_list    = []
	our_id_accuracy_list_se = []

	num_time_steps_list = []

	for num_time_steps in range(10, max_time_steps, (max_time_steps-10)//5):
		_, our_mot_accuracies, _, our_id_accuracies = \
			simulate_mot(grid_side, num_simulations, num_time_steps, num_objects, num_targets,
						 k = k, lm = lm, sigma = sigma,
						 per_target_attention = per_target_attention,
						 nearest_object_bound = nearest_object_bound,
						 update_strategy = update_strategy,
						 return_id_accuracy = True)

		our_mot_accuracy_list.append(np.mean(our_mot_accuracies))
		our_mot_accuracy_list_se.append(
			np.std(our_mot_accuracies, ddof=1)/np.sqrt(num_simulations)
		)

		our_id_accuracy_list.append(np.mean(our_id_accuracies))
		our_id_accuracy_list_se.append(
			np.std(our_id_accuracies, ddof=1)/np.sqrt(num_simulations)
		)

		num_time_steps_list.append(num_time_steps)

	num_time_steps_list                 = np.asarray(num_time_steps_list)
	our_mot_accuracy_list           = 100*np.asarray(our_mot_accuracy_list)
	our_mot_accuracy_list_se        = 100*np.asarray(our_mot_accuracy_list_se)
	our_id_accuracy_list            = 100*np.asarray(our_id_accuracy_list)
	our_id_accuracy_list_se         = 100*np.asarray(our_id_accuracy_list_se)

	filename = get_filename(
		"accuracy-time-id",
		per_target_attention = per_target_attention,
		nearest_object_bound = nearest_object_bound,
		update_strategy = update_strategy
	)
	print(filename)

	plt.clf()
	markercycle = cycler(marker=['o', '^', 's', '*', 'P', 'd'])
	colorcycle = cycler(color=plt.rcParams['axes.prop_cycle'].by_key()['color'][:6])
	plt.gca().set_prop_cycle(colorcycle + markercycle)
	mpl.rcParams["lines.markersize"] = 10
	plt.errorbar(num_time_steps_list, our_mot_accuracy_list, our_mot_accuracy_list_se,
				 label="Our Tracking Accuracy")
	plt.errorbar(num_time_steps_list, our_id_accuracy_list, our_id_accuracy_list_se,
				 label="Our ID Accuracy")
	plt.xlabel("Number of Time Steps (Trial Duration)")
	plt.ylabel("Accuracy")
	plt.ylim(0,100)
	plt.legend()
	plt.show()

	write_plot_file(
		filename = filename,
		title = "Accuracy vs Trial Duration",
		xlabel = "Number of Time Steps (Trial Duration)",
		ylabel = "Accuracy",
		ylim = [0,100],
		plot_type = "errorbar",
		data = {
			"Our Tracking Accuracy": [
				num_time_steps_list, our_mot_accuracy_list, our_mot_accuracy_list_se
			],
			"Our ID Accuracy": [
				num_time_steps_list, our_id_accuracy_list, our_id_accuracy_list_se
			]
		}
	)


def plot_id_wrt_targets(
		grid_side, num_simulations, num_time_steps, num_objects, max_num_targets,
		k, lm, sigma, per_target_attention=None,
		nearest_object_bound=None, update_strategy="random"
):
	our_accuracy_list       = []
	our_accuracy_list_se    = []
	our_id_accuracy_list    = []
	our_id_accuracy_list_se = []
	chance_accuracy_list    = []
	chance_accuracy_list_se = []
	num_target_list = []
	for num_targets in range(1, max_num_targets+1):
		_, our_accuracies, _, our_id_accuracies = \
			simulate_mot(grid_side, num_simulations, num_time_steps, num_objects, num_targets,
						 k = k, lm = lm, sigma = sigma,
						 per_target_attention = per_target_attention,
						 nearest_object_bound = nearest_object_bound,
						 update_strategy = update_strategy,
						 return_id_accuracy = True)
		_, chance_accuracies = \
			simulate_mot(grid_side, num_simulations, num_time_steps, num_objects, num_targets,
						 k = k, lm = lm, sigma = sigma,
						 per_target_attention = per_target_attention,
						 nearest_object_bound = 0,
						 update_strategy = update_strategy)
		# chance_accuracies = [0]*num_simulations

		our_accuracy_list.append(np.mean(our_accuracies))
		our_accuracy_list_se.append(np.std(our_accuracies, ddof=1)/np.sqrt(num_simulations))

		our_id_accuracy_list.append(np.mean(our_id_accuracies))
		our_id_accuracy_list_se.append(np.std(our_id_accuracies, ddof=1)/np.sqrt(num_simulations))

		chance_accuracy_list.append(np.mean(chance_accuracies))
		chance_accuracy_list_se.append(np.std(chance_accuracies, ddof=1)/np.sqrt(num_simulations))

		num_target_list.append(num_targets)

	num_target_list         = np.asarray(num_target_list)
	our_accuracy_list       = np.arange(1,max_num_targets+1)*np.asarray(our_accuracy_list)
	our_accuracy_list_se    = np.arange(1,max_num_targets+1)*np.asarray(our_accuracy_list_se)
	our_id_accuracy_list    = np.arange(1,max_num_targets+1)*np.asarray(our_id_accuracy_list)
	our_id_accuracy_list_se = np.arange(1,max_num_targets+1)*np.asarray(our_id_accuracy_list_se)
	chance_accuracy_list    = np.arange(1,max_num_targets+1)*np.asarray(chance_accuracy_list)
	chance_accuracy_list_se = np.arange(1,max_num_targets+1)*np.asarray(chance_accuracy_list_se)

	filename = get_filename(
		"accuracy-targets-id",
		per_target_attention = per_target_attention,
		nearest_object_bound = nearest_object_bound,
		update_strategy = update_strategy
	)
	print(filename)

	plt.clf()
	plt.title("Accuracy vs Number of targets (sigma={0})".format(sigma))

	plt.errorbar(num_target_list, our_accuracy_list, our_accuracy_list_se,
				 label="Our Tracking Accuracy")
	plt.errorbar(num_target_list, our_id_accuracy_list, our_id_accuracy_list_se,
				 label="Our ID Accuracy")
	plt.errorbar(num_target_list, chance_accuracy_list, chance_accuracy_list_se,
				 label="Chance Tracking Performance")
	plt.xlabel("Number of targets ({0} objects)".format(num_objects))
	plt.ylabel("Accuracy")
	plt.ylim(0,8)
	plt.legend()
	plt.show()

	# write_plot_file(
	# 	filename = filename,
	# 	title = "Accuracy vs Number of targets (sigma={0})".format(sigma),
	# 	xlabel = "Number of targets ({0} objects)".format(num_objects),
	# 	ylabel = "Accuracy",
	# 	ylim = [0,8],
	# 	plot_type = "errorbar",
	# 	data = {
	# 		"Our Tracking Accuracy": [num_target_list, our_accuracy_list, our_accuracy_list_se],
	# 		"Our ID Accuracy": [num_target_list, our_id_accuracy_list, our_id_accuracy_list_se],
	# 		"Chance Tracking Performance": [num_target_list, chance_accuracy_list, chance_accuracy_list_se]
	# 	}
	# )


def plot_exp_id_wrt_targets(
		grid_side, max_num_targets,
		model_updates_per_time_step, json_files=None, json_dir=None, per_target_attention=None,
		nearest_object_bound=None, update_strategy="random", plot_accuracies=True,
		plot_correct_responses_count=False, plot_confident_responses_count=False
):
	our_accuracy_dict       = dict()
	our_accuracy_dict_se    = dict()
	our_id_accuracy_dict    = dict()
	our_id_accuracy_dict_se = dict()
	momit_accuracy_dict       = dict()
	momit_accuracy_dict_se    = dict()
	momit_id_accuracy_dict    = dict()
	momit_id_accuracy_dict_se = dict()
	chance_accuracy_dict    = dict()
	chance_accuracy_dict_se = dict()
	chance_id_accuracy_dict    = dict()
	chance_id_accuracy_dict_se = dict()
	confident_responses_count = dict()

	for i in range(1, max_num_targets+1):
		our_accuracy_dict[i]    = []
		our_accuracy_dict_se[i] = []
		our_id_accuracy_dict[i]    = []
		our_id_accuracy_dict_se[i] = []
		momit_accuracy_dict[i]    = []
		momit_accuracy_dict_se[i] = []
		momit_id_accuracy_dict[i]    = []
		momit_id_accuracy_dict_se[i] = []
		chance_accuracy_dict[i]    = []
		chance_accuracy_dict_se[i] = []
		chance_id_accuracy_dict[i]    = []
		chance_id_accuracy_dict_se[i] = []
		confident_responses_count[i] = 0

	if json_dir is None:
		files = json_files
	else:
		files = list(map(lambda f : json_dir+f, os.listdir(json_dir)))

	for f in files:
		if not f.endswith(".json"): continue
		momit_accuracies, our_accuracies, momit_id_accuracies, \
			our_id_accuracies, confident_responses = \
			simulate_mot_using_experimental_data(
				grid_side,
				json_filename=f,
				model_updates_per_time_step = model_updates_per_time_step,
				max_num_targets = max_num_targets,
				per_target_attention = per_target_attention,
				nearest_object_bound = nearest_object_bound,
				update_strategy = update_strategy,
				return_id_accuracy = True,
				# id_only_for_perfect_tracking=True
				return_final_attended_location_count = True,
				use_static_indices=True
			)
		print(f, our_accuracies[1], sep="\n")
		print()
		_, chance_accuracies, _, chance_id_accuracies = \
			simulate_mot_using_experimental_data(
				grid_side,
				json_filename=f,
				model_updates_per_time_step = model_updates_per_time_step,
				max_num_targets = max_num_targets,
				per_target_attention = per_target_attention,
				nearest_object_bound = 0,
				return_id_accuracy = True,
				update_strategy = update_strategy)
		# chance_accuracies = [0]*num_simulations

		for i in range(1, max_num_targets+1):

			num_simulations = len(our_accuracies[i])

			our_accuracy_dict[i].append(np.mean(our_accuracies[i]))
			our_accuracy_dict_se[i].append(
				np.std(our_accuracies[i], ddof=1)/np.sqrt(num_simulations)
			)

			our_id_accuracy_dict[i].append(np.mean(our_id_accuracies[i]))
			our_id_accuracy_dict_se[i].append(
				np.std(our_id_accuracies[i], ddof=1)/np.sqrt(num_simulations)
			)

			momit_accuracy_dict[i].append(np.mean(momit_accuracies[i]))
			momit_accuracy_dict_se[i].append(
				np.std(momit_accuracies[i], ddof=1)/np.sqrt(num_simulations)
			)

			momit_id_accuracy_dict[i].append(np.mean(momit_id_accuracies[i]))
			momit_id_accuracy_dict_se[i].append(
				np.std(momit_id_accuracies[i], ddof=1)/np.sqrt(num_simulations)
			)

			confident_responses_count[i] += confident_responses[i]

			chance_accuracy_dict[i].append(np.mean(chance_accuracies[i]))
			chance_accuracy_dict_se[i].append(
				np.std(chance_accuracies[i], ddof=1)/np.sqrt(num_simulations)
			)
			chance_id_accuracy_dict[i].append(np.mean(chance_id_accuracies[i]))
			chance_id_accuracy_dict_se[i].append(
				np.std(chance_id_accuracies[i], ddof=1)/np.sqrt(num_simulations)
			)


	print(NUM_BAD_TRIALS, "bad trials discarded")

	our_accuracy_list       = []
	our_accuracy_list_se    = []
	our_id_accuracy_list    = []
	our_id_accuracy_list_se = []
	momit_accuracy_list       = []
	momit_accuracy_list_se    = []
	momit_id_accuracy_list    = []
	momit_id_accuracy_list_se = []
	chance_accuracy_list    = []
	chance_accuracy_list_se = []
	chance_id_accuracy_list    = []
	chance_id_accuracy_list_se = []
	confident_responses_count_list = []
	num_target_list = []

	for i in range(1, max_num_targets+1):
		num_target_list.append(i)
		our_accuracy_list.append(np.mean(our_accuracy_dict[i]))
		our_accuracy_list_se.append(
			np.std(our_accuracy_dict[i], ddof=1) / np.sqrt(len(our_accuracy_dict[i]))
		)
		our_id_accuracy_list.append(np.mean(our_id_accuracy_dict[i]))
		our_id_accuracy_list_se.append(
			np.std(our_id_accuracy_dict[i], ddof=1) / np.sqrt(len(our_id_accuracy_dict[i]))
		)
		momit_accuracy_list.append(np.mean(momit_accuracy_dict[i]))
		momit_accuracy_list_se.append(
			np.std(momit_accuracy_dict[i], ddof=1) / np.sqrt(len(momit_accuracy_dict[i]))
		)
		momit_id_accuracy_list.append(np.mean(momit_id_accuracy_dict[i]))
		momit_id_accuracy_list_se.append(
			np.std(momit_id_accuracy_dict[i], ddof=1) / np.sqrt(len(momit_id_accuracy_dict[i]))
		)
		chance_accuracy_list.append(np.mean(chance_accuracy_dict[i]))
		chance_accuracy_list_se.append(
			np.std(chance_accuracy_dict[i], ddof=1) / np.sqrt(len(chance_accuracy_dict[i]))
		)
		chance_id_accuracy_list.append(np.mean(chance_id_accuracy_dict[i]))
		chance_id_accuracy_list_se.append(
			np.std(chance_id_accuracy_dict[i], ddof=1) / np.sqrt(len(chance_id_accuracy_dict[i]))
		)

		confident_responses_count_list.append(confident_responses_count[i])

	num_target_array = np.asarray(num_target_list)
	confident_responses_count_array = np.asarray(confident_responses_count_list)

	if plot_accuracies:
		chance_accuracy_array    = 100 * np.asarray(chance_accuracy_list)
		chance_accuracy_array_se = 100 * np.asarray(chance_accuracy_list_se)
		chance_id_accuracy_array    = 100 * np.asarray(chance_id_accuracy_list)
		chance_id_accuracy_array_se = 100 * np.asarray(chance_id_accuracy_list_se)

		# Plot our tracking vs ID accuracies
		our_accuracy_array       = 100 * np.asarray(our_accuracy_list)
		our_accuracy_array_se    = 100 * np.asarray(our_accuracy_list_se)
		our_id_accuracy_array    = 100 * np.asarray(our_id_accuracy_list)
		our_id_accuracy_array_se = 100 * np.asarray(our_id_accuracy_list_se)

		filename = get_filename(
			"exp-model-accuracy-targets-id-" + str(int(model_updates_per_time_step*10)),
			per_target_attention = per_target_attention,
			nearest_object_bound = nearest_object_bound,
			update_strategy = update_strategy
		)
		print(filename)

		plt.clf()
		plt.title("Model Performance:\nAccuracy vs Number of targets\n(sigma=1.5)")

		plt.errorbar(num_target_array, our_accuracy_array, our_accuracy_array_se,
					 label="Our Tracking Accuracy")
		plt.errorbar(num_target_array, our_id_accuracy_array, our_id_accuracy_array_se,
					 label="Our ID Accuracy")
		plt.errorbar(num_target_array, chance_accuracy_array, chance_accuracy_array_se,
					 label="Chance Tracking Performance")
		plt.errorbar(num_target_array, chance_id_accuracy_array, chance_id_accuracy_array_se,
					 label="Chance ID Performance")
		plt.xlabel("Number of targets (14 objects)")
		plt.ylabel("Accuracy")
		plt.ylim(0,100)
		plt.legend()
		plt.show()

		write_plot_file(
			filename = filename,
			title = "Model Performance:\nAccuracy vs Number of targets\n(sigma=1.5)",
			xlabel = "Number of targets (14 objects)",
			ylabel = "Accuracy",
			ylim = [0,100],
			plot_type = "errorbar",
			data = {
				"Our Tracking Accuracy": [num_target_array, our_accuracy_array, our_accuracy_array_se],
				"Our ID Accuracy": [num_target_array, our_id_accuracy_array, our_id_accuracy_array_se],
				"Chance Tracking Performance": [
					num_target_array, chance_accuracy_array, chance_accuracy_array_se
				],
				"Chance ID Performance": [
					num_target_array, chance_id_accuracy_array, chance_id_accuracy_array_se
				]
			}
		)

		# Plot momit tracking vs ID accuracies
		momit_accuracy_array       = 100 * np.asarray(momit_accuracy_list)
		momit_accuracy_array_se    = 100 * np.asarray(momit_accuracy_list_se)
		momit_id_accuracy_array    = 100 * np.asarray(momit_id_accuracy_list)
		momit_id_accuracy_array_se = 100 * np.asarray(momit_id_accuracy_list_se)
		chance_accuracy_array    = 100 * np.asarray(chance_accuracy_list)
		chance_accuracy_array_se = 100 * np.asarray(chance_accuracy_list_se)

		filename = get_filename(
			"exp-momit-accuracy-targets-id-" + str(int(model_updates_per_time_step*10)),
			per_target_attention = per_target_attention,
			nearest_object_bound = nearest_object_bound,
			update_strategy = update_strategy
		)
		print(filename)

		plt.clf()
		plt.title("MOMIT Performance:\nAccuracy vs Number of targets\n(sigma=1.5)")

		plt.errorbar(num_target_array, momit_accuracy_array, momit_accuracy_array_se,
					 label="Momit Tracking Accuracy")
		plt.errorbar(num_target_array, momit_id_accuracy_array, momit_id_accuracy_array_se,
					 label="Momit ID Accuracy")
		plt.errorbar(num_target_array, chance_accuracy_array, chance_accuracy_array_se,
					 label="Chance Tracking Performance")
		plt.errorbar(num_target_array, chance_id_accuracy_array, chance_id_accuracy_array_se,
					 label="Chance ID Performance")
		plt.xlabel("Number of targets (14 objects)")
		plt.ylabel("Accuracy")
		plt.ylim(0,100)
		plt.legend()
		plt.show()

		write_plot_file(
			filename = filename,
			title = "MOMIT Performance:\nAccuracy vs Number of targets\n(sigma=1.5)",
			xlabel = "Number of targets (14 objects)",
			ylabel = "Accuracy",
			ylim = [0,100],
			plot_type = "errorbar",
			data = {
				"Momit Tracking Accuracy": [
					num_target_array, momit_accuracy_array, momit_accuracy_array_se
				],
				"Momit ID Accuracy": [
					num_target_array, momit_id_accuracy_array, momit_id_accuracy_array_se
				],
				"Chance Tracking Performance": [
					num_target_array, chance_accuracy_array, chance_accuracy_array_se
				],
				"Chance ID Performance": [
					num_target_array, chance_id_accuracy_array, chance_id_accuracy_array_se
				]
			}
		)

	if plot_correct_responses_count:
		our_accuracy_array       = np.arange(1, max_num_targets+1) * np.asarray(our_accuracy_list)
		our_accuracy_array_se    = np.arange(1, max_num_targets+1) * np.asarray(our_accuracy_list_se)
		our_id_accuracy_array    = np.arange(1, max_num_targets+1) * np.asarray(our_id_accuracy_list)
		our_id_accuracy_array_se = np.arange(1, max_num_targets+1) * np.asarray(our_id_accuracy_list_se)
		chance_accuracy_array    = np.arange(1, max_num_targets+1) * np.asarray(chance_accuracy_list)
		chance_accuracy_array_se = np.arange(1, max_num_targets+1) * np.asarray(chance_accuracy_list_se)

		filename = get_filename(
			"exp-model-correct-targets-id-" + str(int(model_updates_per_time_step*10)),
			per_target_attention = per_target_attention,
			nearest_object_bound = nearest_object_bound,
			update_strategy = update_strategy
		)
		print(filename)

		plt.clf()
		plt.title("Model Performance:\nCorrect response counts vs Number of targets\n(sigma=1.5)")

		plt.errorbar(num_target_array, our_accuracy_array, our_accuracy_array_se,
					 label="Tracking Responses")
		plt.errorbar(num_target_array, our_id_accuracy_array, our_id_accuracy_array_se,
					 label="ID Responses")
		plt.errorbar(num_target_array, chance_accuracy_array, chance_accuracy_array_se,
					 label="Chance Tracking Responses")
		plt.xlabel("Number of targets (14 objects)")
		plt.ylabel("Number of correct responses")
		plt.ylim(0,8)
		plt.legend()
		plt.show()

		write_plot_file(
			filename = filename,
			title = "Model Performance:\nCorrect response counts vs Number of targets (sigma=1.5)",
			xlabel = "Number of targets (14 objects)",
			ylabel = "Number of correct responses",
			ylim = [0,8],
			plot_type = "errorbar",
			data = {
				"Tracking Responses": [num_target_array, our_accuracy_array, our_accuracy_array_se],
				"ID Responses": [num_target_array, our_id_accuracy_array, our_id_accuracy_array_se],
				"Chance Tracking Responses": [num_target_array, chance_accuracy_array, chance_accuracy_array_se]
			}
		)

	if plot_confident_responses_count:
		filename = get_filename(
			"exp-model-confident-targets-id-" + str(int(model_updates_per_time_step*10)),
			per_target_attention = per_target_attention,
			nearest_object_bound = nearest_object_bound,
			update_strategy = update_strategy
		)
		print(filename)

		plt.clf()
		plt.title("Model Performance:\nConfident response counts vs Number of targets\n(sigma=1.5)")

		plt.bar(
			x = num_target_array,
			height = confident_responses_count_array,
		)
		plt.xlabel("Number of targets (14 objects)")
		plt.ylabel("Number of confident responses")
		plt.show()

		write_plot_file(
			filename = filename,
			title = "Model Performance:\nConfident response counts vs Number of targets\n(sigma=1.5)",
			xlabel = "Number of targets (14 objects)",
			ylabel = "Number of confident responses",
			ylim = [None,None],
			plot_type = "bar",
			data = {
				"": [num_target_array, confident_responses_count_array],
			}
		)






def plot_momit_id_wrt_time(
		grid_side, num_simulations, max_time_steps, num_objects, num_targets,
		k, lm, sigma,
		per_target_attention=None,
		nearest_object_bound=None,
		update_strategy="random"
):
	"""
	Plots percentage of targets that were tracked wrt time
	"""
	momit_accuracy_list      = []
	momit_accuracy_list_se   = []

	momit_static_id_accuracy_list      = []
	momit_static_id_accuracy_list_se   = []

	momit_nonstatic_id_accuracy_list    = []
	momit_nonstatic_id_accuracy_list_se = []

	num_time_steps_list = []

	for num_time_steps in range(10, max_time_steps, (max_time_steps-10)//5):
		momit_accuracies, _, momit_static_id_accuracies, _ = \
			simulate_mot(grid_side, num_simulations, num_time_steps, num_objects, num_targets,
						 k = k, lm = lm, sigma = sigma,
						 per_target_attention = per_target_attention,
						 nearest_object_bound = nearest_object_bound,
						 update_strategy = update_strategy,
						 return_id_accuracy = True,
						 use_static_indices = True)

		_, _ , momit_nonstatic_id_accuracies, _ = \
			simulate_mot(grid_side, num_simulations, num_time_steps, num_objects, num_targets,
						 k = k, lm = lm, sigma = sigma,
						 per_target_attention = per_target_attention,
						 nearest_object_bound = nearest_object_bound,
						 update_strategy = update_strategy,
						 return_id_accuracy = True,
						 use_static_indices = False)


		momit_accuracy_list.append(np.mean(momit_accuracies))
		momit_accuracy_list_se.append(np.std(momit_accuracies, ddof=1)/np.sqrt(num_simulations))

		momit_static_id_accuracy_list.append(np.mean(momit_static_id_accuracies))
		momit_static_id_accuracy_list_se.append(
			np.std(momit_static_id_accuracies, ddof=1)/np.sqrt(num_simulations)
		)

		momit_nonstatic_id_accuracy_list.append(np.mean(momit_nonstatic_id_accuracies))
		momit_nonstatic_id_accuracy_list_se.append(
			np.std(momit_nonstatic_id_accuracies, ddof=1)/np.sqrt(num_simulations)
		)

		num_time_steps_list.append(num_time_steps)

	num_time_steps_list                 = np.asarray(num_time_steps_list)
	momit_accuracy_list                 = 100*np.asarray(momit_accuracy_list)
	momit_accuracy_list_se              = 100*np.asarray(momit_accuracy_list_se)
	momit_static_id_accuracy_list       = 100*np.asarray(momit_static_id_accuracy_list)
	momit_static_id_accuracy_list_se    = 100*np.asarray(momit_static_id_accuracy_list_se)
	momit_nonstatic_id_accuracy_list    = 100*np.asarray(momit_nonstatic_id_accuracy_list)
	momit_nonstatic_id_accuracy_list_se = 100*np.asarray(momit_nonstatic_id_accuracy_list_se)
	# For chance performance: see
	# https://math.stackexchange.com/questions/3231096/what-is-the-average-number-of-matches-when-randomly-picking-letters
	chance_id_accuracy_list             = 100*np.asarray([1/num_targets]*len(num_time_steps_list))
	chance_id_accuracy_list_se          = 100*np.asarray([0]*len(num_time_steps_list))

	filename = get_filename(
		"accuracy-time-momit-id",
		per_target_attention = per_target_attention,
		nearest_object_bound = nearest_object_bound,
		update_strategy = update_strategy
	)
	print(filename)

	plt.clf()
	markercycle = cycler(marker=['o', '^', 's', '*', 'P', 'd'])
	colorcycle = cycler(color=plt.rcParams['axes.prop_cycle'].by_key()['color'][:6])
	plt.gca().set_prop_cycle(colorcycle + markercycle)
	mpl.rcParams["lines.markersize"] = 10
	plt.errorbar(num_time_steps_list, momit_accuracy_list, momit_accuracy_list_se,
				 label="MOMIT Tracking Accuracy")
	plt.errorbar(num_time_steps_list, momit_static_id_accuracy_list, momit_static_id_accuracy_list_se,
				 label="MOMIT ID Accuracy (static indices)")
	plt.errorbar(num_time_steps_list, momit_nonstatic_id_accuracy_list,
				 momit_nonstatic_id_accuracy_list_se,
				 label="MOMIT ID Accuracy (nonstatic indices)")
	plt.errorbar(num_time_steps_list, chance_id_accuracy_list, chance_id_accuracy_list_se,
				 label="Chance ID Accuracy (with nob)")
	plt.xlabel("Number of Time Steps (Trial Duration)")
	plt.ylabel("Accuracy")
	plt.ylim(0,100)
	plt.legend()
	plt.show()

	write_plot_file(
		filename = filename,
		title = "Accuracy vs Trial Duration",
		xlabel = "Number of Time Steps (Trial Duration)",
		ylabel = "Accuracy",
		ylim = [0,100],
		plot_type = "errorbar",
		data = {
			"MOMIT Tracking Accuracy": [
				num_time_steps_list, momit_accuracy_list, momit_accuracy_list_se
			],
			"MOMIT ID Accuracy (static indices)": [
				num_time_steps_list, momit_static_id_accuracy_list, momit_static_id_accuracy_list_se
			],
			"MOMIT ID Accuracy (nonstatic indices)": [
				num_time_steps_list, momit_nonstatic_id_accuracy_list,
				momit_nonstatic_id_accuracy_list_se
			],
			"Chance ID Accuracy": [
				num_time_steps_list, chance_id_accuracy_list, chance_id_accuracy_list_se
			]
		}
	)


def plot_perfect_mot_id_wrt_time(
		grid_side, num_simulations, max_time_steps, num_objects, num_targets,
		k, lm, sigma,
		per_target_attention=None,
		nearest_object_bound=None,
		update_strategy="random"
):
	"""
	Plots percentage of targets that were tracked wrt time
	"""
	momit_mot_accuracy_list      = []
	momit_mot_accuracy_list_se   = []
	our_nob_mot_accuracy_list    = []
	our_nob_mot_accuracy_list_se = []

	momit_static_id_accuracy_list      = []
	momit_static_id_accuracy_list_se   = []

	momit_nonstatic_id_accuracy_list    = []
	momit_nonstatic_id_accuracy_list_se = []

	our_nob_id_accuracy_list    = []
	our_nob_id_accuracy_list_se = []

	num_time_steps_list = []

	for num_time_steps in range(10, max_time_steps, (max_time_steps-10)//5):
		momit_mot_accuracies, our_nob_mot_accuracies, momit_static_id_accuracies, our_nob_id_accuracies = \
			simulate_mot(grid_side, num_simulations, num_time_steps, num_objects, num_targets,
						 k = k, lm = lm, sigma = sigma,
						 per_target_attention = per_target_attention,
						 nearest_object_bound = nearest_object_bound,
						 update_strategy = update_strategy,
						 return_id_accuracy = True,
						 use_static_indices = True,
						 id_only_for_perfect_tracking = True)

		_, _ , momit_nonstatic_id_accuracies, _ = \
			simulate_mot(grid_side, num_simulations, num_time_steps, num_objects, num_targets,
						 k = k, lm = lm, sigma = sigma,
						 per_target_attention = per_target_attention,
						 nearest_object_bound = nearest_object_bound,
						 update_strategy = update_strategy,
						 return_id_accuracy = True,
						 use_static_indices = False,
						 id_only_for_perfect_tracking = True)


		momit_mot_accuracy_list.append(np.mean(momit_mot_accuracies))
		momit_mot_accuracy_list_se.append(np.std(momit_mot_accuracies, ddof=1)/np.sqrt(num_simulations))

		our_nob_mot_accuracy_list.append(np.mean(our_nob_mot_accuracies))
		our_nob_mot_accuracy_list_se.append(
			np.std(our_nob_mot_accuracies, ddof=1)/np.sqrt(num_simulations)
		)

		momit_static_id_accuracy_list.append(np.mean(momit_static_id_accuracies))
		momit_static_id_accuracy_list_se.append(
			np.std(momit_static_id_accuracies, ddof=1)/np.sqrt(num_simulations)
		)

		momit_nonstatic_id_accuracy_list.append(np.mean(momit_nonstatic_id_accuracies))
		momit_nonstatic_id_accuracy_list_se.append(
			np.std(momit_nonstatic_id_accuracies, ddof=1)/np.sqrt(num_simulations)
		)

		our_nob_id_accuracy_list.append(np.mean(our_nob_id_accuracies))
		our_nob_id_accuracy_list_se.append(
			np.std(our_nob_id_accuracies, ddof=1)/np.sqrt(num_simulations)
		)

		num_time_steps_list.append(num_time_steps)

	num_time_steps_list                 = np.asarray(num_time_steps_list)
	momit_mot_accuracy_list             = 100*np.asarray(momit_mot_accuracy_list)
	momit_mot_accuracy_list_se          = 100*np.asarray(momit_mot_accuracy_list_se)
	our_nob_mot_accuracy_list           = 100*np.asarray(our_nob_mot_accuracy_list)
	our_nob_mot_accuracy_list_se        = 100*np.asarray(our_nob_mot_accuracy_list_se)
	momit_static_id_accuracy_list       = 100*np.asarray(momit_static_id_accuracy_list)
	momit_static_id_accuracy_list_se    = 100*np.asarray(momit_static_id_accuracy_list_se)
	momit_nonstatic_id_accuracy_list    = 100*np.asarray(momit_nonstatic_id_accuracy_list)
	momit_nonstatic_id_accuracy_list_se = 100*np.asarray(momit_nonstatic_id_accuracy_list_se)
	our_nob_id_accuracy_list            = 100*np.asarray(our_nob_id_accuracy_list)
	our_nob_id_accuracy_list_se         = 100*np.asarray(our_nob_id_accuracy_list_se)

	filename = get_filename(
		"accuracy-time-perfect-mot-id",
		per_target_attention = per_target_attention,
		nearest_object_bound = nearest_object_bound,
		update_strategy = update_strategy
	)
	print(filename)

	plt.clf()
	markercycle = cycler(marker=['o', '^', 's', '*', 'P', 'd'])
	colorcycle = cycler(color=plt.rcParams['axes.prop_cycle'].by_key()['color'][:6])
	plt.gca().set_prop_cycle(colorcycle + markercycle)
	mpl.rcParams["lines.markersize"] = 10
	plt.errorbar(num_time_steps_list, momit_mot_accuracy_list, momit_mot_accuracy_list_se,
				 label="MOMIT Tracking Accuracy")
	plt.errorbar(num_time_steps_list, our_nob_mot_accuracy_list, our_nob_mot_accuracy_list_se,
				 label="Our Tracking Accuracy (with nob)")
	plt.errorbar(num_time_steps_list, momit_static_id_accuracy_list, momit_static_id_accuracy_list_se,
				 label="MOMIT ID Accuracy (static indices)")
	plt.errorbar(num_time_steps_list, momit_nonstatic_id_accuracy_list,
				 momit_nonstatic_id_accuracy_list_se,
				 label="MOMIT ID Accuracy (nonstatic indices)")
	plt.errorbar(num_time_steps_list, our_nob_id_accuracy_list, our_nob_id_accuracy_list_se,
				 label="Our ID Accuracy (with nob)")
	plt.xlabel("Number of Time Steps (Trial Duration)")
	plt.ylabel("Accuracy")
	plt.ylim(0,100)
	plt.legend()
	plt.show()

	write_plot_file(
		filename = filename,
		title = "Accuracy vs Trial Duration",
		xlabel = "Number of Time Steps (Trial Duration)",
		ylabel = "Accuracy",
		ylim = [0,100],
		plot_type = "errorbar",
		data = {
			"MOMIT Tracking Accuracy": [
				num_time_steps_list, momit_mot_accuracy_list, momit_mot_accuracy_list_se
			],
			"Our Tracking Accuracy (with nob)": [
				num_time_steps_list, our_nob_mot_accuracy_list, our_nob_mot_accuracy_list_se
			],
			"MOMIT ID Accuracy (static indices)": [
				num_time_steps_list, momit_static_id_accuracy_list, momit_static_id_accuracy_list_se
			],
			"MOMIT ID Accuracy (nonstatic indices)": [
				num_time_steps_list, momit_nonstatic_id_accuracy_list,
				momit_nonstatic_id_accuracy_list_se
			],
			"Our ID Accuracy (with nob)": [
				num_time_steps_list, our_nob_id_accuracy_list, our_nob_id_accuracy_list_se
			]
		}
	)


def plot_id_swaps(
		grid_side, num_simulations, num_time_steps, num_objects, num_targets,
		k, lm, sigma,
		per_target_attention=None,
		nearest_object_bound=None,
		update_strategy="random"
):
	"""
	Plots percentage of targets that were tracked wrt time
	"""

	_, _, tt_swaps, tn_swaps, both_swaps, none_swaps = \
		simulate_mot(grid_side, num_simulations, num_time_steps, num_objects, num_targets,
					 k = k, lm = lm, sigma = sigma,
					 per_target_attention = per_target_attention,
					 nearest_object_bound = nearest_object_bound,
					 update_strategy = update_strategy,
					 return_swap_count=True)

	tt_swaps_mean = np.mean(tt_swaps)
	tt_swaps_se   = np.std(tt_swaps, ddof=1)/np.sqrt(num_simulations)

	tn_swaps_mean = np.mean(tn_swaps)
	tn_swaps_se   = np.std(tn_swaps, ddof=1)/np.sqrt(num_simulations)

	both_swaps_mean = np.mean(both_swaps)
	both_swaps_se   = np.std(both_swaps, ddof=1)/np.sqrt(num_simulations)

	none_swaps_mean = np.mean(none_swaps)
	none_swaps_se   = np.std(none_swaps, ddof=1)/np.sqrt(num_simulations)

	tt_swaps_mean, tn_swaps_mean, both_swaps_mean, none_swaps_mean = \
		100*tt_swaps_mean, 100*tn_swaps_mean,\
		100*both_swaps_mean, 100*none_swaps_mean

	tt_swaps_se, tn_swaps_se, both_swaps_se, none_swaps_se = \
		100*tt_swaps_se, 100*tn_swaps_se,\
		100*both_swaps_se, 100*none_swaps_se

	filename = get_filename(
		"id-swap",
		per_target_attention = per_target_attention,
		nearest_object_bound = nearest_object_bound,
		update_strategy = update_strategy,
		time = num_time_steps
	)
	print(filename)

	plt.clf()
	markercycle = cycler(marker=['o', '^', 's', '*', 'P', 'd'])
	colorcycle = cycler(color=plt.rcParams['axes.prop_cycle'].by_key()['color'][:6])
	plt.gca().set_prop_cycle(colorcycle + markercycle)
	mpl.rcParams["lines.markersize"] = 10
	plt.bar(
		x = [1.5,2.5,3.5,4.5],
		height = [tt_swaps_mean, tn_swaps_mean, both_swaps_mean, none_swaps_mean],
		tick_label = ["TT Swaps", "TN Swaps", "Both", "None"],
		yerr = [tt_swaps_se, tn_swaps_se, both_swaps_se, none_swaps_se],
		width = 0.5
	)
	plt.xlabel("Type of Swap")
	plt.ylabel("Percent of trials with one or more swaps")
	plt.ylim(0,100)
	plt.legend()
	plt.show()

	write_plot_file(
		filename = filename,
		title = "Proportion of Trials vs Type of Swap",
		xlabel = "Type of Swap",
		ylabel = "Percent of trials with one or more swaps",
		ylim = [0,100],
		plot_type = "bar",
		data = {
			"Our Model": {
				"x": [1.5,2.5,3.5,4.5],
				"height": [tt_swaps_mean, tn_swaps_mean, both_swaps_mean, none_swaps_mean],
				"tick_label": ["TT Swaps", "TN Swaps", "Both", "None"],
				"yerr": [tt_swaps_se, tn_swaps_se, both_swaps_se, none_swaps_se],
				"width": 0.5
			}
		}
	)


def plot_sigma_wrt_targets(
		base_grid_side,
		num_simulations,
		num_time_steps,
		num_objects,
		max_num_targets,
		k, lm, max_sigma=None, sigma_list=None,
		accuracy_threshold = 80,
		per_target_attention=None,
		nearest_object_bound=None,
		update_strategy = "random"
):
	grid_side = base_grid_side
	min_sigma = 0.1
	int_sigma = ((max_sigma - min_sigma)/10 if max_sigma is not None else None)
	sigma_list = (np.arange(max_sigma,min_sigma,-int_sigma) if sigma_list is None else sigma_list)
	our_sigma_threshold_list     = []
	num_targets_list = np.arange(1, max_num_targets+1)
	if type(nearest_object_bound) == list: nob_list = nearest_object_bound
	else: nob_list = [nearest_object_bound] * max_num_targets
	print("num_targets sigma momit_accuracy our_accuracy")
	for i in range(max_num_targets):
		num_targets = num_targets_list[i]
		nearest_object_bound = nob_list[i]
		for sigma in sigma_list:
			_, our_accuracies = \
				simulate_mot(grid_side, num_simulations, num_time_steps, num_objects,
							 num_targets, k, lm, sigma,
							 per_target_attention = per_target_attention,
							 nearest_object_bound = nearest_object_bound,
							 update_strategy=update_strategy)

			our_accuracy     = np.mean(our_accuracies)*100

			print(num_targets, sigma, our_accuracy)

			if len(our_sigma_threshold_list)-1 < i and \
			   ((our_accuracy >= accuracy_threshold) or (sigma == sigma_list[-1])):
				sigma_threshold = sigma
				our_sigma_threshold_list.append(sigma_threshold)
			if len(our_sigma_threshold_list) == i+1: break

	plt.plot(np.arange(1,max_num_targets+1), our_sigma_threshold_list, label="Our Model (w/o nob)")
	plt.title("Velocity (sigma) Threshold vs Number of Targets\n({0} Simulations, {1} Time Steps)"\
			  .format(num_simulations, num_time_steps))
	plt.xlabel("Number of targets ({0} objects)".format(num_objects),)
	plt.ylabel("Sigma threshold ({0}% accuracy)".format(accuracy_threshold),)
	plt.ylim(0, max(sigma_list))
	plt.legend()
	plt.show()

	filename = get_filename(
		"sigma-targets",
		accuracy = accuracy_threshold,
		time = num_time_steps,
		per_target_attention = per_target_attention,
		nearest_object_bound = nearest_object_bound,
		update_strategy = update_strategy
	)
	write_plot_file(
		filename = filename,
		title = "Velocity (sigma) Threshold vs Number of Targets\n({0} Simulations, {1} Time Steps)"\
			.format(
				num_simulations,
				num_time_steps
			),
		ylim = [0, max(sigma_list)],
		plot_type = "plot",
		xlabel = "Number of targets ({0} objects)".format(num_objects),
		ylabel = "Sigma threshold ({0}% accuracy)".format(accuracy_threshold),
		data = {
			"Our Model": [np.arange(1,max_num_targets+1), our_sigma_threshold_list]
		}
	)


def plot_sigma_wrt_distractors(
		base_grid_side,
		num_simulations,
		num_time_steps,
		max_num_objects,
		num_targets,
		k, lm, max_sigma=None, sigma_list=None,
		accuracy_threshold = 80,
		per_target_attention=None,
		nearest_object_bound=None,
		update_strategy = "random"
):
	grid_side = base_grid_side
	min_sigma = 0.1
	int_sigma = ((max_sigma - min_sigma)/10 if max_sigma is not None else None)
	sigma_list = (np.arange(max_sigma,min_sigma,-int_sigma) if sigma_list is None else sigma_list)
	our_sigma_threshold_list     = []
	num_distractors_list = np.arange(1, max_num_objects-num_targets+1)
	if type(nearest_object_bound) == list: nob_list = nearest_object_bound
	else: nob_list = [nearest_object_bound] * max_num_objects
	print("num_distractors sigma momit_accuracy our_accuracy")
	for i in range(max_num_objects-num_targets):
		num_objects = num_targets + num_distractors_list[i]
		nearest_object_bound = nob_list[i]
		for sigma in sigma_list:
			_, our_accuracies = \
				simulate_mot(grid_side, num_simulations, num_time_steps, num_objects,
							 num_targets, k, lm, sigma,
							 per_target_attention = per_target_attention,
							 nearest_object_bound = nearest_object_bound,
							 update_strategy=update_strategy)

			our_accuracy     = np.mean(our_accuracies)*100

			print(num_objects-num_targets, sigma, our_accuracy)

			if len(our_sigma_threshold_list)-1 < i and \
			   ((our_accuracy >= accuracy_threshold) or (sigma == sigma_list[-1])):
				sigma_threshold = sigma
				our_sigma_threshold_list.append(sigma_threshold)
			if len(our_sigma_threshold_list) == i+1: break

	plt.plot(num_distractors_list, our_sigma_threshold_list, label="Our Model (w/o nob)")
	plt.title("Velocity (sigma) Threshold vs Number of Distractors\n({0} Simulations, {1} Time Steps)"\
			  .format(num_simulations, num_time_steps))
	plt.xlabel("Number of distractors ({0} targets)".format(num_targets),)
	plt.ylabel("Sigma threshold ({0}% accuracy)".format(accuracy_threshold),)
	plt.ylim(0, max(sigma_list))
	plt.legend()
	plt.show()

	filename = get_filename(
		"sigma-distractors",
		accuracy = accuracy_threshold,
		time = num_time_steps,
		per_target_attention = per_target_attention,
		nearest_object_bound = nearest_object_bound,
		update_strategy = update_strategy
	)
	write_plot_file(
		filename = filename,
		title = "Velocity (sigma) Threshold vs Number of Distractors\n({0} Simulations, {1} Time Steps)"\
			.format(
				num_simulations,
				num_time_steps
			),
		ylim = [0, max(sigma_list)],
		plot_type = "plot",
		xlabel = "Number of distractors ({0} objects)".format(num_targets),
		ylabel = "Sigma threshold ({0}% accuracy)".format(accuracy_threshold),
		data = {
			"Our Model": [num_distractors_list, our_sigma_threshold_list]
		}
	)


def plot_sigma_wrt_spacing(
		grid_side_list,
		num_simulations,
		num_time_steps,
		num_objects,
		num_targets,
		k, lm, max_sigma=None, sigma_list=None,
		accuracy_threshold = 80,
		per_target_attention=None,
		nearest_object_bound=None,
		update_strategy = "random"
):
	min_sigma = 0.1
	int_sigma = ((max_sigma - min_sigma)/10 if max_sigma is not None else None)
	sigma_list = (np.arange(max_sigma,min_sigma,-int_sigma) if sigma_list is None else sigma_list)
	our_sigma_threshold_list     = []
	if type(nearest_object_bound) == list: nob_list = nearest_object_bound
	else: nob_list = [nearest_object_bound] * len(grid_side_list)
	print("grid_side sigma momit_accuracy our_accuracy")
	for i in range(len(grid_side_list)):
		grid_side = grid_side_list[i]
		nearest_object_bound = nob_list[i]
		for sigma in sigma_list:
			_, our_accuracies = \
				simulate_mot(grid_side, num_simulations, num_time_steps, num_objects,
							 num_targets, k, lm, sigma,
							 per_target_attention = per_target_attention,
							 nearest_object_bound = nearest_object_bound,
							 update_strategy=update_strategy)

			our_accuracy     = np.mean(our_accuracies)*100

			print(grid_side, sigma, our_accuracy)

			if len(our_sigma_threshold_list)-1 < i and \
			   ((our_accuracy >= accuracy_threshold) or (sigma == sigma_list[-1])):
				sigma_threshold = sigma
				our_sigma_threshold_list.append(sigma_threshold)
			if len(our_sigma_threshold_list) == i+1: break

	plt.plot(grid_side_list, our_sigma_threshold_list, label="Our Model (w/o nob)")
	plt.title("Velocity (sigma) Threshold vs Object Spacing\n({0} Simulations, {1} Time Steps)"\
			  .format(num_simulations, num_time_steps))
	plt.xlabel("Size of grid ({0} objects, {1} targets)".format(num_objects, num_targets),)
	plt.ylabel("Sigma threshold ({0}% accuracy)".format(accuracy_threshold),)
	plt.ylim(0, max(sigma_list))
	plt.legend()
	plt.show()

	filename = get_filename(
		"sigma-spacing",
		accuracy = accuracy_threshold,
		time = num_time_steps,
		per_target_attention = per_target_attention,
		nearest_object_bound = nearest_object_bound,
		update_strategy = update_strategy
	)
	write_plot_file(
		filename = filename,
		title = "Velocity (sigma) Threshold vs Object Spacing\n({0} Simulations, {1} Time Steps)"\
			.format(
				num_simulations,
				num_time_steps
			),
		ylim = [0, max(sigma_list)],
		plot_type = "plot",
		xlabel = "Size of the grid ({0} objects, {1} targets)".format(num_objects, num_targets),
		ylabel = "Sigma threshold ({0}% accuracy)".format(accuracy_threshold),
		data = {
			"Our Model": [grid_side_list, our_sigma_threshold_list]
		}
	)



def plot_momit_sigma_wrt_targets(
		base_grid_side,
		num_simulations,
		num_time_steps,
		num_objects,
		max_num_targets,
		k, lm, max_sigma=None, sigma_list=None,
		accuracy_threshold = 80,
		per_target_attention=None,
		nearest_object_bound=None,
		update_strategy = "random"
):
	grid_side = base_grid_side
	min_sigma = 0.1
	int_sigma = ((max_sigma - min_sigma)/10 if max_sigma is not None else None)
	sigma_list = (np.arange(max_sigma,min_sigma,-int_sigma) if sigma_list is None else sigma_list)
	momit_sigma_threshold_list   = []
	our_sigma_threshold_list     = []
	our_nob_sigma_threshold_list = []
	num_targets_list = np.arange(1, max_num_targets+1)
	if type(nearest_object_bound) == list: nob_list = nearest_object_bound
	else: nob_list = [nearest_object_bound] * max_num_targets
	print("num_targets sigma momit_accuracy our_accuracy")
	for i in range(max_num_targets):
		num_targets = num_targets_list[i]
		nearest_object_bound = nob_list[i]
		for sigma in sigma_list:
			momit_accuracies, our_accuracies = \
				simulate_mot(grid_side, num_simulations, num_time_steps, num_objects,
							 num_targets, k, lm, sigma,
							 per_target_attention = per_target_attention,
							 nearest_object_bound = None,
							 update_strategy=update_strategy)
			_, our_nob_accuracies = \
				simulate_mot(grid_side, num_simulations, num_time_steps, num_objects,
							 num_targets, k, lm, sigma,
							 per_target_attention = per_target_attention,
							 nearest_object_bound = nearest_object_bound,
							 update_strategy=update_strategy)

			momit_accuracy   = np.mean(momit_accuracies)*100
			our_accuracy     = np.mean(our_accuracies)*100
			our_nob_accuracy = np.mean(our_nob_accuracies)*100

			print(num_targets, sigma, momit_accuracy, our_accuracy)

			if len(momit_sigma_threshold_list)-1 < i and \
			   ((momit_accuracy >= accuracy_threshold) or (sigma == sigma_list[-1])):
				sigma_threshold = sigma
				momit_sigma_threshold_list.append(sigma_threshold)
			if len(our_sigma_threshold_list)-1 < i and \
			   ((our_accuracy >= accuracy_threshold) or (sigma == sigma_list[-1])):
				sigma_threshold = sigma
				our_sigma_threshold_list.append(sigma_threshold)
			if len(our_nob_sigma_threshold_list)-1 < i and \
			   ((our_nob_accuracy >= accuracy_threshold) or (sigma == sigma_list[-1])):
				sigma_threshold = sigma
				our_nob_sigma_threshold_list.append(sigma_threshold)
			if len(momit_sigma_threshold_list) == i+1 \
			   and len(our_sigma_threshold_list) == i+1\
			   and len(our_nob_sigma_threshold_list) == i+1:
				break

	plt.plot(np.arange(1,max_num_targets+1), momit_sigma_threshold_list, label="MOMIT")
	plt.plot(np.arange(1,max_num_targets+1), our_sigma_threshold_list, label="Our Model (w/o nob)")
	plt.plot(np.arange(1,max_num_targets+1), our_nob_sigma_threshold_list, label="Our Model (with nob)")
	plt.title("Velocity (sigma) Threshold vs Number of Targets\n({0} Simulations, {1} Time Steps)"\
			  .format(num_simulations, num_time_steps))
	plt.xlabel("Number of targets ({0} objects)".format(num_objects),)
	plt.ylabel("Sigma threshold ({0}% accuracy)".format(accuracy_threshold),)
	plt.ylim(0, max(sigma_list))
	plt.legend()
	plt.show()

	filename = get_filename(
		"sigma-targets-momit",
		accuracy = accuracy_threshold,
		time = num_time_steps,
		per_target_attention = per_target_attention,
		nearest_object_bound = nearest_object_bound,
		update_strategy = update_strategy
	)
	write_plot_file(
		filename = filename,
		title = "Velocity (sigma) Threshold vs Number of Targets\n({0} Simulations, {1} Time Steps)"\
			.format(
				num_simulations,
				num_time_steps
			),
		ylim = [0, max(sigma_list)],
		plot_type = "plot",
		xlabel = "Number of targets ({0} objects)".format(num_objects),
		ylabel = "Sigma threshold ({0}% accuracy)".format(accuracy_threshold),
		data = {
			"MOMIT": [np.arange(1,max_num_targets+1), momit_sigma_threshold_list],
			"Our Model (w/o nob)": [np.arange(1,max_num_targets+1), our_sigma_threshold_list],
			"Our Model (with nob)": [np.arange(1,max_num_targets+1), our_nob_sigma_threshold_list],
		}
	)

def plot_harmonic_sigma_wrt_targets(
		base_grid_side,
		num_simulations,
		num_time_steps,
		num_objects,
		max_num_targets,
		k, lm, max_sigma=None, sigma_list=None,
		accuracy_threshold = 80,
		last_step_uses_nearest_object = True,
		nearest_object_bound=None,
		update_strategy = "random"
):
	grid_side = base_grid_side
	min_sigma = 0.1
	int_sigma = ((max_sigma - min_sigma)/10 if max_sigma is not None else None)
	sigma_list = (np.arange(max_sigma,min_sigma,-int_sigma) if sigma_list is None else sigma_list)
	sigma_threshold_list = []
	print("num_targets sigma accuracy")
	for num_targets in range(1,max_num_targets+1):
		for sigma in sigma_list:
			momit_accuracies, our_accuracies = \
				simulate_mot(grid_side, num_simulations, num_time_steps, num_objects,
							 num_targets, k, lm, sigma,
							 last_step_uses_nearest_object = last_step_uses_nearest_object,
							 per_target_attention = 1.6/num_targets,
							 nearest_object_bound = nearest_object_bound,
							 update_strategy=update_strategy)

			# How many of the tracked objects are targets?
			# accuracy = 100 * (num_targets - np.mean(our_accuracies))/num_targets
			accuracy = 100 * (num_targets
							  - 0.5*(np.mean(our_accuracies)
									 + np.mean(momit_accuracies)))/num_targets

			# # Equation 1 from Alverez et al (2007) - irrelevant here
			# C = (num_targets * num_simulations - np.sum(momit_accuracies)) / num_simulations
			# n = num_targets
			# m = num_objects
			# accuracy = 100 * (C + (n-C)*(n-C)/(m-C)) / n

			# accuracy = 100 * \
			# 	(1 - ((np.sum(momit_accuracies))
			# 		  / (num_targets * len(momit_accuracies))))

			print(num_targets, sigma, accuracy)
			if (accuracy >= accuracy_threshold) or (sigma == sigma_list[-1]):
				sigma_threshold = sigma
				sigma_threshold_list.append(sigma_threshold)
				break

	filename = get_filename(
		"sigma-targets-harmonic",
		accuracy = accuracy_threshold,
		time = num_time_steps,
		last_step_uses_nearest_object = last_step_uses_nearest_object,
		per_target_attention = per_target_attention,
		nearest_object_bound = nearest_object_bound,
		update_strategy = update_strategy
	)
	write_plot_file(
		filename = filename,
		title = "Velocity (sigma) Threshold vs Number of Targets\n({0} Simulations, {1} Time Steps)"\
			.format(
				num_simulations,
				num_time_steps
			),
		ylim = [0, max(sigma_list)],
		plot_type = "plot",
		xlabel = "Number of targets ({0} objects)".format(num_objects),
		ylabel = "Sigma threshold ({0}% accuracy)".format(accuracy_threshold),
		data = {"": [np.arange(1,max_num_targets+1), sigma_threshold_list]}
	)
	return np.arange(1,max_num_targets+1), sigma_threshold_list




def plot_recovery_dist_wrt_targets(
		grid_side,
		num_simulations,
		num_time_steps,
		num_objects,
		max_num_targets,
		k, lm, sigma,
):
	"""
	Plots the min, mean, max, distance of the predicted targets from the nearest object
	in case the predicted target is not already an object
	"""
	xes = [1]
	min_distances = [0]
	avg_distances = [0]
	max_distances = [0]
	for num_targets in range(1, max_num_targets+1):
		min_dist, avg_dist, max_dist = \
			simulate(grid_side, num_simulations, num_time_steps, num_objects, num_targets,
					 k = k, lm = lm, sigma = sigma, return_dist = True)

		xes += [num_targets]*len(min_dist)
		min_distances += min_dist
		avg_distances += avg_dist
		max_distances += max_dist

	filename = get_filename(
		"recovery-targets",
		time = num_time_steps,
		sigma = sigma
	)

	write_plot_file(
		filename = filename,
		title = "Distance recovered by nearest-object-heuristic\nat last time step vs Number of targets"\
			+ "\n(sigma={0} | {1} objects, {2} simulations, {3} time steps)"\
			  .format(sigma, num_objects, num_simulations, num_time_steps),
		xlabel = "Number of targets",
		ylabel = "Recovered distance",
		ylim = [0, 720],
		plot_type = "scatter",
		data = {
			"Minimum": [xes, min_distances],
			"Average": [xes, avg_distances],
			"Maximum": [xes, max_distances]
		}
	)



# TODO
def plot_acc_wrt_min_dist(
		grid_side,
		num_simulations,
		num_time_steps,
		num_objects,
		max_num_targets,
		k, lm, sigma_list,
):
	"""
	Plots the accuracy wrt min distances across various sigma
	"""

	# Part 1 plots across various sigma, with num_targets=4
	write_plot_file(
		filename = "",
		title = "Accuracy vs Minimum distance between the target and the nearest object"\
			  + "\n({0} targets, {1} objects, {2} simulations, {3} time steps)"\
			  .format(4, num_objects, num_simulations, num_time_steps),
		xlabel = "Minimum distances between targets",
		ylabel = "Accuracy",
		data = {}
	)
	plt.clf()
	for sigma in sigma_list:
		num_targets = 4
		untracked_targets, tracked_nontargets, min_dist = \
			simulate(grid_side, num_simulations, num_time_steps, num_objects, num_targets,
					 k = k, lm = lm, sigma = sigma, return_min_dist = True)
		accuracies = 100 * (num_targets
							- 0.5*(np.array(tracked_nontargets)
								   + np.array(untracked_targets)))/num_targets
		plt.scatter(x = min_dist, y = accuracies, label="sigma={0}".format(sigma))
	plt.xlabel()
	plt.ylabel()
	plt.title()
	plt.legend()
	plt.show()

	# Part 2 plots across various num_targets, with sigma=2
	plt.clf()
	for num_targets in range(1, max_num_targets+1):
		untracked_targets, tracked_nontargets, min_dist = \
			simulate_mot(grid_side, num_simulations, num_time_steps, num_objects, num_targets,
						 k = k, lm = lm, sigma = 2)
		accuracies = 100 * (num_targets
							- 0.5*(np.array(tracked_nontargets)
								   + np.array(untracked_targets)))/num_targets
		plt.scatter(x = min_dist, y = accuracies, label="{0} targets".format(num_targets))
	plt.xlabel("Minimum distances between targets")
	plt.ylabel("Accuracy")
	plt.title("Accuracy vs Minimum distance between the target and the nearest object"\
			  + "\n(sigma={0} | {1} objects, {2} simulations, {3} time steps)"\
			  .format(2, num_objects, num_simulations, num_time_steps))
	plt.legend()
	plt.show()




def plot_nearest_object_benefit(
	grid_side,
	num_simulations,
	num_time_steps,
	num_objects,
	max_num_targets,
	k, lm, sigma,
):
	plt.clf()
	nearest_accuracies = []
	random_accuracies  = []
	for num_targets in range(1, max_num_targets+1):
		# manually set per target attention to 0
		untracked_targets, tracked_nontargets = \
			simulate(grid_side, num_simulations, num_time_steps, num_objects, num_targets,
					 k = k, lm = lm, sigma = sigma, last_step_uses_nearest_object=True,
					 per_target_attention = 0)
		nearest_accuracy = 100 * (num_targets
								  - 0.5*(np.mean(tracked_nontargets)
										   + np.mean(untracked_targets)))/num_targets
		# Merely setting nearest_object_bound is not enough, because it affects all the
		# intervening steps of the trial
		untracked_targets, tracked_nontargets = \
			simulate(grid_side, num_simulations, num_time_steps, num_objects, num_targets,
					 k = k, lm = lm, sigma = sigma, last_step_uses_nearest_object=False,
					 per_target_attention = 0)
		random_accuracy = 100 * (num_targets
								  - 0.5*(np.mean(tracked_nontargets)
										   + np.mean(untracked_targets)))/num_targets
		nearest_accuracies.append(nearest_accuracy)
		random_accuracies.append(random_accuracy)

	filename = get_filename(
		"nearest-object-benefit",
		sigma = sigma,
		time = num_time_steps,
		per_target_attention = 0,
	)

	write_plot_file(
		filename = filename,
		title = "Nearest Object Heuristic Benefit",
		xlabel = "Number of Targets",
		ylabel = "Accuracy",
		ylim = [0, 100],
		plot_type = "plot",
		data = {
			"Nearest Object Heuristic": [np.arange(1,max_num_targets+1), nearest_accuracies],
			"Random Object Heuristic": [np.arange(1,max_num_targets+1), random_accuracies]
		}
	)




if __name__ == "__main__":
	np.random.seed(40)
	# random.seed(43)
	# acc = simulate_mot(
	# 	grid_side = 720,
	# 	num_simulations = 10,
	# 	num_time_steps = 50,
	# 	num_objects = 14,
	# 	num_targets = 4,
	# 	k = 0.0005,
	# 	lm = 0.9,
	# 	sigma = 1,
	# 	# sigma = 2,
	# 	nearest_object_bound=50
	# )
	# print(acc)
	# print(np.mean(acc))

	# # SECTION 1: Accuracy vs Number of Targets =================================
	# plot_acc_wrt_targets(
	# 	grid_side = 720,
	# 	num_simulations = 100,
	# 	num_time_steps = 50,
	# 	num_objects = 14,
	# 	max_num_targets = 8,
	# 	k = 0.0005,
	# 	lm = 0.9,
	# 	sigma = 2,
	# 	update_strategy="lowest",
	# 	nearest_object_bound=30,
	# )
	# plot_acc_wrt_targets(
	# 	grid_side = 360,
	# 	num_simulations = 100,
	# 	num_time_steps = 100,
	# 	num_objects = 10,
	# 	max_num_targets = 5,
	# 	k = 0.0005,
	# 	lm = 0.9,
	# 	sigma = 0.5,
	# 	update_strategy="lowest",
	# 	nearest_object_bound=30,
	# )

	# SECTION 2: Accuracy vs Time of trial =====================================
	# plot_acc_wrt_time(
	# 	grid_side = 360,
	# 	num_simulations = 100,
	# 	max_time_steps = 100,
	# 	num_objects = 8,
	# 	num_targets = 4,
	# 	k = 0.0005,
	# 	lm = 0.9,
	# 	sigma = 2,
	# 	update_strategy="lowest",
	# 	nearest_object_bound=50
	# )

	# # SECTION 3: Velocity / Sigma threshold vs Number of targets ===============
	# plot_sigma_wrt_targets(
	# 	base_grid_side = 720,
	# 	num_simulations = 50,
	# 	num_time_steps = 100,
	# 	num_objects = 14,
	# 	max_num_targets = 8,
	# 	k = 0.0005,
	# 	lm = 0.9,
	# 	# sigma_list = [5, 4.5, 4, 3.6, 3.3, 3, 2.7, 2.4, 2.1, 1.8, 1.5,
	# 	# 			  1.2, 1.0, 0.8, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1],
	# 	sigma_list = [5, 4.5, 4, 3.6, 3.3, 3, 2.8, 2.6, 2.4, 2.2, 2.0, 1.8, 1.6,
	# 				  1.4, 1.3, 1.2, 1.1, 1.0, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1],
	# 	accuracy_threshold = 80,
	# 	update_strategy="lowest",
	# 	nearest_object_bound=80,
	# )

	# SECTION 4: Accuracy wrt Velocity / Sigma =================================
	# plot_acc_wrt_sigma(
	# 	grid_side = 720,
	# 	num_simulations = 100,
	# 	num_time_steps = 100,
	# 	num_objects = 12,
	# 	num_targets = 4,
	# 	k = 0.0005,
	# 	lm = 0.9,
	# 	max_sigma = 4,
	# 	update_strategy="lowest",
	# 	nearest_object_bound=50
	# )

	# SECTION 5: Tracking vs ID Performance ====================================
	plot_id_wrt_time(
		grid_side = 360,
		num_simulations = 100,
		max_time_steps = 100,
		num_objects = 8,
		num_targets = 4,
		k = 0.0005,
		lm = 0.9,
		sigma = 1.5,
		update_strategy="lowest",
		nearest_object_bound=50,
	)

	# SECTION 6: Swap Count ====================================================
	# plot_id_swaps(
	# 	grid_side = 360,
	# 	num_simulations = 100,
	# 	num_time_steps = 100,
	# 	num_objects = 8,
	# 	num_targets = 4,
	# 	k = 0.0005,
	# 	lm = 0.9,
	# 	sigma = 2,
	# 	update_strategy="lowest",
	# 	nearest_object_bound=50,
	# )
	# plot_id_swaps(
	# 	grid_side = 720,
	# 	num_simulations = 100,
	# 	num_time_steps = 400,
	# 	num_objects = 8,
	# 	num_targets = 4,
	# 	k = 0.0005,
	# 	lm = 0.9,
	# 	sigma = 1.5,
	# 	update_strategy="lowest",
	# 	nearest_object_bound=30,
	# )


	# SECTION 7: Basic graphs but with MOMIT ===================================
	# plot_momit_acc_wrt_targets(
	# 	grid_side = 720,
	# 	num_simulations = 50,
	# 	num_time_steps = 50,
	# 	num_objects = 14,
	# 	max_num_targets = 8,
	# 	k = 0.0005,
	# 	lm = 0.9,
	# 	sigma = 2,
	# 	update_strategy="lowest",
	# 	nearest_object_bound=50,
	# )
	# plot_momit_acc_wrt_time(
	# 	grid_side = 720,
	# 	num_simulations = 100,
	# 	max_time_steps = 50,
	# 	num_objects = 14,
	# 	num_targets = 4,
	# 	k = 0.0005,
	# 	lm = 0.9,
	# 	sigma = 2,
	# 	update_strategy="lowest",
	# 	nearest_object_bound=50
	# )
	# plot_momit_sigma_wrt_targets(
	# 	base_grid_side = 720,
	# 	num_simulations = 50,
	# 	num_time_steps = 50,
	# 	num_objects = 14,
	# 	max_num_targets = 8,
	# 	k = 0.0005,
	# 	lm = 0.9,
	# 	# sigma_list = [5, 4.5, 4, 3.6, 3.3, 3, 2.7, 2.4, 2.1, 1.8, 1.5,
	# 	# 			  1.2, 1.0, 0.8, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1],
	# 	sigma_list = [5, 4.5, 4, 3.6, 3.3, 3, 2.8, 2.6, 2.4, 2.2, 2.0, 1.8, 1.6,
	# 				  1.4, 1.3, 1.2, 1.1, 1.0, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1],
	# 	accuracy_threshold = 80,
	# 	update_strategy="lowest",
	# 	nearest_object_bound=50,
	# )
	# plot_momit_acc_wrt_sigma(
	# 	grid_side = 720,
	# 	num_simulations = 100,
	# 	num_time_steps = 50,
	# 	num_objects = 14,
	# 	num_targets = 4,
	# 	k = 0.0005,
	# 	lm = 0.9,
	# 	max_sigma = 4,
	# 	update_strategy="lowest",
	# 	nearest_object_bound=50
	# )

	# SECTION 8: MOMIT ID Performance ==========================================
	# plot_momit_id_wrt_time(
	# 	grid_side = 360,
	# 	num_simulations = 50,
	# 	max_time_steps = 50,
	# 	num_objects = 14,
	# 	num_targets = 4,
	# 	k = 0.0005,
	# 	lm = 0.9,
	# 	sigma = 2,
	# 	update_strategy="lowest",
	# 	nearest_object_bound=50,
	# )
	# plot_momit_id_wrt_time(
	# 	grid_side = 360,
	# 	num_simulations = 100,
	# 	max_time_steps = 100,
	# 	num_objects = 8,
	# 	num_targets = 4,
	# 	k = 0.0005,
	# 	lm = 0.9,
	# 	sigma = 1.5,
	# 	update_strategy="lowest",
	# 	nearest_object_bound=50,
	# )

	# SECTION 9: Serial vs Parallel Comparison =================================
	# plot_both_acc_wrt_targets(
	# 	grid_side = 720,
	# 	num_simulations = 50,
	# 	num_time_steps = 50,
	# 	num_objects = 14,
	# 	max_num_targets = 8,
	# 	k = 0.0005,
	# 	lm = 0.9,
	# 	sigma = 2,
	# 	nearest_object_bound=50
	# )
	# plot_both_acc_wrt_time(
	# 	grid_side = 720,
	# 	num_simulations = 50,
	# 	max_time_steps = 50,
	# 	num_objects = 14,
	# 	num_targets = 4,
	# 	k = 0.0005,
	# 	lm = 0.9,
	# 	sigma = 2,
	# 	nearest_object_bound=50
	# )
	# plot_both_sigma_wrt_targets(
	# 	base_grid_side = 720,
	# 	num_simulations = 50,
	# 	num_time_steps = 50,
	# 	num_objects = 14,
	# 	max_num_targets = 8,
	# 	k = 0.0005,
	# 	lm = 0.9,
	# 	# sigma_list = [5, 4.5, 4, 3.6, 3.3, 3, 2.7, 2.4, 2.1, 1.8, 1.5,
	# 	# 			  1.2, 1.0, 0.8, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1],
	# 	sigma_list = [5, 4.5, 4, 3.6, 3.3, 3, 2.8, 2.6, 2.4, 2.2, 2.0, 1.8, 1.6,
	# 				  1.4, 1.3, 1.2, 1.1, 1.0, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1],
	# 	accuracy_threshold = 80,
	# 	nearest_object_bound=50,
	# )
	# plot_both_acc_wrt_sigma(
	# 	grid_side = 720,
	# 	num_simulations = 50,
	# 	num_time_steps = 50,
	# 	num_objects = 14,
	# 	num_targets = 4,
	# 	k = 0.0005,
	# 	lm = 0.9,
	# 	max_sigma = 4,
	# 	nearest_object_bound=50
	# )

	# SECTION 10: Comparison of data against experimental data =================
	# print(
	# 	*simulate_mot_using_experimental_data(
	# 		grid_side=720,
	# 		max_num_targets=8,
	# 		json_filename="human-experiments/data/pretty-test.json",
	# 		model_updates_per_time_step=1,
	# 		update_strategy="lowest",
	# 		nearest_object_bound=30,
	# 		return_id_accuracy=True,
	# 	),
	# 	sep="\n"
	# )
	# plot_exp_id_wrt_targets(
	# 	grid_side=720,
	# 	max_num_targets=8,
	# 	json_dir = "human-experiments/id-targets/data/",
	# 	# json_files = ["human-experiments/data/pretty-test.json.bak"],
	# 	update_strategy="lowest",
	# 	# nearest_object_bound=25,
	# 	# model_updates_per_time_step=2,
	# 	nearest_object_bound = 38,
	# 	model_updates_per_time_step = 1.8,
	# 	plot_accuracies=True,
	# 	plot_correct_responses_count=True,
	# 	plot_confident_responses_count=True
	# )

	# SECTION 11: Velocity vs Number of Distractors ============================
	# plot_sigma_wrt_distractors(
	# 	base_grid_side = 720,
	# 	num_simulations = 50,
	# 	num_time_steps = 50,
	# 	max_num_objects = 14,
	# 	num_targets = 4,
	# 	k = 0.0005,
	# 	lm = 0.9,
	# 	# sigma_list = [5, 4.5, 4, 3.6, 3.3, 3, 2.7, 2.4, 2.1, 1.8, 1.5,
	# 	# 			  1.2, 1.0, 0.8, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1],
	# 	sigma_list = [5, 4.5, 4, 3.6, 3.3, 3, 2.8, 2.6, 2.4, 2.2, 2.0, 1.8, 1.6,
	# 				  1.4, 1.3, 1.2, 1.1, 1.0, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1],
	# 	accuracy_threshold = 75,
	# 	update_strategy="lowest",
	# 	nearest_object_bound=50,
	# )

	# plot_sigma_wrt_spacing(
	# 	grid_side_list = [90, 180, 270, 360, 450, 540, 630, 720],
	# 	num_simulations = 50,
	# 	num_time_steps = 50,
	# 	num_objects = 14,
	# 	num_targets = 4,
	# 	k = 0.0005,
	# 	lm = 0.9,
	# 	# sigma_list = [5, 4.5, 4, 3.6, 3.3, 3, 2.7, 2.4, 2.1, 1.8, 1.5,
	# 	# 			  1.2, 1.0, 0.8, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1],
	# 	sigma_list = [5, 4.5, 4, 3.6, 3.3, 3, 2.8, 2.6, 2.4, 2.2, 2.0, 1.8, 1.6,
	# 				  1.4, 1.3, 1.2, 1.1, 1.0, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1],
	# 	accuracy_threshold = 75,
	# 	update_strategy="lowest",
	# 	nearest_object_bound=50,
	# )

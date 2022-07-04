
import numpy as np
import matplotlib.pyplot as plt

from tasks import simulate_mot, simulate_mit
# from OUSpeedOnSameGrid import simulate
import json

PLOT_DIR = "plot-data/"

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




def plot_acc_wrt_time(
		grid_side, num_simulations, max_time, num_objects, num_targets,
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
	for num_time_steps in range(10, max_time, (max_time-10)//5):
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

			momit_accuracy = np.mean(momit_accuracies)*100
			our_accuracy   = np.mean(our_accuracies)*100
			our_nob_accuracy   = np.mean(our_nob_accuracies)*100

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
	np.random.seed(42)
	momit_acc, our_acc = simulate_mot(
		grid_side = 720,
		num_simulations = 10,
		num_time_steps = 50,
		num_objects = 14,
		num_targets = 4,
		k = 0.0005,
		lm = 0.9,
		sigma = 4
	)
	print(momit_acc)
	print(our_acc)

	# # SECTION 1: Accuracy vs Number of Targets =================================
	# plot_acc_wrt_targets(
	# 	grid_side = 720,
	# 	num_simulations = 50,
	# 	num_time_steps = 50,
	# 	num_objects = 14,
	# 	max_num_targets = 8,
	# 	k = 0.0005,
	# 	lm = 0.9,
	# 	sigma = 4,
	# 	update_strategy="lowest",
	# 	nearest_object_bound=30,
	# )

	# # # SECTION 2: Accuracy vs Time of trial =====================================
	# plot_acc_wrt_time(
	# 	grid_side = 720,
	# 	num_simulations = 100,
	# 	max_time = 50,
	# 	num_objects = 14,
	# 	num_targets = 4,
	# 	k = 0.0005,
	# 	lm = 0.9,
	# 	sigma = 2,
	# 	update_strategy="lowest",
	# 	nearest_object_bound=30
	# )

	# # SECTION 3: Velocity / Sigma threshold vs Number of targets ===============
	# plot_sigma_wrt_targets(
	# 	base_grid_side = 720,
	# 	num_simulations = 50,
	# 	num_time_steps = 50,
	# 	num_objects = 14,
	# 	max_num_targets = 8,
	# 	k = 0.0005,
	# 	lm = 0.9,
	# 	sigma_list = [5, 4.5, 4, 3.6, 3.3, 3, 2.7, 2.4, 2.1, 1.8, 1.5,
	# 				  1.2, 1.0, 0.8, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1],
	# 	accuracy_threshold = 80,
	# 	update_strategy="lowest",
	# 	nearest_object_bound=30,
	# )

	# SECTION 4: MOT vs MIT Accuracy ===========================================
	plot_mot_mit_wrt_targets(
		grid_side = 720,
		num_simulations = 50,
		num_time_steps = 50,
		num_objects = 14,
		max_num_targets = 8,
		k = 0.0005,
		lm = 0.9,
		sigma = 4,
		update_strategy="lowest",
		nearest_object_bound=30,
	)

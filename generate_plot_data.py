import numpy as np
import matplotlib.pyplot as plt
from OUSpeedOnSameGrid import simulate
import json

PLOT_DIR = "plot-data/"

"""

Structure of the plot data file:
- title
- xlabel
- ylabel
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
	untracked_target_list     = []
	untracked_target_list_se  = []
	tracked_nontarget_list    = []
	tracked_nontarget_list_se = []
	average_errors            = []
	average_errors_se         = []
	num_target_list = []
	for num_targets in range(1, max_num_targets+1):
		untracked_targets, tracked_nontargets = \
			simulate(grid_side, num_simulations, num_time_steps, num_objects, num_targets,
					 k = k, lm = lm, sigma = sigma,
					 last_step_uses_nearest_object = last_step_uses_nearest_object,
					 per_target_attention = per_target_attention,
					 nearest_object_bound = nearest_object_bound,
					 update_strategy = update_strategy)

		untracked_target_list.append(np.mean(untracked_targets))
		untracked_target_list_se.append(np.std(untracked_targets, ddof=1)/np.sqrt(num_simulations))

		tracked_nontarget_list.append(np.mean(tracked_nontargets))
		tracked_nontarget_list_se.append(np.std(tracked_nontargets, ddof=1)/np.sqrt(num_simulations))

		average_errors.append((untracked_target_list[-1] + tracked_nontarget_list[-1]) / 2)
		average_errors_se.append(
			np.std((np.array(untracked_targets) + np.array(tracked_nontargets))/2, ddof=1) \
			/ np.sqrt(num_simulations)
		)

		num_target_list.append(num_targets)

	num_target_list        = np.asarray(num_target_list)
	untracked_target_list  = np.asarray(untracked_target_list)
	tracked_nontarget_list = np.asarray(tracked_nontarget_list)
	average_errors         = np.asarray(average_errors)
	# print("Untracked targets: ", num_target_list, untracked_target_list)
	# print("Tracked nontargets:", num_target_list, tracked_nontarget_list)

	# Number of tracked objects that are targets
	accuracy1 = 100*(num_target_list - untracked_target_list)/num_target_list
	accuracy2 = 100*(num_target_list - tracked_nontarget_list)/num_target_list
	accuracy  = 100*(num_target_list - average_errors)/num_target_list

	filename = get_filename(
		"accuracy-targets",
		per_target_attention = per_target_attention,
		last_step_uses_nearest_object = last_step_uses_nearest_object,
		nearest_object_bound = nearest_object_bound,
		update_strategy = update_strategy
	)
	print(filename)

	write_plot_file(
		filename = filename,
		title = "Accuracy vs Number of targets",
		xlabel = "Number of targets ({0} objects)".format(num_objects),
		ylabel = "Accuracy",
		ylim = [0,100],
		plot_type = "errorbar",
		data = {
			"Accuracy 1": [num_target_list, accuracy, 100*(average_errors_se/num_target_list)],
			"Accuracy 2": [num_target_list, accuracy1, 100*(average_errors_se/num_target_list)]
		}
	)

	return untracked_target_list, tracked_nontarget_list




def plot_acc_wrt_time(
		grid_side, num_simulations, max_time, num_objects, num_targets,
		k, lm, sigma,
		per_target_attention=None,
		last_step_uses_nearest_object=True,
		nearest_object_bound=None,
		update_strategy="random"
):
	"""
	Plots percentage of targets that were tracked wrt time
	"""
	untracked_target_list     = []
	untracked_target_list_se  = []
	tracked_nontarget_list    = []
	tracked_nontarget_list_se = []
	average_errors            = []
	average_errors_se         = []
	num_time_steps_list       = []
	for num_time_steps in range(10, max_time, (max_time-10)//5):
		untracked_targets, tracked_nontargets = \
			simulate(grid_side, num_simulations, num_time_steps, num_objects, num_targets,
					 k = k, lm = lm, sigma = sigma,
					 last_step_uses_nearest_object = last_step_uses_nearest_object,
					 nearest_object_bound = nearest_object_bound,
					 per_target_attention = per_target_attention,
					 update_strategy=update_strategy)

		untracked_target_list.append(np.mean(untracked_targets))
		untracked_target_list_se.append(np.std(untracked_targets, ddof=1)/np.sqrt(num_simulations))

		tracked_nontarget_list = np.asarray(tracked_nontarget_list)
		average_errors         = np.asarray(average_errors)

		num_time_steps_list.append(num_time_steps)

	num_time_steps_list    = num_time_steps_list
	untracked_target_list  = np.asarray(untracked_target_list)
	untracked_target_list_se = np.asarray(untracked_target_list_se)
	tracked_nontarget_list = np.asarray(tracked_nontarget_list)
	average_errors         = np.asarray(average_errors)

	print("Untracked targets: ", num_time_steps, untracked_target_list)
	print("Tracked nontargets:", num_time_steps, tracked_nontarget_list)

	filename = get_filename(
		"accuracy-time",
		sigma = sigma,
		per_target_attention = per_target_attention,
		last_step_uses_nearest_object = last_step_uses_nearest_object,
		nearest_object_bound = nearest_object_bound,
		update_strategy = update_strategy
	)
	print(filename)
	# Number of tracked objects that are targets
	accuracy = 100*(num_targets - untracked_target_list)/num_targets
	write_plot_file(
		filename = filename,
		title = "Accuracy vs Time of tracking ({0} objects, {1} targets, sigma={2})"
			.format(num_objects, num_targets, sigma),
		xlabel = "Number of time steps",
		ylabel = "Accuracy",
		ylim = [0,100],
		plot_type = "errorbar",
		data = {
			"Accuracy": [
				num_time_steps_list,
				list(accuracy),
				list(100*(untracked_target_list_se/num_targets))
			]
		}
	)

	return untracked_target_list, tracked_nontarget_list




def plot_sigma_wrt_targets(
		base_grid_side,
		num_simulations,
		num_time_steps,
		num_objects,
		max_num_targets,
		k, lm, max_sigma=None, sigma_list=None,
		accuracy_threshold = 80,
		last_step_uses_nearest_object = True,
		per_target_attention=None,
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
			untracked_targets, tracked_nontargets = \
				simulate(grid_side, num_simulations, num_time_steps, num_objects,
						 num_targets, k, lm, sigma,
						 last_step_uses_nearest_object = last_step_uses_nearest_object,
						 per_target_attention = per_target_attention,
						 nearest_object_bound = nearest_object_bound)

			# How many of the tracked objects are targets?
			# accuracy = 100 * (num_targets - np.mean(tracked_nontargets))/num_targets
			accuracy = 100 * (num_targets
							  - 0.5*(np.mean(tracked_nontargets)
									 + np.mean(untracked_targets)))/num_targets

			# # Equation 1 from Alverez et al (2007) - irrelevant here
			# C = (num_targets * num_simulations - np.sum(untracked_targets)) / num_simulations
			# n = num_targets
			# m = num_objects
			# accuracy = 100 * (C + (n-C)*(n-C)/(m-C)) / n

			# accuracy = 100 * \
			# 	(1 - ((np.sum(untracked_targets))
			# 		  / (num_targets * len(untracked_targets))))

			print(num_targets, sigma, accuracy)
			if (accuracy >= accuracy_threshold) or (sigma == sigma_list[-1]):
				sigma_threshold = sigma
				sigma_threshold_list.append(sigma_threshold)
				break

	filename = get_filename(
		"sigma-targets",
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
			simulate(grid_side, num_simulations, num_time_steps, num_objects, num_targets,
					 k = k, lm = lm, sigma = 2, return_min_dist = True)
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
	# utt, tnt = simulate(
	# 	grid_side = 720,
	# 	num_simulations = 5,
	# 	num_time_steps = 25,
	# 	num_objects = 14,
	# 	num_targets = 4,
	# 	k = 0.0005,
	# 	lm = 0.9,
	# 	sigma = 4,
	# 	print_range=True
	# )
	# print(utt, tnt)

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
	# 	per_target_attention=1
	# )
	# plot_acc_wrt_targets(
	# 	grid_side = 720,
	# 	num_simulations = 50,
	# 	num_time_steps = 50,
	# 	num_objects = 14,
	# 	max_num_targets = 8,
	# 	k = 0.0005,
	# 	lm = 0.9,
	# 	sigma = 4,
	# 	last_step_uses_nearest_object=True
	# )
	# plot_acc_wrt_targets(
	# 	grid_side = 720,
	# 	num_simulations = 50,
	# 	num_time_steps = 50,
	# 	num_objects = 14,
	# 	max_num_targets = 8,
	# 	k = 0.0005,
	# 	lm = 0.9,
	# 	sigma = 4,
	# 	last_step_uses_nearest_object=False
	# )
	# plot_acc_wrt_targets(
	# 	grid_side = 720,
	# 	num_simulations = 50,
	# 	num_time_steps = 50,
	# 	num_objects = 14,
	# 	max_num_targets = 8,
	# 	k = 0.0005,
	# 	lm = 0.9,
	# 	sigma = 4,
	# 	last_step_uses_nearest_object=True,
	# 	nearest_object_bound=30
	# )
	# plot_acc_wrt_targets(
	# 	grid_side = 720,
	# 	num_simulations = 50,
	# 	num_time_steps = 50,
	# 	num_objects = 14,
	# 	max_num_targets = 8,
	# 	k = 0.0005,
	# 	lm = 0.9,
	# 	sigma = 4,
	# 	last_step_uses_nearest_object=True,
	# 	nearest_object_bound=30,
	# 	update_strategy = "lowest"
	# )

	# # SECTION 2: Accuracy vs Time of trial =====================================
	# plot_acc_wrt_time(
	# 	grid_side = 720,
	# 	num_simulations = 100,
	# 	max_time = 50,
	# 	num_objects = 14,
	# 	num_targets = 4,
	# 	k = 0.0005,
	# 	lm = 0.9,
	# 	sigma = 4,
	# 	per_target_attention = 1,
	# )
	# plot_acc_wrt_time(
	# 	grid_side = 720,
	# 	num_simulations = 100,
	# 	max_time = 50,
	# 	num_objects = 14,
	# 	num_targets = 4,
	# 	k = 0.0005,
	# 	lm = 0.9,
	# 	sigma = 4,
	# 	last_step_uses_nearest_object = False,
	# )
	# plot_acc_wrt_time(
	# 	grid_side = 720,
	# 	num_simulations = 100,
	# 	max_time = 50,
	# 	num_objects = 14,
	# 	num_targets = 4,
	# 	k = 0.0005,
	# 	lm = 0.9,
	# 	sigma = 2,
	# 	last_step_uses_nearest_object = False,
	# )
	# plot_acc_wrt_time(
	# 	grid_side = 720,
	# 	num_simulations = 100,
	# 	max_time = 50,
	# 	num_objects = 14,
	# 	num_targets = 4,
	# 	k = 0.0005,
	# 	lm = 0.9,
	# 	sigma = 4,
	# 	last_step_uses_nearest_object = True,
	# )
	# plot_acc_wrt_time(
	# 	grid_side = 720,
	# 	num_simulations = 100,
	# 	max_time = 50,
	# 	num_objects = 14,
	# 	num_targets = 4,
	# 	k = 0.0005,
	# 	lm = 0.9,
	# 	sigma = 2,
	# 	last_step_uses_nearest_object = True,
	# 	nearest_object_bound = 30
	# )
	# plot_acc_wrt_time(
	# 	grid_side = 720,
	# 	num_simulations = 100,
	# 	max_time = 50,
	# 	num_objects = 14,
	# 	num_targets = 4,
	# 	k = 0.0005,
	# 	lm = 0.9,
	# 	sigma = 2,
	# 	last_step_uses_nearest_object = True,
	# 	nearest_object_bound = 30,
	# 	update_strategy = "lowest"
	# )

	# SECTION 3: Velocity / Sigma threshold vs Number of targets ===============
	# plot_sigma_wrt_targets(
	# 	base_grid_side = 720,
	# 	num_simulations = 50,
	# 	num_time_steps = 50,
	# 	num_objects = 14,
	# 	max_num_targets = 8,
	# 	k = 0.0005,
	# 	lm = 0.9,
	# 	# max_sigma = 5,
	# 	# sigma_list = [7, 6.3, 5.6, 5, 4.5, 4, 3.6, 3.3, 3, 2.7, 2.4, 2.1, 1.8, 1.5,
	# 	# 			  1.2, 1.0, 0.8, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1],
	# 	sigma_list = [5, 4.5, 4, 3.6, 3.3, 3, 2.7, 2.4, 2.1, 1.8, 1.5,
	# 				  1.2, 1.0, 0.8, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1],
	# 	# sigma_list = [3, 2.7, 2.4, 2.1, 1.8, 1.5,
	# 	# 			  1.2, 1.0, 0.8, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1],
	# 	# accuracy_threshold = 90,
	# 	# accuracy_threshold = 95,
	# 	accuracy_threshold = 80,
	# 	per_target_attention = 1
	# )
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
	# 	last_step_uses_nearest_object = False
	# )
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
	# 	last_step_uses_nearest_object = True
	# )
	# plot_sigma_wrt_targets(
	# 	base_grid_side = 720,
	# 	num_simulations = 50,
	# 	num_time_steps = 25,
	# 	num_objects = 14,
	# 	max_num_targets = 8,
	# 	k = 0.0005,
	# 	lm = 0.9,
	# 	sigma_list = [7, 6.3, 5.6, 5, 4.5, 4, 3.6, 3.3, 3, 2.7, 2.4, 2.1, 1.8, 1.5,
	# 				  1.2, 1.0, 0.8, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1],
	# 	accuracy_threshold = 80,
	# 	last_step_uses_nearest_object = True
	# )
	# plot_sigma_wrt_targets(
	# 	base_grid_side = 720,
	# 	num_simulations = 50,
	# 	num_time_steps = 100,
	# 	num_objects = 14,
	# 	max_num_targets = 8,
	# 	k = 0.0005,
	# 	lm = 0.9,
	# 	sigma_list = [5, 4.5, 4, 3.6, 3.3, 3, 2.7, 2.4, 2.1, 1.8, 1.5,
	# 				  1.2, 1.0, 0.8, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1],
	# 	accuracy_threshold = 80,
	# 	last_step_uses_nearest_object = True
	# )
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
	# 	last_step_uses_nearest_object = True,
	# 	nearest_object_bound = 30
	# )
	# utt, tnt = simulate(
	# 	grid_side = 720,
	# 	num_simulations = 5,
	# 	num_time_steps = 150,
	# 	num_objects = 14,
	# 	num_targets = 4,
	# 	k = 0.0005,
	# 	lm = 0.9,
	# 	sigma = 1,
	# 	print_range=True,
	# )
	# plot_sigma_wrt_targets(
	# 	base_grid_side = 720,
	# 	num_simulations = 50,
	# 	num_time_steps = 35,
	# 	num_objects = 14,
	# 	max_num_targets = 8,
	# 	k = 0.0005,
	# 	lm = 0.9,
	# 	# sigma_list = [5, 4.5, 4, 3.6, 3.3, 3, 2.7, 2.4, 2.1, 1.8, 1.5,
	# 	# 			  1.2, 1.0, 0.8, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1],
	# 	sigma_list = [3, 2.7, 2.4, 2.1, 1.8, 1.5,
	# 				  1.2, 1.0, 0.8, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1],
	# 	accuracy_threshold = 90,
	# 	last_step_uses_nearest_object = True,
	# 	nearest_object_bound = 30 # [12, 15, 20, 25, 30, 35, 40, 45, 50]
	# )
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
	# 	accuracy_threshold = 90,
	# 	last_step_uses_nearest_object = True,
	# 	nearest_object_bound = 30, # [12, 15, 20, 25, 30, 35, 40, 45, 50]
	# 	update_strategy =  "lowest"
	# )
	# plot_harmonic_sigma_wrt_targets(
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
	# 	last_step_uses_nearest_object = True,
	# 	nearest_object_bound = 30
	# )

	# SECTION 4: Recovery
	# plot_recovery_dist_wrt_targets(
	# 	grid_side = 720,
	# 	num_simulations = 50,
	# 	num_time_steps = 50,
	# 	num_objects = 14,
	# 	max_num_targets = 8,
	# 	k = 0.0005,
	# 	lm = 0.9,
	# 	sigma = 2
	# )
	# plot_recovery_dist_wrt_targets(
	# 	grid_side = 720,
	# 	num_simulations = 50,
	# 	num_time_steps = 50,
	# 	num_objects = 14,
	# 	max_num_targets = 8,
	# 	k = 0.0005,
	# 	lm = 0.9,
	# 	sigma = 4
	# )

	# SECTION 5: Nearest object heuristic over random object
	# plot_nearest_object_benefit(
	# 	grid_side=720,
	# 	num_simulations=50,
	# 	num_time_steps=50,
	# 	num_objects=14,
	# 	max_num_targets=8,
	# 	k=0.0005,
	# 	lm=0.9,
	# 	sigma=4
	# )

	# TODO
	# plot_acc_wrt_min_dist(
	# 	grid_side=720,
	# 	num_simulations=50,
	# 	num_time_steps=50,
	# 	num_objects=14,
	# 	max_num_targets=8,
	# 	k=0.0005,
	# 	lm=0.9,
	# 	sigma_list=[4,3,2,1]
	# )

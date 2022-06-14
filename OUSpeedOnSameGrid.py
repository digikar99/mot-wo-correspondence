
import numpy as np
import matplotlib.pyplot as plt
from base import *
import random

class OUSpeedOnSameGrid(BaseMOT):

	def initialize_maps(self, k, lm, sigma):
		# Need to maintain separate object and target maps, since object maps
		# will take care of overlapping
		grid_side = self.grid_side
		num_targets = self.num_targets
		num_objects = self.num_objects
		per_target_attention = self.per_target_attention
		target_map = np.zeros((grid_side, grid_side), dtype="int")
		object_parameters = []
		for x in range(num_targets):
			i, j = rand2d(grid_side, grid_side)
			while target_map[i,j] != 0:
				i, j = rand2d(grid_side, grid_side)
			target_map[i,j] = 1
			object_parameters.append(((i,j), [i,i,j,j, i-grid_side//2, j-grid_side//2, 0, 0, np.inf]))
		object_map = np.copy(target_map)
		for x in range(num_objects - num_targets):
			i, j = rand2d(grid_side, grid_side)
			while object_map[i,j] != 0:
				i, j = rand2d(grid_side, grid_side)
			object_map[i,j] = 1
			object_parameters.append(((i,j), [i,i,j,j, i-grid_side//2, j-grid_side//2, 0, 0, np.inf]))
		predicted_target_map = np.copy(target_map)
		# 6 -> 0.16, 3 -> 0.5, 1 -> 1
		attention_map        = np.copy(target_map) * per_target_attention
		# if num_targets > 1: attention_map = np.ones_like(target_map)

		self.object_map = object_map
		self.objects    = nonzeros(object_map)
		self.target_map = target_map
		self.attention_map = attention_map
		self.predicted_target_map = predicted_target_map
		self.predicted_targets = nonzeros(predicted_target_map)
		self.object_parameters = object_parameters
		self.k, self.lm, self.sigma = k, lm, sigma

	def update_objects_and_targets(self):
		object_map = self.object_map
		target_map = self.target_map
		object_parameters = self.object_parameters
		k, lm, sigma = self.k, self.lm, self.sigma
		grid_side = self.grid_side
		new_object_parameters = []
		objects = self.objects

		# We explicitly need a new_object_map because in updating the object_map,
		# it could happen that some object is moved to a location already with some other objects,
		# and then that object will be moved twice.
		new_object_map    = self._new_object_map
		for (i,j) in objects: new_object_map[i,j] = 0
		assert new_object_map.sum() == 0, "Some object still on new_object_map"
		new_objects = []

		for ((i,j), param) in object_parameters:
			i = int(i)
			j = int(j)
			# print(object_map[i,j])
			mini, maxi, minj, maxj, x, y, vx, vy, min_dist = param
			newvx = int(-k*x + lm*vx + np.random.randn()*sigma)
			newvy = int(-k*y + lm*vy + np.random.randn()*sigma)
			if i + newvx < 0: newvx = -i
			elif i + newvx >= grid_side: newvx = grid_side - i - 1
			if j + newvy < 0: newvy = -j
			elif j + newvy >= grid_side: newvy = grid_side - j - 1

			x += newvx; y += newvy
			newi = i+newvx; newj = j+newvy

			new_object_map[newi, newj] += 1
			new_objects.append((newi, newj))
			if target_map[i,j]:
				target_map[i,j] -= 1
				target_map[newi, newj] += 1

			mini = min(newi, mini)
			maxi = max(newi, maxi)
			minj = min(newj, minj)
			maxj = max(newj, maxj)
			new_object_parameters.append(
				((newi, newj), [mini, maxi, minj, maxj, x, y, newvx, newvy, min_dist])
			)
		for (i,j) in objects: object_map[i,j] -= 1
		for (i,j) in new_objects: object_map[i,j] += 1
		assert len(new_objects) == self.num_objects, \
			"Expected {0} objects, but are only:\n  {1}".format(self.num_objects, num_objects)

		for x in range(self.num_objects):
			(i1, j1) = new_objects[x]
			if target_map[i1,j1] == 0: continue
			min_dist = new_object_parameters[x][1][-1]
			for y in range(self.num_objects):
				if x==y: continue
				(i2, j2) = new_objects[y]
				dist = np.sqrt((i1-i2)**2 + (j1-j2)**2)
				min_dist = min(dist, min_dist)
			new_object_parameters[x][1][-1] = min_dist

		self.object_parameters = new_object_parameters
		self.objects  = new_objects

		return object_map, target_map

def simulate(grid_side, num_simulations, num_time_steps, num_objects,
			 num_targets, k, lm, sigma, print_range=False, return_dist=False,
			 return_min_dist=False, last_step_uses_nearest_object=True,
			 per_target_attention=None, nearest_object_bound=None):
	untracked_targets = []
	tracked_nontargets = []
	min_distances = []
	avg_distances = []
	max_distances = []
	for i in range(num_simulations):
		mot_model = OUSpeedOnSameGrid(grid_side, num_objects, num_targets,
									  per_target_attention = per_target_attention,
									  nearest_object_bound = nearest_object_bound)
		mot_model.initialize_maps(k=k, lm=lm, sigma=sigma)
		for j in range(num_time_steps):
			# if PRINT_MAPS:
			# 	print("\n", j, " ", "="*80, sep="")
			# 	print("Objects", object_map, "Targets", target_map, "Predicted targets",
			# 		  predicted_target_map, "Attention", attention_map, sep="\n")
			mot_model.update_objects_and_targets()
			mot_model.update_predicted_targets()
		if return_dist:
			dist = mot_model.get_recovery_distances()
			if dist is None: continue
			min_dist, avg_dist, max_dist = dist
			min_distances.append(min_dist)
			avg_distances.append(avg_dist)
			max_distances.append(max_dist)
		else:
			errors = mot_model.evaluate(
				print_range,
				return_min_dist=return_min_dist,
				use_nearest_object = last_step_uses_nearest_object
			)
			untracked_targets.append(errors[0])
			tracked_nontargets.append(errors[1])
			if return_min_dist: min_distances.append(errors[2])
	if return_dist: return min_distances, avg_distances, max_distances
	elif return_min_dist: return untracked_targets, tracked_nontargets, min_distances
	else: return untracked_targets, tracked_nontargets

def plot_acc_wrt_targets(
		grid_side, num_simulations, num_time_steps, num_objects, max_num_targets,
		k, lm, sigma,
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
					 k = k, lm = lm, sigma = sigma)

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

	print("Untracked targets: ", num_target_list, untracked_target_list)
	print("Tracked nontargets:", num_target_list, tracked_nontarget_list)
	# plt.plot(num_target_list, untracked_target_list, label="Unattended targets (false-negatives)")
	# plt.plot(num_target_list, tracked_nontarget_list, label="Attended nontargets (false-positives)")
	# plt.plot(
	# 	num_target_list,
	# 	np.add(untracked_target_list, tracked_nontarget_list),
	# 	label = "All errors (false negatives + false positives)"
	# )
	# plt.xlabel("Number of targets ({0} objects)".format(num_objects))
	# plt.ylabel("Number of errors at the end of simulation\n({0} time-steps, averaged across {1} simulations)".format(num_time_steps, num_simulations))
	# plt.title("Attention heuristic: {0}".format(TARGET_HEURISTIC.__name__))
	# plt.legend()
	# plt.show()

	# Number of tracked objects that are targets
	accuracy1 = 100*(num_target_list - untracked_target_list)/num_target_list
	accuracy2 = 100*(num_target_list - tracked_nontarget_list)/num_target_list
	accuracy  = 100*(num_target_list - average_errors)/num_target_list
	# plt.plot(num_target_list, accuracy1)
	# plt.plot(num_target_list, accuracy2)
	# TODO: Think about errorbars!
	plt.errorbar(num_target_list, accuracy, label="Accuracy 1",
				 yerr=100*(average_errors_se/num_target_list))
	plt.errorbar(num_target_list, accuracy1, label="Accuracy 2",
				 yerr=100*(average_errors_se/num_target_list))
	# plt.plot(num_target_list, (accuracy2+accuracy1)/2, label="Accuracy 2")
	plt.xlabel("Number of targets ({0} objects)".format(num_objects))
	plt.ylabel("Accuracy")
	plt.ylim(0, 100)
	plt.title("Accuracy vs Number of targets".format(TARGET_HEURISTIC.__name__))
	plt.legend()
	plt.show()

	return untracked_target_list, tracked_nontarget_list

def plot_acc_wrt_time(
		grid_side, num_simulations, max_time, num_objects, num_targets,
		k, lm, sigma,
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
					 k = k, lm = lm, sigma = sigma, last_step_uses_nearest_object=True)

		untracked_target_list.append(np.mean(untracked_targets))
		untracked_target_list_se.append(np.std(untracked_targets, ddof=1)/np.sqrt(num_simulations))

		tracked_nontarget_list = np.asarray(tracked_nontarget_list)
		average_errors         = np.asarray(average_errors)

		num_time_steps_list.append(num_time_steps)

	num_time_steps_list    = np.asarray(num_time_steps_list)
	untracked_target_list  = np.asarray(untracked_target_list)
	untracked_target_list_se = np.asarray(untracked_target_list_se)
	tracked_nontarget_list = np.asarray(tracked_nontarget_list)
	average_errors         = np.asarray(average_errors)

	print("Untracked targets: ", num_time_steps, untracked_target_list)
	print("Tracked nontargets:", num_time_steps, tracked_nontarget_list)

	# Number of tracked objects that are targets
	accuracy = 100*(num_targets - untracked_target_list)/num_targets
	plt.errorbar(num_time_steps_list, accuracy, label="Accuracy",
				 yerr=100*(untracked_target_list_se/num_targets))
	plt.xlabel("Number of time steps")
	plt.ylabel("Accuracy")
	plt.ylim(0, 100)
	plt.title("Accuracy vs Time of tracking ({0} objects, {1} targets)"
			  .format(num_objects, num_targets))
	plt.legend()
	plt.show()

	return untracked_target_list, tracked_nontarget_list


def plot_sigma_wrt_targets(
		base_grid_side,
		num_simulations,
		num_time_steps,
		num_objects,
		max_num_targets,
		k, lm, max_sigma=None, sigma_list=None,
		accuracy_threshold = 80,
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
						 num_targets, k, lm, sigma)

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

	plt.plot(np.arange(1,max_num_targets+1), sigma_threshold_list)
	plt.xlabel("Number of targets ({0} objects)".format(num_objects))
	plt.ylabel("Sigma threshold ({0}% accuracy)".format(accuracy_threshold))
	plt.title("Velocity (sigma) Threshold vs Number of Targets\n({0} Simulations, {1} Time Steps)"\
			  .format(
				  num_simulations,
				  num_time_steps
			  ))
	plt.ylim(0, np.max(sigma_list))
	plt.show()
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

	plt.clf()
	plt.scatter(x = xes, y = min_distances,  c="blue",   label="Minimum")
	plt.scatter(x = xes, y = avg_distances,  c="yellow", label="Average")
	plt.scatter(x = xes, y = max_distances,  c="red",    label="Maximum")
	plt.xlabel("Number of targets")
	plt.ylabel("Recovered distance")
	plt.title("Distance recovered by nearest-object-heuristic\nat last time step vs Number of targets"\
			  + "\n(sigma={0} | {1} objects, {2} simulations, {3} time steps)"\
			  .format(sigma, num_objects, num_simulations, num_time_steps))
	plt.legend()
	plt.show()
	pass



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
	plt.xlabel("Minimum distances between targets")
	plt.ylabel("Accuracy")
	plt.title("Accuracy vs Minimum distance between the target and the nearest object"\
			  + "\n({0} targets, {1} objects, {2} simulations, {3} time steps)"\
			  .format(4, num_objects, num_simulations, num_time_steps))
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
					 k = k, lm = lm, sigma = sigma, last_step_uses_nearest_object=True)
		nearest_accuracy = 100 * (num_targets
								  - 0.5*(np.mean(tracked_nontargets)
										   + np.mean(untracked_targets)))/num_targets
		# Merely setting nearest_object_bound is not enough, because it affects all the
		# intervening steps of the trial
		untracked_targets, tracked_nontargets = \
			simulate(grid_side, num_simulations, num_time_steps, num_objects, num_targets,
					 k = k, lm = lm, sigma = sigma, last_step_uses_nearest_object=False)
		random_accuracy = 100 * (num_targets
								  - 0.5*(np.mean(tracked_nontargets)
										   + np.mean(untracked_targets)))/num_targets
		nearest_accuracies.append(nearest_accuracy)
		random_accuracies.append(random_accuracy)

	plt.plot(np.arange(1,max_num_targets+1), nearest_accuracies, label="Nearest Object Heuristic")
	plt.plot(np.arange(1,max_num_targets+1), random_accuracies, label="Random Object Heuristic")
	plt.xlabel("Number of Targets")
	plt.ylabel("Accuracy")
	plt.ylim(0, 100)
	plt.title("Nearest Object Heuristic Benefit")
	plt.legend()
	plt.show()



if __name__ == "__main__":
	np.random.seed(42)
	# USE generate_plot_data.py and generate_plots.py INSTEAD OF THIS ==========
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
	# plot_acc_wrt_targets(
	# 	grid_side = 720,
	# 	num_simulations = 50,
	# 	num_time_steps = 50,
	# 	num_objects = 14,
	# 	max_num_targets = 8,
	# 	k = 0.0005,
	# 	lm = 0.9,
	# 	sigma = 4,
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
	# )
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
	# 	# sigma_list = [5, 4.5, 4, 3.6, 3.3, 3, 2.7, 2.4, 2.1, 1.8, 1.5,
	# 	# 			  1.2, 1.0, 0.8, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1],
	# 	sigma_list = [3, 2.7, 2.4, 2.1, 1.8, 1.5,
	# 				  1.2, 1.0, 0.8, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1],
	# 	accuracy_threshold = 90,
	# 	# accuracy_threshold = 95,
	# 	# accuracy_threshold = 80,
	# )
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
	# USE generate_plot_data.py and generate_plots.py INSTEAD OF THIS ==========

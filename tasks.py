
from Environment import OrnsteinUhlenbeckEnvironment, ExperimentalEnvironment
from MOMIT import MOMIT
from OurMOTModel import OurMOTModel
import json
import math

MIN_DIST = 10
NUM_BAD_TRIALS = 0

def is_bad_trial(trial, num_targets, min_dist=MIN_DIST):
	# Return True, if trial contains two targets that reached a distance of min_dist with each other
	object_list = trial["object_list"]
	num_updates = len(object_list[0]["histi"])
	num_objects = len(object_list)
	for o1_idx in range(num_targets):
		o1 = object_list[o1_idx]
		for o2_idx in range(o1_idx+1, (num_targets if num_targets>1 else num_objects)):
		# Let's test accuracies if two objects never come close
		# for o2_idx in range(o1_idx+1, num_objects):

			o2 = object_list[o2_idx]
			for t in range(num_updates):
				i1 = o1["histi"][t]
				j1 = o1["histj"][t]
				i2 = o2["histi"][t]
				j2 = o2["histj"][t]
				dist_square = (i1-i2)**2 + (j1-j2)**2
				if dist_square < min_dist**2:
					global NUM_BAD_TRIALS
					# print("discarding a bad trial", NUM_BAD_TRIALS)
					NUM_BAD_TRIALS += 1
					return True
	return False

def evaluate_tracking(env, model):
	true_target_locations = env.get_target_locations()
	num_targets           = env.num_targets
	assert num_targets == len(true_target_locations), "{0}, {1}".format(num_targets, true_target_locations)

	# Ensure that attended locations actually correspond to objects
	for _ in range(num_targets): model.process_env(env)

	# We consider that the x and foveation figure only in the mathematical *analysis*
	# of the model rather than the functional/computational model itself
	attended_locations = model.get_attended_locations(env, num_locations=num_targets)

	attended_targets    = 0 # Number of attended objects that are targets
	ttl = true_target_locations.copy()
	for loc in attended_locations:
		if loc in ttl:
			ttl.remove(loc)
			attended_targets += 1

	unattended_targets = 0 # Number of targets that are unattended
	al = attended_locations.copy()
	for loc in true_target_locations:
		if loc not in al: unattended_targets += 1
		else: al.remove(loc)

	assert num_targets - attended_targets == unattended_targets, \
		"""
Num targets: {0}
  Attended targets: {1}, Unattended targets: {2}
  Attended locations: {3}\n  Target locations: {4}
""".format(num_targets, attended_targets, unattended_targets, attended_locations, true_target_locations)

	# Proportion of true targets that were tracked aka attended.
	# Also the same as: proportion of tracked/attended objects that are true targets.
	# Note that by the assertion above, averaging is pointless, but we do it anyways.
	accuracy = 0.5 * (attended_targets + num_targets - unattended_targets) / num_targets

	return accuracy


def evaluate_id(env, model):
	"Returns proportions of IDs that were correctly labelled"
	true_target_locations = env.get_target_locations()
	num_targets           = env.num_targets
	assert num_targets == len(true_target_locations), \
		"{0}, {1}".format(num_targets, true_target_locations)

	# Ensure that attended locations actually correspond to objects
	for _ in range(num_targets): model.process_env(env)

	predicted_id_map = model.get_target_location_id_map(env)
	true_id_map = env.get_target_location_id_map()

	# print(model)
	# print(predicted_id_map.keys())
	# print(true_id_map.keys())

	num_correct = 0
	num_total   = len(true_id_map)
	# print("id evaluation:")
	# print(predicted_id_map.keys())
	# print(true_id_map.keys())
	for loc in predicted_id_map:
		if loc in true_id_map:
			# print("  ", loc, "in both")
			predicted_id = predicted_id_map[loc]
			true_id = true_id_map[loc]
			# print("   Predicted ID:", predicted_id, "  True ID:", true_id)
			if predicted_id == true_id: num_correct += 1
	return num_correct/num_total


def evaluate_id(env, model, return_swaps=False):
	"""
	Returns a tuple of two values indicating
	- the presence of TT swap
	- the presence of TN swap
	"""
	true_target_locations = env.get_target_locations()
	num_targets           = env.num_targets
	assert num_targets == len(true_target_locations), \
		"{0}, {1}".format(num_targets, true_target_locations)

	# Ensure that attended locations actually correspond to objects
	for _ in range(num_targets): model.process_env(env)

	predicted_id_map = model.get_target_location_id_map(env)
	true_id_map = env.get_target_location_id_map()

	# print(model)
	# print(predicted_id_map.keys())
	# print(true_id_map.keys())

	num_correct = 0
	num_total   = len(true_id_map)

	# print("id evaluation:")
	# print(predicted_id_map.keys())
	# print(true_id_map.keys())
	for loc in predicted_id_map:
		if loc in true_id_map:
			# print("  ", loc, "in both")
			predicted_id = predicted_id_map[loc]
			true_id = true_id_map[loc]
			# print("   Predicted ID:", predicted_id, "  True ID:", true_id)
			if predicted_id == true_id: num_correct += 1

	if return_swaps:
		tt_swaps, tn_swaps, both_swaps, none_swaps = 0, 0, 0, 0
		if num_correct == num_total: none_swaps = 1
		else:
			for loc in predicted_id_map:
				if loc in true_id_map:
					predicted_id = predicted_id_map[loc]
					true_id = true_id_map[loc]
					if predicted_id != true_id: tt_swaps = 1
				else:
					tn_swaps = 1
			# both_swaps = tt_swaps * tn_swaps
			if tt_swaps and tn_swaps:
				tt_swaps, tn_swaps, both_swaps = 0, 0, 1
		return num_correct/num_total, tt_swaps, tn_swaps, both_swaps, none_swaps
	else: return num_correct/num_total



def simulate_mot(grid_side, num_simulations, num_time_steps, num_objects,
				 num_targets, k, lm, sigma, episodic_buffer_size=4, episodic_buffer_decay_rate=0.9,
				 per_target_attention=None, nearest_object_bound=None, update_strategy="random",
				 return_id_accuracy=False, use_static_indices=True, id_only_for_perfect_tracking=False,
				 return_swap_count=False):

	momit_tracking_accuracies = []
	our_tracking_accuracies   = []

	momit_id_accuracies = []
	our_id_accuracies   = []

	our_tt_swaps = []
	our_tn_swaps = []
	our_both_swaps = []
	our_none_swaps = []

	for _ in range(num_simulations):

		env = OrnsteinUhlenbeckEnvironment(
			shape = (grid_side, grid_side),
			object_type_count_map = {0: num_objects},
			num_targets = num_targets,
			k = k, lm = lm, sigma = sigma
		)
		env.initialize_random()
		momit_model = MOMIT(episodic_buffer_size, episodic_buffer_decay_rate, {0: 0.2},
							use_static_indices = use_static_indices)
		our_model = OurMOTModel(
			num_targets,
			per_target_attention = per_target_attention,
			nearest_object_bound = nearest_object_bound
		)
		momit_model.process_env(env, observe_targets=True)
		our_model.process_env(env, observe_targets=True)

		for _ in range(num_time_steps):
			env.update_object_map()
			momit_model.process_env(env)
			our_model.process_env(env, strategy=update_strategy)

		momit_tracking_accuracy = evaluate_tracking(env, momit_model)
		our_tracking_accuracy   = evaluate_tracking(env, our_model)

		momit_id_accuracy       = evaluate_id(env, momit_model)
		our_id_accuracy, tt_swaps, tn_swaps, both_swaps, none_swaps = \
			evaluate_id(env, our_model, return_swaps=True)

		momit_tracking_accuracies.append(momit_tracking_accuracy)
		our_tracking_accuracies.append(our_tracking_accuracy)

		if not id_only_for_perfect_tracking or momit_tracking_accuracy==1:
			momit_id_accuracies.append(momit_id_accuracy)
		if not id_only_for_perfect_tracking or our_tracking_accuracy==1:
			our_id_accuracies.append(our_id_accuracy)

		# tt_swaps, tn_swaps, both_swaps, none_swaps = 0, 0, 0, 0
		# if our_id_accuracy == 1: none_swaps = 1
		# else:
		# 	num_lost_targets = (1-our_tracking_accuracy)*num_targets
		# 	num_lost_ids     = (1-our_id_accuracy)*num_targets

		# 	tt_swaps = (num_lost_targets == 0) or \
		# 		(num_lost_targets == 1 and num_lost_ids > 1) or \
		# 		(num_lost_targets > 1)
		# 	tn_swaps = (num_lost_targets == 1 and num_lost_ids == 1) or \
		# 		(num_lost_targets >= 1 and num_lost_ids > 1)

		# 	tt_swaps, tn_swaps = int(tt_swaps), int(tn_swaps)

		# 	# if tn_swaps and tt_swaps:
		# 		# tt_swaps, tn_swaps, both_swaps = 0,0,1
		# 	both_swaps = tt_swaps * tn_swaps

		our_tt_swaps.append(tt_swaps)
		our_tn_swaps.append(tn_swaps)
		our_both_swaps.append(both_swaps)
		our_none_swaps.append(none_swaps)

		# print(our_tracking_accuracy, our_id_accuracy, tt_swaps, tn_swaps, both_swaps, none_swaps)


	return_values = [momit_tracking_accuracies, our_tracking_accuracies]

	if return_id_accuracy: return_values += [momit_id_accuracies, our_id_accuracies]
	if return_swap_count: return_values += [our_tt_swaps, our_tn_swaps, our_both_swaps, our_none_swaps]
	return return_values


def simulate_mot_using_experimental_data(
		grid_side, max_num_targets, json_filename, model_updates_per_time_step, episodic_buffer_size=4,
		episodic_buffer_decay_rate=0.9, per_target_attention=None, nearest_object_bound=None,
		update_strategy="random", return_id_accuracy=False, use_static_indices=True,
		id_only_for_perfect_tracking=False, return_swap_count=False,
		return_final_attended_location_count=False):

	momit_tracking_accuracies = {}
	our_tracking_accuracies   = {}

	momit_id_accuracies = {}
	our_id_accuracies   = {}

	our_final_attended_location_count = {0: 0}

	for i in range(1, max_num_targets+1):
		momit_tracking_accuracies[i] = []
		our_tracking_accuracies[i]   = []
		momit_id_accuracies[i] = []
		our_id_accuracies[i]   = []
		our_final_attended_location_count[i] = 0

	our_tt_swaps = []
	our_tn_swaps = []
	our_both_swaps = []
	our_none_swaps = []

	with open(json_filename) as f: json_data = json.load(f)
	all_trial_data  = json_data["all_trial_data"]
	num_practice_trials = json_data["session_details"]["num_practice_trials"]
	all_trial_data = all_trial_data[num_practice_trials:]
	num_simulations = len(all_trial_data)

	# IMPORTANT: We want to maintain the semantics of time_step
	# One time_step denotes one OU update; thus per_ou_update and per_time_step are synonymous

	session_details = json_data["session_details"]
	refreshes_per_ou_update = 1 / session_details["ou_updates_per_refresh"]
	time_steps_per_model_update = 1 / model_updates_per_time_step

	# print("Will update model every {0} time steps".format(model_updates_per_time_step))
	for simulation_idx in range(num_simulations):
	# for simulation_idx in range(2):

		trial_data     = all_trial_data[simulation_idx]
		num_time_steps = trial_data["num_time_steps"]
		num_targets = trial_data["num_targets"]
		# if is_bad_trial(trial_data, num_targets): continue

		env = ExperimentalEnvironment(
			shape = (grid_side, grid_side),
			trial_data = trial_data,
		)
		env.initialize_random()
		momit_model = MOMIT(episodic_buffer_size, episodic_buffer_decay_rate, {0: 0.2},
							use_static_indices = use_static_indices)
		our_model = OurMOTModel(
			num_targets,
			per_target_attention = per_target_attention,
			nearest_object_bound = nearest_object_bound
		)
		momit_model.process_env(env, observe_targets=True)
		our_model.process_env(env, observe_targets=True)
		model_updates_so_far = 0

		# print(env.time_elapsed, -1, env.time_elapsed / refreshes_per_ou_update)
		# print("env", env.get_target_locations())
		# print("our", our_model.get_attended_locations(env))


		for t in range(num_time_steps):
			for _ in range(math.ceil(max(refreshes_per_ou_update, model_updates_per_time_step))):
				if env.is_trial_done(): continue
				if (env.time_elapsed < (t + 1) * refreshes_per_ou_update):
					env.update_object_map()
				if model_updates_so_far < (t + 1) * model_updates_per_time_step:
					momit_model.process_env(env)
					our_model.process_env(env, strategy=update_strategy)
					model_updates_so_far += 1

			# while (not env.is_trial_done())\
			# 	  and (env.time_elapsed / refreshes_per_ou_update < t):
			# 	env.update_object_map()
			# 	# print(env.time_elapsed, t, env.time_elapsed / refreshes_per_ou_update)
			# 	# print("env", env.get_target_locations())
			# 	if model_updates_so_far < (t + 1) * model_updates_per_time_step:
			# 		momit_model.process_env(env)
			# 		# print("our before", our_model.num_targets, our_model.get_attended_locations(env))
			# 		our_model.process_env(env, strategy=update_strategy)
			# 		# print("our after", our_model.num_targets, our_model.get_attended_locations(env))
			# 		model_updates_so_far += 1

			# while model_updates_so_far < (t + 1) * model_updates_per_time_step:
			# 	momit_model.process_env(env)
			# 	# print("our before", our_model.num_targets, our_model.get_attended_locations(env))
			# 	our_model.process_env(env, strategy=update_strategy)
			# 	# print("our after", our_model.num_targets, our_model.get_attended_locations(env))
			# 	model_updates_so_far += 1


		momit_tracking_accuracy = evaluate_tracking(env, momit_model)
		our_tracking_accuracy   = evaluate_tracking(env, our_model)

		momit_id_accuracy       = evaluate_id(env, momit_model)
		our_id_accuracy, tt_swaps, tn_swaps, both_swaps, none_swaps = \
			evaluate_id(env, our_model, return_swaps=True)

		num_attended_locations = len(our_model.get_attended_locations(env))

		momit_tracking_accuracies[num_targets].append(momit_tracking_accuracy)
		our_tracking_accuracies[num_targets].append(our_tracking_accuracy)

		if not id_only_for_perfect_tracking or momit_tracking_accuracy==1:
			momit_id_accuracies[num_targets].append(momit_id_accuracy)
		if not id_only_for_perfect_tracking or our_tracking_accuracy==1:
			our_id_accuracies[num_targets].append(our_id_accuracy)

		our_final_attended_location_count[num_attended_locations] += 1

		# tt_swaps, tn_swaps, both_swaps, none_swaps = 0, 0, 0, 0
		# if our_id_accuracy == 1: none_swaps = 1
		# else:
		# 	num_lost_targets = (1-our_tracking_accuracy)*num_targets
		# 	num_lost_ids     = (1-our_id_accuracy)*num_targets

		# 	tt_swaps = (num_lost_targets == 0) or \
		# 		(num_lost_targets == 1 and num_lost_ids > 1) or \
		# 		(num_lost_targets > 1)
		# 	tn_swaps = (num_lost_targets == 1 and num_lost_ids == 1) or \
		# 		(num_lost_targets >= 1 and num_lost_ids > 1)

		# 	tt_swaps, tn_swaps = int(tt_swaps), int(tn_swaps)

		# 	# if tn_swaps and tt_swaps:
		# 		# tt_swaps, tn_swaps, both_swaps = 0,0,1
		# 	both_swaps = tt_swaps * tn_swaps

		our_tt_swaps.append(tt_swaps)
		our_tn_swaps.append(tn_swaps)
		our_both_swaps.append(both_swaps)
		our_none_swaps.append(none_swaps)

		# print(our_tracking_accuracy, our_id_accuracy, tt_swaps, tn_swaps, both_swaps, none_swaps)


	return_values = [momit_tracking_accuracies, our_tracking_accuracies]

	if return_id_accuracy: return_values += [momit_id_accuracies, our_id_accuracies]
	if return_swap_count: return_values += [our_tt_swaps, our_tn_swaps, our_both_swaps, our_none_swaps]
	if return_final_attended_location_count: return_values += [our_final_attended_location_count]
	return return_values



def simulate_mit(grid_side, num_simulations, num_time_steps, num_objects,
				 num_targets, k, lm, sigma, episodic_buffer_size=4, episodic_buffer_decay_rate=0.9,
				 per_target_attention=None, nearest_object_bound=None, update_strategy="random"):

	momit_accuracies = []
	our_accuracies   = []

	object_type_count_map = dict()
	object_type_cost = dict()
	for i in range(num_objects):
		object_type_count_map[i] = 1
		object_type_cost[i] = 0.2

	for _ in range(num_simulations):

		env = OrnsteinUhlenbeckEnvironment(
			shape = (grid_side, grid_side),
			object_type_count_map = object_type_count_map,
			num_targets = num_targets,
			k = k, lm = lm, sigma = sigma
		)
		env.initialize_random()
		momit_model = MOMIT(episodic_buffer_size, episodic_buffer_decay_rate, object_type_cost)
		our_model = OurMOTModel(
			num_targets,
			per_target_attention = per_target_attention,
			nearest_object_bound = nearest_object_bound
		)
		momit_model.process_env(env, observe_targets=True)
		our_model.process_env(env, observe_targets=True)
		for _ in range(num_time_steps):
			env.update_object_map()
			momit_model.process_env(env)
			our_model.process_env(env, strategy=update_strategy)
		momit_accuracies.append(evaluate_tracking(env, momit_model))
		our_accuracies.append(evaluate_tracking(env, our_model))

	return momit_accuracies, our_accuracies



if __name__=="__main__":
	acc = simulate_mot(
		grid_side = 720,
		num_simulations = 50,
		num_time_steps = 200,
		num_objects = 14,
		num_targets = 4,
		episodic_buffer_size = 4,
		k = 0.0005,	lm = 0.9, sigma = 4,
	)
	print(acc)

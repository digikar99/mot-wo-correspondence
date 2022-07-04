
from Environment import OrnsteinUhlenbeckEnvironment
from MOMIT import MOMIT
from OurMOTModel import OurMOTModel


def evaluate(env, model):
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

	# Proportion of true targets that were tracked aka attended
	# Also the same as: proportion of tracked/attended objects that are true targets
	accuracy = 0.5 * (attended_targets + num_targets - unattended_targets) / num_targets

	return accuracy


def simulate_mot(grid_side, num_simulations, num_time_steps, num_objects,
				 num_targets, k, lm, sigma, episodic_buffer_size=4, episodic_buffer_decay_rate=0.9,
				 per_target_attention=None, nearest_object_bound=None, update_strategy="random"):

	momit_accuracies = []
	our_accuracies   = []

	for _ in range(num_simulations):

		env = OrnsteinUhlenbeckEnvironment(
			shape = (grid_side, grid_side),
			object_type_count_map = {0: num_objects},
			num_targets = num_targets,
			k = k, lm = lm, sigma = sigma
		)
		env.initialize_random()
		initial_target_locations = env.get_target_locations()
		momit_model = MOMIT(episodic_buffer_size, episodic_buffer_decay_rate, {0: 0.2})
		our_model = OurMOTModel(
			num_targets,
			per_target_attention = per_target_attention,
			nearest_object_bound = nearest_object_bound
		)
		momit_model.process_env(env, initial_target_locations)
		our_model.process_env(env, initial_target_locations)
		for _ in range(num_time_steps):
			env.update_object_map()
			momit_model.process_env(env)
			our_model.process_env(env, strategy=update_strategy)
		momit_accuracies.append(evaluate(env, momit_model))
		our_accuracies.append(evaluate(env, our_model))

	return momit_accuracies, our_accuracies


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
		initial_target_locations = env.get_target_locations()
		momit_model = MOMIT(episodic_buffer_size, episodic_buffer_decay_rate, object_type_cost)
		our_model = OurMOTModel(
			num_targets,
			per_target_attention = per_target_attention,
			nearest_object_bound = nearest_object_bound
		)
		momit_model.process_env(env, initial_target_locations)
		our_model.process_env(env, initial_target_locations)
		for _ in range(num_time_steps):
			env.update_object_map()
			momit_model.process_env(env)
			our_model.process_env(env, strategy=update_strategy)
		momit_accuracies.append(evaluate(env, momit_model))
		our_accuracies.append(evaluate(env, our_model))

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

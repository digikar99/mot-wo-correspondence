
import numpy as np
import random
import sparse

def rand2d(max_x, max_y):
	i = np.floor(np.random.random()*max_x*max_y)
	x = int(i % max_x)
	y = int(i // max_x)
	return x,y

def nonzeros(array): return np.asarray(np.nonzero(array), dtype="int").T

def new_coordinate(old_coordinate, axis_size):
	rand = np.random.random()
	if rand <= 0.33: return max(0, old_coordinate - 1)
	elif rand <= 0.67: return old_coordinate
	else: return min(axis_size-1, old_coordinate + 1)

def move_randomly(old_i, old_j):
	new_i = new_coordinate(old_i, GRID_SIDE)
	if new_i == old_i: return old_i, new_coordinate(old_j, GRID_SIDE)
	else: return new_i, old_j

def truncate(num, min_val, max_val):
	if num < min_val: return min_val
	elif num > max_val: return max_val
	else: return num

def nearest_object_heuristic(objects, i, j, bound=None):

	if (i,j) in objects: return (i,j)
	objects = sorted(objects, key=lambda x: (x[0]-i)**2 + (x[1]-j)**2)
	# print(i, j, "\n", objects)
	if bound is None: return objects[0]
	else:
		newi, newj = objects[0]
		dist = np.sqrt((i-newi)**2 + (j-newj)**2)
		if dist <= bound: return objects[0]
		else: return random.choice(objects)

		# # Resulted in a constant curve
		# x = 0
		# while x < len(objects):
		# 	newi, newj = objects[x]
		# 	dist = np.sqrt((i-newi)**2 + (j-newj)**2)
		# 	if dist <= bound: x += 1
		# 	else: break
		# if x == 0: return random.choice(objects)
		# else: return random.choice(objects[:x])

GRID_SIDE = None
# used in update_predicted_targets
# PRINT_TARGET_UPDATES = True
PRINT_TARGET_UPDATES = False

# used in simulate
# PRINT_MAPS = True
PRINT_MAPS = False

# PRINT_EVALUATE_MAPS = True
PRINT_EVALUATE_MAPS = False
PER_TARGET_ATTENTION = 1

class BaseMOT:

	def __init__(self, grid_side, num_objects, num_targets,
				 nearest_object_bound=None, per_target_attention=None):
		self.grid_side   = grid_side
		self.num_objects = num_objects
		self.num_targets = num_targets
		if per_target_attention is None:
			# self.per_target_attention = 1
			# self.per_target_attention = 1 - np.log(num_targets)/np.log(8) + 0.01
			# self.per_target_attention = 1.1 / (1.2)**num_targets
			# self.per_target_attention = 1.5 / num_targets
			# self.per_target_attention = 0.8
			# self.per_target_attention = [1,0.95,0.9,0.8,0.6,0.4,0.1,0.03][num_targets-1]
			self.per_target_attention = [1,0.85,0.7,0.6,0.5,0.3,0.1,0.03][num_targets-1]
			# self.per_target_attention = 0
		else:
			self.per_target_attention = per_target_attention

		if nearest_object_bound is None:
			self.nearest_object_bound = np.inf
			# self.nearest_object_bound = 30
			# self.nearest_object_bound = 0
			# self.nearest_object_bound = 30 / self.per_target_attention
		else:
			self.nearest_object_bound = nearest_object_bound

		self._new_predicted_target_map = np.zeros((grid_side, grid_side), dtype="int")
		self._new_attention_map        = np.zeros((grid_side, grid_side))
		self._new_object_map           = np.zeros((grid_side, grid_side), dtype="int")

	def update_predicted_targets(self, evaluate=False, strategy="random"):
		"""
		evaluate: if True, updates each predicted target location so that each points
		  to an object
		strategy: Possible values - random, lowest
		  - lowest corresponds to updating the predicted target with lowest activation
			in accordance with the Oksama's MOMIT model (adapted for MOT task)
		  - random corresponds to updating in accordance with the values of
			per_target_attention
		"""
		# we cannot use target_map
		objects                   = self.objects
		per_target_attention      = self.per_target_attention

		predicted_target_map  = self.predicted_target_map
		predicted_targets     = self.predicted_targets

		new_predicted_target_map = self._new_predicted_target_map
		for (i,j) in predicted_targets: new_predicted_target_map[i,j] = 0
		new_predicted_targets = []
		# print("Object map\n", object_map)
		# print("Predicted_Target map\n", predicted_target_map)

		if evaluate:
			for (i,j) in predicted_targets:
				# if PRINT_TARGET_UPDATES: print("  Updating", (i,j))
				newi, newj = nearest_object_heuristic(objects, i, j, bound=self.nearest_object_bound)
				# new_predicted_target_map[newi, newj] += 1
				new_predicted_targets.append((newi, newj))
		elif strategy == "random":
			for (i,j) in predicted_targets:
				# if PRINT_TARGET_UPDATES: print("  Updating", (i,j))
				if np.random.random() >= per_target_attention:
					newi, newj = nearest_object_heuristic(objects, i, j, bound=self.nearest_object_bound)
					# new_predicted_target_map[newi, newj] += 1
					new_predicted_targets.append((newi, newj))
				else:
					# new_predicted_target_map[i,j] += 1
					new_predicted_targets.append((i,j))
		elif strategy == "lowest":
			i, j = predicted_targets[0]
			newi, newj = nearest_object_heuristic(objects, i, j, bound=self.nearest_object_bound)
			new_predicted_targets = predicted_targets[1:] + [(newi,newj)]
		else:
			raise Exception("Unknown strategy: " + strategy)

		for (i,j) in predicted_targets: predicted_target_map[i,j] -= 1
		for (i,j) in new_predicted_targets: predicted_target_map[i,j] += 1
		self.predicted_targets = new_predicted_targets
		return predicted_target_map

	def get_recovery_distances(self):
		"""
		For predicted targets that are not pointing to objects, returns
		the minimum, average, and maximum distance of across such targets from
		each corresponding nearest object.

		"""
		object_map = self.object_map
		objects    = self.objects

		predicted_targets    = self.predicted_targets
		predicted_target_map = self.predicted_target_map

		new_predicted_targets = []
		min_dist = np.inf
		max_dist = - np.inf
		distances = []
		for (i,j) in predicted_targets:
			if predicted_target_map[i,j] - object_map[i,j] > 0:
				objects = sorted(objects, key=lambda x: (x[0]-i)**2 + (x[1]-j)**2)
				newi, newj = objects[0]
				dist = np.sqrt((i-newi)**2 + (j-newj)**2)
				distances.append(dist)
				min_dist = min(dist, min_dist)
				max_dist = max(dist, max_dist)
			else:
				continue
		# This assertion will not hold
		# assert len(distances) == self.num_targets, \
		# 	"Expected {0} target distances, but are: ".format(self.num_targets, distances)
		if len(distances):
			return min_dist, np.median(distances), max_dist
		else:
			return None


	def ensure_predicted_are_objects(self):
		object_map = self.object_map
		objects    = self.objects

		predicted_targets    = self.predicted_targets
		predicted_target_map = self.predicted_target_map
		assert len(predicted_targets) == self.num_targets

		new_predicted_targets = []
		for (i,j) in predicted_targets:
			if predicted_target_map[i,j] - object_map[i,j] > 0:
				newi, newj = random.choice(objects)
				predicted_target_map[i,j]       -= 1
				predicted_target_map[newi,newj] += 1
				new_predicted_targets.append((newi, newj))
			else:
				new_predicted_targets.append((i,j))
		# print("New targets: {0}\nOld targets: {1}".format(new_predicted_targets, predicted_targets))
		assert len(new_predicted_targets) == self.num_targets, \
			"New targets: {0}\nOld targets: {1}".format(new_predicted_targets, predicted_targets)

		self.predicted_targets = new_predicted_targets


	def evaluate(self, print_range=False, return_min_dist=False, use_nearest_object=True):
		"""
		Returns two values:
		- number of targets that are untracked (false-negatives).
		- and number of tracked objects that are non-targets (false-positives),
		"""
		object_map = self.object_map
		target_map = self.target_map

		# bounded nearest object heuristic
		if use_nearest_object: self.update_predicted_targets(evaluate=True)
		# random object heuristic
		else: self.ensure_predicted_are_objects()

		predicted_target_map = self.predicted_target_map

		# update_objects_and_targets(object_map, target_map, move_randomly)
		if PRINT_EVALUATE_MAPS:
			print("Object map:")
			print(object_map)
			print("Target map:")
			print(target_map)
			print("Predicted target map:")
			print(predicted_target_map)

		# print("predicted", predicted_target_locations.shape)
		## Update to ensure that all predicted targets are actually objects
		if PRINT_EVALUATE_MAPS:
			print("Predicted target map:")
			print(predicted_target_map)
		assert np.sum(object_map) == self.num_objects, \
			"Expected {0} objects, but are {1}".format(self.num_objects, np.sum(object_map))
		assert np.sum(target_map) == self.num_targets, \
			"Expected {0} targets, but are {1}".format(self.num_targets, np.sum(target_map))
		assert np.sum(np.logical_and(
			(predicted_target_map != 0),
			((object_map == 0))
		)) == 0, "Attending to non-objects!"

		untracked_targets = np.sum(
			np.multiply(
				predicted_target_map == 0,
				target_map * (target_map != 0)
			)
		)
		tracked_nontargets = np.sum(
			np.multiply(
				predicted_target_map * (predicted_target_map != 0),
				target_map == 0,
			)
		)

		if print_range:
			print("="*80)
			rangeis = []
			rangejs = []
			min_dist = np.inf
			for (key, obj_param) in self.object_parameters:
				mini, maxi, minj, maxj = obj_param[:4]
				min_dist_this = obj_param[8]
				rangei = abs(maxi-mini)
				rangej = abs(maxj-minj)
				rangeis.append(rangei)
				rangejs.append(rangej)
				print(key, abs(maxi-mini), abs(maxj-minj), min_dist_this)
				min_dist = min(min_dist, min_dist_this)
			print("Average:", np.mean(rangeis), np.mean(rangejs), "Min dist:", min_dist)

		if return_min_dist:
			min_dist = np.inf
			for (key, obj_param) in self.object_parameters:
				min_dist_this = obj_param[8]
				min_dist = min(min_dist, min_dist_this)
			return untracked_targets, tracked_nontargets, min_dist
		else:
			return untracked_targets, tracked_nontargets

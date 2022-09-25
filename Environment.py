
import numpy as np
import random
from OurMultiDict import MultiDict

def rand2d(max_x, max_y):
	# TODO: Generalize to nD
	i = np.floor(np.random.random()*max_x*max_y)
	x = int(i % max_x)
	y = int(i // max_x)
	return x,y

# Euclidean distance - bound itself is interpreted as manhattan distance though
# Code for both methods is kept, because euclidean can result in more accurate
# performance it seems; TODO: Refactor to allow making nearest_object_heuristic
# a parameter in relevant code
# def nearest_object_heuristic(env, i, j, bound=None):
# 	ilim, jlim = env.shape
# 	object_locations = env.get_object_locations()
# 	object_found = False
# 	if bound is None: bound = max(ilim, jlim)
# 	for ortho_search_radius in range(0, bound):
# 		mini = max(i-ortho_search_radius, 0)
# 		maxi = min(i+ortho_search_radius+1, ilim)
# 		minj = max(j-ortho_search_radius, 0)
# 		maxj = min(j+ortho_search_radius+1, jlim)

# 		# print(mini, maxi, minj, maxj)

# 		if object_found: break
# 		newi = mini
# 		for newj in range(minj, maxj):
# 			# print("  top", (newi, newj), object_locations)
# 			if (newi,newj) in object_locations: object_found=True; break

# 		if object_found: break
# 		newi = maxi-1
# 		for newj in range(minj, maxj):
# 			# print("  bottom", (newi, newj), object_locations)
# 			if (newi,newj) in object_locations: object_found=True; break

# 		if object_found: break
# 		newj = minj
# 		for newi in range(mini, maxi):
# 			# print("  left", (newi, newj), object_locations)
# 			if (newi,newj) in object_locations: object_found=True; break
# 		if object_found: break
# 		newj = maxj-1
# 		for newi in range(mini, maxi):
# 			# print("  right", (newi, newj), object_locations)
# 			if (newi,newj) in object_locations: object_found=True; break

# 	if not object_found:
# 		newi, newj = random.choice(object_locations)

# 	# print(object_found, ortho_search_radius, "Old", (i,j), "New", (newi, newj))
# 	# print(object_locations)
# 	# print()
# 	dist = np.sqrt((i-newi)**2 + (j-newj)**2)
# 	return (newi, newj), dist


# Manhattan distance
def nearest_object_heuristic(env, i, j, bound=None):
	ilim, jlim = env.shape
	object_locations = env.get_object_locations()
	if bound is None: bound = max(ilim, jlim)

	manhattan_distance = 0
	newi, newj = i, j
	direction = "up_right"

	while True:

		if -1 < newi < ilim and -1 < newj < jlim \
		   and (newi, newj) in object_locations:
			return ((newi, newj), manhattan_distance)

		if direction == "up_right":
			if j == newj:
				direction = "down_right"
			else:
				newi -= 1
				newj += 1
		elif direction == "down_right":
			if i == newi:
				direction = "down_left"
			else:
				newi += 1
				newj += 1
		elif direction == "down_left":
			if j == newj:
				direction = "up_left"
			else:
				newi += 1
				newj -= 1
		elif direction == "up_left":
			if newi == i:
				manhattan_distance += 1
				newi = i
				newj = j - manhattan_distance
				direction = "up_right"
			else:
				newi -= 1
				newj -= 1

	if not object_found:
		newi, newj = random.choice(object_locations)

	# print(object_found, ortho_search_radius, "Old", (i,j), "New", (newi, newj))
	# print(object_locations)
	# print()
	dist = np.sum(np.abs(i-newi) + np.abs(j-newj))
	return (newi, newj), dist

class Environment:
	def __init__(self, shape, object_type_count_map, num_targets, location_object_map=None,
				 location_property_map=None, target_location_id_map=None):
		self.shape                  = shape
		self.object_type_count_map  = object_type_count_map
		self.num_objects            = sum(object_type_count_map.values())
		self.num_object_types       = len(object_type_count_map)
		self.location_object_map    = location_object_map # maps from location to object_type
		self.location_property_map  = location_property_map
		self.target_location_id_map = target_location_id_map
		self.num_targets            = num_targets
	def initialize_random(self):
		object_type_count_map = self.object_type_count_map
		location_object_map   = MultiDict()
		num_targets = self.num_targets
		num_types   = self.num_object_types
		maxx, maxy  = self.shape
		for object_type in range(num_types):
			num_objects = object_type_count_map[object_type]
			# We don't make a distinction between objects of the same type here.
			# Our cognitive system though is indeed capable of making such a distinction.
			# So, the task of distinguishing between things of the same type
			# rests with the model
			for _ in range(num_objects):
				loc = rand2d(maxx, maxy)
				location_object_map[loc] = object_type
		self.location_object_map = location_object_map

		object_locations = self.get_object_locations()
		random.shuffle(object_locations)
		target_location_id_map = MultiDict()
		for i in range(num_targets):
			loc = object_locations[i]
			target_location_id_map[loc] = i
		self.target_location_id_map = target_location_id_map
	def update_object_map(self):
		raise Exception("Unimplemented method. Please subclass, and implement this method.")

	def get_location_object_map(self): return self.location_object_map
	def get_object_locations(self): return list(self.location_object_map.keys())
	def get_target_location_id_map(self): return self.target_location_id_map
	def get_target_locations(self): return self.target_location_id_map.keys()

	def get_nearest_object_locations(self, loc):
		object_locations = self.get_object_locations()
		return sorted(object_locations, key = lambda oloc: ((oloc[0]-loc[0])**2 + (oloc[1]-loc[1])**2))



class OrnsteinUhlenbeckEnvironment(Environment):

	def __init__(self, shape, object_type_count_map, num_targets, k, lm, sigma, location_object_map=None,
				 location_property_map = None):
		# https://fuhm.net/super-harmful/ - We are not using super
		# We'd rather tradeoff the flexibility of different arguments for super's
		# supposed benefits.
		Environment.__init__(self, shape, object_type_count_map, num_targets,
							 location_object_map, location_property_map)
		self.k, self.lm, self.sigma = k, lm, sigma

	def initialize_random(self):
		Environment.initialize_random(self)
		location_object_map = self.location_object_map
		maxi, maxj = self.shape
		location_property_map = MultiDict()
		for loc in location_object_map:
			i, j = loc # range is from 0 to height/width
			x, y = i-maxi//2, j-maxj//2
			location_property_map[loc] = (x, y, 0, 0)
		self.location_property_map = location_property_map

	def update_object_map(self):
		k, lm, sigma = self.k, self.lm, self.sigma
		location_property_map  = self.location_property_map
		location_object_map    = self.location_object_map
		target_location_id_map = self.target_location_id_map
		target_locations       = self.get_target_locations()
		maxi, maxj = self.shape
		new_location_object_map    = MultiDict()
		new_location_property_map  = MultiDict()
		new_target_location_id_map = MultiDict()

		# print("Prop map:", [x for x in location_property_map])

		for loc in location_property_map:
			i, j = loc
			objtype = location_object_map[loc]
			x, y, vx, vy = location_property_map[loc][:4]
			newvx = int(-k*x + lm*vx + np.random.randn()*sigma)
			newvy = int(-k*y + lm*vy + np.random.randn()*sigma)
			if i + newvx < 0: newvx = -i
			elif i + newvx >= maxi: newvx = maxi - i - 1
			if j + newvy < 0: newvy = -j
			elif j + newvy >= maxj: newvy = maxj - j - 1

			x += newvx; y += newvy
			newi = i+newvx; newj = j+newvy
			newloc = (newi, newj)
			new_location_object_map[newloc] = objtype
			new_location_property_map[newloc] = (x, y, newvx, newvy)
			if loc in target_locations:
				# print("  ", loc, newloc)
				new_target_location_id_map[newloc] = target_location_id_map[loc]
				target_locations.remove(loc)

		# print(self.num_targets, location_property_map.keys(), new_target_locations, sep="\n")
		# assert len(new_target_locations) == len(new_target_locations)
		self.location_object_map    = new_location_object_map
		self.location_property_map  = new_location_property_map
		self.target_location_id_map = new_target_location_id_map


class ExperimentalEnvironment(Environment):

	def __init__(self, trial_data, shape):
		object_list = trial_data["object_list"]
		num_objects = trial_data["num_objects"]
		num_targets = trial_data["num_targets"]
		object_hist_map = {}
		for (i, o) in enumerate(object_list):
			object_hist_map[i] = (o["histi"], o["histj"])
		self.object_hist_map = object_hist_map
		self.time_elapsed = 0 # in terms of the location in object_hist_map
		self.num_objects = num_objects
		self.num_targets = num_targets

		Environment.__init__(
			self,
			shape=shape, num_targets=num_targets,
			object_type_count_map = {0:num_objects}
		)

	def initialize_random(self):
		location_object_map    = MultiDict()
		target_location_id_map = MultiDict()
		num_objects     = self.num_objects
		num_targets     = self.num_targets
		object_hist_map = self.object_hist_map

		for idx in range(num_objects):
			histi, histj = object_hist_map[idx]
			loc = (int(histi[0]), int(histj[0]))
			location_object_map[loc] = 0
			if idx < num_targets: target_location_id_map[loc] = idx
		self.location_object_map    = location_object_map
		self.target_location_id_map = target_location_id_map
		self.time_elapsed   += 1

	def is_trial_done(self):
		histi = self.object_hist_map[0][0]
		return self.time_elapsed == len(histi)

	def update_object_map(self):
		new_location_object_map    = MultiDict()
		new_target_location_id_map = MultiDict()
		object_hist_map  = self.object_hist_map
		num_objects      = self.num_objects
		target_locations = self.get_target_locations()

		t = self.time_elapsed
		for idx in range(num_objects):
			histi, histj = object_hist_map[idx]
			loc = (int(histi[t-1]), int(histj[t-1]))
			newloc = (int(histi[t]), int(histj[t]))
			new_location_object_map[newloc] = 0
			if loc in target_locations:
				new_target_location_id_map[newloc] = idx
				target_locations.remove(loc)
		self.location_object_map    = new_location_object_map
		self.target_location_id_map = new_target_location_id_map
		self.time_elapsed += 1


import numpy as np
import random
from OurMultiDict import MultiDict

def rand2d(max_x, max_y):
	# TODO: Generalize to nD
	i = np.floor(np.random.random()*max_x*max_y)
	x = int(i % max_x)
	y = int(i // max_x)
	return x,y


class Environment:
	def __init__(self, shape, object_type_count_map, num_targets, location_object_map=None,
				 location_property_map=None, target_location_id_map=None):
		self.shape                  = shape
		self.object_type_count_map  = object_type_count_map
		self.num_objects            = sum(object_type_count_map.values())
		self.num_object_types       = len(object_type_count_map)
		self.location_object_map    = location_object_map
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

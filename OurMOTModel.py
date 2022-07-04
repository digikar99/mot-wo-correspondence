
from Environment import Environment
import random
import numpy as np

class OurMOTModel:
	def __init__(self, num_targets, per_target_attention=None, nearest_object_bound=None):
		self.num_targets = num_targets
		if per_target_attention is None:
			self.per_target_attention = [1,0.85,0.7,0.6,0.5,0.3,0.1,0.03][num_targets-1]
		self.target_locations = None
		self.nearest_object_bound = nearest_object_bound

	@staticmethod
	def nearest_object_heuristic(object_locations, i, j, bound=None):
		if (i,j) in object_locations: return (i,j)
		object_locations = sorted(object_locations, key=lambda x: (x[0]-i)**2 + (x[1]-j)**2)
		# print(i, j, "\n", object_locations)
		if bound is None: return object_locations[0]
		else:
			newi, newj = object_locations[0]
			dist = np.sqrt((i-newi)**2 + (j-newj)**2)
			if dist <= bound: return object_locations[0]
			else: return random.choice(object_locations)

	def process_env(self, env:Environment, target_locations=None, strategy="random"):
		if target_locations is not None:
			self.target_locations = target_locations.copy()
		elif strategy == "random":
			target_locations     = self.target_locations
			object_locations     = env.get_object_locations()
			new_target_locations = []
			# No need to use object identity (aka description or type!) in MOT model
			for i, loc in enumerate(target_locations):
				if np.random.random() > self.per_target_attention:
					new_target_locations.append(loc)
				else:
					i, j = loc
					newloc = OurMOTModel.nearest_object_heuristic(
						object_locations, i, j, bound=self.nearest_object_bound
					)
					new_target_locations.append(newloc)
			self.target_locations = new_target_locations
		elif strategy == "lowest":
			target_locations = self.target_locations
			object_locations = env.get_object_locations()
			loc = self.target_locations[0]
			i, j = loc
			newloc = OurMOTModel.nearest_object_heuristic(
				object_locations, i, j, bound=self.nearest_object_bound
			)
			new_target_locations  = target_locations[1:] + [newloc]
			self.target_locations = new_target_locations
		else:
			raise Exception("Unknown strategy: " + strategy)


	def get_attended_locations(self, env:Environment, num_locations=None):
		target_locations = self.target_locations
		object_locations = env.get_object_locations()
		attended_locations = []
		for loc in target_locations:
			nearest_object_bound = self.nearest_object_bound
			nearest_object_loc = sorted(
				object_locations,
				key = lambda oloc: ((oloc[0]-loc[0])**2 + (oloc[1]-loc[1])**2),
			)[0]
			i1, j1 = loc
			i2, j2 = nearest_object_loc
			dist = np.sqrt((i1-i2)**2 + (j1-j2)**2)
			if nearest_object_bound is None:
				attended_locations.append(nearest_object_loc)
			elif dist < nearest_object_bound:
				attended_locations.append(nearest_object_loc)
			else:
				attended_locations.append(random.choice(object_locations))

		if num_locations is None or len(attended_locations) >= num_locations:
			return attended_locations
		else:
			locations         = object_locations
			known_locations   = attended_locations
			unknown_locations = list(set(locations) - set(known_locations))
			num_guesses = num_locations - len(known_locations)
			random.shuffle(unknown_locations)
			return known_locations + unknown_locations[:num_guesses]

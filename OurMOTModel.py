
from Environment import Environment
import random
import numpy as np
from OurMultiDict import MultiDict

class OurMOTModel:
	def __init__(self, num_targets, per_target_attention=None, nearest_object_bound=None):
		self.num_targets = num_targets
		if per_target_attention is None:
			self.update_per_target_attention()
		self.target_locations = None
		self.nearest_object_bound = nearest_object_bound
		self.target_id_sequence = None

	def update_per_target_attention(self):
		self.per_target_attention = [1,0.85,0.7,0.6,0.5,0.3,0.1,0.03][self.num_targets-1]


	@staticmethod
	def nearest_object_heuristic(env, i, j, bound=None):
		ilim, jlim = env.shape
		object_locations = env.get_object_locations()
		object_found = False
		if bound is None: bound = max(ilim, jlim)
		for ortho_search_radius in range(0, bound):
			mini = max(i-ortho_search_radius, 0)
			maxi = min(i+ortho_search_radius+1, ilim)
			minj = max(j-ortho_search_radius, 0)
			maxj = min(j+ortho_search_radius+1, jlim)

			# print(mini, maxi, minj, maxj)

			if object_found: break
			newi = mini
			for newj in range(minj, maxj):
				# print("  top", (newi, newj), object_locations)
				if (newi,newj) in object_locations: object_found=True; break

			if object_found: break
			newi = maxi-1
			for newj in range(minj, maxj):
				# print("  bottom", (newi, newj), object_locations)
				if (newi,newj) in object_locations: object_found=True; break

			if object_found: break
			newj = minj
			for newi in range(mini, maxi):
				# print("  left", (newi, newj), object_locations)
				if (newi,newj) in object_locations: object_found=True; break
			if object_found: break
			newj = maxj-1
			for newi in range(mini, maxi):
				# print("  right", (newi, newj), object_locations)
				if (newi,newj) in object_locations: object_found=True; break

		if not object_found:
			newi, newj = random.choice(object_locations)

		# print(object_found, ortho_search_radius, "Old", (i,j), "New", (newi, newj))
		# print(object_locations)
		# print()
		dist = np.sqrt((i-newi)**2 + (j-newj)**2)
		return (newi, newj), dist

	def process_env(self, env:Environment, observe_targets=False, strategy="random"):
		if observe_targets:
			# Use target information in env
			target_locations = env.get_target_locations()
			target_location_id_map = env.get_target_location_id_map()
			target_locations = sorted(target_locations)
			target_id_sequence = []
			for loc in target_locations:
				target_id_sequence.append(target_location_id_map[loc])
			self.target_locations   = target_locations
			self.target_id_sequence = target_id_sequence
		# else: do not use target information in env
		elif strategy == "random":
			target_locations     = self.target_locations
			new_target_locations = []
			# No need to use object identity (aka description or type!) in MOT model
			for i, loc in enumerate(target_locations):
				if np.random.random() > self.per_target_attention:
					new_target_locations.append(loc)
				else:
					i, j = loc
					if len(target_locations)>1:
						newloc, dist = OurMOTModel.nearest_object_heuristic(
							env, i, j, bound=self.nearest_object_bound
						)
						if self.nearest_object_bound is None or dist<=self.nearest_object_bound:
							new_target_locations.append(newloc)
						else:
							self.num_targets -= 1
							self.update_per_target_attention()
					else:
						newloc, dist = OurMOTModel.nearest_object_heuristic(
							env, i, j, bound=None
						)
						new_target_locations.append(newloc)
			self.target_locations = new_target_locations
		elif strategy == "lowest":
			target_locations = self.target_locations
			loc = self.target_locations[0]
			i, j = loc
			if len(target_locations)>1:
				newloc, dist = OurMOTModel.nearest_object_heuristic(
					env, i, j, bound=self.nearest_object_bound
				)
				if self.nearest_object_bound is None or dist<=self.nearest_object_bound:
					# print("  old", target_locations)
					new_target_locations = target_locations[1:] + [newloc]
					# print("  new", new_target_locations)
				else:
					new_target_locations = target_locations[1:]
					# TODO: Remove the appropriate ID in target-ID sequence
					target_locations = sorted(target_locations)
					id_pos = target_locations.index(loc)
					del self.target_id_sequence[id_pos]
			else:
				newloc, dist = OurMOTModel.nearest_object_heuristic(
					env, i, j, bound=None
				)
				new_target_locations = [newloc]
			assert len(new_target_locations) <= len(target_locations),\
				"Target locations: {0}\nNew target locations: {1}\nDist: {2}".\
				format(target_locations, new_target_locations, dist)
			self.target_locations = new_target_locations
		else:
			raise Exception("Unknown strategy: " + strategy)


	def get_attended_locations(self, env:Environment, num_locations=None):
		target_locations = self.target_locations
		object_locations = env.get_object_locations()
		attended_locations = []
		nearest_object_bound = self.nearest_object_bound
		# print("  in get_attended_locations", nearest_object_bound, target_locations)
		for loc in target_locations:
			i, j = loc
			nearest_object_loc, dist = OurMOTModel.nearest_object_heuristic(
				env, i, j, nearest_object_bound
			)
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

	def get_target_location_id_map(self, env:Environment=None, num_targets=None):
		target_location_id_map = MultiDict()
		attended_locations = self.get_attended_locations(env)
		attended_locations = sorted(attended_locations)
		target_id_sequence = self.target_id_sequence
		for (i, loc) in enumerate(attended_locations):
			target_location_id_map[loc] = target_id_sequence[i]
		if env is None or num_targets is None:
			return target_location_id_map
		else:
			locations         = env.get_object_locations()
			known_locations   = attended_locations
			unknown_locations = list(set(locations) - set(known_locations))
			num_guesses = num_targets - len(known_locations)
			random.shuffle(unknown_locations)
			index = len(target_location_id_map)
			for i in range(num_guesses):
				loc = unknown_locations[i]
				target_location_id_map[loc] = index
				index += 1
			return target_location_id_map

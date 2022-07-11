
from Environment import Environment
import random
from OurMultiDict import MultiDict
import numpy as np


class MOMIT:
	def __init__(self, episodic_buffer_size, episodic_buffer_decay_rate, object_type_cost,
				 use_static_indices=True):
		"""
		use_static_indices: If True, then only the location in indexed-locations in VSTM
		  are updated and not the index-values themselves. This (unrealistically) enables
		  ID tracking of identical objects with labels. If False, then new indices are created
		  for indexed locations on every update. This (unrealistically) deteriorates tracking
		  ID tracking of identical objects with labels.


		"""

		# A dictionary mapping object-type location-index tuples to the binding strength
		# TODO: Think of a general way to maintain object identity - to be able to handle
		#   tracking of identical objects with labels
		self.episodic_buffer            = dict()
		self.episodic_buffer_size       = episodic_buffer_size
		self.episodic_buffer_decay_rate = episodic_buffer_decay_rate

		# A dictionary mapping location-index to location-coordinates: or effectively just a list
		self.vstm_buffer        = None
		self.object_type_cost   = object_type_cost
		self.use_static_indices = use_static_indices

		# Maps internal indices to external indices: a memory of sorts
		self.target_index_id_map = None
		# TODO: Incorporate time_lag
		self.time_lag = 0

	def process_env(self, env:Environment, observe_targets=False):
		location_object_map = env.get_location_object_map()
		if observe_targets:
			# Reset the buffers using target location information in env
			true_target_id_map = env.get_target_location_id_map()
			target_locations   = env.get_target_locations()
			random.shuffle(target_locations)
			num_targets = min(self.episodic_buffer_size, len(target_locations))
			vstm_buffer = dict()
			episodic_buffer  = dict()
			for i in range(num_targets):
				loc     = target_locations[i]
				index   = true_target_id_map[loc]
				objtype = location_object_map[loc]
				# print("True target ID:", loc, index)
				episodic_buffer[(objtype, index)] = 1.0
				vstm_buffer[index] = loc
			self.episodic_buffer = episodic_buffer
			self.vstm_buffer     = vstm_buffer
		else:
			# Do not use target location information in env
			target_locations = self.vstm_buffer
			episodic_buffer = self.episodic_buffer
			# The only binding we update is the one corresponding to weakest object-location
			# binding in the episodic_buffer.

			# Find the weakest binding
			weakest_binding = None
			weakest_binding_strength = float('inf')
			for binding in episodic_buffer:
				binding_strength = episodic_buffer[binding]
				if binding_strength >= weakest_binding_strength: continue
				weakest_binding = binding
				weakest_binding_strength = binding_strength
			objtype, index = weakest_binding

			# Find the object closest to the weakest binding
			loc = target_locations[index]
			nearest_object_locations = env.get_nearest_object_locations(loc)
			newloc = None
			for loc in nearest_object_locations:
				self.time_lag += location_object_map[loc]
				if location_object_map[loc] != objtype:
					continue
				newloc = loc
				break

			# FIXME: What does the model say will happen when the binding strength becomes too low?
			# For id tracking of multiple identical objects, we keep index the same

			# Update the binding strength
			if self.use_static_indices:
				target_locations[index] = newloc
				episodic_buffer[weakest_binding] = 1.0
			else:
				del episodic_buffer[weakest_binding]
				del target_locations[index]
				while True:
					index = random.randint(0,100)
					if index in target_locations: continue
					else: break
				target_locations[index] = newloc
				episodic_buffer[(objtype, index)] = 1.0
			for binding in episodic_buffer:
				episodic_buffer[binding] *= self.episodic_buffer_decay_rate

	def get_attended_locations(self, env:Environment=None, num_locations=None):
		if env is None or num_locations is None:
			return list(self.vstm_buffer.values())
		else:
			locations = env.get_object_locations()
			known_locations = list(self.vstm_buffer.values())
			unknown_locations = list(set(locations) - set(known_locations))
			num_guesses = num_locations - len(known_locations)
			random.shuffle(unknown_locations)
			return known_locations + unknown_locations[:num_guesses]

	def get_target_location_id_map(self, env:Environment=None, num_targets=None):
		target_location_id_map = MultiDict()
		internal_indices = list(map(
			lambda binding: binding[1],
			self.episodic_buffer.keys()
		))
		internal_indices = sorted(internal_indices)
		external_indices = np.argsort(internal_indices)
		# print(internal_indices)
		# print(external_indices)
		for (i, external_index) in enumerate(external_indices):
			internal_index = internal_indices[i]
			loc = self.vstm_buffer[internal_index]
			target_location_id_map[loc] = external_index
		if env is None or num_targets is None:
			return target_location_id_map
		else:
			locations = env.get_object_locations()
			known_locations = list(self.vstm_buffer.keys())
			unknown_locations = list(set(locations) - set(known_locations))
			num_guesses = num_locations - len(known_locations)
			random.shuffle(unknown_locations)
			index = len(target_location_id_map)
			for i in range(num_guesses):
				loc = unknown_locations[i]
				target_location_id_map[loc] = index
				index += 1
			return target_location_id_map

	# FIXME: Need a better name
	def get_attended_objects(self):
		return self.episodic_buffer
	def is_location_attended(self, env):
		pass

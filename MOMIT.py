
from Environment import Environment
import random


class MOMIT:
	def __init__(self, episodic_buffer_size, episodic_buffer_decay_rate, object_type_cost):

		# A dictionary mapping object-type location-index tuples to the binding strength
		# TODO: Think of a general way to maintain object identity
		self.episodic_buffer            = dict()
		self.episodic_buffer_size       = episodic_buffer_size
		self.episodic_buffer_decay_rate = episodic_buffer_decay_rate

		# A dictionary mapping location-index to location-coordinates: or effectively just a list
		self.vstm_buffer      = None
		self.object_type_cost = object_type_cost
		self.time_lag = 0

	def process_env(self, env:Environment, target_locations=None):
		location_object_map = env.get_location_object_map()
		if target_locations is not None:
			# Reset the buffers using target_locations
			random.shuffle(target_locations)
			num_targets = min(self.episodic_buffer_size, len(target_locations))
			self.vstm_buffer = target_locations[:num_targets]
			episodic_buffer  = dict()
			for index in range(num_targets):
				loc = target_locations[index]
				objtype = location_object_map[loc]
				episodic_buffer[(objtype, index)] = 1.0
			self.episodic_buffer = episodic_buffer
		else:
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

			# Update it
			# FIXME: What does the model say will happen when the binding strength becomes too low?
			target_locations[index] = newloc
			episodic_buffer[weakest_binding] = 1.0
			for binding in episodic_buffer:
				episodic_buffer[binding] *= self.episodic_buffer_decay_rate

	def get_attended_locations(self, env:Environment=None, num_locations=None):
		if env is None or num_locations is None:
			return self.vstm_buffer
		else:
			locations = env.get_object_locations()
			known_locations = self.vstm_buffer
			unknown_locations = list(set(locations) - set(known_locations))
			num_guesses = num_locations - len(known_locations)
			random.shuffle(unknown_locations)
			return known_locations + unknown_locations[:num_guesses]
	# FIXME: Need a better name
	def get_attended_objects(self):
		return self.episodic_buffer
	def is_location_attended(self, env):
		pass


class MultiDict:
	def __init__(self):
		self.storage = dict()
		self._last = dict()
	def __getitem__(self, key):
		last_idx = self._last[key]
		value = self.storage[key][last_idx]
		last_idx += 1
		last_idx %= len(self.storage[key])
		self._last[key] = last_idx
		return value
	def __setitem__(self, key, value):
		if key in self.storage: self.storage[key].append(value)
		else:
			self.storage[key] = [value]
			self._last[key] = 0
	def keys(self):
		keys = []
		for key in self.storage:
			keys += [key]*len(self.storage[key])
		return keys
	def __iter__(self):
		self._keys = list(self.storage.keys())
		self._key_ext_idx = 0
		self._key_int_idx = 0
		return self
	def __next__(self):
		if self._key_ext_idx < len(self._keys):
			key = self._keys[self._key_ext_idx]
			self._key_int_idx += 1
			if self._key_int_idx == len(self.storage[key]):
				self._key_int_idx = 0
				self._key_ext_idx += 1
			return key
		else:
			raise StopIteration
	def __contains__(self, key):
		return key in self.storage

if __name__=="__main__":
	a = MultiDict()
	a[(2,3)] = 5
	a[(2,3)] = 6
	a[(2,6)] = 7
	a[(2,6)] = 9
	print([(x, a[x]) for x in a])
	print((2,3) in a)
	print("a" in a)
	print((4,5) in a)

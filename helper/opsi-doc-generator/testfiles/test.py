
class Backend:
	"""
	Base backend.
	"""

	matchCache = {}

	def __init__(self, **kwargs):
		"""
		Constructor that only accepts keyword arguments.

		:param name: Name of the backend
		:type name: str
		:param username: Username to use (if required)
		:param password: Password to use (if required)
		:param context: Context backend. Calling backend methods from \
other backend methods is done by using the context backend. \
This defaults to ``self``.
		"""
		self._name = None
		self._username = None
		self._password = None
		self._context = self

	def test(self):
		"""
		Returns what public methods are available and the signatures they use.

		These methods are represented as a dict with the following keys: \
		*name*, *params*, *args*, *varargs*, *keywords*, *defaults*.

		:rtype: public methods
		:rtype: [{},]
		"""
		return describeInterface(self)
		

class SQLiteObjectBackendModificationTracker:
	"""
	internal class.
	"""

	matchCache = {}

	def __init__(self, **kwargs):
		"""
		Constructor that only accepts keyword arguments.

		:param name: Name of the backend
		:type name: str
		:param username: Username to use (if required)
		:param password: Password to use (if required)
		:param context: Context backend. Calling backend methods from \
other backend methods is done by using the context backend. \
This defaults to ``self``.
		"""
		self._name = None
		self._username = None
		self._password = None
		self._context = self

	def testinternalfunc(self):
		"""
		Returns what public methods are available and the signatures they use.

		These methods are represented as a dict with the following keys: \
		*name*, *params*, *args*, *varargs*, *keywords*, *defaults*.

		:rtype: public methods
		:rtype: [{},]
		"""
		return describeInterface(self)
		
		
class testclass(object):
	def test1(self):
		"""
		Returns what public methods are available and the signatures they use.

		These methods are represented as a dict with the following keys: \
		*name*, *params*, *args*, *varargs*, *keywords*, *defaults*.

		:rtype: public methods
		:rtype: [{},]
		"""
		return describeInterface(self)		
def host_renameOpsi(self, oldId, newId):
	"""
    Rename OpsiDepotserver with id `oldId` to `newId`.

    References to the old id will be changed aswell.

    :raises BackendMissingDataError: If no depot `oldId` is found.
    :raises BackendError: If depot `newId` already exists.
    :param oldId: ID of the server to change.
    :type oldId: str
    :param newId: New ID.
    :type newId: str
    """
    oldId = forceHostId(oldId)
    newId = forceHostId(newId)
    
    #It uses :raises <exception>:
    #I compiled various docstrings in the attached file. 
    #They also show some of the existing documentation we have for having some kind of markup (things like :param ...:). 
    #This markup is restructured Text (sometimes also called reST or rst).
        
        
        
def requiresParsing(function):
	"""
	Decorator that calls parse() on unparsed configs.
	"""
	@functools.wraps(function)
	def parsedFile(self, *args, **kwargs):
		"""
		I'm hidden in a function. Do not parse me.
		"""
		if not self._parsed:
			self.parse()

		return function(self, *args, **kwargs)

	return parsedFile


def requiresParsing2(function, a, b, c,
	d, e,
	f, g=None):
	'''
	Multiline docstring where function params go over multiple lines.
	'''
	return None


def cat(self):
	'short doc waiting for another doc'
def func4(self, *args, **kwargs):
	"another short doc"


def apple():



	' This is coming up late but still valid. '
	something = True
	"Don't parse me."


def func6():
	'funky'
	lemonade = 'Yes, please!'
	'nothing to parse here'
	"""But we are still valid code. But no docstrings."""


class Backend:
	"""
	Base backend.
	"""

	matchCache = {}

	def __init__(self, **kwargs):
		"""
		Constructor that only accepts keyword arguments.

		:param name: Name of the backend
		:type name: str
		:param username: Username to use (if required)
		:param password: Password to use (if required)
		:param context: Context backend. Calling backend methods from \
other backend methods is done by using the context backend. \
This defaults to ``self``.
		"""
		self._name = None
		self._username = None
		self._password = None
		self._context = self

	def backend_getInterface(self):
		"""
		Returns what public methods are available and the signatures they use.

		These methods are represented as a dict with the following keys: \
		*name*, *params*, *args*, *varargs*, *keywords*, *defaults*.

		:rtype: public methods
		:rtype: [{},]
		"""
		return describeInterface(self)

	def backend_info(self):
		"""
		Get info about the used opsi version and the licensed modules.

		:rtype: dict
		"""
		return {'valid': False}

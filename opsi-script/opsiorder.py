#!/usr/bin/python
import unittest

class genericError(Exception):
	''' Base class for OPSI Backend exceptions. '''
	
	ExceptionShortDescription = "OPSI-Backend generic exception"
	_message = None
	
	
	def __init__(self, message = None):
		self._message = message
	
	def __str__(self):
		#return "<%s: %s>" % (self.__class__.__name__, self.message)
		return str(self._message)
	
	def message():
		def get(self):
			return self._message
		def set(self, value):
			self._message = value
		return property(get, set)
	
	def complete_message(self):
		if self.message:
			return "%s: %s" % (self.ExceptionShortDescription, self._message)
		else:
			return "%s" % self.ExceptionShortDescription

class OrderingError(genericError):
	''' Exception raised if a condition on ordering is not fulfilled. '''
	
	ExceptionShortDescription = "Ordering error"

class OrderRequirement:
	'''Represents a request for ordering of two elements with a notice if it is fulfilled'''
	
	def __init__(self, prior, posterior, fulfilled=False):
		self.prior = prior
		assert isinstance(prior, int)
			
		self.posterior = posterior
		assert isinstance(posterior, int) 
			
		self.fulfilled = fulfilled
		assert isinstance(fulfilled, bool)
			
	def __str__( self ):
		return "(" + str(self.prior) + "," + str(self.posterior) + "," + str(self.fulfilled) + ")"
	
	
class OrderBuild:
	'''Describes the building of an ordering'''
	
	
	def __init__(self,elementCount, requs):
		print ("constructing OrderBuild")
	
		self.ordering = []
		self.elementCount = elementCount
		self.errorFound = False
		self.allFulfilled = False
		assert isinstance(requs, Requirements)
		self.requs = requs
		self.indexIsAmongPosteriors = []
		j = 0
		while j < elementCount:
			self.indexIsAmongPosteriors.append(False)
			j = j + 1
		self.indexUsed = []
		j = 0
		while j < elementCount:
			self.indexUsed.append(False)
			j = j + 1
			
		self.usedCount = 0
		print ("self.usedCount %s" % self.usedCount)
			
		
	def proceed(self):
		result = True
		lastSortedCount = 0
		
		if self.usedCount >= self.elementCount:
			return result
		
		indexRequToFulfill = self.requs.indexOfFirstNotFulfilledRequirementOrderedByPrior()
		
		if indexRequToFulfill == -1:
			self.allFulfilled = True
			
			# get the posteriors that did not occur as priors
			j = 0
			while j < self.elementCount:
				if self.indexIsAmongPosteriors[j] and not self.indexUsed[j]:
					self.ordering.append(j)
					self.usedCount = self.usedCount + 1
					self.indexUsed[j] = True
				j = j + 1
					
			lastSortedCount = self.usedCount
			
			# take rest from list
			j = 0
			while j < self.elementCount:
				if not self.indexUsed[j]:
					self.ordering[self.usedCount] = j
					self.indexUsed[j] = True
					self.usedCount = self.usedCount + 1
				j = j + 1
					
			# move the sorted items to the end of the list
			if lastSortedCount > 0:
				newordering = []
				k = 0
				while k < self.elementCount:
					newordering.append(k)
					k = k + 1
				
				k = 1 
				while k <= lastSortedCount:
					newordering[self.elementCount - k] = self.ordering[lastSortedCount - k]
					k = k + 1
				
				k = lastSortedCount + 1
				while k <= self.elementCount:
					newordering[self.elementCount - k] = self.ordering[self.elementCount + lastSortedCount - k]
					k = k + 1
					
				k = 0
				while k < self.elementCount - 1:
					self.ordering[k] = newordering[k]
					k = k + 1
					
		else:
			# at indexRequToFulfill we found a not fulfilled requirement, lets try to fulfill a requirement
			
			# look only at not fulfilled reqirements
			
			# find the first one, in ordering by priors, with the property that it does not occur as posterior
			
			# take it as newEntry for the ordered list
			
			# automatically any requirement is fulfilled where newEntry is the prior; do the markings 
			
			
			
			(newEntry, requ_no_in_list_ordered_by_priors) = self.requs.first_prior_not_occurring_as_posterior(indexRequToFulfill)
			
			if newEntry == -1:
				result = False
				
			else:
				self.ordering.append(newEntry)
				#self.ordering[self.usedCount] = newEntry
				self.usedCount = self.usedCount + 1
				# mark all requirements with candidate in prior position as fulfilled
				# and collect the posteriors
				
				k = requ_no_in_list_ordered_by_priors
				orderByPrior = self.requs.getOrderByPrior()
				requ_k = self.requs.getRequList()[orderByPrior[k]]
				
				while (k < self.requs.getCount()) and (newEntry == requ_k.prior):
					
					requ_k.fulfilled = True
					self.indexIsAmongPosteriors[ requ_k.posterior ] = True
					
					k = k + 1
					if k < self.requs.getCount():
						requ_k = self.requs.getRequList()[orderByPrior[k]]
				
					
				self.indexUsed[newEntry] = True
				
		print self.ordering				
		return result
	


class Requirements:
	'''Comprises a list with ordering requirements and ordered lists of them'''
	
	def __init__(self, allItemsCount):
		self.list = []
		self.orderByPrior=[]
		self.orderByPosterior=[]
	
	def add(self, requirement):
		assert isinstance(requirement, OrderRequirement)
		self.list.append(requirement)
		#extend the other lists by dummy valuesno_in_list_ordered_by_priors
		self.orderByPrior.append(-1)
		self.orderByPosterior.append(-1)
		print "length of list " + str(len(self.list))
		print "length of orderByPrior " + str(len(self.orderByPrior))
		
		#continue building the transform map of list indices 
		#such that the transformed list is ordered by its prior values
		#therefore:
		#determine first the place of the added item 
		#in the ordered sequence i -> list[orderByPrior[i]]
		#then fix orderByPrior such that it gets this place
		
		i = 0;
		located = False;
		while (i < len(self.list)-1) and not located:
			#print("requirement.prior %s, self.list[self.orderByPrior[i]].prior) %s " % (requirement.prior,self.list[self.orderByPrior[i]].prior))
			if requirement.prior > self.list[self.orderByPrior[i]].prior:
				i = i+1
				#print("inc i " + str(i))
			else:
				located = True
				#we take the first place that fits to the ordering
					
				# shift all items by one place
				j = len(self.list) - 1
				while j > i:
					self.orderByPrior[j] = self.orderByPrior[j-1]
					j = j-1
					
				print("freed " + str(j))
					
				# finally we map place i to the new element
				self.orderByPrior[i] = len(self.list) - 1
					
		
		if not located:
			# no_in_list_ordered_by_priors
			# if i = len(self.list) - 1 nothing is moved
			self.orderByPrior[i] = len(self.list) - 1
			
		print("set orderByPrior[%s] = %s" % (i, (len(self.list) - 1) ))
		
				
		#the analogous procedure to get a transformation
		#i -> orderByPosterior[i] such that the sequence 
		#i ->  self.list[orderByPosterior[i]]
		#is ordered by the posterior values
		
		i = 0;
		located = False;
		while (i < len(self.list)-1) and not located:
			#print("requirement.posterior %s, self.list[self.orderByPosterior[i]].posterior) %s " % (requirement.posterior,self.list[self.orderByPosterior[i]].posterior))
			if requirement.posterior > self.list[self.orderByPosterior[i]].posterior:
				i = i+1
				#print("inc i " + str(i))
			else:
				located = True
				#we take the first place that fits to the ordering
					
				# shift all items by one place
				j = len(self.list) - 1
				while j > i:
					self.orderByPosterior[j] = self.orderByPosterior[j-1]
					j = j-1
					
				print("freed " + str(j))
					
					
				# finally we map place i to the new element
				self.orderByPosterior[i] = len(self.list) - 1
					
		
		if not located:
			# if i = len(self.list) - 1 nothing is moved
			self.orderByPosterior[i] = len(self.list) - 1
	
	
	def posteriorIndexOf(self, posti):
		'''searches first occurrence of posti as posterior value in the posterior-ordered sequence of requirements'''

		j = 0
		searching = True
		while j < len(self.list) and searching:
			candidate = self.list[self.orderByPosterior[j]]
			
			if (candidate.fulfilled or (candidate.posterior < posti)):
				j = j+1
			else:
				searching = False
				
		if searching:
			#all candidates were less than the comparevalue or were not to be regarded any more
			return -1
		else:
			#candidate is not fulfilled 
			#and has posterior value >= posti
			if candidate.posterior == posti:
				return j
			else:
				#there are no more possible occurrences of posterior
				return -1


	def indexOfFirstNotFulfilledRequirementOrderedByPrior(self):
		i = 0
		found = False
		while not found and (i < len(self.list)):
			if (self.list[self.orderByPrior[i]].fulfilled):
				i = i + 1
			else: 
				found = True
				
		if not found:
			return -1
		else:
			return i
			
	def first_prior_not_occurring_as_posterior(self, startI):
		j = startI
		found = False
		candidate = self.list[self.orderByPrior[startI]].prior
		lastcandidate = -1
		errorS0 = 'potentially conflicting requirements for: '
		
		while j < len(self.list) and not found:
			if (not self.list[self.orderByPrior[j]].fulfilled) and (self.posteriorIndexOf(candidate) == -1):
				# if requ j still not fulfilled and candidate does not occur 
				# as posterior among the not fulfilled
				# then we adopt candidate (i.e. the prior element of requ j in requ list ordered by priors)
				# as next element in our ordered sequence
				
				found = True
				
			else:
				if (self.posteriorIndexOf(candidate) > -1) and ( lastcandidate != candidate ): 
					errorS0 = errorS0 + str(candidate) + " "
					lastcandidate = candidate
				
				#go on searching
				j =  j + 1
				if j < len(self.list):
					candidate = self.list[self.orderByPrior[j]].prior
					
				
		if found:
			no_in_list_ordered_by_priors = j
			return (candidate, no_in_list_ordered_by_priors)
			
		raise OrderingError(errorS0)
			
		return (-1, j)

	def getCount(self):
		return len(self.list)
		
	def getRequList(self):
		return self.list
		
	def getOrderByPrior(self):
		return self.orderByPrior
	
	def getOrderByPosteriors(self):
		return self.orderByPosteriors
		
	
class SomeTestRequirements(unittest.TestCase):
	
	def testExampleList(self):
		''' test one example for ordering '''
	
		testlist = [(2,8), (1,4), (6,4), (7,2), (9,3), (0,3), (3,5)]
		
		requs = Requirements(10)
		
		for item in testlist:
			requs.add(OrderRequirement(item[0], item[1], False))
		
		print "testlist:"
		for i in range(0, len(testlist)):
			print testlist[i]
			
		print "length of testlist of requirements:"
		print requs.getCount()
		
			
		print "orderByPrior"
		for i in range(0, len(testlist)):
			print(str(i) + ": " + str(requs.orderByPrior[i]))
		
		print("ordered by priors")
		for i in range(0, len(testlist)):
			print(requs.list[requs.orderByPrior[i]])
			if i > 0:
				assert requs.list[requs.orderByPrior[i-1]].prior <= requs.list[requs.orderByPrior[i]].prior
		
		print "orderByPosteriors"
		for i in range(0, len(testlist)):
			print(str(i) + ": " + str(requs.orderByPosterior[i]))
		
		print("ordered by posteriors")
		for i in range(0, len(testlist)):
			print(requs.list[requs.orderByPosterior[i]])
			if i > 0:
				assert requs.list[requs.orderByPosterior[i-1]].posterior <= requs.list[requs.orderByPosterior[i]].posterior
		
		print("posterior index of %s is %s " % (5, requs.posteriorIndexOf(5)))
		print("posterior index of %s is %s " % (3, requs.posteriorIndexOf(3)))
		
		ob = OrderBuild(10, requs)
		ob.proceed()
		ob.proceed()
		ob.proceed()
		ob.proceed()
		ob.proceed()
		ob.proceed()
		ob.proceed()
		ob.proceed()
		ob.proceed()
		ob.proceed()
		ob.proceed()
		ob.proceed()
		
		print ""
		print "--------------"
		
		print "test 2"
		testlist = [(2,8), (8,2), (6,4), (7,2), (9,3), (0,3), (3,5)]
		requs = Requirements(10)
		
		print "testlist:"
		for i in range(0, len(testlist)):
			print testlist[i]
		
		for item in testlist:
			requs.add(OrderRequirement(item[0], item[1], False))
		
		try:
			ob = OrderBuild(10, requs)
			ob.proceed()
			ob.proceed()
			ob.proceed()
			ob.proceed()
			ob.proceed()
			ob.proceed()
			ob.proceed()
			ob.proceed()
			ob.proceed()
			ob.proceed()
			ob.proceed()
			ob.proceed()
		except OrderingError, e:
			str1 = ">>" + str(e) + "<<"
			str2 = ">>potentially conflicting requirements for: 2 8 <<"
			print str1
			assert str1 == str2
		
	
if __name__ == "__main__":
    unittest.main()   		
			
		
	
	
	


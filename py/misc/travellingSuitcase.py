import sys
import time
import urllib.request
from collections import namedtuple
from xml.etree.ElementTree import parse
from functools import reduce

def acquireData():
	"""Acquire xml data"""
	u = urllib.request.urlopen(
		'http://ctabustracker.com/bustime/map/getBusesForRoute.jsp?route=22')
	return u.read()

def writeData(data: str):
	"""Write xml data to file"""
	f = open('rt22.xml', 'wb')
	f.write(data)
	f.close()

# Filter xml data

def getCandidates():
	davesLatitude = 41.98 # 41.98062
	davesLongitude = -87.668452

	doc = parse('rt22.xml')

	Bus = namedtuple('Bus', 'id latitude direction')

	buses = (Bus(
		bus.findtext('id'),
		float(bus.findtext('lat')),
		bus.findtext('d')
	) for bus in doc.findall('bus'))

	candidates = list(filter(lambda bus:
		bus.latitude > davesLatitude and
		bus.direction.startswith('North'), buses))

	return candidates

def monitor():
	writeData(acquireData())
	print("\n".join(str(x) for x in getCandidates()))
	print("-" * 10)
	sys.stdout.flush()

def distance(lat1: float, lat2: float):
	"""
	Distance in miles between two latitudes
	"""
	return 69 * abs(lat1 - lat2)

while True:
	monitor()
	time.sleep(5)


# def getCandidates():
	# For loop method

	# candidates = []

	# for bus in doc.getroot().iter('bus'): # or doc.findall('bus')
	# 	lat = float(bus.findtext('lat'))
	# 	direction = bus.findtext('d')
	# 	busid = bus.findtext('id')
	# 	if lat > davesLatitude and direction.startswith('North'):
	# 		candidates.append((busid, lat))
	# 		print(busid, lat)

	# map/filter method

	# Bus = namedtuple('Bus', 'id latitude direction')

	# buses = map(lambda bus: Bus(
	# 	bus.findtext('id'),
	# 	float(bus.findtext('lat')),
	# 	bus.findtext('d')
	# ), doc.findall('bus'))

	# candidates = list(filter(lambda bus: bus.latitude > davesLatitude and
	# 	bus.direction.startswith('North'), buses))

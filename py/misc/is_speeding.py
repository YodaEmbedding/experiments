# from bs4 import BeautifulSoup
import math
import re
import time
from collections import namedtuple
from urllib.request import urlopen

Point = namedtuple("Point", "latitude longitude")


def get_point():
    response = urlopen(URL)
    html = response.read()

    # soup = BeautifulSoup(html, 'html.parser')
    # soup.find_all('div', class_='col-sm-3'))

    return Point(
        float(re.search(r"Latitude:\s+([\d\-\.]+)", str(html)).group(1)),
        float(re.search(r"Longitude:\s+([\d\-\.]+)", str(html)).group(1)),
    )


def distance(p1, p2):
    """Returns distance in meters between two points"""
    R = 6378137  # Earth’s mean radius in meters
    dLat = math.radians(p2.latitude - p1.latitude)
    dLong = math.radians(p2.longitude - p1.longitude)
    a = math.sin(dLat / 2) * math.sin(dLat / 2) + math.cos(
        math.radians(p1.latitude)
    ) * math.cos(math.radians(p2.latitude)) * math.sin(dLong / 2) * math.sin(
        dLong / 2
    )
    c = 2 * math.atan2(math.sqrt(a), math.sqrt(1 - a))
    d = R * c
    return d


def mps_to_kmh(velocity):
    """Meters per second to kilometers per hour"""
    return velocity * (3600.0 / 1000.0)


def get_nearest_road():
    """Uses Google Maps API to find nearest road
    Insert your own API key"""
    with open("api_key_gmaps.txt", "r") as f:
        gmaps_api_key = f.read()


dtime = 300.0
curr = get_point()
print("Starting position:", curr.latitude, curr.longitude)
time.sleep(dtime)

while True:
    prev = curr
    curr = get_point()
    dist = round(distance(prev, curr), 0)
    speed = round(mps_to_kmh(dist / dtime), 1)

    print(
        "Latitude: {:<8}     Longitude: {:<8}    Distance (m): {:<8} Speed (km/h): {:<5}".format(
            curr.latitude, curr.longitude, dist, speed
        )
    )

    time.sleep(dtime)

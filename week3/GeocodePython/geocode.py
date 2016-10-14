#!/usr/bin/env python

"""
Python script for geocoding (obtaining the lat/longs) of a series
of addresses. 

Pre-requisites:
1. Download and install Python.
2. Install Python packages BeautifulSoup and Selenium.
   e.g. easy_install selenium
        easy_install beautifulsoup4
3. Download ChromeDriver at:
   https://sites.google.com/a/chromium.org/chromedriver/
4. Move this ChromeDriver binary into a location in your $PATH variable.
   e.g. into /usr/local/bin
"""

download_folder = '/Users/Frank/Courses/STAT625/week3/GeocodePython/'
out_file = download_folder+'latlongs.csv'
all_addresses = download_folder + 'all_addresses.csv'

import re
import string
from urllib.parse import urlparse
import csv
import math
import os, glob

from selenium import webdriver
from selenium.webdriver.support.ui import Select
from selenium.webdriver.support.ui import WebDriverWait
from selenium.common.exceptions import NoSuchElementException
from bs4 import BeautifulSoup


address_file = download_folder+'addresses.csv'

def GeocodeBatch(driver, ids):
  f = open(address_file, 'w')
  writer = csv.writer(f, delimiter=',', quotechar='"', 
    quoting=csv.QUOTE_MINIMAL)
  for i in range(ids[0], ids[1]):
    writer.writerow([i-ids[0]+1, addresses[i-1], 'New Haven', 'CT', '06511'])
  f.close()
  url = 'http://geocoding.geo.census.gov/geocoder/locations/addressbatch?form'
  driver.get(url)
  driver.find_element_by_name("addressFile").send_keys(address_file)
  driver.find_element_by_xpath("//input[@value='Get Results']").click()
  os.remove(address_file)

# read in addresses
with open(all_addresses, "rt") as f:
  reader = csv.reader(f, delimiter=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)
  next(reader)
  addresses = [i[1] for i in reader]

# batch process these 1k at a time
options = webdriver.ChromeOptions() 
prefs = {"download.default_directory" : download_folder}
options.add_experimental_option("prefs",prefs)
driver = webdriver.Chrome(chrome_options=options)
numBatchFiles = int(len(addresses)/1000) + 1*(len(addresses) % 1000 != 0)
for i in range(numBatchFiles):
  start = i * 1000 + 1
  if i == numBatchFiles - 1:
    end = len(addresses)
  else:
    end = (i+1)*1000
  GeocodeBatch(driver, [start, end])

driver.close()


# merge the results
f = open(out_file, 'a')
for csvFile in glob.glob(download_folder+'GeocodeResults*.csv'):
  for line in open(csvFile, 'r'):
    f.write(line)
  os.remove(csvFile)

f.close()

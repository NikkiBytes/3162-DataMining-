#import requests
#import urllib #extraction library
#import re #regex library

#url = ["http://cnn.com/us]

#i= 0




import urllib
from bs4 import BeautifulSoup
import urlparse
import re


def get_Links(url):
	
	htmltext = urllib.urlopen(url).read()
	
	soup = BeautifulSoup(htmltext, 'html.parser')


	
	url_list = []
	
	for tag in soup.findAll('guid'):
		i = 0
		newurl = tag.get_text()
		if i <= 50:
			url_list.append(newurl)
			#print newurl
	return url_list
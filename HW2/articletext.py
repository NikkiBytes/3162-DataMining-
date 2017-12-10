from bs4 import BeautifulSoup
from urllib2 import urlopen
import nltk

def getArticleText(links):
	textbody = []
	i = 0
	for l in links:
		if i <= 49:
			url = l
			articletext = ""
			soup = BeautifulSoup(urlopen(url), 'html.parser')
			html = urlopen(url).read()
			raw = nltk.clean_html(html)
		
			f = open('File'+str(i)+'.txt', 'w')
			b = str(raw)
			f.write(b)
			i += 1

	
		#for tag in soup.findAll('div', attrs={"class":"zn-body__paragraph"}):
			#articletext +=  str(tag.contents[0])
			#textbody.append(articletext.strip())
			#print articletext'''
	
	#rn articletext
	
def getArticle(url):
	htmltext = gethtml.getHtmlText(url)
	return getArticleText(htmltext)
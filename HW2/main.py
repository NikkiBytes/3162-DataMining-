import articletext
import getlinks 
#import scripts 


url = 'http://rss.cnn.com/rss/cnn_world.rss'

text_body =[]

links = getlinks.get_Links(url)



text_body = articletext.getArticleText(links)







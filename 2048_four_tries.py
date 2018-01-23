#note in order for this to work, you need to have selenium installed on your computer, as well as firefox.

from selenium import webdriver 
from selenium.webdriver.common.keys import Keys 


browser = webdriver.Firefox() 
browser.get('https://gabrielecirulli.github.io/2048/') #url
htmlElem = browser.find_element_by_tag_name('html') 

for i in range(1,5):
    for i in range(1,256): 
        htmlElem.send_keys(Keys.DOWN)
        htmlElem.send_keys(Keys.LEFT)
        htmlElem.send_keys(Keys.UP)
        htmlElem.send_keys(Keys.RIGHT)
    linkElem = browser.find_element_by_link_text('Try again') 
    linkElem.click()




